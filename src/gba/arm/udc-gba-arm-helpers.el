;;; udc-gba-arm-helpers --- ARM helpers -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)
(require 'udc-gba-linker)
(require 'udc-gba-codegen)

;;;; Helper functions and macros
(defmacro u/gba/arm-toplevel (symtab sym &rest body)
  "Run BODY in a new code generation context and add the generated code to SYMTAB.
The code is placed in :code with name SYM.
No registers will be available."
  `(u/gba/toplevel ,symtab :code ,sym 'arm ,@body))

(defun u/gba/arm-push (&rest regs)
  "Generate a push of REGS to the stack."
  (u/gba/emit!
   `(stm down wb ,u/gba/arm-sp ,regs)))
(defun u/gba/arm-pop (&rest regs)
  "Generate a pop of REGS to the stack."
  (u/gba/emit!
   `(ldm wb post ,u/gba/arm-sp ,regs)))
(defmacro u/gba/arm-function (symtab sym &rest body)
  "Run BODY in a new code generation context.
Place the generated code in SYMTAB at SYM.
\(SYM is placed in the :code section).
The code will be wrapped in the function header and footer.
Callee-saved registers will be available."
  `(let* ((u/gba/codegen
            (u/gba/make-codegen
              :type 'arm
              :regs-available (copy-sequence u/gba/arm-regs-callee-saved))))
     (u/gba/emit! ,@body)
     (u/gba/codegen-extract-with-literals ,symtab :code ,sym t)))

(defun u/gba/arm-constant (r constant)
  "Generate code loading the (maximum 32-bit CONSTANT) into R."
  (unless (integerp constant)
    (error "Attempt to generate bad constant: %s" constant))
  (u/gba/scope
   (u/gba/emit! `(mov ,r ,(logand #x000000ff constant)))
   (when (> constant #xff)
     (u/gba/emit! `(orr ,r ,r ,(lsh (logand #x0000ff00 constant) -8) 12)))
   (when (> constant #xffff)
     (u/gba/emit! `(orr ,r ,r ,(lsh (logand #x00ff0000 constant) -16) 8)))
   (when (> constant #xffffff)
     (u/gba/emit! `(orr ,r ,r ,(lsh (logand #xff000000 constant) -24) 4)))))
(defun u/gba/arm-addr (r symtab sym)
  "Generate code loading the address in SYMTAB for SYM into R."
  (u/gba/arm-constant r (u/gba/symtab-entry-addr (u/gba/symtab-lookup symtab sym))))

(defun u/gba/arm-add-offset (dest reg &optional offset)
  "Move REG to DEST and add OFFSET."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit! `(mov ,dest ,reg))
    (cond
     ((u/gba/arm-reg? offset)
      (u/gba/emit! `(add ,dest ,dest ,offset)))
     ((integerp offset)
      (let ((tmp (u/gba/fresh!)))
        (u/gba/emit!
         (u/gba/arm-constant tmp offset)
        `(add ,dest ,dest ,tmp))))))))
(defun u/gba/arm-loc (symtab loc)
  "Place the address/offset pair LOC from SYMTAB in a fresh register.
Return that register."
  (let*
      ((isreg (or (u/gba/arm-reg? loc) (and (consp loc) (u/gba/arm-reg? (car loc)))))
       (sym (cond
             ((u/gba/arm-reg? loc) loc)
             ((and (consp loc) (u/gba/arm-reg? (car loc))) (car loc))
             ((keywordp loc) loc)
             ((and (consp loc) (keywordp (car loc))) (car loc))
             (t (error "Malformed symbol: %s" loc))))
       (offset (if (consp loc) (cdr loc) nil))
       (dest (u/gba/fresh!)))
    (u/gba/emit!
     (u/gba/scope
      (if isreg
          (u/gba/arm-add-offset dest sym offset)
        (let*
            ((entry (or (u/gba/symtab-lookup symtab sym) (error "Failed to find symbol: %s" sym)))
             (addr (u/gba/symtab-entry-addr entry)))
          (cond
           ((u/gba/arm-reg? offset)
            (u/gba/emit!
             (u/gba/arm-constant dest addr)
             `(add ,dest ,dest ,offset)))
           ((integerp offset)
            (u/gba/emit! (u/gba/arm-constant dest (+ addr offset))))
           ((null offset)
            (u/gba/emit! (u/gba/arm-constant dest addr)))
           (t (error "Don't know how to add offset: %s" offset)))))))
    dest))

(defun u/gba/arm-get8 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(ldr byte ,reg ,(u/gba/arm-loc symtab loc))))))

(defun u/gba/arm-get16 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(ldrh ,reg ,(u/gba/arm-loc symtab loc))))))

(defun u/gba/arm-get32 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(ldr ,reg ,(u/gba/arm-loc symtab loc))))))

(defun u/gba/arm-set8 (symtab loc x) ;; NOTE: This will not work properly in VRAM
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
   (u/gba/scope
    (let ((reg
           (cond
            ((u/gba/arm-reg? x)
             x)
            ((integerp x)
             (let ((r (u/gba/fresh!)))
               (u/gba/emit! `(mov ,r ,(logand #x000000ff x)))
               r))
            (t (error "Don't know how to write value: %s" x)))))
      (u/gba/emit! `(str byte ,reg ,(u/gba/arm-loc symtab loc)))))))

(defun u/gba/arm-set16 (symtab loc x)
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
   (u/gba/scope
    (let
        ((reg
          (cond
           ((u/gba/arm-reg? x)
            x)
           ((integerp x)
            (let ((r (u/gba/fresh!)))
              (u/gba/emit! (u/gba/arm-constant r (logand #xffff x)))
              r))
           (t (error "Don't know how to write value: %s" x)))))
      (u/gba/emit!
       `(strh ,reg ,(u/gba/arm-loc symtab loc)))))))

(defun u/gba/arm-set32 (symtab loc x)
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
   (u/gba/scope
    (let
        ((reg
          (cond
           ((u/gba/arm-reg? x)
            x)
           ((integerp x)
            (let ((r (u/gba/fresh!)))
              (u/gba/emit! (u/gba/arm-constant r x))
              r))
           (t (error "Don't know how to write value: %s" x)))))
      (u/gba/emit! `(str ,reg ,(u/gba/arm-loc symtab loc)))))))

(defun u/gba/arm-call (symtab sym &rest args)
  "Generate code calling the function at SYM in SYMTAB.
The locations in ARGS are copied to the argument registers."
  (when (> (length args) (length u/gba/regs-arg))
    (error "Attempted to call function %s with too many (%s) arguments: %s" sym (length args) args))
  (u/gba/emit!
    (u/gba/scope
      (--each (-zip-pair args u/gba/regs-arg)
        (let ((x (car it)))
          (cond
            ((u/gba/arm-reg? it)
              (u/gba/emit! `(mov ,(cdr it) ,it)))
            ((integerp x)
              (u/gba/emit! (u/gba/arm-constant (cdr it) x)))
            ((keywordp x)
              (u/gba/emit! (u/gba/arm-addr (cdr it) symtab x))
              )
            (t (error "Don't know how to pass argument: %s" x)))))
      (u/gba/emit!
        `(bl ,sym)))))

(defun u/gba/arm-write-struct (r st &rest fields)
  "Given a `u/structdef' ST, write FIELDS to the address in R."
  (let ((field-offsets (-sort (-on #'< #'cdr) (ht->alist (u/structdef-fields st))))
        (prev 0))
    (u/gba/emit!
     (u/gba/scope
      (--each field-offsets
        (when-let* ((v (plist-get fields (car it))))
          (cond
           ((integerp v) ;; integer constant
            (u/gba/emit!
             (let ((fr (u/gba/fresh!)))
               (u/gba/arm-constant fr v)
               `(str wb ,fr ,r ,(- (cdr it) prev)))))
           ((symbolp v) ;; another register
            (u/gba/emit!
             `(str wb ,v ,r ,(- (cdr it) prev))))
           ((listp v) ;; a list of bytes
            (error "TODO: pack lists of bytes into words and write them"))
           (t
            (error "Unsupported structure field value: %s" v)))
          (setf prev (cdr it))))))))

(defun u/gba/arm-write-pixel (x y r g b)
  "Write R G B pixel to X,Y."
  (u/gba/emit!
   `(,@(u/gba/arm-constant 'r0 (+ #x06000000 (* 2 (+ x (* y 240)))))
     ,@(u/gba/arm-constant
        'r1
        (logior
         (lsh (logand #b11111 r) 10)
         (lsh (logand #b11111 g) 5)
         (logand #b11111 b)))
     (str byte r1 r0)
     (mov r1 r1 (lsr 8))
     (str byte r1 r0 1))))

(defun u/gba/arm-when-cond? (cond body)
  "Run BODY if COND is set."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(b ,cond :end))
    (funcall body)
    (u/gba/emit! :end))))

(defun u/gba/arm-do-times (end k)
  "Generate a loop incrementing a counter from 0 to END - 1.
K represents the body of the loop, and is passed the counter register."
  (u/gba/emit!
   (u/gba/scope
    (let ((counter (u/gba/fresh!))
          (max (u/gba/fresh!)))
      (u/gba/emit!
       `(mov ,counter 0)
       (cond
        ((symbolp end) `(mov ,max ,end))
        ((integerp end) (u/gba/arm-constant max end)))
       :start
       `(cmp s ,counter ,counter ,max)
       `(b eq :end))
      (funcall k counter)
      (u/gba/emit!
       `(add ,counter ,counter 1)
       '(b :start)
       :end)))))

(defun u/gba/arm-button-pressed? (st button body)
  "Run BODY if BUTTON is pressed in symbol table ST."
  (let ((mask (alist-get button u/gba/button-masks)))
    (u/gba/emit!
     (u/gba/scope
      (let ((inpr (u/gba/fresh!))
            (maskr (u/gba/fresh!)))
        (u/gba/emit!
         (u/gba/arm-get16 st inpr :reg-keyinput)
         (u/gba/arm-constant maskr mask)
         `(bic s ,inpr ,maskr ,inpr)
         (u/gba/arm-when-cond? 'eq body)))))))

(defconst u/gba/arm-compile-expression-handlers
  '((+
     . (lambda (ret o0 o1)
         (unless (and o0 o1) (error "Operator + takes two arguments, was given: %s %s" o0 o1))
         `((add ,ret ,o0 ,o1))))
    (-
     . (lambda (ret o0 o1)
         (unless (and o0 o1) (error "Operator - takes two arguments, was given: %s %s" o0 o1))
         `((sub ,ret ,o0 ,o1))))
    (*
     . (lambda (ret o0 o1)
         (unless (and o0 o1) (error "Operator * takes two arguments, was given: %s %s" o0 o1))
         `((mul ,ret ,o0 ,o1))))
    (=
     . (lambda (ret o0 o1)
         (unless (and o0 o1) (error "Operator = takes two arguments, was given: %s %s" o0 o1))
         `((sub ,ret ,o0 ,o1) (rsb s ,o0 ,ret ,ret) (adc ,ret ,o0 ,ret))))
    (&
     . (lambda (ret o0 o1)
         (unless (and o0 o1) (error "Operator & takes two arguments, was given: %s %s" o0 o1))
         `((and ,ret ,o0 ,o1))))
    (|
     . (lambda (ret o0 o1)
         (unless (and o0 o1) (error "Operator | takes two arguments, was given: %s %s" o0 o1))
         `((orr ,ret ,o0 ,o1))))
    (^
     . (lambda (ret o0 o1)
         (unless (and o0 o1) (error "Operator ^ takes two arguments, was given: %s %s" o0 o1))
         `((eor ,ret ,o0 ,o1))))
    (@8
     . (lambda (ret o0 _)
         `((ldr byte ,ret ,o0))))
    (@16
     . (lambda (ret o0 _)
         `((ldrh ,ret ,o0))))
    (@32
     . (lambda (ret o0 _)
         `((ldr ,ret ,o0))))
    ))
(defun u/gba/arm-compile-expression-label-sethi-ullman (exp)
  "Return the registers needed to evaluate EXP."
  (cond
   ((keywordp exp) (cons 1 exp))
   ((symbolp exp) (cons 1 exp))
   ((integerp exp) (cons 1 exp))
   ((and (listp exp) (symbolp (car exp)))
    (let* ((children (-map #'u/gba/arm-compile-expression-label-sethi-ullman (cdr exp)))
           (sorted (-sort (-on #'> #'car) children)))
      (cons
       (--reduce-from
        (max (+ 1 (car it)) acc)
        (caar sorted)
        (cdr sorted))
       (cons (car exp) children))))
   (t (error "Unknown expression shape: %s" exp))))
(defun u/gba/arm-compile-expression-labeled-helper (regs lexp)
  "Compile LEXP into assembly code using REGS and return the result register."
  (let ((exp (cdr lexp)))
    (cond
     ((null exp) nil)
     ((keywordp exp)
      (let ((ret (car regs)))
        (u/gba/emit! `((symbol ,ret ,exp)))
        ret))
     ((symbolp exp)
      (let ((ret (car regs)))
        (u/gba/emit! `((mov ,ret ,exp)))
        ret))
     ((integerp exp)
      (let ((ret (car regs)))
        (u/gba/emit! `((const ,ret ,exp)))
        ret))
     ((and (listp exp) (symbolp (car exp)))
      (let ((ret (car regs))
            (p0 (car (cadr exp)))
            (p1 (car (caddr exp)))
            (handler
             (or
              (alist-get (car exp) u/gba/arm-compile-expression-handlers)
              (error "Error: no handler found for operator %s" (car exp)))))
        (if (and (numberp p0) (numberp p1) (> p1 p0))
            (let ((o1 (u/gba/arm-compile-expression-labeled-helper regs (caddr exp)))
                  (o0 (u/gba/arm-compile-expression-labeled-helper (cdr regs) (cadr exp))))
              (u/gba/emit! (funcall handler ret o0 o1)))
          (let ((o0 (u/gba/arm-compile-expression-labeled-helper regs (cadr exp)))
                (o1 (u/gba/arm-compile-expression-labeled-helper (cdr regs) (caddr exp))))
            (u/gba/emit! (funcall handler ret o0 o1))))
        ret))
     )))
(defun u/gba/arm-compile-expression-labeled (regs lexp)
  "Compile LEXP into assembly code using REGS and return the result register."
  (u/gba/scope
   (u/gba/arm-compile-expression-labeled-helper regs lexp)))
(defun u/gba/arm-compile-expression (symtab regs exp)
  "Compile EXP into assembly code using REGS and SYMTAB."
  (u/gba/emit!
   (u/gba/scope
    (let ((labeled (u/gba/arm-compile-expression-label-sethi-ullman exp)))
      (when (> (car labeled) (length regs))
        (error "Expression needs %s registers to compile, but only %s are available" (car labeled) regs))
      (--each (u/gba/arm-compile-expression-labeled regs labeled)
        (u/gba/emit!
         (cl-case (car it)
           (symbol (u/gba/arm-addr (cadr it) symtab (caddr it)))
           (const (u/gba/arm-constant (cadr it) (caddr it)))
           (t it))))))))
(defun u/gba/arm-expr (symtab res exp)
  "Compile EXP into assembly code using SYMTAB.
Place the result in RES."
  (let ((regs (u/gba/codegen-regs-available (u/gba/codegen))))
    (when (-contains? regs res) (error "Result register %s is available" res))
    (u/gba/arm-compile-expression symtab (cons res regs) exp)))

(defun u/gba/sprite-attr1 (sp)
  "The location of the first attributes for sprite SP."
  `(:oam . ,(* sp 8)))
(defun u/gba/sprite-attr2 (sp)
  "The location of the second attributes for sprite SP."
  `(:oam . ,(+ 2 (* sp 8))))
(defun u/gba/sprite-attr3 (sp)
  "The location of the third attributes for sprite SP."
  `(:oam . ,(+ 4 (* sp 8))))
(defun u/gba/arm-set-sprite-coords (syms sp x y)
  "Set the coordinates for sprite SP to (X, Y) in the context of SYMS."
  (u/gba/emit!
    (u/gba/scope
      (let ( (loc (u/gba/arm-loc syms (u/gba/sprite-attr1 sp)))
             (r (u/gba/fresh!)))
        (u/gba/arm-get16 syms r loc)
        (u/gba/emit! `(orr ,r ,r ,y))
        (u/gba/arm-set16 syms loc r)))
    (u/gba/scope
      (let ( (loc (u/gba/arm-loc syms (u/gba/sprite-attr2 sp)))
             (r (u/gba/fresh!)))
        (u/gba/arm-get16 syms r loc)
        (u/gba/emit! `(orr ,r ,r ,x))
        (u/gba/arm-set16 syms loc r)))))

(provide 'udc-gba-arm-helpers)
;;; udc-gba-arm-helpers.el ends here
