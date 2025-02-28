;;; udc-gba --- Compiler Upside-Down Cake - Game Boy Advance tools -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)

(add-to-list 'load-path (f-canonical "./gba/"))
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc-gba-constants)
(require 'udc-gba-assembler)
(require 'udc-gba-linker)
(require 'udc-gba-codegen)
(require 'udc-gba-image)

;;;; Helpful "macros" / codegen snippets
(defun u/gba/add-offset (dest reg &optional offset)
  "Move REG to DEST and add OFFSET."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit! `(mov ,dest ,reg))
    (cond
     ((u/gba/assemble-reg offset)
      (u/gba/emit! `(add ,dest ,dest ,offset)))
     ((integerp offset)
      (let ((tmp (u/gba/fresh!)))
        (u/gba/emit!
         (u/gba/constant tmp offset)
        `(add ,dest ,dest ,tmp))))))))
(defun u/gba/loc (symtab loc)
  "Place the address/offset pair LOC from SYMTAB in a fresh register.
Return that register."
  (let*
      ((isreg (or (u/gba/assemble-reg loc) (and (consp loc) (u/gba/assemble-reg (car loc)))))
       (sym (cond
             ((u/gba/assemble-reg loc) loc)
             ((and (consp loc) (u/gba/assemble-reg (car loc))) (car loc))
             ((keywordp loc) loc)
             ((and (consp loc) (keywordp (car loc))) (car loc))
             (t (error "Malformed symbol: %s" loc))))
       (offset (if (consp loc) (cdr loc) nil))
       (dest (u/gba/fresh!)))
    (u/gba/emit!
     (u/gba/scope
      (if isreg
          (u/gba/add-offset dest sym offset)
        (let*
            ((entry (or (u/symtab-lookup symtab sym) (error "Failed to find symbol: %s" sym)))
             (addr (u/symtab-entry-addr entry)))
          (cond
           ((u/gba/assemble-reg offset)
            (u/gba/emit!
             (u/gba/constant dest addr)
             `(add ,dest ,dest ,offset)))
           ((integerp offset)
            (u/gba/emit! (u/gba/constant dest (+ addr offset))))
           ((null offset)
            (u/gba/emit! (u/gba/constant dest addr)))
           (t (error "Don't know how to add offset: %s" offset)))))))
    dest))

(defun u/gba/get8 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(ldr byte ,reg ,(u/gba/loc symtab loc))))))

(defun u/gba/get16 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(ldrh ,reg ,(u/gba/loc symtab loc))))))

(defun u/gba/get32 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(ldr ,reg ,(u/gba/loc symtab loc))))))

(defun u/gba/set8 (symtab loc x) ;; NOTE: This will not work properly in VRAM
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
   (u/gba/scope
    (let ((reg
           (cond
            ((u/gba/assemble-reg x)
             x)
            ((integerp x)
             (let ((r (u/gba/fresh!)))
               (u/gba/emit! `(mov ,r ,(logand #x000000ff x)))
               r))
            (t (error "Don't know how to write value: %s" x)))))
      (u/gba/emit! `(str byte ,reg ,(u/gba/loc symtab loc)))))))

(defun u/gba/set16 (symtab loc x)
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
   (u/gba/scope
    (let
        ((reg
          (cond
           ((u/gba/assemble-reg x)
            x)
           ((integerp x)
            (let ((r (u/gba/fresh!)))
              (u/gba/emit! (u/gba/constant r (logand #xffff x)))
              r))
           (t (error "Don't know how to write value: %s" x)))))
      (u/gba/emit!
       `(strh ,reg ,(u/gba/loc symtab loc)))))))

(defun u/gba/set32 (symtab loc x)
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
   (u/gba/scope
    (let
        ((reg
          (cond
           ((u/gba/assemble-reg x)
            x)
           ((integerp x)
            (let ((r (u/gba/fresh!)))
              (u/gba/emit! (u/gba/constant r x))
              r))
           (t (error "Don't know how to write value: %s" x)))))
      (u/gba/emit! `(str ,reg ,(u/gba/loc symtab loc)))))))

(defun u/gba/call (symtab sym &rest args)
  "Generate code calling the function at SYM in SYMTAB.
The locations in ARGS are copied to the argument registers."
  (when (> (length args) (length u/gba/regs-arg))
    (error "Attempted to call function %s with too many (%s) arguments: %s" sym (length args) args))
  (u/gba/emit!
    (u/gba/scope
      (apply #'u/gba/burn! (-take (length args) u/gba/regs-arg))
      (--each (-zip-pair args u/gba/regs-arg)
        (let ((x (car it)))
          (cond
            ((u/gba/assemble-reg it)
              (u/gba/emit! `(mov ,(cdr it) ,it)))
            ((integerp x)
              (u/gba/emit! (u/gba/constant (cdr it) x)))
            ((keywordp x)
              (u/gba/emit! (u/gba/addr (cdr it) symtab x))
              )
            (t (error "Don't know how to pass argument: %s" x)))))
      (u/gba/emit!
        `(bl ,sym)))))

(defun u/gba/write-struct (r st &rest fields)
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
               (u/gba/constant fr v)
               `(str wb ,fr ,r ,(- (cdr it) prev)))))
           ((symbolp v) ;; another register
            (u/gba/emit!
             `(str wb ,v ,r ,(- (cdr it) prev))))
           ((listp v) ;; a list of bytes
            (error "TODO: pack lists of bytes into words and write them"))
           (t
            (error "Unsupported structure field value: %s" v)))
          (setf prev (cdr it))))))))

(defun u/gba/write-pixel (x y r g b)
  "Write R G B pixel to X,Y."
  (u/gba/emit!
   `(,@(u/gba/constant 'r0 (+ #x06000000 (* 2 (+ x (* y 240)))))
     ,@(u/gba/constant
        'r1
        (logior
         (lsh (logand #b11111 r) 10)
         (lsh (logand #b11111 g) 5)
         (logand #b11111 b)))
     (str byte r1 r0)
     (mov r1 r1 (lsr 8))
     (str byte r1 r0 1))))

(defun u/gba/when-cond? (cond body)
  "Run BODY if COND is set."
  (u/gba/emit!
   (u/gba/scope
    (u/gba/emit!
     `(b ,cond :end))
    (funcall body)
    (u/gba/emit! :end))))

(defun u/gba/do-times (end k)
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
        ((integerp end) (u/gba/constant max end)))
       :start
       `(cmp s ,counter ,counter ,max)
       `(b eq :end))
      (funcall k counter)
      (u/gba/emit!
       `(add ,counter ,counter 1)
       '(b :start)
       :end)))))

(defconst u/gba/button-masks
  '((a . #b1)
    (b . #b10)
    (select . #b100)
    (start . #b1000)
    (right . #b10000)
    (left . #b100000)
    (up . #b1000000)
    (down . #b10000000)
    (r . #b100000000)
    (l . #b1000000000)))
(defun u/gba/button-pressed? (st button body)
  "Run BODY if BUTTON is pressed in symbol table ST."
  (let ((mask (alist-get button u/gba/button-masks)))
    (u/gba/emit!
     (u/gba/scope
      (let ((inpr (u/gba/fresh!))
            (maskr (u/gba/fresh!)))
        (u/gba/emit!
         (u/gba/get16 st inpr :reg-keyinput)
         (u/gba/constant maskr mask)
         `(bic s ,inpr ,maskr ,inpr)
         (u/gba/when-cond? 'eq body)))))))

(defun u/gba/test ()
  "Test function."
  (u/gba/scope ;; len in r1, src in r2, dst in r3
   (u/gba/function-header)
   (u/gba/push 'r4)
   (u/gba/emit!
    :start
    '(sub s r1 r1 1)
    '(b lt :end)
    '(ldr post wb r4 r2 4)
    '(str post wb r4 r3 4)
    '(b :start)
    :end
    )
   (u/gba/pop 'r4)
   (u/gba/function-footer)))

(defconst u/gba/compile-expression-handlers
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
(defun u/gba/compile-expression-label-sethi-ullman (exp)
  "Return the registers needed to evaluate EXP."
  (cond
   ((keywordp exp) (cons 1 exp))
   ((symbolp exp) (cons 1 exp))
   ((integerp exp) (cons 1 exp))
   ((and (listp exp) (symbolp (car exp)))
    (let* ((children (-map #'u/gba/compile-expression-label-sethi-ullman (cdr exp)))
           (sorted (-sort (-on #'> #'car) children)))
      (cons
       (--reduce-from
        (max (+ 1 (car it)) acc)
        (caar sorted)
        (cdr sorted))
       (cons (car exp) children))))
   (t (error "Unknown expression shape: %s" exp))))
(defun u/gba/compile-expression-labeled-helper (regs lexp)
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
              (alist-get (car exp) u/gba/compile-expression-handlers)
              (error "Error: no handler found for operator %s" (car exp)))))
        (if (and (numberp p0) (numberp p1) (> p1 p0))
            (let ((o1 (u/gba/compile-expression-labeled-helper regs (caddr exp)))
                  (o0 (u/gba/compile-expression-labeled-helper (cdr regs) (cadr exp))))
              (u/gba/emit! (funcall handler ret o0 o1)))
          (let ((o0 (u/gba/compile-expression-labeled-helper regs (cadr exp)))
                (o1 (u/gba/compile-expression-labeled-helper (cdr regs) (caddr exp))))
            (u/gba/emit! (funcall handler ret o0 o1))))
        ret))
     )))
(defun u/gba/compile-expression-labeled (regs lexp)
  "Compile LEXP into assembly code using REGS and return the result register."
  (u/gba/scope
   (u/gba/compile-expression-labeled-helper regs lexp)))
(defun u/gba/compile-expression (symtab regs exp)
  "Compile EXP into assembly code using REGS and SYMTAB."
  (u/gba/emit!
   (u/gba/scope
    (let ((labeled (u/gba/compile-expression-label-sethi-ullman exp)))
      (when (> (car labeled) (length regs))
        (error "Expression needs %s registers to compile, but only %s are available" (car labeled) regs))
      (--each (u/gba/compile-expression-labeled regs labeled)
        (u/gba/emit!
         (cl-case (car it)
           (symbol (u/gba/addr (cadr it) symtab (caddr it)))
           (const (u/gba/constant (cadr it) (caddr it)))
           (t it))))))))
(defun u/gba/expr (symtab res exp)
  "Compile EXP into assembly code using SYMTAB.
Place the result in RES."
  (let ((regs (u/gba/codegen-regs-available (u/gba/codegen))))
    (when (-contains? regs res) (error "Result register %s is available" res))
    (u/gba/compile-expression symtab (cons res regs) exp)))

(defun u/gba/sprite-attr1 (sp) 
  "The location of the first attributes for sprite SP."
  `(:oam . ,(* sp 8)))
(defun u/gba/sprite-attr2 (sp)
  "The location of the second attributes for sprite SP."
  `(:oam . ,(+ 2 (* sp 8))))
(defun u/gba/sprite-attr3 (sp)
  "The location of the third attributes for sprite SP."
  `(:oam . ,(+ 4 (* sp 8))))
(defun u/gba/set-sprite-coords (syms sp x y)
  "Set the coordinates for sprite SP to (X, Y) in the context of SYMS."
  (u/gba/emit!
    (u/gba/scope
      (let ( (loc (u/gba/loc syms (u/gba/sprite-attr1 sp)))
             (r (u/gba/fresh!)))
        (u/gba/get16 syms r loc)
        (u/gba/emit! `(orr ,r ,r ,y))
        (u/gba/set16 syms loc r)))
    (u/gba/scope
      (let ( (loc (u/gba/loc syms (u/gba/sprite-attr2 sp)))
             (r (u/gba/fresh!)))
        (u/gba/get16 syms r loc)
        (u/gba/emit! `(orr ,r ,r ,x))
        (u/gba/set16 syms loc r)))))

(provide 'udc-gba)
;;; udc-gba.el ends here
