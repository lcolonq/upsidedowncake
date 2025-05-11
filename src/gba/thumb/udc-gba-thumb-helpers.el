;;; udc-gba-thumb-helpers --- Thumb helpers -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
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
(defmacro u/gba/thumb-toplevel (symtab sym &rest body)
  "Run BODY in a new code generation context and add the generated code to SYMTAB.
The code is placed in :code with name SYM.
No registers will be available."
  `(u/gba/toplevel ,symtab :code ,sym 'thumb ,@body))

(defmacro u/gba/thumb-function (symtab sym &rest body)
  "Run BODY in a new code generation context.
Place the generated code in SYMTAB at SYM.
\(SYM is placed in the :code section).
The code will be wrapped in the function header and footer.
Callee-saved registers will be available."
  `(let* ((u/gba/codegen
            (u/gba/make-codegen
              :type 'thumb
              :regs-available (copy-sequence u/gba/thumb-regs-callee-saved))))
     (u/gba/emit! ,@body)
     (u/gba/codegen-extract-with-literals ,symtab :code ,sym t)))

(defun u/gba/thumb-constant (r constant)
  "Generate code loading the (maximum 32-bit CONSTANT) into R."
  (unless (integerp constant)
    (error "Attempt to generate bad constant: %s" constant))
  (u/gba/emit! `(:const ,r ,constant)))
(defun u/gba/thumb-addr (r symtab sym)
  "Generate code loading the address in SYMTAB for SYM into R."
  (u/gba/thumb-constant r (u/gba/symtab-entry-addr (u/gba/symtab-lookup symtab sym))))

(defun u/gba/thumb-add-offset (dest reg &optional offset)
  "Move REG to DEST and add OFFSET."
  (u/gba/emit!
    (u/gba/scope
      (u/gba/emit! `(mov ,dest ,reg))
      (cond
        ((u/gba/thumb-reg? offset)
          (u/gba/emit! `(add ,dest ,dest ,offset)))
        ((integerp offset)
          (let ((tmp (u/gba/fresh!)))
            (u/gba/emit!
              (u/gba/thumb-constant tmp offset)
              `(add ,dest ,dest ,tmp))))))))
(defun u/gba/thumb-loc-base (loc)
  "Return the base component of LOC."
  (cond
    ((u/gba/thumb-reg? loc) loc)
    ((and (consp loc) (u/gba/thumb-reg? (car loc))) (car loc))
    ((keywordp loc) loc)
    ((and (consp loc) (keywordp (car loc))) (car loc))
    (t (error "Malformed location base: %s" loc))))
(defun u/gba/thumb-loc-offset (loc)
  "Return the offset component of LOC."
  (if (consp loc) (cdr loc) 0))
(defun u/gba/thumb-loc (symtab loc)
  "Place the address/offset pair LOC from SYMTAB in a fresh register.
Return that register."
  (let*
    ((sym (u/gba/thumb-loc-base loc))
      (offset (u/gba/thumb-loc-offset loc))
      (dest (u/gba/fresh!)))
    (u/gba/emit!
      (u/gba/scope
        (if (u/gba/thumb-reg? sym) ;; if the base is a register, add the offset
          (u/gba/thumb-add-offset dest sym offset)
          (let* ;; if the base is a keyword (that is, a symbol name):
            ((entry (or (u/gba/symtab-lookup symtab sym) (error "Failed to find symbol: %s" sym)))
              (addr (u/gba/symtab-entry-addr entry)))
            (cond
              ((u/gba/thumb-reg? offset) ;; if the offset is a register
                (u/gba/emit! ;; we need to generate an add instruction
                  (u/gba/thumb-constant dest addr)
                  `(add ,dest ,dest ,offset)))
              ((integerp offset) ;; if the offset is an integer, this is a constant
                (u/gba/emit! (u/gba/thumb-constant dest (+ addr offset))))
              (t (error "Don't know how to add offset: %s" offset)))))))
    dest))
(defun u/gba/thumb-memop-args (symtab loc f)
  "Generate an appropriate base address and offset from LOC.
Modify the offset with F."
  (let*
    ( (sym (u/gba/thumb-loc-base loc))
      (off (u/gba/thumb-loc-offset loc))
      (addr (u/gba/thumb-loc symtab (if (integerp off) sym loc)))
      (moff (funcall f off)))
    (list addr moff)))

(defun u/gba/thumb-get8 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
    (u/gba/scope
      (u/gba/emit!
        `(ldrbi ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) o)))))))

(defun u/gba/thumb-get16 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
    (u/gba/scope
      (u/gba/emit!
        `(ldrhi ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 2))))))))

(defun u/gba/thumb-get32 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
    (u/gba/scope
      (u/gba/emit!
        `(ldri ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 4))))))))

(defun u/gba/thumb-set8 (symtab loc x) ;; NOTE: This will not work properly in VRAM
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
    (u/gba/scope
      (let
        ( (reg
           (cond
             ((u/gba/thumb-reg? x) x)
             ((integerp x)
               (let ((r (u/gba/fresh!)))
                 (when (> x #xff)
                   (error "Constant %s larger than 8 bits" x))
                 (u/gba/emit! `(mov ,r ,(logand #x000000ff x)))
                 r))
             (t (error "Don't know how to write value: %s" x)))))
        (u/gba/emit! `(strbi ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) o))))))))

(defun u/gba/thumb-set16 (symtab loc x)
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
    (u/gba/scope
      (let
        ((reg
           (cond
             ((u/gba/thumb-reg? x) x)
             ((integerp x)
               (let ((r (u/gba/fresh!)))
                 (when (> x #xffff)
                   (error "Constant %s larger than 16 bits" x))
                 (u/gba/emit! (u/gba/thumb-constant r (logand #xffff x)))
                 r))
             (t (error "Don't know how to write value: %s" x)))))
        (u/gba/emit!
          `(strhi ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 2)))))))))

(defun u/gba/thumb-set32 (symtab loc x)
  "Generate code setting the memory at LOC in SYMTAB to X.
X is either a register name or a constant."
  (u/gba/emit!
    (u/gba/scope
      (let
        ((reg
           (cond
             ((u/gba/thumb-reg? x) x)
             ((integerp x)
               (let ((r (u/gba/fresh!)))
                 (when (> x #xffffffff)
                   (error "Constant %s larger than 32 bits" x))
                 (u/gba/emit! (u/gba/thumb-constant r x))
                 r))
             (t (error "Don't know how to write value: %s" x)))))
        (u/gba/emit! `(stri ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 4)))))))))

(defun u/gba/thumb-call (symtab sym &rest args)
  "Generate code calling the function at SYM in SYMTAB.
The locations in ARGS are copied to the argument registers."
  (when (> (length args) (length u/gba/regs-arg))
    (error "Attempted to call function %s with too many (%s) arguments: %s" sym (length args) args))
  (u/gba/emit!
    (u/gba/scope
      (--each (-zip-pair args u/gba/regs-arg)
        (let ((x (car it)))
          (cond
            ((u/gba/thumb-reg? it)
              (u/gba/emit! `(mov ,(cdr it) ,it)))
            ((integerp x)
              (u/gba/emit! (u/gba/thumb-constant (cdr it) x)))
            ((keywordp x)
              (u/gba/emit! (u/gba/thumb-addr (cdr it) symtab x)))
            (t (error "Don't know how to pass argument: %s" x)))))
      (u/gba/emit!
        `(bl0 ,sym)
        `(bl1 ,sym)))))

(provide 'udc-gba-thumb-helpers)
;;; udc-gba-thumb-helpers.el ends here
