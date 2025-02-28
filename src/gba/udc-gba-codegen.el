;;; udc-gba-codegen --- GBA codegen -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)
(require 'udc-gba-constants)
(require 'udc-gba-linker)

;;;; Code generation contexts
(defvar u/gba/codegen nil) ;; dynamically bound code generation context
(cl-defstruct (u/gba/codegen (:constructor u/gba/make-codegen))
  (regs-available nil) ;; list of registers that are available for us
  (frame-offset -8) ;; negative offset applied to frame pointer for next local
  (locals (ht-create)) ;; mapping from local variable symbols to frame offsets
  (instructions nil) ;; reversed list of generated instructions
  (local-labels (ht-create)) ;; mapping from label keywords to word offsets from start
  )
(defun u/gba/codegen ()
  "Retrieve the current code generation context."
  (or u/gba/codegen (error "Code generation context is not bound")))
(defun u/gba/codegen-replace-local-labels (ins idx)
  "Replace local labels from the current codegen context in INS at IDX."
  (--map
   (if (and (keywordp it) (ht-contains? (u/gba/codegen-local-labels (u/gba/codegen)) it))
       (let ((insoff (+ 1 idx))
             (laboff (ht-get (u/gba/codegen-local-labels (u/gba/codegen)) it)))
         (- laboff insoff 1))
     it)
   ins))
(defun u/gba/codegen-extract ()
  "Extract the generated code from the current codegen context."
  (--map-indexed
   (u/gba/codegen-replace-local-labels it it-index)
   (reverse (u/gba/codegen-instructions (u/gba/codegen)))))
(defmacro u/gba/toplevel (&rest body)
  "Run BODY in a new code generation context and return the generated instructions.
No registers will be available."
  `(let* ((u/gba/codegen
           (u/gba/make-codegen)))
     ,@body
     (u/gba/codegen-extract)))
(defmacro u/gba/scope (&rest body)
  "Run BODY in a new code generation context and return the generated instructions.
Registers from the enclosing scope will be available."
  `(let* ((u/gba/codegen
           (u/gba/make-codegen
            :regs-available (copy-sequence (u/gba/codegen-regs-available (u/gba/codegen))))))
     ,@body
     (u/gba/codegen-extract)))
(defun u/gba/push (&rest regs)
  "Generate a push of REGS to the stack."
  (u/gba/emit!
   `(stm down wb ,u/gba/sp ,regs)))
(defun u/gba/pop (&rest regs)
  "Generate a pop of REGS to the stack."
  (u/gba/emit!
   `(ldm wb post ,u/gba/sp ,regs)))
(defun u/gba/function-header ()
  "Generate the function header."
  (u/gba/push u/gba/lr u/gba/fp)
  (u/gba/emit! `(mov ,u/gba/fp ,u/gba/sp)))
(defun u/gba/function-footer ()
  "Generate the function footer."
  (u/gba/emit! `(mov ,u/gba/sp ,u/gba/fp))
  (u/gba/pop u/gba/lr u/gba/fp)
  (u/gba/emit! `(bx ,u/gba/lr)))
(defmacro u/gba/function (&rest body)
  "Run BODY in a new code generation context and return the generated instructions.
The code will be wrapped in the function header and footer.
Callee-saved registers will be available."
  `(let* ((u/gba/codegen (u/gba/make-codegen :regs-available (copy-sequence u/gba/regs-callee-saved))))
     (u/gba/function-header)
     (apply #'u/gba/push u/gba/regs-callee-saved)
     ,@body
     (apply #'u/gba/pop u/gba/regs-callee-saved)
     (u/gba/function-footer)
     (u/gba/codegen-extract)))
(defun u/gba/emit! (&rest ins)
  "Emit INS to the current codegen context."
  (--each ins
    (cond
     ((keywordp it)
      (ht-set!
       (u/gba/codegen-local-labels (u/gba/codegen))
       it (length (u/gba/codegen-instructions (u/gba/codegen)))))
     ((listp (car it))
      (apply #'u/gba/emit! it))
     ((symbolp (car it))
      (push it (u/gba/codegen-instructions (u/gba/codegen))))
     (t
      (error "Emitted malformed instruction: %s" it)))))
(defun u/gba/fresh! ()
  "Return a new register and mark it as unusable for the rest of the context."
  (unless u/gba/codegen
    (error "Attempted to obtain fresh register outside of code generation context"))
  (unless (u/gba/codegen-regs-available u/gba/codegen)
    (error "No more registers are available"))
  (pop (u/gba/codegen-regs-available u/gba/codegen)))
(defun u/gba/burn! (&rest rs)
  "Mark registers RS as unusable for the rest of the current codegen context."
  (let ((regs (u/gba/codegen-regs-available (u/gba/codegen))))
    (--each rs
      (if (-contains? regs it)
          (delete it regs)
        (error "Tried to burn unavailable register %s; currently available: %s" it regs)))))
(defun u/gba/claim! (&rest rs)
  "Mark registers RS as usable in the current codegen context.
This tells the system that it is ok to clobber those registers. Beware!"
  (--each rs
    (cl-pushnew it (u/gba/codegen-regs-available (u/gba/codegen)))))

(defun u/gba/constant (r constant)
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
(defun u/gba/addr (r symtab sym)
  "Generate code loading the address in SYMTAB for SYM into R."
  (u/gba/constant r (u/gba/symtab-entry-addr (u/gba/symtab-lookup symtab sym))))

(provide 'udc-gba-codegen)
;;; udc-gba-codegen.el ends here
