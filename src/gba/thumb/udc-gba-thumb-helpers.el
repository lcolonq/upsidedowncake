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
(defun u/gba/thumb-function-header ()
  "Generate the function header."
  (u/gba/emit!
    `(push (,u/gba/thumb-lr ,u/gba/thumb-fp))
    `(mov ,u/gba/thumb-fp ,u/gba/thumb-sp)))
(defun u/gba/thumb-function-footer ()
  "Generate the function footer."
  (u/gba/emit!
    `(mov ,u/gba/thumb-sp ,u/gba/thumb-fp)
    `(pop (,u/gba/thumb-lr ,u/gba/thumb-fp))
    `(bx ,u/gba/thumb-lr)))
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
     (u/gba/thumb-function-header)
     (u/gba/emit! `(push ,u/gba/thumb-regs-callee-saved))
     (u/gba/emit! ,@body)
     (u/gba/emit! `(pop ,u/gba/thumb-regs-callee-saved))
     (u/gba/thumb-function-footer)
     (u/gba/codegen-extract-with-literals ,symtab :code ,sym)))

(provide 'udc-gba-thumb-helpers)
;;; udc-gba-thumb-helpers.el ends here
