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

(provide 'udc-gba-thumb-helpers)
;;; udc-gba-thumb-helpers.el ends here
