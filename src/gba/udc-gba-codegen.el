;;; udc-gba-codegen --- Helpers for codegen -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
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
(require 'udc-gba-constants)

;;;; Code generation contexts
(defvar u/gba/codegen nil) ;; dynamically bound code generation context
(cl-defstruct (u/gba/literals (:constructor u/gba/make-literals))
  (pool 0) ;; offset past the end of the generated code to place large literals
  (offsets (ht-create)) ;; mapping from literal value to offsets
  (bytes nil) ;; reversed list of actual literal bytes to append
  )
(cl-defstruct (u/gba/codegen (:constructor u/gba/make-codegen))
  (type 'arm) ;; either 'arm or 'thumb
  (regs-available nil) ;; list of registers that are available for us
  (instructions nil) ;; reversed list of generated instructions
  (local-labels (ht-create)) ;; mapping from label keywords to word offsets from start
  (literals (u/gba/make-literals)) ;; literal pool information
  )
(defun u/gba/codegen ()
  "Retrieve the current code generation context."
  (or u/gba/codegen (error "Code generation context is not bound")))
(defun u/gba/codegen-replace-pseudo (ins idx)
  "Replace INS if it is a pseudo-instruction at IDX."
  (let ((op (car ins)))
    (cl-case op
      (:const
        (let* ( (ty (u/gba/codegen-type (u/gba/codegen)))
                (rd (cadr ins))
                (lit (caddr ins))
                (loc
                  (if-let* ((cur (ht-get (u/gba/literals-offsets (u/gba/codegen-literals (u/gba/codegen))) lit)))
                    cur
                    (let ((cur (u/gba/literals-pool (u/gba/codegen-literals (u/gba/codegen)))))
                      (setf (u/gba/literals-bytes (u/gba/codegen-literals (u/gba/codegen)))
                        (append
                          (reverse (u/split32le lit))
                          (u/gba/literals-bytes (u/gba/codegen-literals (u/gba/codegen)))))
                      (ht-set (u/gba/literals-offsets (u/gba/codegen-literals (u/gba/codegen))) lit cur)
                      (incf (u/gba/literals-pool (u/gba/codegen-literals (u/gba/codegen))) 4)
                      cur)))
                (insoff (+ 1 idx))
                (litoff (+ (length (u/gba/codegen-instructions (u/gba/codegen))) loc)))
          (cl-case ty
            (arm `(ldr ,rd ,u/gba/pc ,(* 4 (- litoff insoff 1))))
            (thumb (error "Unimplemented literal pool usage in Thumb"))
            (t (error "Unknown assembly type: %s" ty)))))
      (t ins))))
(defun u/gba/codegen-replace-local-labels (ins idx)
  "Replace local labels from the current codegen context in INS at IDX."
  (--map
    (if (and (keywordp it) (ht-contains? (u/gba/codegen-local-labels (u/gba/codegen)) it))
      (let ((insoff (+ 1 idx))
             (laboff (ht-get (u/gba/codegen-local-labels (u/gba/codegen)) it)))
        (- laboff insoff 1))
      it)
    ins))
(defun u/gba/codegen-transform (pins idx)
  "Transform the pseudo-instruction PINS at IDX to an instruction."
  (-> pins
    (u/gba/codegen-replace-pseudo idx)
    (u/gba/codegen-replace-local-labels idx)))
(defun u/gba/codegen-extract ()
  "Extract the generated code from the current codegen context."
  (--map-indexed
    (u/gba/codegen-transform it it-index)
    (reverse (u/gba/codegen-instructions (u/gba/codegen)))))
(defun u/gba/codegen-extract-with-literals (symtab section sym)
  "Extract the generated code and literals from the current codegen context.
Place the resulting code in SYMTAB at SYM in SECTION."
  (let* ((code (u/gba/codegen-extract)))
    (u/gba/symtab-add! symtab section sym (u/gba/codegen-type (u/gba/codegen)) code)
    (when-let* ((bytes (reverse (u/gba/literals-bytes (u/gba/codegen-literals (u/gba/codegen))))))
      (u/gba/symtab-add! symtab section (intern (format "%s-literals" sym)) 'bytes bytes))))

(defmacro u/gba/toplevel (symtab section sym ty &rest body)
  "Run BODY in a new code generation context and add the generated code to SYMTAB.
The code is placed in SECTION with name SYM.
The code is assumed to be assembly of TY, either arm or thumb.
No registers will be available."
  `(let* ((u/gba/codegen
            (u/gba/make-codegen :type ,ty)))
     (u/gba/emit-body! ,@body)
     (u/gba/codegen-extract-with-literals ,symtab ,section ,sym)))
(defmacro u/gba/scope (&rest body)
  "Run BODY in a new code generation context and return the generated instructions.
Registers from the enclosing scope will be available."
  `(let* ((u/gba/codegen
            (u/gba/make-codegen
              :type (u/gba/codegen-type (u/gba/codegen))
              :literals (u/gba/codegen-literals (u/gba/codegen))
              :regs-available (copy-sequence (u/gba/codegen-regs-available (u/gba/codegen))))))
     (u/gba/emit-body! ,@body)
     (u/gba/codegen-extract)))

(defun u/gba/emit! (&rest ins)
  "Emit INS to the current codegen context."
  (--each ins
    (cond
      ((null it) nil)
      ((keywordp it)
        (ht-set!
          (u/gba/codegen-local-labels (u/gba/codegen))
          it (length (u/gba/codegen-instructions (u/gba/codegen)))))
      ((listp (car it))
        (apply #'u/gba/emit! it))
      ((symbolp (car it))
        (let ((op (or (car it) (error "Emitting instruction with nil opcode"))))
          (cl-case op
            (const
              (let ((rd (or (cadr it) (error "Malformed const pseudo-instruction"))))
                (push `(:const ,rd ,(caddr it)) (u/gba/codegen-instructions (u/gba/codegen)))))
            (t (push it (u/gba/codegen-instructions (u/gba/codegen)))))))
      (t
        (error "Emitted malformed instruction: %s" it)))))
(defmacro u/gba/emit-body! (&rest body)
  "Emit every term in BODY."
  (cons 'progn (--map `(u/gba/emit! ,it) body)))
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

(provide 'udc-gba-codegen)
;;; udc-gba-codegen.el ends here
