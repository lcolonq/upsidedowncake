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
  (let ( (op (car ins))
         (ty (u/gba/codegen-type (u/gba/codegen))))
    (cl-case op
      (:const ;; constant in the literal pool
        (let* ( (rd (cadr ins))
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
                (inslen (cl-case ty (arm 4) (thumb 2) (t (error "Unknown assembly type: %s" ty))))
                (insoff (logand #b11111111111111111111111111111100 (* inslen idx)))
                (litoff (+ (* inslen (+ 1 (length (u/gba/codegen-instructions (u/gba/codegen))))) loc)))
        (message "%s" `(literal ,ins ,inslen ,insoff ,litoff ,loc))
          (cl-case ty
            (arm `((ldr ,rd ,u/gba/pc ,(- litoff insoff 12))))
            (thumb `((ldrpc ,rd ,(/ (- litoff insoff) 4))))
            (t (error "Unknown assembly type: %s" ty)))))
      (:swap ;; swap registers
        (cl-case ty
          (arm
            `( (mov ,u/gba/scratch ,(cadr ins))
               (mov ,(cadr ins) ,(caddr ins))
               (mov ,(caddr ins) ,u/gba/scratch)))
          (thumb
            `( (mov ,u/gba/scratch ,(cadr ins))
               (mov ,(cadr ins) ,(caddr ins))
               (mov ,(caddr ins) ,u/gba/scratch)))
          (t (error "Unknown assembly type: %s" ty))))
      (:set-local ;; write memory at offset from stack frame
        (cl-case ty
          (arm `((str ,(caddr ins) ,u/gba/arm-fp ,(* 4 (cadr ins)))))
          (thumb `((stri ,(caddr ins) ,u/gba/thumb-fp ,(cadr ins))))
          (t (error "Unknown assembly type: %s" ty))))
      (:get-local ;; write memory at offset from stack frame
        (cl-case ty
          (arm `((ldr ,(caddr ins) ,u/gba/arm-fp ,(* 4 (cadr ins)))))
          (thumb `((ldri ,(caddr ins) ,u/gba/thumb-fp ,(cadr ins))))
          (t (error "Unknown assembly type: %s" ty))))
      (:jmp ;; unconditional jump
        (cl-case ty
          (arm `((b ,(cadr ins))))
          (thumb `((b ,(cadr ins))))
          (t (error "Unknown assembly type: %s" ty))))
      (:jmptrue ;; conditional jump (if true)
        (cl-case ty
          (arm
            `( (cmp s r0 ,(cadr ins) 0)
               (b eq ,(caddr ins))))
          (thumb
            `( (cmpi ,(cadr ins) 0)
               (beq ,(caddr ins))))
          (t (error "Unknown assembly type: %s" ty))))
      (:jmpfalse ;; conditional jump (if false)
        (cl-case ty
          (arm
            `( (cmp s r0 ,(cadr ins) 0)
               (b ne ,(caddr ins))))
          (thumb
            `( (cmpi ,(cadr ins) 0)
               (bne ,(caddr ins))))
          (t (error "Unknown assembly type: %s" ty))))
      (t (list ins)))))
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
    (apply #'-concat
      (--map-indexed
        (u/gba/codegen-replace-pseudo it it-index)
        (reverse (u/gba/codegen-instructions (u/gba/codegen)))))))
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
  (print `(emit ,ins))
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
        (push it (u/gba/codegen-instructions (u/gba/codegen))))
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
