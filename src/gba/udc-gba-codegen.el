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
(defun u/gba/codegen-replace-pseudo-multi (ins idx)
  "Replace INS if it is a pseudo-instruction at IDX."
  (cond
    ((keywordp ins) ;; label
      (ht-set (u/gba/codegen-local-labels (u/gba/codegen)) ins idx)
      nil)
    ((listp ins) ;; instruction
      (let ( (op (car ins))
             (ty (u/gba/codegen-type (u/gba/codegen))))
        (cl-case op
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
          (:set-local ;; write memory at offset from stack
            (cl-case ty
              (arm `((str ,(caddr ins) ,u/gba/arm-sp ,(* 4 (cadr ins)))))
              (thumb `((strsp ,(caddr ins) ,(cadr ins))))
              (t (error "Unknown assembly type: %s" ty))))
          (:get-local ;; write memory at offset from stack
            (cl-case ty
              (arm `((ldr ,(caddr ins) ,u/gba/arm-sp ,(* 4 (cadr ins)))))
              (thumb `((ldrsp ,(caddr ins) ,(cadr ins))))
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
    (t (error "Unknown multi pseudo-instruction: %s" ins))))
(defun u/gba/codegen-replace-pseudo-single (len ins idx)
  "Replace INS if it is a pseudo-instruction at IDX.
LEN is the total number of instructions being generated."
  (cond
    ((listp ins) ;; instruction
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
                          (cl-incf (u/gba/literals-pool (u/gba/codegen-literals (u/gba/codegen))) 4)
                          cur)))
                    (inslen (cl-case ty (arm 4) (thumb 2) (t (error "Unknown assembly type: %s" ty))))
                    (pcoff (cl-case ty (arm 1) (thumb 2) (t (error "Unknown assembly type: %s" ty))))
                    (insoff (logand #b11111111111111111111111111111100 (* inslen idx)))
                    (litoff (+ (* inslen (+ pcoff len)) loc)))
              (message "%s" `(literal ,ins ,idx ,inslen ,insoff ,litoff ,loc))
              (cl-case ty
                (arm `(ldr ,rd ,u/gba/pc ,(- litoff insoff 12)))
                (thumb `(ldrpc ,rd ,(/ (- litoff insoff 8) 4)))
                (t (error "Unknown assembly type: %s" ty)))))
          (t ins))))
    (t (error "Unknown single pseudo-instruction: %s" ins))))
(defun u/gba/codegen-replace-local-labels (hlen ins idx)
  "Replace local labels from the current codegen context in INS at IDX.
HLEN is the length of the prepended header (in instructions)."
  (--map
    (if (and (keywordp it) (ht-contains? (u/gba/codegen-local-labels (u/gba/codegen)) it))
      (let* ( (insoff (+ 1 idx))
              (blaboff (ht-get (u/gba/codegen-local-labels (u/gba/codegen)) it))
              (laboff (+ hlen blaboff))
              (ty (u/gba/codegen-type (u/gba/codegen))))
        (message "%s" `(blaboff ,it ,blaboff ,laboff ,insoff ,(+ (- laboff insoff) 2)))
        (cl-case ty
          (arm (- laboff insoff 1))
          (thumb (- laboff insoff 1))
          (t (error "Unknown assembly type: %s" ty))))
      it)
    ins))
(defun u/gba/codegen-function (ty ps)
  "Allocate enough space for all literals used in the pseudo-instructions PS.
Return a pair of the code for the header and the code for the footer for TY."
  (let*
    ( (final
        (-max
          (or
            (--mapcat
              (cl-case it
                (:set-local (list (cadr it)))
                (:get-local (list (cadr it)))
                (t nil))
              ps)
            (list 0))))
      (cnt (+ final 1))
      (tymul (cl-case ty (arm 4) (thumb 2) (t (error "Unknown assembly type: %s" ty))))
      (bytes (* cnt tymul)))
    (cl-case ty
      (arm `( ( (stm down wb ,u/gba/arm-sp (,u/gba/arm-lr ,u/gba/arm-fp))
                (mov ,u/gba/arm-fp ,u/gba/arm-sp)
                (stm down wb ,u/gba/arm-sp ,u/gba/arm-regs-callee-saved)
                (sub ,u/gba/arm-sp ,u/gba/arm-sp ,bytes))
              . ( (ldm wb post ,u/gba/arm-sp ,u/gba/arm-regs-callee-saved)
                  (add ,u/gba/arm-sp ,u/gba/arm-sp ,bytes)
                  (ldm wb post ,u/gba/arm-sp (,u/gba/arm-lr ,u/gba/arm-fp))
                  (bx ,u/gba/arm-lr))))
      (thumb `( ( (push (lr ,u/gba/thumb-fp))
                  (addsp ,u/gba/thumb-fp 0)
                  (push ,u/gba/thumb-regs-callee-saved)
                  (decsp ,bytes)
                  )
                . ( (pop ,u/gba/thumb-regs-callee-saved)
                    (incsp ,bytes)
                    (pop (pc ,u/gba/thumb-fp))))))))
(defun u/gba/codegen-collect (f inss)
  "Call F on elements of INSS along with the number of accumulated results."
  (let ( (prev 0)
         (acc nil))
    (--each inss
      (let ((res (funcall f it prev)))
        (-each res
          (lambda (x) (push x acc)))
        (cl-incf prev (length res))))
    (reverse acc)))
(defun u/gba/codegen-extract ()
  "Extract the generated code from the current codegen context."
  (let* ( (g (u/gba/codegen))
          (ps (u/gba/codegen-instructions g))
          (reved (reverse ps)))
    reved))
(defun u/gba/codegen-extract-with-literals (symtab section sym &optional func)
  "Extract the generated code and literals from the current codegen context.
Place the resulting code in SYMTAB at SYM in SECTION.
Add stack bookkeeping if SP is non-nil."
  (-let* ( (g (u/gba/codegen))
           (ty (u/gba/codegen-type g))
           (ps (u/gba/codegen-instructions g))
           ((header . footer) (u/gba/codegen-function ty ps))
           (ecode (u/gba/codegen-extract))
           (icode (if func (-concat header ecode footer) ecode))
           (postmulti (u/gba/codegen-collect #'u/gba/codegen-replace-pseudo-multi icode))
           (postsingle 
             (--map-indexed
               (u/gba/codegen-replace-pseudo-single (length postmulti) it it-index)
               postmulti))
           (code 
             (--map-indexed
               (u/gba/codegen-replace-local-labels (length header) it it-index)
               postsingle)))
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
      ((keywordp it) ;; label
        (push it (u/gba/codegen-instructions (u/gba/codegen))))
      ((listp (car it)) ;; multiple instructions
        (apply #'u/gba/emit! it))
      ((symbolp (car it)) ;; single instruction
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
