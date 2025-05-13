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
(defun u/gba/thumb-fresh-constant (constant)
  "Generate code loading the (maximum 32-bit CONSTANT) into a fresh register.
Return that register."
  (let ((r (u/gba/fresh!)))
    (u/gba/thumb-constant r constant)
    r))
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
            (u/gba/thumb-constant tmp offset)
            (u/gba/emit!
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
                (u/gba/thumb-constant dest addr)
                (u/gba/emit! ;; we need to generate an add instruction
                  `(add ,dest ,dest ,offset)))
              ((integerp offset) ;; if the offset is an integer, this is a constant
                (u/gba/thumb-constant dest (+ addr offset)))
              (t (error "Don't know how to add offset: %s" offset)))))))
    dest))
(defun u/gba/thumb-memop-args (symtab loc f)
  "Generate a base address register and offset from LOC and SYMTAB.
Modify the offset with F."
  (let*
    ( (sym (u/gba/thumb-loc-base loc))
      (off (u/gba/thumb-loc-offset loc))
      (base (if (u/gba/thumb-reg? sym) sym (u/gba/thumb-loc symtab sym)))
      (moff (if (integerp off) (funcall f off) off)))
    (list base moff)))

(defun u/gba/thumb-get8 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
    (u/gba/scope
      (u/gba/emit!
        `( ,(if (integerp (u/gba/thumb-loc-offset loc)) 'ldrbi 'ldrb)
           ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) o)))))))

(defun u/gba/thumb-get16 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
    (u/gba/scope
      (u/gba/emit!
        `( ,(if (integerp (u/gba/thumb-loc-offset loc)) 'ldrhi 'ldrh)
           ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 2))))))))

(defun u/gba/thumb-get32 (symtab reg loc)
  "Generate code reading the memory at LOC in SYMTAB to REG."
  (u/gba/emit!
    (u/gba/scope
      (u/gba/emit!
        `( ,(if (integerp (u/gba/thumb-loc-offset loc)) 'ldri 'ldr)
           ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 4))))))))

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
        (u/gba/emit!
          `( ,(if (integerp (u/gba/thumb-loc-offset loc)) 'strbi 'strb)
             ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) o))))))))

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
                 (u/gba/thumb-constant r (logand #xffff x))
                 r))
             (t (error "Don't know how to write value: %s" x)))))
        (u/gba/emit!
          `( ,(if (integerp (u/gba/thumb-loc-offset loc)) 'strhi 'strh)
             ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 2)))))))))

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
                 (u/gba/thumb-constant r x)
                 r))
             (t (error "Don't know how to write value: %s" x)))))
        (u/gba/emit!
          `( ,(if (integerp (u/gba/thumb-loc-offset loc)) 'stri 'str)
             ,reg ,@(u/gba/thumb-memop-args symtab loc (lambda (o) (/ o 4)))))))))

(defun u/gba/thumb-call (symtab sym &rest args)
  "Generate code calling the function at SYM in SYMTAB.
The locations in ARGS are copied to the argument registers."
  (when (> (length args) (length u/gba/regs-arg))
    (error "Attempted to call function %s with too many (%s) arguments: %s" sym (length args) args))
  (message "args: %s" args)
  (u/gba/emit!
    (u/gba/scope
      (--each (-zip-pair args u/gba/regs-arg)
        (let ((x (car it)))
          (message "arg is %s" it)
          (cond
            ((u/gba/thumb-reg? it)
              (u/gba/emit! `(mov ,(cdr it) ,it)))
            ((integerp x)
              (u/gba/thumb-constant (cdr it) x))
            ((keywordp x)
              (message "emitting addr")
              (u/gba/thumb-addr (cdr it) symtab x))
            (t (error "Don't know how to pass argument: %s" x)))))
      (u/gba/emit!
        `(bl0 ,sym)
        `(bl1 ,sym)))))

(defun u/gba/thumb-for (start end f)
  "Generate a loop from the constant START to the constant or register END -1.
The function F generates the body of the loop, and is passed the register
containing the loop counter."
  (u/gba/scope
    (let ( (idx (u/gba/thumb-fresh-constant start))
           (max (if (u/gba/thumb-reg? end) end (u/gba/thumb-fresh-constant end))))
      (u/gba/emit!
        :loop)
      (funcall f idx)
      (u/gba/emit!
        `(inc ,idx 1)
        `(cmp ,idx ,max)
        '(blt :loop)))))

(defconst u/gba/dispcnt-video-modes '(:videomode0 :videomode1 :videomode2 :videomode3 :videomode4 :videomode5))
(defun u/gba/dispcnt-flag (f)
  "Generate the bit pattern for the display control flag F."
  (cl-case f
    (:videomode0    #b0000000000000000)
    (:videomode1    #b0000000000000001)
    (:videomode2    #b0000000000000010)
    (:videomode3    #b0000000000000011)
    (:videomode4    #b0000000000000100)
    (:videomode5    #b0000000000000101)
    (:pageselect    #b0000000000010000)
    (:oam-hblank    #b0000000000100000)
    (:object1d      #b0000000001000000)
    (:force-blank   #b0000000010000000)
    (:bg0           #b0000000100000000)
    (:bg1           #b0000001000000000)
    (:bg2           #b0000010000000000)
    (:bg3           #b0000100000000000)
    (:sprites       #b0001000000000000)
    (:window0       #b0010000000000000)
    (:window1       #b0100000000000000)
    (:object-window #b1000000000000000)
    (t (error "Unknown display control flag: %s" f))))
(defun u/gba/thumb-dispcnt (syms &rest flags)
  "Set the display control register :reg-dispcnt in SYMS based on FLAGS."
  (let ((vmodes (--filter (-contains? u/gba/dispcnt-video-modes it) flags)))
    (when (> (length vmodes) 1)
      (error "Specified multiple video modes in display control: %s" vmodes)))
  (u/gba/scope
    (let ((v (apply #'logior (-map #'u/gba/dispcnt-flag flags)) ))
      (u/gba/thumb-set16 syms :reg-dispcnt v))))

(defconst u/gba/bgcnt-priorities '(:priority0 :priority1 :priority2 :priority3))
(defconst u/gba/bgcnt-charblocks '(:charblock0 :charblock1 :charblock2 :charblock3))
(defconst u/gba/bgcnt-screenblocks
  '( :screenblock0 :screenblock1 :screenblock2 :screenblock3
     :screenblock4 :screenblock5 :screenblock6 :screenblock7
     :screenblock8 :screenblock9 :screenblock10 :screenblock11
     :screenblock12 :screenblock13 :screenblock14 :screenblock15
     :screenblock16 :screenblock17 :screenblock18 :screenblock19
     :screenblock20 :screenblock21 :screenblock22 :screenblock23
     :screenblock24 :screenblock25 :screenblock26 :screenblock27
     :screenblock28 :screenblock29 :screenblock30 :screenblock31
     ))
(defun u/gba/bgcnt-flag (f)
  "Generate the bit pattern for the display control flag F."
  (cl-case f
    (:priority0       #b0000000000000000)
    (:priority1       #b0000000000000001)
    (:priority2       #b0000000000000010)
    (:priority3       #b0000000000000011)
    (:charblock0      #b0000000000000000) (:charblock1      #b0000000000000100)
    (:charblock2      #b0000000000001000) (:charblock3      #b0000000000001100)
    (:mosaic          #b0000000001000000)
    (:8bpp            #b0000000010000000)
    (:screenblock0    #b0000000000000000) (:screenblock1    #b0000000100000000)
    (:screenblock2    #b0000001000000000) (:screenblock3    #b0000001100000000)
    (:screenblock4    #b0000010000000000) (:screenblock5    #b0000010100000000)
    (:screenblock6    #b0000011000000000) (:screenblock7    #b0000011100000000)
    (:screenblock8    #b0000100000000000) (:screenblock9    #b0000100100000000)
    (:screenblock10   #b0000101000000000) (:screenblock11   #b0000101100000000)
    (:screenblock12   #b0000110000000000) (:screenblock13   #b0000110100000000)
    (:screenblock14   #b0000111000000000) (:screenblock15   #b0000111100000000)
    (:screenblock16   #b0001000000000000) (:screenblock17   #b0001000100000000)
    (:screenblock18   #b0001001000000000) (:screenblock19   #b0001001100000000)
    (:screenblock20   #b0001010000000000) (:screenblock21   #b0001010100000000)
    (:screenblock22   #b0001011000000000) (:screenblock23   #b0001011100000000)
    (:screenblock24   #b0001100000000000) (:screenblock25   #b0001100100000000)
    (:screenblock26   #b0001101000000000) (:screenblock27   #b0001101100000000)
    (:screenblock28   #b0001110000000000) (:screenblock29   #b0001110100000000)
    (:screenblock30   #b0001111000000000) (:screenblock31   #b0001111100000000)
    (:affine-wrapping #b0010000000000000)
    (:32x32tiles      #b0000000000000000)
    (:64x32tiles      #b0100000000000000)
    (:32x64tiles      #b1000000000000000)
    (:64x64tiles      #b1100000000000000)
    (t (error "Unknown display control flag: %s" f))))
(defun u/gba/thumb-bgcnt (syms idx &rest flags)
  "Set the background control register :reg-bg IDX cnt in SYMS based on FLAGS."
  (let ((xs (--filter (-contains? u/gba/bgcnt-priorities it) flags)))
    (when (> (length xs) 1)
      (error "Specified multiple priorities in background control: %s" xs)))
  (let ((xs (--filter (-contains? u/gba/bgcnt-charblocks it) flags)))
    (when (> (length xs) 1)
      (error "Specified multiple base charblocks in background control: %s" xs)))
  (let ((xs (--filter (-contains? u/gba/bgcnt-screenblocks it) flags)))
    (when (> (length xs) 1)
      (error "Specified multiple base screenblocks in background control: %s" xs)))
  (u/gba/scope
    (let ((v (apply #'logior (-map #'u/gba/bgcnt-flag flags)) ))
      (u/gba/thumb-set16 syms (intern (format ":reg-bg%scnt" idx)) v))))

(provide 'udc-gba-thumb-helpers)
;;; udc-gba-thumb-helpers.el ends here
