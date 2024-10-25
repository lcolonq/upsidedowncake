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

;;;; Constants
(defconst u/gba/rom-start #x8000000)
(defconst u/gba/size-word 4)
(defconst u/gba/pc 'r15)
(defconst u/gba/lr 'r14)
(defconst u/gba/sp 'r13)
(defconst u/gba/fp 'r11)
(defconst u/gba/scratch 'r4) ;; we designate r4 as our general-purpose scratch register (frequently clobbered)

;;;; The assembler proper
(defun u/gba/assemble-ins-string-system (ins)
  "Assemble the instruction mnemonic string INS using arm-none-eabi-as."
  (let ((asmpath (f-join "/tmp" (format "%s.asm" (make-temp-name "udc_gba_"))))
        (objpath (f-join "/tmp" (format "%s.o" (make-temp-name "udc_gba_"))))
        (dumppath (f-join "/tmp" (format "%s.dump" (make-temp-name "udc_gba_"))))
        (as (executable-find "arm-none-eabi-as"))
        (objcopy (executable-find "arm-none-eabi-objcopy")))
    (with-temp-buffer
      (f-write-text ins 'utf-8 asmpath)
      (unless (= 0 (call-process as nil (current-buffer) nil asmpath "-o" objpath))
        (message (buffer-string))
        (error "Failed to assemble"))
      (unless (= 0 (call-process objcopy nil (current-buffer) nil "-O" "binary" "-j" ".text" objpath dumppath))
        (message (buffer-string))
        (error "Failed to objcopy"))
      (seq-into (f-read-bytes dumppath) 'list))))

(defun u/gba/assemble-imm (x)
  "Return the value for X if X is an immediate."
  (and (integerp x) x))

(defconst u/gba/flags '(s post down pst wb byte))
(defconst u/gba/conds '(eq ne cs cc mi pl vs vc hi ls ge lt gt le al))
(defun u/gba/assemble-cond (cond)
  "Given COND, return its 4-bit encoding."
  (cl-case cond
    (eq #b0000) (ne #b0001)
    (cs #b0010) (cc #b0011)
    (mi #b0100) (pl #b0101)
    (vs #b0110) (vc #b0111)
    (hi #b1000) (ls #b1001)
    (ge #b1010) (lt #b1011)
    (gt #b1100) (le #b1101)
    (al #b1110)))

(defun u/gba/assemble-reg (reg)
  "Given REG, return its 4-bit encoding."
  (cl-case reg
    (r0 0) (r1 1) (r2 2) (r3 3) (r4 4) (r5 5) (r6 6) (r7 7)
    (r8 8) (r9 9) (r10 10) (r11 11) (r12 12) (r13 13) (r14 14) (r15 15)))

(defun u/gba/assemble-flag (flag)
  "Given FLAG, return its 1-bit encoding."
  (if flag 1 0))

(defun u/gba/assemble-ins-dataprocessing-op (op)
  "Given OP, return its 4-bit encoding."
  (cl-case op
    (and #b0000) (eor #b0001)
    (sub #b0010) (rsb #b0011)
    (add #b0100) (adc #b0101)
    (sbc #b0110) (rsc #b0111)
    (tst #b1000) (teq #b1001)
    (cmp #b1010) (cmn #b1011)
    (orr #b1100) (mov #b1101)
    (bic #b1110) (mvn #b1111)))

(defun u/gba/assemble-dataprocessing-shift-type (ty)
  "Given TY, return its 2-bit shift type."
  (cl-case ty
    (lsl #b00)
    (lsr #b01)
    (asr #b10)
    (ror #b11)
    (t (error "Invalid shift type: %s" ty))))

(defun u/gba/assemble-ins-dataprocessing-arg2-shift (arg shift)
  "Given ARG and SHIFT, return properly assembled data."
  (let ((reg (u/gba/assemble-reg arg))
        (shiftreg (and (listp shift) (u/gba/assemble-reg (cadr shift))))
        (shty (and (listp shift) (car shift) (u/gba/assemble-dataprocessing-shift-type (car shift)))))
    (cond
     (reg
      (list
       nil
       (logior
        (lsh
         (cond
          ((not (and (listp shift) shty)) 0)
          (shiftreg (logior (lsh shiftreg 3) (lsh shty 1) 1))
          ((integerp (cadr shift)) (logior (lsh (logand #b11111 (cadr shift)) 3) (lsh shty 1) 0))
          (t (error "Invalid shift: %s" shift)))
         4)
        reg)))
     ((integerp arg)
      (list
       t
       (logior
        (lsh
         (cond
          ((not shift) 0)
          ((integerp shift) (logand #xf shift))
          (t (error "Invalid immediate rotation: %s" shift)))
         8)
        (logand #xff arg))))
     (t (error "Invalid second operand: %s" arg)))))

(defun u/gba/assemble-ins-dataprocessing (cond op set-cond arg1 dest arg2 shift)
  "Given COND, OP, SET-COND, ARG1, DEST, ARG2, and SHIFT produce 4 bytes."
  (let ((a2 (if arg2 (u/gba/assemble-ins-dataprocessing-arg2-shift arg2 shift) (list nil 0))))
    (u/split32le
     (logior
      (lsh (u/gba/assemble-cond cond) 28)
      (lsh #b00 26)
      (lsh (u/gba/assemble-flag (car a2)) 25)
      (lsh (u/gba/assemble-ins-dataprocessing-op op) 21)
      (lsh (u/gba/assemble-flag set-cond) 20)
      (lsh (u/gba/assemble-reg arg1) 16)
      (lsh (u/gba/assemble-reg dest) 12)
      (cadr a2)))))

(defun u/gba/assemble-ins-multiply (cond acc set-cond rd rn rs rm)
  "Given COND, ACC, SET-COND, RD, RN, RS, RM produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b000000 22)
    (lsh (u/gba/assemble-flag acc) 21)
    (lsh (u/gba/assemble-flag set-cond) 20)
    (lsh (u/gba/assemble-reg rd) 16)
    (lsh (or (u/gba/assemble-reg rn) 0) 12)
    (lsh (u/gba/assemble-reg rs) 8)
    (lsh #b1001 4)
    (u/gba/assemble-reg rm))))

(defun u/gba/assemble-ins-multiplylong (cond signed acc set-cond rdhi rdlo rs rm)
  "Given COND, SIGNED, ACC, SET-COND, RDHI, RDLO, RS, RM produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b00001 23)
    (lsh (u/gba/assemble-flag signed) 22)
    (lsh (u/gba/assemble-flag acc) 21)
    (lsh (u/gba/assemble-flag set-cond) 20)
    (lsh (u/gba/assemble-reg rdhi) 16)
    (lsh (u/gba/assemble-reg rdlo) 12)
    (lsh (u/gba/assemble-reg rs) 8)
    (lsh #b1001 4)
    (u/gba/assemble-reg rm))))

(defun u/gba/assemble-ins-singledataswap (cond byte rn rd rm)
  "Given COND, BYTE, RN, RD, and RM, produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b00010 23)
    (lsh (u/gba/assemble-flag byte) 22)
    (lsh #b00 20)
    (lsh (u/gba/assemble-reg rn) 16)
    (lsh (u/gba/assemble-reg rd) 12)
    (lsh #b00001001 4)
    (u/gba/assemble-reg rm))))

(defun u/gba/assemble-ins-branchexchange (cond rn)
  "Given COND and RN produce 4 bytes."
  (let ((reg (u/gba/assemble-reg rn)))
    (unless reg
      (error "Invalid BX target: %s" rn))
    (u/split32le
     (logior
      (lsh (u/gba/assemble-cond cond) 28)
      (lsh #b000100101111111111110001 4)
      reg))))

(defun u/gba/assemble-ins-hdtregister (cond pre up writeback load rn rd signed halfword rm)
  "Given COND, PRE, UP, WRITEBACK, LOAD, RN, RD, SIGNED, HALFWORD, and RM, :3."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b000 25)
    (lsh (u/gba/assemble-flag pre) 24)
    (lsh (u/gba/assemble-flag up) 23)
    (lsh #b0 22)
    (lsh (u/gba/assemble-flag writeback) 21)
    (lsh (u/gba/assemble-flag load) 20)
    (lsh (u/gba/assemble-reg rn) 16)
    (lsh (u/gba/assemble-reg rd) 12)
    (lsh #b00001 7)
    (lsh (u/gba/assemble-flag signed) 6)
    (lsh (u/gba/assemble-flag halfword) 5)
    (lsh #b1 4)
    (u/gba/assemble-reg rm))))

(defun u/gba/assemble-ins-hdtimmediate (cond pre up writeback load rn rd offset1 signed halfword offset2)
  "Produce 4 bytes.
Given COND, PRE, UP, WRITEBACK, LOAD, RN, RD, OFFSET1, SIGNED,
HALFWORD, and OFFSET2."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b000 25)
    (lsh (u/gba/assemble-flag pre) 24)
    (lsh (u/gba/assemble-flag up) 23)
    (lsh #b1 22)
    (lsh (u/gba/assemble-flag writeback) 21)
    (lsh (u/gba/assemble-flag load) 20)
    (lsh (u/gba/assemble-reg rn) 16)
    (lsh (u/gba/assemble-reg rd) 12)
    (lsh (logand offset1 #xf) 8)
    (lsh #b1 7)
    (lsh (u/gba/assemble-flag signed) 6)
    (lsh (u/gba/assemble-flag halfword) 5)
    (lsh #b1 4)
    (logand offset2 #xf))))

(defun u/gba/assemble-ins-sdt-offset (arg shift)
  "Given ARG and SHIFT, return properly assembled data."
  (let ((reg (u/gba/assemble-reg arg))
        (shiftreg (u/gba/assemble-reg (cadr shift)))
        (shty (and (car shift) (u/gba/assemble-dataprocessing-shift-type (car shift)))))
    (cond
     (reg
      (list
       nil
       (logior
        (lsh
         (cond
          ((not (and (listp shift) shty)) 0)
          (shiftreg (logior (lsh shiftreg 3) (lsh shty 1) 1))
          ((integerp (cadr shift)) (logior (lsh (logand #b11111 (cadr shift)) 3) (lsh shty 1) 0))
          (t (error "Invalid shift: %s" shift)))
         4)
        reg)))
     ((integerp arg)
      (logand #xfff arg))
     (t 0))))

(defun u/gba/assemble-ins-sdt (cond imm pre up byte writeback load rd rn offset shift)
  "Given COND, IMM, PRE, UP, BYTE, WRITEBACK, LOAD, RN, RD, OFFSET, and SHIFT."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b01 26)
    (lsh (u/gba/assemble-flag imm) 25)
    (lsh (u/gba/assemble-flag pre) 24)
    (lsh (u/gba/assemble-flag up) 23)
    (lsh (u/gba/assemble-flag byte) 22)
    (lsh (u/gba/assemble-flag writeback) 21)
    (lsh (u/gba/assemble-flag load) 20)
    (lsh (u/gba/assemble-reg rn) 16)
    (lsh (u/gba/assemble-reg rd) 12)
    (u/gba/assemble-ins-sdt-offset offset shift))))

(defun u/gba/assemble-ins-bdt-reglist (reglist)
  "Return a 16-bit integer representing REGLIST."
  (--reduce-from
   (logior acc (lsh 1 (u/gba/assemble-reg it)))
   0
   reglist))
(defun u/gba/assemble-ins-bdt (cond pre up byte writeback load rn reglist)
  "Given COND, PRE, UP, BYTE, WRITEBACK, LOAD, RN, and REGLIST, :3."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b100 25)
    (lsh (u/gba/assemble-flag pre) 24)
    (lsh (u/gba/assemble-flag up) 23)
    (lsh (u/gba/assemble-flag byte) 22)
    (lsh (u/gba/assemble-flag writeback) 21)
    (lsh (u/gba/assemble-flag load) 20)
    (lsh (u/gba/assemble-reg rn) 16)
    (logand (u/gba/assemble-ins-bdt-reglist reglist) #xffff))))

(defun u/gba/assemble-ins-branch (cond link offset)
  "Given COND, LINK, and OFFSET, produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b101 25)
    (lsh (u/gba/assemble-flag link) 24)
    (logand offset #xffffff))))

(defun u/gba/assemble-ins-interrupt (cond data)
  "Given COND and DATA, produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b1111 24)
    (logand data #xffffff))))

(defun u/gba/render-op (op)
  "Convert the symbol OP to a mnemonic string."
  (format "%s" op))
(defun u/gba/render-cond (cond)
  "Convert the symbol COND to a mnemonic string."
  (format "%s" cond))
(defun u/gba/render-arg (arg)
  "Convert ARG to a mnemonic string."
  (cond
   ((symbolp arg) (format "%s" arg))
   ((integerp arg) (format "#%s" arg))
   ((listp arg) ;; we assume this is a shift specifier
    (format "%s %s" (u/gba/render-arg (car arg)) (u/gba/render-arg (cadr arg))))))
(defun u/gba/render-ins (ins)
  "Convert the S-expression INS to a GAS-syntax string."
  (let* ((op (if (listp ins) (car ins) ins))
         (opbase (if (listp ins) ins nil))
         (cond (car (--filter (-contains? u/gba/conds it) opbase)))
         (rcond (if cond (u/gba/render-cond cond) ""))
         (args (--filter (not (-contains? (cons cond u/gba/flags) it)) (cdr opbase))))
    (cond
     ((-contains? '(b bl) op)
      (s-concat (u/gba/render-op op) rcond " " (format "pc+%d" (* (+ (car args) 2) 4))))
     ((-contains? '(ldr str) op)
      (s-concat
       (u/gba/render-op op) rcond (if (-contains? opbase 'byte) "b" "") " "
       (u/gba/render-arg (car args)) ", ["
       (u/gba/render-arg (cadr args)) ", "
       (if (caddr args) (u/gba/render-arg (caddr args)) "#0")
       "]"))
     (t
      (s-concat
       (u/gba/render-op op)
       rcond " "
       (s-join ", " (-map #'u/gba/render-arg args)))))))

(defun u/gba/assemble-ins (ins &optional check)
  "Assemble INS to a sequence of bytes.
If CHECK is non-nil, check against system assembler.
INS is either:
 - an opcode symbol
 - a list of an opcode symbol followed by operands"
  (let*
      ((op (if (listp ins) (car ins) ins))
       (opbase (if (listp ins) ins nil))
       (set-cond (-contains? opbase 's))
       (cond (or (car (--filter (-contains? u/gba/conds it) opbase)) 'al))
       (args (--filter (not (-contains? (cons cond u/gba/flags) it)) (cdr opbase)))
       (d (car args))
       (dimm (u/gba/assemble-imm d))
       (a0 (cadr args))
       (a1 (caddr args))
       (a2 (cadddr args))
       (res
        (cl-case op
          (adc (u/gba/assemble-ins-dataprocessing cond 'adc set-cond a0 d a1 a2))
          (add (u/gba/assemble-ins-dataprocessing cond 'add set-cond a0 d a1 a2))
          (and (u/gba/assemble-ins-dataprocessing cond 'and set-cond a0 d a1 a2))
          (b (u/gba/assemble-ins-branch cond nil dimm))
          (bic (u/gba/assemble-ins-dataprocessing cond 'bic set-cond a0 d a1 a2))
          (bl (u/gba/assemble-ins-branch cond t dimm))
          (bx (u/gba/assemble-ins-branchexchange cond d))
          (cdp (error "Unsupported instruction: %s" ins))
          (cmn (u/gba/assemble-ins-dataprocessing cond 'cmn set-cond a0 d a1 a2))
          (cmp (u/gba/assemble-ins-dataprocessing cond 'cmp set-cond a0 d a1 a2))
          (eor (u/gba/assemble-ins-dataprocessing cond 'eor set-cond a0 d a1 a2))
          (ldc (error "Unsupported instruction: %s" ins))
          (ldm (u/gba/assemble-ins-bdt cond (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'psr) (-contains? opbase 'wb) t d a0))
          (ldr
           (u/gba/assemble-ins-sdt
            cond
            (u/gba/assemble-reg a1) (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'byte) (-contains? opbase 'wb) t
            d a0 a1 a2))
          (mcr (error "Unsupported instruction: %s" ins))
          (mla (u/gba/assemble-ins-multiply cond t set-cond d a2 a1 a0))
          (mov (u/gba/assemble-ins-dataprocessing cond 'mov set-cond 'r0 d a0 a1))
          (mrc (error "Unsupported instruction: %s" ins))
          (mrs (error "Unsupported instruction: %s" ins))
          (msr (error "Unsupported instruction: %s" ins))
          (mul (u/gba/assemble-ins-multiply cond nil set-cond d nil a1 a0))
          (mvn (u/gba/assemble-ins-dataprocessing cond 'mvn set-cond a0 d a1 a2))
          (orr (u/gba/assemble-ins-dataprocessing cond 'orr set-cond a0 d a1 a2))
          (rsb (u/gba/assemble-ins-dataprocessing cond 'rsb set-cond a0 d a1 a2))
          (rsc (u/gba/assemble-ins-dataprocessing cond 'rsc set-cond a0 d a1 a2))
          (sbc (u/gba/assemble-ins-dataprocessing cond 'sbc set-cond a0 d a1 a2))
          (stc (error "Unsupported instruction: %s" ins))
          (stm (u/gba/assemble-ins-bdt cond (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'psr) (-contains? opbase 'wb) nil d a0))
          (str
           (u/gba/assemble-ins-sdt
            cond
            (u/gba/assemble-reg a1) (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'byte) (-contains? opbase 'wb) nil
            d a0 a1 a2))
          (sub (u/gba/assemble-ins-dataprocessing cond 'sub set-cond a0 d a1 a2))
          (swi (u/gba/assemble-ins-interrupt cond d))
          (swp (u/gba/assemble-ins-singledataswap cond (-contains? opbase 'swap) a0 a1 a2))
          (teq (u/gba/assemble-ins-dataprocessing cond 'teq set-cond a0 d a1 a2))
          (tst (u/gba/assemble-ins-dataprocessing cond 'tst set-cond a0 d a1 a2))
          (t (error "Unknown opcode: %s" ins)))))
    (when check
      (unless (equal res (u/gba/assemble-ins-string-system (u/gba/render-ins ins)))
        (error "Instruction %s did not match system assembly" ins)))
    res))

(defun u/gba/assemble (prog)
  "Assemble the list of instructions PROG."
  (--mapcat
   (u/gba/assemble-ins it)
   prog))

(defun u/gba/test-assemble-ins (ins)
  "Compare our assembly of INS with arm-none-eabi-as."
  (let ((render (u/gba/render-ins ins)))
    (equal
     (u/gba/assemble-ins ins)
     (u/gba/assemble-ins-string-system render))))

;;;; Linker and header generation
(defun u/gba/relocate-ins (symtab idx ins)
  "Replace all keywords in INS by looking up relative addresses in SYMTAB.
We assume that this instruction is at word IDX in memory."
  (--map
   (if (keywordp it)
       (if-let ((ent (u/symtab-lookup-relative symtab idx it)))
           ent
         (error "Unknown symbol: %s" it))
     it)
   ins))

(defun u/gba/relocate (symtab base prog)
  "Replace all keywords in PROG by looking up relative addresses in SYMTAB.
We assume that PROG starts in memory at word BASE."
  (--map-indexed (u/gba/relocate-ins symtab (+ base it-index) it) prog))

(defun u/gba/link (symtab base size)
  "Convert SYMTAB to a vector of bytes to be placed at address BASE.
SIZE is the length of the resulting vector."
  (let* ((mem (make-vector size 0))
         (memwrite
          (lambda (name addr idx bytes)
            (if (and (>= idx 0) (< (+ idx (length bytes)) size))
                (u/write! mem idx bytes)
              (warn "Symbol table entry %s at %s is out of bounds" name addr)))))
    (--each (ht->alist (u/symtab-symbols symtab))
      (let* ((name (car it))
             (entry (cdr it))
             (type (u/symtab-entry-type entry))
             (addr (u/symtab-entry-addr entry))
             (idx (- addr base)))
        (cl-case type
          (code
           (let* ((relocated (u/gba/relocate symtab (/ addr 4) (u/symtab-entry-data entry)))
                  (bytes (u/gba/assemble relocated)))
             (funcall memwrite name addr idx bytes)))
          (bytes
           (funcall memwrite name addr idx (u/symtab-entry-data entry)))
          (var nil)
          (const
           (funcall
            memwrite name addr idx
            (funcall (u/symtab-entry-data entry) symtab addr)))
          (t (error "Unknown symbol table entry type: %s" (u/symtab-entry-type it))))))
    mem))

(cl-defstruct (u/gba/header (:constructor u/gba/make-header))
  entry ;; instruction branching to entrypoint
  title ;; game title, max 12 characters
  code ;; game code, 4 characters
  maker ;; make code, 2 characters
  )

(defun u/gba/header (header)
  "Return a function from a symbol table to a HEADER."
  (lambda (symtab addr)
   (-concat
    (u/gba/assemble-ins
     `(b ,(or (u/symtab-lookup-relative symtab (/ addr 4) (u/gba/header-entry header)) -2)))
    (u/pad-to 156 '()) ;; nintendo logo
    (u/pad-to 12 (seq-into (s-upcase (u/gba/header-title header)) 'list)) ;; game title
    (u/pad-to 4 (seq-into (s-upcase (u/gba/header-code header)) 'list)) ;; game code
    (u/pad-to 2 (seq-into (s-upcase (u/gba/header-maker header)) 'list)) ;; maker code
    '(#x96) ;; fixed value
    '(#x00) ;; main unit code
    '(#x00) ;; device type
    (u/pad-to 7 '()) ;; reserved area
    '(#x00) ;; software version
    '(#x00) ;; header checksum
    (u/pad-to 2 '()) ;; reserved area
    )))

;;;; Code generation blocks
(defvar u/gba/codegen nil) ;; dynamically bound code generation context
(cl-defstruct (u/gba/codegen (:constructor u/gba/make-codegen))
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
(defun u/gba/codegen-extract ()
  "Extract the generated code from the current codegen context."
  (--map-indexed
   (u/gba/codegen-replace-local-labels it it-index)
   (reverse (u/gba/codegen-instructions (u/gba/codegen)))))
(defmacro u/gba/gen (&rest body)
  "Run BODY in a new code generation context and return the generated instructions."
  `(let ((u/gba/codegen (u/gba/make-codegen)))
     ,@body
     (u/gba/codegen-extract)))

;;;; Palettes and tilemaps
(cl-defstruct (u/gba/palette (:constructor u/gba/make-palette))
 colors)

(defun u/gba/palette-bytes (pal)
  "Given PAL, return the corresponding bytes."
  (--mapcat
   (let ((r (car it)) (g (cadr it)) (b (caddr it)))
     (u/split16le
      (logior
       (lsh (logand #b11111 (lsh b -3)) 10)
       (lsh (logand #b11111 (lsh g -3)) 5)
       (logand #b11111 (lsh r -3)))))
   (u/gba/palette-colors pal)))

(defun u/gba/palette-closest (pal rgb)
  "Return the index of the color in PAL that is closest to RGB."
  (cdr
   (-min-by
    (-on #'> #'car)
    (--map-indexed
     (cons
      (u/rgb-color-distance it rgb)
      it-index)
     (u/gba/palette-colors pal)))))

(defun u/gba/image-quantize-palette (pal)
  "Return a quantize function that will return indices in PAL."
  (lambda (rgb)
    (u/gba/palette-closest pal rgb)))

(defun u/gba/image-tiles (quantize image)
  "Convert IMAGE (a list of width, height, pixels) into tile data and a tilemap.
QUANTIZE should be a function that converts an RBG pixel to a 4-bit color index."
  (let*
      ((width (car image))
       (height (cadr image))
       (width-tiles (ceiling (/ width 8.0)))
       (height-tiles (ceiling (/ height 8.0)))
       (quantized (seq-into (-map quantize (caddr image)) 'vector)))
    (cl-flet*
        ((getpixel (x y)
           (if (or (< x 0) (>= x width) (< y 0) (>= y height))
               0
             (aref quantized (+ x (* y width)))))
         (getrow (x y)
           (--map (getpixel (+ x it) y) (-iota 8)))
         (gettile (tx ty)
           (let ((x (* tx 8))
                 (y (* ty 8)))
             (-flatten
              (--map
               (getrow x (+ y it))
               (-iota 8))))))
      (let* ((tiles
              (-mapcat
               (lambda (y)
                 (-map
                  (lambda (x)
                    (cons (cons x y) (gettile x y)))
                  (-iota width-tiles)))
               (-iota height-tiles))))
        tiles))))

(defun u/gba/tile-s-bytes (stile)
  "Convert the list of nibbles STILE into bytes."
  (if (and stile (car stile) (cadr stile))
      (cons (logior (car stile) (lsh (cadr stile) 4)) (u/gba/tile-s-bytes (cddr stile)))
    nil))

;;;; Helpful "macros" / codegen snippets
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

(defun u/gba/constant (r constant)
  "Generate code loading the (maximum 32-bit CONSTANT) into R."
  (unless (integerp constant)
    (error "Attempt to generate bad constant: %s" constant))
  (u/gba/gen
   (u/gba/emit! `(mov ,r ,(logand #x000000ff constant)))
   (when (> constant #xff)
     (u/gba/emit! `(orr ,r ,r ,(lsh (logand #x0000ff00 constant) -8) 12)))
   (when (> constant #xffff)
     (u/gba/emit! `(orr ,r ,r ,(lsh (logand #x00ff0000 constant) -16) 8)))
   (when (> constant #xffffff)
     (u/gba/emit! `(orr ,r ,r ,(lsh (logand #xff000000 constant) -24) 4)))))

(defun u/gba/addr (r symtab sym)
  "Generate code loading the address in SYMTAB for SYM into R."
  (u/gba/constant r (u/symtab-entry-addr (u/symtab-lookup symtab sym))))

(defun u/gba/write-struct (r st &rest fields)
  "Given a `u/structdef' ST, write FIELDS to the address in R."
  (let ((field-offsets (-sort (-on #'< #'cdr) (ht->alist (u/structdef-fields st))))
        (prev 0))
    (u/gba/gen
     (--each field-offsets
       (when-let ((v (plist-get fields (car it))))
         (cond
          ((integerp v) ;; integer constant
           (u/gba/emit!
            (u/gba/constant u/gba/scratch v)
            `(str wb ,u/gba/scratch ,r ,(- (cdr it) prev))))
          ((symbolp v) ;; another register
           (u/gba/emit!
            `(str wb ,v ,r ,(- (cdr it) prev))))
          ((listp v) ;; a list of bytes
           (error "TODO: pack lists of bytes into words and write them"))
          (t
           (error "Unsupported structure field value: %s" v)))
         (setf prev (cdr it)))))))

(defun u/gba/write-pixel (x y r g b)
  "Write R G B pixel to X,Y."
  `(,@(u/gba/constant 'r0 (+ #x06000000 (* 2 (+ x (* y 240)))))
    ,@(u/gba/constant
       'r1
       (logior
        (lsh (logand #b11111 r) 10)
        (lsh (logand #b11111 g) 5)
        (logand #b11111 b)))
    (str byte r1 r0)
    (mov r1 r1 (lsr 8))
    (str byte r1 r0 1)))

(defun u/gba/test ()
  "Test function."
  (u/gba/gen ;; len in r1, src in r2, dst in r3
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

(provide 'udc-gba)
;;; udc-gba.el ends here
