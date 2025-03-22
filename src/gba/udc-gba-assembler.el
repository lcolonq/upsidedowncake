;;; udc-gba-assembler --- GBA assembler -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)

;;;; ARM assembler
(defun u/gba/arm-assemble-ins-string-system (ins)
  "Assemble the instruction mnemonic string INS using arm-none-eabi-as."
  (let ( (asmpath (f-join "/tmp" (format "%s.asm" (make-temp-name "udc_gba_"))))
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

(defun u/gba/arm-assemble-imm (x)
  "Return the value for X if X is an immediate."
  (and (integerp x) x))

(defconst u/gba/arm-flags '(s post down pst wb byte))
(defconst u/gba/arm-conds '(eq ne cs cc mi pl vs vc hi ls ge lt gt le al))
(defun u/gba/arm-assemble-cond (cond)
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

(defun u/gba/arm-assemble-reg (reg)
  "Given REG, return its 4-bit encoding."
  (cl-case reg
    (r0 0) (r1 1) (r2 2) (r3 3) (r4 4) (r5 5) (r6 6) (r7 7)
    (r8 8) (r9 9) (r10 10) (r11 11) (r12 12) (r13 13) (r14 14) (r15 15)
    (t (error "Invalid register: %s" reg))))

(defun u/gba/arm-assemble-flag (flag)
  "Given FLAG, return its 1-bit encoding."
  (if flag 1 0))

(defun u/gba/arm-assemble-ins-dataprocessing-op (op)
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

(defun u/gba/arm-assemble-dataprocessing-shift-type (ty)
  "Given TY, return its 2-bit shift type."
  (cl-case ty
    (lsl #b00)
    (lsr #b01)
    (asr #b10)
    (ror #b11)
    (t (error "Invalid shift type: %s" ty))))

(defun u/gba/arm-assemble-ins-dataprocessing-arg2-shift (arg shift)
  "Given ARG and SHIFT, return properly assembled data."
  (let ((reg (u/gba/arm-assemble-reg arg))
         (shiftreg (and (listp shift) (u/gba/arm-assemble-reg (cadr shift))))
         (shty (and (listp shift) (car shift) (u/gba/arm-assemble-dataprocessing-shift-type (car shift)))))
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

(defun u/gba/arm-assemble-ins-dataprocessing (cond op set-cond arg1 dest arg2 shift)
  "Given COND, OP, SET-COND, ARG1, DEST, ARG2, and SHIFT produce 4 bytes."
  (let ((a2 (if arg2 (u/gba/arm-assemble-ins-dataprocessing-arg2-shift arg2 shift) (list nil 0))))
    (u/split32le
      (logior
        (lsh (u/gba/arm-assemble-cond cond) 28)
        (lsh #b00 26)
        (lsh (u/gba/arm-assemble-flag (car a2)) 25)
        (lsh (u/gba/arm-assemble-ins-dataprocessing-op op) 21)
        (lsh (u/gba/arm-assemble-flag set-cond) 20)
        (lsh (u/gba/arm-assemble-reg arg1) 16)
        (lsh (u/gba/arm-assemble-reg dest) 12)
        (cadr a2)))))

(defun u/gba/arm-assemble-ins-multiply (cond acc set-cond rd rn rs rm)
  "Given COND, ACC, SET-COND, RD, RN, RS, RM produce 4 bytes."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b000000 22)
      (lsh (u/gba/arm-assemble-flag acc) 21)
      (lsh (u/gba/arm-assemble-flag set-cond) 20)
      (lsh (u/gba/arm-assemble-reg rd) 16)
      (lsh (or (u/gba/arm-assemble-reg rn) 0) 12)
      (lsh (u/gba/arm-assemble-reg rs) 8)
      (lsh #b1001 4)
      (u/gba/arm-assemble-reg rm))))

(defun u/gba/arm-assemble-ins-multiplylong (cond signed acc set-cond rdhi rdlo rs rm)
  "Given COND, SIGNED, ACC, SET-COND, RDHI, RDLO, RS, RM produce 4 bytes."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b00001 23)
      (lsh (u/gba/arm-assemble-flag signed) 22)
      (lsh (u/gba/arm-assemble-flag acc) 21)
      (lsh (u/gba/arm-assemble-flag set-cond) 20)
      (lsh (u/gba/arm-assemble-reg rdhi) 16)
      (lsh (u/gba/arm-assemble-reg rdlo) 12)
      (lsh (u/gba/arm-assemble-reg rs) 8)
      (lsh #b1001 4)
      (u/gba/arm-assemble-reg rm))))

(defun u/gba/arm-assemble-ins-singledataswap (cond byte rn rd rm)
  "Given COND, BYTE, RN, RD, and RM, produce 4 bytes."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b00010 23)
      (lsh (u/gba/arm-assemble-flag byte) 22)
      (lsh #b00 20)
      (lsh (u/gba/arm-assemble-reg rn) 16)
      (lsh (u/gba/arm-assemble-reg rd) 12)
      (lsh #b00001001 4)
      (u/gba/arm-assemble-reg rm))))

(defun u/gba/arm-assemble-ins-branchexchange (cond rn)
  "Given COND and RN produce 4 bytes."
  (let ((reg (u/gba/arm-assemble-reg rn)))
    (unless reg
      (error "Invalid BX target: %s" rn))
    (u/split32le
      (logior
        (lsh (u/gba/arm-assemble-cond cond) 28)
        (lsh #b000100101111111111110001 4)
        reg))))

(defun u/gba/arm-assemble-ins-hdtregister (cond pre up writeback load rn rd signed halfword rm)
  "Given COND, PRE, UP, WRITEBACK, LOAD, RN, RD, SIGNED, HALFWORD, and RM, :3."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b000 25)
      (lsh (u/gba/arm-assemble-flag pre) 24)
      (lsh (u/gba/arm-assemble-flag up) 23)
      (lsh #b0 22)
      (lsh (u/gba/arm-assemble-flag writeback) 21)
      (lsh (u/gba/arm-assemble-flag load) 20)
      (lsh (u/gba/arm-assemble-reg rn) 16)
      (lsh (u/gba/arm-assemble-reg rd) 12)
      (lsh #b00001 7)
      (lsh (u/gba/arm-assemble-flag signed) 6)
      (lsh (u/gba/arm-assemble-flag halfword) 5)
      (lsh #b1 4)
      (u/gba/arm-assemble-reg rm))))

(defun u/gba/arm-assemble-ins-hdtimmediate (cond pre up writeback load rn rd offset1 signed halfword offset2)
  "Produce 4 bytes.
Given COND, PRE, UP, WRITEBACK, LOAD, RN, RD, OFFSET1, SIGNED,
HALFWORD, and OFFSET2."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b000 25)
      (lsh (u/gba/arm-assemble-flag pre) 24)
      (lsh (u/gba/arm-assemble-flag up) 23)
      (lsh #b1 22)
      (lsh (u/gba/arm-assemble-flag writeback) 21)
      (lsh (u/gba/arm-assemble-flag load) 20)
      (lsh (u/gba/arm-assemble-reg rn) 16)
      (lsh (u/gba/arm-assemble-reg rd) 12)
      (lsh (logand offset1 #xf) 8)
      (lsh #b1 7)
      (lsh (u/gba/arm-assemble-flag signed) 6)
      (lsh (u/gba/arm-assemble-flag halfword) 5)
      (lsh #b1 4)
      (logand offset2 #xf))))

(defun u/gba/arm-assemble-ins-sdt-offset (arg shift)
  "Given ARG and SHIFT, return properly assembled data."
  (let ((reg (u/gba/arm-assemble-reg arg))
         (shiftreg (u/gba/arm-assemble-reg (cadr shift)))
         (shty (and (car shift) (u/gba/arm-assemble-dataprocessing-shift-type (car shift)))))
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

(defun u/gba/arm-assemble-ins-sdt (cond imm pre up byte writeback load rd rn offset shift)
  "Given COND, IMM, PRE, UP, BYTE, WRITEBACK, LOAD, RN, RD, OFFSET, and SHIFT."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b01 26)
      (lsh (u/gba/arm-assemble-flag imm) 25)
      (lsh (u/gba/arm-assemble-flag pre) 24)
      (lsh (u/gba/arm-assemble-flag up) 23)
      (lsh (u/gba/arm-assemble-flag byte) 22)
      (lsh (u/gba/arm-assemble-flag writeback) 21)
      (lsh (u/gba/arm-assemble-flag load) 20)
      (lsh (u/gba/arm-assemble-reg rn) 16)
      (lsh (u/gba/arm-assemble-reg rd) 12)
      (u/gba/arm-assemble-ins-sdt-offset offset shift))))

(defun u/gba/arm-assemble-ins-bdt-reglist (reglist)
  "Return a 16-bit integer representing REGLIST."
  (--reduce-from
    (logior acc (lsh 1 (u/gba/arm-assemble-reg it)))
    0
    reglist))
(defun u/gba/arm-assemble-ins-bdt (cond pre up byte writeback load rn reglist)
  "Given COND, PRE, UP, BYTE, WRITEBACK, LOAD, RN, and REGLIST, :3."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b100 25)
      (lsh (u/gba/arm-assemble-flag pre) 24)
      (lsh (u/gba/arm-assemble-flag up) 23)
      (lsh (u/gba/arm-assemble-flag byte) 22)
      (lsh (u/gba/arm-assemble-flag writeback) 21)
      (lsh (u/gba/arm-assemble-flag load) 20)
      (lsh (u/gba/arm-assemble-reg rn) 16)
      (logand (u/gba/arm-assemble-ins-bdt-reglist reglist) #xffff))))

(defun u/gba/arm-assemble-ins-branch (cond link offset)
  "Given COND, LINK, and OFFSET, produce 4 bytes."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b101 25)
      (lsh (u/gba/arm-assemble-flag link) 24)
      (logand offset #xffffff))))

(defun u/gba/arm-assemble-ins-interrupt (cond data)
  "Given COND and DATA, produce 4 bytes."
  (u/split32le
    (logior
      (lsh (u/gba/arm-assemble-cond cond) 28)
      (lsh #b1111 24)
      (logand data #xffffff))))

(defun u/gba/arm-render-op (op)
  "Convert the symbol OP to a mnemonic string."
  (format "%s" op))
(defun u/gba/arm-render-cond (cond)
  "Convert the symbol COND to a mnemonic string."
  (format "%s" cond))
(defun u/gba/arm-render-arg (arg)
  "Convert ARG to a mnemonic string."
  (cond
    ((symbolp arg) (format "%s" arg))
    ((integerp arg) (format "#%s" arg))
    ((listp arg) ;; we assume this is a shift specifier
      (format "%s %s" (u/gba/arm-render-arg (car arg)) (u/gba/arm-render-arg (cadr arg))))))
(defun u/gba/arm-render-ins (ins)
  "Convert the S-expression INS to a GAS-syntax string."
  (let* ((op (if (listp ins) (car ins) ins))
          (opbase (if (listp ins) ins nil))
          (cond (car (--filter (-contains? u/gba/arm-conds it) opbase)))
          (rcond (if cond (u/gba/arm-render-cond cond) ""))
          (args (--filter (not (-contains? (cons cond u/gba/arm-flags) it)) (cdr opbase))))
    (cond
      ((-contains? '(b bl) op)
        (s-concat (u/gba/arm-render-op op) rcond " " (format "pc+%d" (* (+ (car args) 2) 4))))
      ((-contains? '(ldr str ldrh strh) op)
        (s-concat
          (u/gba/arm-render-op op) rcond (if (-contains? opbase 'byte) "b" "") " "
          (u/gba/arm-render-arg (car args)) ", ["
          (u/gba/arm-render-arg (cadr args)) ", "
          (if (caddr args) (u/gba/arm-render-arg (caddr args)) "#0")
          "]"))
      (t
        (s-concat
          (u/gba/arm-render-op op)
          rcond " "
          (s-join ", " (-map #'u/gba/arm-render-arg args)))))))

(defun u/gba/arm-assemble-ins (ins &optional check)
  "Assemble INS to a sequence of bytes.
If CHECK is non-nil, check against system assembler.
INS is either:
 - an opcode symbol
 - a list of an opcode symbol followed by operands"
  (let*
    ( (op (if (listp ins) (car ins) ins))
      (opbase (if (listp ins) ins nil))
      (set-cond (-contains? opbase 's))
      (cond (or (car (--filter (-contains? u/gba/arm-conds it) opbase)) 'al))
      (args (--filter (not (-contains? (cons cond u/gba/arm-flags) it)) (cdr opbase)))
      (d (car args))
      (dimm (u/gba/arm-assemble-imm d))
      (a0 (cadr args))
      (a1 (caddr args))
      (a2 (cadddr args))
      (res
        (cl-case op
          (adc (u/gba/arm-assemble-ins-dataprocessing cond 'adc set-cond a0 d a1 a2))
          (add (u/gba/arm-assemble-ins-dataprocessing cond 'add set-cond a0 d a1 a2))
          (and (u/gba/arm-assemble-ins-dataprocessing cond 'and set-cond a0 d a1 a2))
          (b (u/gba/arm-assemble-ins-branch cond nil dimm))
          (bic (u/gba/arm-assemble-ins-dataprocessing cond 'bic set-cond a0 d a1 a2))
          (bl (u/gba/arm-assemble-ins-branch cond t dimm))
          (brk (u/split32le (logior (lsh (u/gba/arm-assemble-cond cond) 28) (lsh #b011 25) (lsh #b1 4))))
          (bx (u/gba/arm-assemble-ins-branchexchange cond d))
          (cdp (error "Unsupported instruction: %s" ins))
          (cmn (u/gba/arm-assemble-ins-dataprocessing cond 'cmn set-cond a0 d a1 a2))
          (cmp (u/gba/arm-assemble-ins-dataprocessing cond 'cmp set-cond a0 d a1 a2))
          (eor (u/gba/arm-assemble-ins-dataprocessing cond 'eor set-cond a0 d a1 a2))
          (ldc (error "Unsupported instruction: %s" ins))
          (ldm (u/gba/arm-assemble-ins-bdt cond (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'psr) (-contains? opbase 'wb) t d a0))
          (ldr
            (u/gba/arm-assemble-ins-sdt
              cond
              (u/gba/arm-assemble-reg a1) (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'byte) (-contains? opbase 'wb) t
              d a0 a1 a2))
          (ldrh
            (if-let* ((roff (u/gba/arm-assemble-reg a1)))
              (u/gba/arm-assemble-ins-hdtregister
                cond
                (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'wb) t
                a0 d (-contains? opbase 'signed) (not (-contains? opbase 'byte))
                a1)
              (u/gba/arm-assemble-ins-hdtimmediate
                cond
                (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'wb) t
                a0 d
                (logand #xf (lsh (or a1 0) -4))
                (-contains? opbase 'signed) (not (-contains? opbase 'byte))
                (logand #xf (or a1 0)))))
          (mcr (error "Unsupported instruction: %s" ins))
          (mla (u/gba/arm-assemble-ins-multiply cond t set-cond d a2 a1 a0))
          (mov (u/gba/arm-assemble-ins-dataprocessing cond 'mov set-cond 'r0 d a0 a1))
          (mrc (error "Unsupported instruction: %s" ins))
          (mrs (error "Unsupported instruction: %s" ins))
          (msr (error "Unsupported instruction: %s" ins))
          (mul (u/gba/arm-assemble-ins-multiply cond nil set-cond d nil a1 a0))
          (mvn (u/gba/arm-assemble-ins-dataprocessing cond 'mvn set-cond 'r0 d a0 a1))
          (orr (u/gba/arm-assemble-ins-dataprocessing cond 'orr set-cond a0 d a1 a2))
          (rsb (u/gba/arm-assemble-ins-dataprocessing cond 'rsb set-cond a0 d a1 a2))
          (rsc (u/gba/arm-assemble-ins-dataprocessing cond 'rsc set-cond a0 d a1 a2))
          (sbc (u/gba/arm-assemble-ins-dataprocessing cond 'sbc set-cond a0 d a1 a2))
          (stc (error "Unsupported instruction: %s" ins))
          (stm (u/gba/arm-assemble-ins-bdt cond (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'psr) (-contains? opbase 'wb) nil d a0))
          (str
            (u/gba/arm-assemble-ins-sdt
              cond
              (u/gba/arm-assemble-reg a1) (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'byte) (-contains? opbase 'wb) nil
              d a0 a1 a2))
          (strh
            (if-let* ((roff (u/gba/arm-assemble-reg a1)))
              (u/gba/arm-assemble-ins-hdtregister
                cond
                (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'wb) nil
                a0 d (-contains? opbase 'signed) (not (-contains? opbase 'byte))
                a1)
              (u/gba/arm-assemble-ins-hdtimmediate
                cond
                (not (-contains? opbase 'post)) (not (-contains? opbase 'down)) (-contains? opbase 'wb) nil
                a0 d
                (logand #xf (lsh (or a1 0) -4))
                (-contains? opbase 'signed) (not (-contains? opbase 'byte))
                (logand #xf (or a1 0)))))
          (sub (u/gba/arm-assemble-ins-dataprocessing cond 'sub set-cond a0 d a1 a2))
          (swi (u/gba/arm-assemble-ins-interrupt cond d))
          (swp (u/gba/arm-assemble-ins-singledataswap cond (-contains? opbase 'swap) a0 a1 a2))
          (teq (u/gba/arm-assemble-ins-dataprocessing cond 'teq set-cond a0 d a1 a2))
          (tst (u/gba/arm-assemble-ins-dataprocessing cond 'tst set-cond a0 d a1 a2))
          (t (error "Unknown opcode: %s" ins)))))
    (when check
      (unless (equal res (u/gba/arm-assemble-ins-string-system (u/gba/arm-render-ins ins)))
        (error "Instruction %s did not match system assembly" ins)))
    res))

(defun u/gba/arm-assemble (prog)
  "Assemble the list of instructions PROG."
  (--mapcat
    (u/gba/arm-assemble-ins it)
    prog))

(defun u/gba/arm-test-assemble-ins (ins)
  "Compare our assembly of INS with arm-none-eabi-as."
  (let ((render (u/gba/arm-render-ins ins)))
    (equal
      (u/gba/arm-assemble-ins ins)
      (u/gba/arm-assemble-ins-string-system render))))

;;;; Thumb assembler
(defun u/gba/thumb-assemble-ins-string-system (ins)
  "Assemble the instruction mnemonic string INS using arm-none-eabi-as."
  (let ( (asmpath (f-join "/tmp" (format "%s.asm" (make-temp-name "udc_gba_"))))
         (objpath (f-join "/tmp" (format "%s.o" (make-temp-name "udc_gba_"))))
         (dumppath (f-join "/tmp" (format "%s.dump" (make-temp-name "udc_gba_"))))
         (as (executable-find "arm-none-eabi-as"))
         (objcopy (executable-find "arm-none-eabi-objcopy")))
    (with-temp-buffer
      (f-write-text ins 'utf-8 asmpath)
      (unless (= 0 (call-process as nil (current-buffer) nil asmpath "-mthumb" "-o" objpath))
        (message (buffer-string))
        (error "Failed to assemble"))
      (unless (= 0 (call-process objcopy nil (current-buffer) nil "-O" "binary" "-j" ".text" objpath dumppath))
        (message (buffer-string))
        (error "Failed to objcopy"))
      (seq-into (f-read-bytes dumppath) 'list))))
(defun u/gba/thumb-assemble-reg (reg)
  "Given REG, return its 3-bit encoding."
  (cl-case reg
    (r0 0) (r1 1) (r2 2) (r3 3) (r4 4) (r5 5) (r6 6) (r7 7)))

(defun u/gba/thumb-assemble-imm (x)
  "Return the value for X if X is an immediate."
  (and (integerp x) x))

(defun u/gba/thumb-assemble-flag (flag)
  "Given FLAG, return its 1-bit encoding."
  (if flag 1 0))

(defun u/gba/thumb-assemble-ins-moveshiftedregister-op (op)
  "Given OP, return its 2-bit encoding."
  (cl-case op
    (lsl #b00) (lsr #b01) (asr #b10)
    (t (error "Invalid shift operator: %s" op))))
(defun u/gba/thumb-assemble-ins-moveshiftedregister (op offset src dest)
  "Given OP, OFFSET, SRC, and DEST produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b000 13)
      (lsh (u/gba/thumb-assemble-ins-moveshiftedregister-op op) 11)
      (lsh (logand #b11111 offset) 6)
      (lsh (u/gba/thumb-assemble-reg src) 3)
      (u/gba/thumb-assemble-reg dest))))

(defun u/gba/thumb-assemble-ins-addsubtract-op (op)
  "Given OP, return its 1-bit encoding."
  (cl-case op
    (add #b0) (sub #b1)
    (t (error "Invalid add/subtract operator: %s" op))))
(defun u/gba/thumb-assemble-ins-addsubtract (op rn src dest)
  "Given OP, RN, SRC, and DEST produce 2 bytes."
  (let ((imm (u/gba/thumb-assemble-imm rn)))
    (u/split16le
      (logior
        (lsh #b00011 11)
        (lsh (u/gba/thumb-assemble-flag imm) 10)
        (lsh (u/gba/thumb-assemble-ins-addsubtract-op op) 9)
        (lsh (if imm rn (u/gba/thumb-assemble-reg rn)) 6)
        (lsh (u/gba/thumb-assemble-reg src) 3)
        (u/gba/thumb-assemble-reg dest)))))

(defun u/gba/thumb-assemble-ins-mcas-immediate-op (op)
  "Given OP, return its 2-bit encoding."
  (cl-case op
    (mov #b00) (cmp #b01) (inc #b10) (dec #b11)
    (t (error "Invalid move/compare/add/subtract immediate operator: %s" op))))
(defun u/gba/thumb-assemble-ins-mcas-immediate (op dest offset)
  "Given OP, DEST, and OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b001 13)
      (lsh (u/gba/thumb-assemble-ins-mcas-immediate-op op) 11)
      (lsh (u/gba/thumb-assemble-reg dest) 8)
      (logand #xff offset))))

(defun u/gba/thumb-assemble-ins-alu-op (op)
  "Given OP, return its 4-bit encoding."
  (cl-case op
    (and #b0000) (eor #b0001) (lsl #b0010) (lsr #b0011)
    (asr #b0100) (adc #b0101) (sbc #b0110) (ror #b0111)
    (tst #b1000) (neg #b1001) (cmp #b1010) (cmn #b1011)
    (orr #b1100) (mul #b1101) (bic #b1110) (mvn #b1111)
    (t (error "Invalid ALU operator: %s" op))))
(defun u/gba/thumb-assemble-ins-alu (op src dest)
  "Given OP, SRC, and DEST produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b010000 10)
      (lsh (u/gba/thumb-assemble-ins-alu-op op) 6)
      (lsh (u/gba/thumb-assemble-reg src) 3)
      (u/gba/thumb-assemble-reg dest))))

(defun u/gba/thumb-assemble-ins-hi-op (op)
  "Given OP, return its 2-bit encoding."
  (cl-case op
    (add #b00) (cmp #b01) (mov #b10) (bx #b11)
    (t (error "Invalid HI register operator: %s" op))))
(defun u/gba/thumb-assemble-ins-hi-reg (reg)
  "Given REG, return its 3-bit encoding."
  (cl-case reg
    (r8 0) (r9 1) (r10 2) (r11 3) (r12 4) (r13 5) (r14 6) (r15 7)
    (t (error "Invalid HI register: %s" reg))))
(defun u/gba/thumb-assemble-ins-hi (op src dest)
  "Given OP, SRC, and DEST produce 2 bytes."
  (let* ( (rs (u/gba/thumb-assemble-reg src))
          (hs (u/gba/thumb-assemble-ins-hi-reg src))
          (rd (u/gba/thumb-assemble-reg dest))
          (hd (u/gba/thumb-assemble-ins-hi-reg dest)))
    (u/split16le
      (logior
        (lsh #b010001 10)
        (lsh (u/gba/thumb-assemble-ins-hi-op op) 8)
        (lsh (u/gba/thumb-assemble-flag hd) 7)
        (lsh (u/gba/thumb-assemble-flag hs) 6)
        (lsh (or hs rs) 3)
        (or hd rd)))))

(defun u/gba/thumb-assemble-ins-pcrelativeload (dest offset)
  "Given DEST and OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b01001 11)
      (lsh (u/gba/thumb-assemble-reg dest) 8)
      (logand #xff offset))))

(defun u/gba/thumb-assemble-ins-loadstore (load byte offset base dest)
  "Given LOAD, BYTE, OFFSET, BASE, and DEST produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b0101 12)
      (lsh (u/gba/thumb-assemble-flag load) 11)
      (lsh (u/gba/thumb-assemble-flag byte) 10)
      (lsh #b0 9)
      (lsh (u/gba/thumb-assemble-reg offset) 6)
      (lsh (u/gba/thumb-assemble-reg base) 3)
      (u/gba/thumb-assemble-reg dest))))

(defun u/gba/thumb-assemble-ins-loadstoresext (h sign offset base dest)
  "Given H, SIGN, OFFSET, BASE, and DEST produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b0101 12)
      (lsh (u/gba/thumb-assemble-flag h) 11)
      (lsh (u/gba/thumb-assemble-flag sign) 10)
      (lsh #b1 9)
      (lsh (u/gba/thumb-assemble-reg offset) 6)
      (lsh (u/gba/thumb-assemble-reg base) 3)
      (u/gba/thumb-assemble-reg dest))))

(defun u/gba/thumb-assemble-ins-loadstoreimmediate (byte load offset base dest)
  "Given LOAD, BYTE, OFFSET, BASE, and DEST produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b011 13)
      (lsh (u/gba/thumb-assemble-flag byte) 12)
      (lsh (u/gba/thumb-assemble-flag load) 11)
      (lsh (logand #b11111 offset) 6)
      (lsh (u/gba/thumb-assemble-reg base) 3)
      (u/gba/thumb-assemble-reg dest))))

(defun u/gba/thumb-assemble-ins-loadstorehalfword (load offset base dest)
  "Given LOAD, OFFSET, BASE, and DEST produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b1000 12)
      (lsh (u/gba/thumb-assemble-flag load) 11)
      (lsh (logand #b11111 offset) 6)
      (lsh (u/gba/thumb-assemble-reg base) 3)
      (u/gba/thumb-assemble-reg dest))))

(defun u/gba/thumb-assemble-ins-sprelativeloadstore (load dest offset)
  "Given LOAD, DEST, and OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b1001 12)
      (lsh (u/gba/thumb-assemble-flag load) 11)
      (lsh (u/gba/thumb-assemble-reg dest) 8)
      (logand #xff offset))))

(defun u/gba/thumb-assemble-ins-loadaddress (sp dest offset)
  "Given SP, DEST, and OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b1010 12)
      (lsh (u/gba/thumb-assemble-flag sp) 11)
      (lsh (u/gba/thumb-assemble-reg dest) 8)
      (logand #xff offset))))

(defun u/gba/thumb-assemble-ins-addoffsetstackpointer (sign offset)
  "Given SIGN and OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b10110000 8)
      (lsh (u/gba/thumb-assemble-flag sign) 7)
      (logand #b1111111 offset))))

(defun u/gba/thumb-assemble-ins-pushpop-reglist (reglist)
  "Return a 8-bit integer representing REGLIST."
  (--reduce-from
    (logior acc (lsh 1 (u/gba/thumb-assemble-reg it)))
    0
    reglist))
(defun u/gba/thumb-assemble-ins-pushpop (load regs)
  "Given LOAD and REGS produce 2 bytes."
  (let* ( (pclr (or (-contains? regs 'pc) (-contains? regs 'lr)))
          (rnopclr (--filter (not (-contains? '(pc lr) it)) regs))
          (rlist (u/gba/thumb-assemble-ins-pushpop-reglist rnopclr)))
    (u/split16le
      (logior
        (lsh #b1011 12)
        (lsh (u/gba/thumb-assemble-flag load) 11)
        (lsh #b10 9)
        (lsh (u/gba/thumb-assemble-flag pclr) 8)
        rlist))))

(defun u/gba/thumb-assemble-ins-multipleloadstore (load base regs)
  "Given LOAD, BASE, and REGS produce 2 bytes."
  (let* ((rlist (u/gba/thumb-assemble-ins-pushpop-reglist regs)))
    (u/split16le
      (logior
        (lsh #b1100 12)
        (lsh (u/gba/thumb-assemble-flag load) 11)
        (lsh (u/gba/thumb-assemble-reg base) 8)
        rlist))))

(defun u/gba/thumb-assemble-ins-conditionalbranch (cond offset)
  "Given COND and OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b1101 12)
      (lsh (u/gba/arm-assemble-cond cond) 8)
      (logand #xff offset))))

(defun u/gba/thumb-assemble-ins-swi (comment)
  "Given COMMENT produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b11011111 8)
      (logand #xff comment))))

(defun u/gba/thumb-assemble-ins-unconditionalbranch (offset)
  "Given OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b11100 11)
      (logand #b11111111111 offset))))

(defun u/gba/thumb-assemble-ins-branchlink (h offset)
  "Given H and OFFSET produce 2 bytes."
  (u/split16le
    (logior
      (lsh #b1111 12)
      (lsh (u/gba/thumb-assemble-flag h) 11)
      (logand #b11111111111 offset))))

(defun u/gba/thumb-render-op (op)
  "Convert the symbol OP to a mnemonic string."
  (format "%s" op))
(defun u/gba/thumb-render-cond (cond)
  "Convert the symbol COND to a mnemonic string."
  (format "%s" cond))
(defun u/gba/thumb-render-arg (arg)
  "Convert ARG to a mnemonic string."
  (cond
    ((symbolp arg) (format "%s" arg))
    ((integerp arg) (format "#%s" arg))
    ((listp arg) ;; we assume this is a shift specifier
      (format "%s %s" (u/gba/thumb-render-arg (car arg)) (u/gba/thumb-render-arg (cadr arg))))))
(defun u/gba/thumb-render-ins (ins)
  "Convert the S-expression INS to a GAS-syntax string."
  (let* ( (op (if (listp ins) (car ins) ins))
          (opbase (if (listp ins) ins nil))
          (args (cdr opbase)))
    (cond
      (t
        (s-concat
          (u/gba/thumb-render-op op)
          " "
          (s-join ", " (-map #'u/gba/thumb-render-arg args)))))))

(defun u/gba/thumb-assemble-ins (ins &optional check)
  "Assemble INS to a sequence of bytes.
If CHECK is non-nil, check against system assembler.
INS is either:
 - an opcode symbol
 - a list of an opcode symbol followed by operands"
  (let*
    ( (op (if (listp ins) (car ins) ins))
      (opbase (if (listp ins) ins nil))
      (args (cdr opbase))
      (o0 (car args))
      (o1 (cadr args))
      (o2 (caddr args))
      (o3 (cadddr args))
      (res
        (cl-case op
          (adc (u/gba/thumb-assemble-ins-alu 'adc o1 o0))
          (add (u/gba/thumb-assemble-ins-addsubtract 'add o2 o1 o0))
          (addhi (u/gba/thumb-assemble-ins-hi 'add o1 o0))
          (addpc (u/gba/thumb-assemble-ins-loadaddress nil o0 o1))
          (addsp (u/gba/thumb-assemble-ins-loadaddress t o0 o1))
          (and (u/gba/thumb-assemble-ins-alu 'and o1 o0))
          (asr (u/gba/thumb-assemble-ins-alu 'asr o1 o0))
          (asrx (u/gba/thumb-assemble-ins-moveshiftedregister 'asr o2 o1 o0))
          (b (u/gba/thumb-assemble-ins-unconditionalbranch o0))
          (beq (u/gba/thumb-assemble-ins-conditionalbranch 'eq o0))
          (bne (u/gba/thumb-assemble-ins-conditionalbranch 'ne o0))
          (bcs (u/gba/thumb-assemble-ins-conditionalbranch 'cs o0))
          (bcc (u/gba/thumb-assemble-ins-conditionalbranch 'cc o0))
          (bmi (u/gba/thumb-assemble-ins-conditionalbranch 'mi o0))
          (bpl (u/gba/thumb-assemble-ins-conditionalbranch 'pl o0))
          (bvs (u/gba/thumb-assemble-ins-conditionalbranch 'vs o0))
          (bhi (u/gba/thumb-assemble-ins-conditionalbranch 'hi o0))
          (bls (u/gba/thumb-assemble-ins-conditionalbranch 'ls o0))
          (bge (u/gba/thumb-assemble-ins-conditionalbranch 'ge o0))
          (blt (u/gba/thumb-assemble-ins-conditionalbranch 'lt o0))
          (bgt (u/gba/thumb-assemble-ins-conditionalbranch 'gt o0))
          (ble (u/gba/thumb-assemble-ins-conditionalbranch 'le o0))
          (bic (u/gba/thumb-assemble-ins-alu 'bic o1 o0))
          (bl0 (u/gba/thumb-assemble-ins-branchlink nil o0))
          (bl1 (u/gba/thumb-assemble-ins-branchlink t o0))
          (bx (u/gba/thumb-assemble-ins-hi 'bx o0 nil))
          (cmn (u/gba/thumb-assemble-ins-alu 'cmn o1 o0))
          (cmp (u/gba/thumb-assemble-ins-alu 'cmp o1 o0))
          (cmpi (u/gba/thumb-assemble-ins-mcas-immediate 'cmp o0 o1))
          (cmphi (u/gba/thumb-assemble-ins-hi 'cmp o1 o0))
          (dec (u/gba/thumb-assemble-ins-mcas-immediate 'dec o0 o1))
          (decsp (u/gba/thumb-assemble-ins-addoffsetstackpointer t o0))
          (eor (u/gba/thumb-assemble-ins-alu 'eor o1 o0))
          (inc (u/gba/thumb-assemble-ins-mcas-immediate 'inc o0 o1))
          (incsp (u/gba/thumb-assemble-ins-addoffsetstackpointer nil o0))
          (ldmia (u/gba/thumb-assemble-ins-multipleloadstore t o0 o1))
          (ldr (u/gba/thumb-assemble-ins-loadstore t nil o2 o1 o0))
          (ldri (u/gba/thumb-assemble-ins-loadstoreimmediate t nil o2 o1 o0))
          (ldrb (u/gba/thumb-assemble-ins-loadstore t t o2 o1 o0))
          (ldrbi (u/gba/thumb-assemble-ins-loadstoreimmediate t t o2 o1 o0))
          (ldrpc (u/gba/thumb-assemble-ins-pcrelativeload o0 o1))
          (ldrsp (u/gba/thumb-assemble-ins-sprelativeloadstore t o0 o1))
          (ldrh (u/gba/thumb-assemble-ins-loadstoresext nil t o2 o1 o0))
          (ldrhi (u/gba/thumb-assemble-ins-loadstorehalfword t o2 o1 o0))
          (lsl (u/gba/thumb-assemble-ins-alu 'lsl o1 o0))
          (lslx (u/gba/thumb-assemble-ins-moveshiftedregister 'lsl o2 o1 o0))
          (ldsb (u/gba/thumb-assemble-ins-loadstoresext t nil o2 o1 o0))
          (ldsh (u/gba/thumb-assemble-ins-loadstoresext t t o2 o1 o0))
          (lsr (u/gba/thumb-assemble-ins-alu 'lsr o1 o0))
          (lsrx (u/gba/thumb-assemble-ins-moveshiftedregister 'lsr o2 o1 o0))
          (mov (u/gba/thumb-assemble-ins-addsubtract 'add 0 o1 o0))
          (movi (u/gba/thumb-assemble-ins-mcas-immediate 'mov o0 o1))
          (movhi (u/gba/thumb-assemble-ins-hi 'mov o1 o0))
          (mul (u/gba/thumb-assemble-ins-alu 'mul o1 o0))
          (mvn (u/gba/thumb-assemble-ins-alu 'mvn o1 o0))
          (neg (u/gba/thumb-assemble-ins-alu 'neg o1 o0))
          (orr (u/gba/thumb-assemble-ins-alu 'orr o1 o0))
          (pop (u/gba/thumb-assemble-ins-pushpop t o0))
          (push (u/gba/thumb-assemble-ins-pushpop nil o0))
          (ror (u/gba/thumb-assemble-ins-alu 'ror o1 o0))
          (sbc (u/gba/thumb-assemble-ins-alu 'sbc o1 o0))
          (stmia (u/gba/thumb-assemble-ins-multipleloadstore nil o0 o1))
          (str (u/gba/thumb-assemble-ins-loadstore nil nil o2 o1 o0))
          (stri (u/gba/thumb-assemble-ins-loadstoreimmediate nil nil o2 o1 o0))
          (strb (u/gba/thumb-assemble-ins-loadstore nil t o2 o1 o0))
          (strbi (u/gba/thumb-assemble-ins-loadstoreimmediate nil t o2 o1 o0))
          (strsp (u/gba/thumb-assemble-ins-sprelativeloadstore nil o0 o1))
          (strh (u/gba/thumb-assemble-ins-loadstoresext nil nil o2 o1 o0))
          (strhi (u/gba/thumb-assemble-ins-loadstorehalfword nil o2 o1 o0))
          (swi (u/gba/thumb-assemble-ins-swi o0))
          (sub (u/gba/thumb-assemble-ins-addsubtract 'sub o2 o1 o0))
          (tst (u/gba/thumb-assemble-ins-alu 'tst o1 o0))
          (t (error "Unknown opcode: %s" ins)))))
    (when check
      (unless (equal res (u/gba/thumb-assemble-ins-string-system (u/gba/thumb-render-ins ins)))
        (error "Instruction %s did not match system assembly" ins)))
    res))

(defun u/gba/thumb-test-assemble-ins (ins)
  "Compare our assembly of INS with arm-none-eabi-as."
  (let ((render (u/gba/thumb-render-ins ins)))
    (equal
      (u/gba/thumb-assemble-ins ins)
      (u/gba/thumb-assemble-ins-string-system render))))

(provide 'udc-gba-assembler)
;;; udc-gba-assembler.el ends here
