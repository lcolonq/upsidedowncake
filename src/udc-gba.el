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

(defun u/gba/assemble-ins-string-system (ins)
  "Assemble the instruction mnemonic string INS using arm-non-eabi-as."
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
      (list
       t
       (logior
        (lsh
         (cond
          ((integerp shift) (logand #xf shift))
          (t (error "Invalid immediate rotation: %s" shift)))
         8)
        (logand #xff arg))))
     (t (error "Invalid second operand: %s" arg)))))

(defun u/gba/assemble-ins-dataprocessing (cond op set-cond arg1 dest arg2 shift)
  "Given COND, OP, SET-COND, ARG1, DEST, ARG2, and SHIFT produce 4 bytes."
  (let ((a2 (u/gba/assemble-ins-dataprocessing-arg2-shift arg2 shift)))
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

(defun u/gba/assemble-ins-sdt (cond imm pre up byte writeback load rn rd offset)
  "Given COND, IMM, PRE, UP, BYTE, WRITEBACK, LOAD, RN, RD, and OFFSET, :3."
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
    (logand offset #xfff))))

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
    (logand reglist #xffff))))

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
         (args (--filter (not (-contains? (list cond) it)) (cdr opbase))))
    (cond
     ((-contains? '(b bl) op)
      (s-concat (u/gba/render-op op) rcond " " (format "pc+%d" (* (+ (car args) 2) 4))))
     (t
      (s-concat
       (u/gba/render-op op)
       rcond " "
       (s-join ", " (-map #'u/gba/render-arg args)))))))

(defun u/gba/assemble-ins (ins)
  "Assemble INS to a sequence of bytes.
INS is either:
 - an opcode symbol
 - a list of an opcode symbol followed by operands"
  (let* ((op (if (listp ins) (car ins) ins))
         (opbase (if (listp ins) ins nil))
         (set-cond (-contains? opbase 's))
         (cond (or (car (--filter (-contains? u/gba/conds it) opbase)) 'al))
         (args (--filter (not (-contains? (list cond 's) it)) (cdr opbase)))
         (d (car args))
         (dimm (u/gba/assemble-imm d))
         (a0 (cadr args))
         (a1 (caddr args))
         (a2 (cadddr args))
         (a2imm (u/gba/assemble-imm a2))
         (a3 (nth 4 args))
         )
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
      (ldm (u/gba/assemble-ins-bdt cond (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'psr) (-contains? opbase 'wb) t a0 a1))
      (ldr
       (u/gba/assemble-ins-sdt
        cond (not a2imm) (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'byte) (-contains? opbase 'wb) t a0 a1
        (or a2imm (logior (lsh a3 4) (u/gba/assemble-reg a2)))))
      (mcr (error "Unsupported instruction: %s" ins))
      (mla (u/gba/assemble-ins-multiply cond t set-cond d a2 a1 a0))
      (mov (u/gba/assemble-ins-dataprocessing cond 'mov set-cond a0 d a1 a2))
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
      (stm (u/gba/assemble-ins-bdt cond (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'psr) (-contains? opbase 'wb) nil a0 a1))
      (str
       (u/gba/assemble-ins-sdt
        cond (not a2imm) (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'byte) (-contains? opbase 'wb) nil a0 a1
        (or a2imm (logior (lsh (nth 3 args) 4) (u/gba/assemble-reg a2)))))
      (sub (u/gba/assemble-ins-dataprocessing cond 'sub set-cond a0 d a1 a2))
      (swi (u/gba/assemble-ins-interrupt cond d))
      (swp (u/gba/assemble-ins-singledataswap cond (-contains? opbase 'swap) a0 a1 a2))
      (teq (u/gba/assemble-ins-dataprocessing cond 'teq set-cond a0 d a1 a2))
      (tst (u/gba/assemble-ins-dataprocessing cond 'tst set-cond a0 d a1 a2)))))

(defun u/gba/test-assemble-ins (ins)
  "Compare our assembly of INS with arm-none-eabi-as."
  (let ((render (u/gba/render-ins ins)))
    (equal
     (u/gba/assemble-ins ins)
     (u/gba/assemble-ins-string-system render))))

(cl-defstruct (u/gba/header (:constructor u/gba/make-header))
  entry ;; instruction branching to entrypoint
  title ;; game title, max 12 characters
  code ;; game code, 4 characters
  maker ;; make code, 2 characters
  )
(defun u/gba/write-header (mem header)
  "Write HEADER to MEM."
  (u/write!
   mem
   #x0000
   (-concat
    (u/gba/assemble-ins (u/gba/header-entry header))
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

(provide 'udc-gba)
;;; udc-gba.el ends here