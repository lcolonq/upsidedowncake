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

(defun u/gba/assemble-ins-dataprocessing-arg2 (arg)
  "Given ARG, truncate to 12 bits."
  (logand arg #xfff))

(defun u/gba/assemble-ins-dataprocessing (cond is-imm op set-cond arg1 dest arg2)
  "Given COND, IS-IMM, OP, SET-COND, ARG1, DEST, and ARG2, produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b00 26)
    (lsh (u/gba/assemble-flag is-imm) 25)
    (lsh (u/gba/assemble-ins-dataprocessing-op op) 21)
    (lsh (u/gba/assemble-flag set-cond) 20)
    (lsh (u/gba/assemble-reg arg1) 16)
    (lsh (u/gba/assemble-reg dest) 12)
    (u/gba/assemble-ins-dataprocessing-arg2 arg2))))

(defun u/gba/assemble-ins-multiply (cond acc set-cond rd rn rs rm)
  "Given COND, ACC, SET-COND, RD, RN, RS, RM produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b000000 22)
    (lsh (u/gba/assemble-flag acc) 21)
    (lsh (u/gba/assemble-flag set-cond) 20)
    (lsh (u/gba/assemble-reg rd) 16)
    (lsh (u/gba/assemble-reg rn) 12)
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
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b000100101111111111110001 4)
    (u/gba/assemble-reg rn))))

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
    (and offset #xfff))))

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
    (and reglist #xffff))))

(defun u/gba/assemble-ins-branch (cond link offset)
  "Given COND, LINK, and OFFSET, produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    (lsh #b101 25)
    (lsh (u/gba/assemble-flag link) 24)
    (and offset #xffffff))))

(defun u/gba/assemble-ins-interrupt (cond)
  "Given COND, produce 4 bytes."
  (u/split32le
   (logior
    (lsh (u/gba/assemble-cond cond) 28)
    #b1111000000000000000000000000)))

(defun u/gba/assemble-ins (ins)
  "Assemble INS to a sequence of bytes.
INS is either:
 - an opcode symbol
 - a list of an opcode symbol followed by operands"
  (let* ((opbase (if (listp ins) (car ins) ins))
         (op (if (listp opbase) (car opbase) opbase))
         (set-cond (-contains? opbase 's))
         (cond (car (--filter (-contains? u/gba/conds it) opbase)))
         (args (if (listp ins) (cdr ins) nil))
         (d (car args))
         (a0 (cadr args))
         (a0imm (u/gba/assemble-imm a0))
         (a1 (caddr args))
         (a1imm (u/gba/assemble-imm a1))
         (a2 (cadddr args))
         (a2imm (u/gba/assemble-imm a2))
         (a3 (nth 4 args))
         ;; (a3imm (u/gba/assemble-imm a3))
         )
    (cl-case op
      (adc (u/gba/assemble-ins-dataprocessing cond a1imm 'adc set-cond a0 d a1))
      (add (u/gba/assemble-ins-dataprocessing cond a1imm 'add set-cond a0 d a1))
      (and (u/gba/assemble-ins-dataprocessing cond a1imm 'and set-cond a0 d a1))
      (b (u/gba/assemble-ins-branch cond 0 a0imm))
      (bic (u/gba/assemble-ins-dataprocessing cond a1imm 'bic set-cond a0 d a1))
      (bl (u/gba/assemble-ins-branch cond 1 a0imm))
      (bx (u/gba/assemble-ins-branchexchange cond a0))
      (cdp (error "Unsupported instruction: %s" ins))
      (cmn (u/gba/assemble-ins-dataprocessing cond a1imm 'cmn set-cond a0 d a1))
      (cmp (u/gba/assemble-ins-dataprocessing cond a1imm 'cmp set-cond a0 d a1))
      (eor (u/gba/assemble-ins-dataprocessing cond a1imm 'eor set-cond a0 d a1))
      (ldc (error "Unsupported instruction: %s" ins))
      (ldm (u/gba/assemble-ins-bdt cond (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'psr) (-contains? opbase 'wb) t a0 a1))
      (ldr
       (u/gba/assemble-ins-sdt
        cond (not a2imm) (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'byte) (-contains? opbase 'wb) t a0 a1
        (or a2imm (logior (lsh a3 4) (u/gba/assemble-reg a2)))))
      (mcr (error "Unsupported instruction: %s" ins))
      (mla (u/gba/assemble-ins-multiply cond t set-cond a0 a1 a2 a3))
      (mov (u/gba/assemble-ins-dataprocessing cond a1imm 'mov set-cond a0 d a1))
      (mrc (error "Unsupported instruction: %s" ins))
      (mrs (error "Unsupported instruction: %s" ins))
      (msr (error "Unsupported instruction: %s" ins))
      (mul (u/gba/assemble-ins-multiply cond nil set-cond a0 a1 a2 a3))
      (mvn (u/gba/assemble-ins-dataprocessing cond a1imm 'mvn set-cond a0 d a1))
      (orr (u/gba/assemble-ins-dataprocessing cond a1imm 'orr set-cond a0 d a1))
      (rsb (u/gba/assemble-ins-dataprocessing cond a1imm 'rsb set-cond a0 d a1))
      (rsc (u/gba/assemble-ins-dataprocessing cond a1imm 'rsc set-cond a0 d a1))
      (sbc (u/gba/assemble-ins-dataprocessing cond a1imm 'sbc set-cond a0 d a1))
      (stc (error "Unsupported instruction: %s" ins))
      (stm (u/gba/assemble-ins-bdt cond (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'psr) (-contains? opbase 'wb) nil a0 a1))
      (str
       (u/gba/assemble-ins-sdt
        cond (not a2imm) (-contains? opbase 'pre) (-contains? opbase 'up) (-contains? opbase 'byte) (-contains? opbase 'wb) nil a0 a1
        (or a2imm (logior (lsh (nth 3 args) 4) (u/gba/assemble-reg a2)))))
      (sub (u/gba/assemble-ins-dataprocessing cond a1imm 'sub set-cond a0 d a1))
      (swi (u/gba/assemble-ins-interrupt cond))
      (swp (u/gba/assemble-ins-singledataswap cond (-contains? opbase 'swap) a0 a1 a2))
      (teq (u/gba/assemble-ins-dataprocessing cond a1imm 'teq set-cond a0 d a1))
      (tst (u/gba/assemble-ins-dataprocessing cond a1imm 'tst set-cond a0 d a1)))))

(provide 'udc-gba)
;;; udc-gba.el ends here
