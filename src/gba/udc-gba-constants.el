;;; udc-gba-constants --- GBA constants -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
(defconst u/gba/rom-start #x8000000)
(defconst u/gba/size-word 4)
(defconst u/gba/pc 'r15)
(defconst u/gba/lr 'r14)
(defconst u/gba/sp 'r13)
(defconst u/gba/fp 'r11)
(defconst u/gba/regs-callee-saved '(r4 r5 r6 r7 r8 r9 r10))
(defconst u/gba/regs-arg '(r0 r1 r2 r3))
(defconst u/gba/regs-all '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10))

(provide 'udc-gba-constants)
;;; udc-gba-constants.el ends here
