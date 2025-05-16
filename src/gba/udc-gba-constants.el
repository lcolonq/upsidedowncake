;;; udc-gba-constants --- GBA constants -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
(defconst u/gba/rom-start #x8000000)
(defconst u/gba/size-word 4)
(defconst u/gba/pc 'r15)
(defconst u/gba/arm-lr 'r14)
(defconst u/gba/arm-sp 'r13)
(defconst u/gba/arm-fp 'r11)
(defconst u/gba/thumb-fp 'r7)
(defconst u/gba/arm-regs-callee-saved '(r4 r5 r6 r7 r8 r9 r10))
(defconst u/gba/thumb-regs-callee-saved '(r4 r5 r6))
(defconst u/gba/thumb-regs-pushed '(r1 r2 r3 r4 r5 r6))
(defconst u/gba/thumb-regs-high '(r8 r9 r10))
(defconst u/gba/regs-arg '(r0 r1 r2 r3))
(defconst u/gba/arm-regs-all '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10))
(defconst u/gba/scratch 'r0)
(defconst u/gba/button-masks
  '((a . #b1)
    (b . #b10)
    (select . #b100)
    (start . #b1000)
    (right . #b10000)
    (left . #b100000)
    (up . #b1000000)
    (down . #b10000000)
    (r . #b100000000)
    (l . #b1000000000)))
(defconst u/gba/dispcnt-video-modes '(:videomode0 :videomode1 :videomode2 :videomode3 :videomode4 :videomode5))
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

(provide 'udc-gba-constants)
;;; udc-gba-constants.el ends here
