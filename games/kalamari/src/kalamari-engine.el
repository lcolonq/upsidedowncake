;;; kalamari-engine --- Kalamari Dominancy: game engine -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)

;;;; "Engine" code
(u/gba/arm-toplevel k/syms :interrupt-handler
  (u/gba/claim! 'r2)
  '(mov r0 1) ;; vblank
  (u/gba/arm-set16 k/syms :reg-if 'r0)
  (u/gba/arm-get16 k/syms 'r1 :reg-ifbios)
  '(orr r0 r0 r1)
  (u/gba/arm-set16 k/syms :reg-ifbios 'r0)
  `(bx ,u/gba/arm-lr))

(u/gba/arm-function k/syms :enable-interrupts
  (u/gba/arm-set32 k/syms :reg-intaddr (u/gba/symtab-entry-addr (u/gba/symtab-lookup k/syms :interrupt-handler)))
  (u/gba/arm-set16 k/syms :reg-dispstat #b0000000000001000) ;; turn on vblank interrupt
  (u/gba/arm-set16 k/syms :reg-ie #b0000000000000001) ;; only enable vblank interrupt
  (u/gba/arm-set32 k/syms :reg-ime 1) ;; enable interrupts
  )

(u/gba/thumb-toplevel k/syms :thumb-main
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (u/gba/thumb-call k/syms :load-assets)
  (u/gba/thumb-bgcnt k/syms 0 :64x64tiles :charblock0 :screenblock28 :8bpp)
  (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg0)
  '(b :mainloop)) ;; start the main loop!

(u/gba/arm-toplevel k/syms :main
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (u/gba/arm-call k/syms :enable-interrupts)
  (let ((r (u/gba/arm-loc k/syms :thumb-main)))
    (u/gba/emit! ;; switch to Thumb
      `(add ,r ,r 1)
      `(bx ,r))))

(u/gba/thumb-toplevel k/syms :mainloop
  (u/gba/thumb-call k/syms :update) ;; update game state
  `(swi #x05) ;; VBlankIntrWait BIOS function, remember to shift in ARM!
  (u/gba/thumb-call k/syms :render) ;; reflect game state in VRAM immediately after vblank
  '(b :mainloop))

;;;; Game logic
(u/gba/thumb-function k/syms :update
  (u/gba/scope
    (-let ( (addr (u/gba/thumb-loc k/syms :var-test))
            (r (u/gba/fresh!)))
      (u/gba/thumb-get32 k/syms r addr)
      (u/gba/emit! `(inc ,r 1))
      (u/gba/thumb-set32 k/syms (cons addr 0) r)
      (u/gba/emit! `(inc ,r 1))
      (u/gba/thumb-set32 k/syms (cons addr 4) r)
      (u/gba/emit! `(inc ,r 1))
      (u/gba/thumb-set32 k/syms (cons addr 8) r))))

(u/gba/thumb-function k/syms :render
  (u/gba/thumb-call k/syms :render-chunk-center :data-chunk-0))

(provide 'kalamari-engine)
;;; kalamari-engine.el ends here
