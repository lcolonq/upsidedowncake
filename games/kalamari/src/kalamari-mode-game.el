;;; kalamari-mode-game --- Kalamari Dominancy: mode game -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)
(require 'kalamari-engine)

(u/gba/thumb-function k/syms :mode-game-handle-key-input
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (u/gba/scope
    (-let ( (addr (u/gba/thumb-loc k/syms :var-x))
            (r (u/gba/fresh!)))
      (k/if-pressed k/syms 'left t
        (lambda ()
          (u/gba/thumb-get32 k/syms r addr)
          (u/gba/emit! `(dec ,r 1))
          (u/gba/thumb-set32 k/syms addr r)))
      (k/if-pressed k/syms 'right t
        (lambda ()
          (u/gba/thumb-get32 k/syms r addr)
          (u/gba/emit! `(inc ,r 1))
          (u/gba/thumb-set32 k/syms addr r)))
      (k/if-pressed k/syms 'up t
        (lambda ()
          (u/gba/thumb-get32 k/syms r (cons addr 4))
          (u/gba/emit! `(dec ,r 1))
          (u/gba/thumb-set32 k/syms (cons addr 4) r)))
      (k/if-pressed k/syms 'down t
        (lambda ()
          (u/gba/thumb-get32 k/syms r (cons addr 4))
          (u/gba/emit! `(inc ,r 1))
          (u/gba/thumb-set32 k/syms (cons addr 4) r))))))

(u/gba/thumb-function k/syms :mode-game-update
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (u/gba/thumb-call k/syms :mode-game-handle-key-input))

(u/gba/thumb-function k/syms :mode-game-render
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (let ( (tmp (u/gba/fresh!))
         (prev (u/gba/fresh!))
         (refresh (u/gba/thumb-fresh-constant 0))
         (mask (u/gba/thumb-fresh-constant (- 16 1))))
    (u/gba/thumb-get32 k/syms tmp :var-x)
    (u/gba/emit! `(and ,tmp ,mask) `(lslx ,tmp ,tmp 3) `(inc ,tmp 12)) ;; compute horizontal scroll
    (u/gba/thumb-bg-scroll-horizontal k/syms 0 tmp)
    (u/gba/thumb-get32 k/syms tmp :var-y)
    (u/gba/emit! `(and ,tmp ,mask) `(lslx ,tmp ,tmp 3) `(inc ,tmp 52)) ;; computer vertical scroll
    (u/gba/thumb-bg-scroll-vertical k/syms 0 tmp)
    (u/gba/thumb-get32 k/syms tmp :var-x)
    (u/gba/thumb-get32 k/syms prev :var-cx)
    (u/gba/emit!
      `(asrx ,tmp ,tmp 4)
      `(cmp ,tmp ,prev)
      '(beq :skiprefreshx)
      `(inc ,refresh 1)
      :skiprefreshx)
    (u/gba/thumb-set32 k/syms :var-cx tmp)
    (u/gba/thumb-get32 k/syms tmp :var-y)
    (u/gba/thumb-get32 k/syms prev :var-cy)
    (u/gba/emit!
      `(asrx ,tmp ,tmp 4)
      `(cmp ,tmp ,prev)
      '(beq :skiprefreshy)
      `(inc ,refresh 1)
      :skiprefreshy)
    (u/gba/thumb-set32 k/syms :var-cy tmp)
    (u/gba/thumb-if refresh
      (lambda ()
        (u/gba/emit!
          (u/gba/thumb-call k/syms :render-current-chunks))))))

(provide 'kalamari-mode-game)
;;; kalamari-mode-game.el ends here
