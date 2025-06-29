;;; kalamari-mode-game --- Kalamari Dominancy: mode game -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)
(require 'kalamari-engine)

(u/gba/thumb-function k/syms :mode-game-activate
  (u/gba/thumb-set32 k/syms :var-mode k/MODE-GAME)
  (u/gba/thumb-set32 k/syms :var-x 0) (u/gba/thumb-set32 k/syms :var-y 0)
  (u/gba/thumb-set32 k/syms :var-cx 1) (u/gba/thumb-set32 k/syms :var-cy 1)
  (u/gba/thumb-set32 k/syms :var-battle-draw 0)
  (u/gba/thumb-set32 k/syms :var-battle-total 0)
  (u/gba/thumb-set32 k/syms :var-battle-bust 15)
  (u/gba/thumb-set32 k/syms :var-battle-target 0)
  (u/gba/thumb-set32 k/syms :var-cursor 0)
  (u/gba/thumb-call k/syms :wordcpy :palette-bg :data-palette-bg 128)
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-charblock1 :data-tiledata-battle (/ (length k/tiledata-battle) 4))
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-screenblock27 :data-tilemap-battle (/ (length k/tilemap-battle) 4))
  (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg0 :sprites))

(u/gba/thumb-function k/syms :mode-game-handle-key-input-move
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (u/gba/scope
    (-let ( (moving (u/gba/thumb-fresh-constant 0))
            (tx (u/gba/fresh!)) (ty (u/gba/fresh!)))
      (u/gba/thumb-get32 k/syms tx :var-x)
      (u/gba/thumb-get32 k/syms ty :var-y)
      (k/if-pressed k/syms 'left t (lambda () (u/gba/emit! `(inc ,moving 1) `(dec ,tx 1))))
      (k/if-pressed k/syms 'right t (lambda () (u/gba/emit! `(inc ,moving 1) `(inc ,tx 1))))
      (k/if-pressed k/syms 'up t (lambda () (u/gba/emit! `(inc ,moving 1) `(dec ,ty 1))))
      (k/if-pressed k/syms 'down t (lambda () (u/gba/emit! `(inc ,moving 1) `(inc ,ty 1))))
      (u/gba/thumb-if moving
        (lambda ()
          (u/gba/emit! `(mov r0 ,tx) `(mov r1 ,ty))
          (u/gba/thumb-call k/syms :walkable?)
          (u/gba/thumb-if 'r0
            (lambda ()
              (u/gba/thumb-set32 k/syms :var-x tx)
              (u/gba/thumb-set32 k/syms :var-y ty)
              (u/gba/thumb-call k/syms :random)
              (u/gba/thumb-constant tx #xf)
              (u/gba/emit!
                `(and r0 ,tx)
                `(cmpi r0 0))
              (u/gba/thumb-if-cond 'eq
                (lambda ()
                  (u/gba/thumb-call k/syms :random-encounter))))))))))

(u/gba/thumb-function k/syms :mode-game-handle-key-input-battle
  (u/gba/claim! 'r2 'r3)
  (k/if-pressed k/syms 'up t
    (lambda ()
      (u/gba/thumb-call k/syms :cursor-up)
      (u/gba/thumb-call k/syms :render-cursor)))
  (k/if-pressed k/syms 'down t
    (lambda ()
      (u/gba/thumb-call k/syms :cursor-down)
      (u/gba/thumb-call k/syms :render-cursor)))
  (k/if-pressed k/syms 'a t
    (lambda ()
      (let ((cur (u/gba/fresh!)))
        (u/gba/thumb-get32 k/syms cur :var-cursor)
        (u/gba/emit! `(cmpi ,cur 0))
        (u/gba/thumb-if-cond 'eq ;; hit
          (lambda ()
            (u/gba/thumb-call k/syms :battle-hit)))
        (u/gba/emit! `(cmpi ,cur 1))
        (u/gba/thumb-if-cond 'eq ;; stand
          (lambda ()
            (u/gba/thumb-call k/syms :battle-stand)))
        (u/gba/emit! `(cmpi ,cur 2))
        (u/gba/thumb-if-cond 'eq ;; flee
          (lambda ()
            (u/gba/thumb-call k/syms :battle-flee)))))))

(u/gba/thumb-function k/syms :mode-game-update
  (let ((battling (u/gba/fresh!)))
    (u/gba/thumb-get32 k/syms battling :var-battle-target)
    (u/gba/thumb-if battling
      (lambda () ;; battling
        (u/gba/thumb-call k/syms :mode-game-handle-key-input-battle))
      (lambda () ;; walking around
        (u/gba/thumb-call k/syms :mode-game-handle-key-input-move)))))

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
