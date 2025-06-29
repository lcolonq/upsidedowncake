;;; kalamari-mode-gameover --- Kalamari Dominancy: mode gameover -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)
(require 'kalamari-engine)

(u/gba/thumb-function k/syms :mode-gameover-activate
  (u/gba/thumb-set32 k/syms :var-mode k/MODE-GAMEOVER)
  (u/gba/thumb-call k/syms :wordcpy :palette-bg :data-palette-gameover 128)
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-charblock1 :data-tiledata-gameover (/ (length k/tiledata-gameover) 4))
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-screenblock27 :data-tilemap-gameover (/ (length k/tilemap-gameover) 4))
  (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg1))

(u/gba/thumb-function k/syms :mode-gameover-update
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (k/if-pressed k/syms 'a t
    (lambda ()
      (u/gba/emit!
        '(b :thumb-main)))))

(provide 'kalamari-mode-gameover)
;;; kalamari-mode-gameover.el ends here
