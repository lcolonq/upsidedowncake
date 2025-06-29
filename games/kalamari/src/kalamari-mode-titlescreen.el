;;; kalamari-mode-titlescreen --- Kalamari Dominancy: mode titlescreen -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)
(require 'kalamari-engine)

(u/gba/thumb-function k/syms :mode-titlescreen-activate
  (u/gba/thumb-set32 k/syms :var-mode k/MODE-TITLESCREEN)
  (u/gba/thumb-call k/syms :wordcpy-dma :palette-bg :data-palette-titlescreen 128)
  (u/gba/thumb-call k/syms :wordcpy-dma :palette-sprite :data-palette-player 128)
  (u/gba/thumb-call k/syms :wordcpy-dma :vram-bg-charblock0 :data-tiledata-tiles (/ (length k/tiledata-tiles) 4))
  (u/gba/thumb-call k/syms :wordcpy-dma :vram-bg-charblock1 :data-tiledata-titlescreen (/ (length k/tiledata-titlescreen) 4))
  (u/gba/thumb-call k/syms :wordcpy-dma :vram-bg-screenblock27 :data-tilemap-titlescreen (/ (length k/tilemap-titlescreen) 4))
  (u/gba/thumb-call k/syms :wordcpy-dma :vram-bg-charblock3 :data-tiledata-font (/ (length k/tiledata-font) 4))
  (u/gba/thumb-call k/syms :wordset :vram-bg-screenblock15
    (logior (ash (k/font-index ? ) 16) (k/font-index ? )) (/ 2048 4))
  (u/gba/thumb-call k/syms :wordcpy-dma :vram-sprite-charblock0 :data-tiledata-player (/ (length k/tiledata-player) 4))
  (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg1))

(u/gba/thumb-function k/syms :mode-titlescreen-update
  (u/gba/claim! 'r1 'r2 'r3)
  (k/if-pressed k/syms 'a t
    (lambda ()
      (u/gba/thumb-call k/syms :mode-game-activate))))

(provide 'kalamari-mode-titlescreen)
;;; kalamari-mode-titlescreen.el ends here
