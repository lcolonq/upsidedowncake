;;; kalamari-assets --- Kalamari Dominancy: assets -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'f)
(require 'udc)
(require 'kalamari-syms)

(defconst k/base-path (f-parent (if load-file-name (f-parent load-file-name) default-directory)))

;;;; Assets
;;;;; World map tiles and palette
(defvar k/source-tiles (u/gba/image-load-png-palette (f-join k/base-path "assets/tiles.png")))
(defconst k/image-tiles (car k/source-tiles))
(defconst k/palette-tiles (cdr k/source-tiles))
(defconst k/tiledata-tiles (--mapcat it (u/gba/image-tiledata k/image-tiles)))
(u/gba/symtab-add! k/syms :data :data-palette-tiles 'bytes (u/gba/palette-bytes k/palette-tiles))
(u/gba/symtab-add! k/syms :data :data-tiledata-tiles 'bytes k/tiledata-tiles)

(defvar k/source-player (u/gba/image-load-png-palette (f-join k/base-path "assets/player.png")))
(defconst k/image-player (car k/source-player))
(defconst k/palette-player (cdr k/source-player))
(defconst k/tiledata-player (--mapcat it (u/gba/image-tiledata k/image-player)))
(u/gba/symtab-add! k/syms :data :data-palette-player 'bytes (u/gba/palette-bytes k/palette-player))
(u/gba/symtab-add! k/syms :data :data-tiledata-player 'bytes k/tiledata-player)

(u/gba/symtab-add! k/syms :data :data-string-test 'bytes (seq-into "hello computer" 'list))

;;;; Loading some fixed assets into VRAM
(u/gba/thumb-function k/syms :load-assets
  (u/gba/thumb-call k/syms :wordcpy :palette-bg :data-palette-tiles 128)
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-charblock0 :data-tiledata-tiles (/ (length k/tiledata-tiles) 4))
  (u/gba/thumb-call k/syms :wordcpy :palette-sprite :data-palette-player 128)
  (u/gba/thumb-call k/syms :wordcpy :vram-sprite-charblock0 :data-tiledata-player (/ (length k/tiledata-player) 4)))

(provide 'kalamari-assets)
;;; kalamari-assets.el ends here
