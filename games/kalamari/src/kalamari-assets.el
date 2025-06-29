;;; kalamari-assets --- Kalamari Dominancy: assets -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'f)
(require 'udc)
(require 'kalamari-syms)

(defconst k/base-path (f-parent (if load-file-name (f-parent load-file-name) default-directory)))

(eval-when-compile
  (defconst k/possible-enemies
    '("beluga" "dolphin" "horse" "hump" "jelly" "manta" "octopus" "orca" "pelican" "scuba" "star" "turtle" "wave"))
  (defun k/shuffle (s)
    "Shuffle S."
    (if (seq-empty-p s)
      nil
      (let ((elt (seq-elt s (random (seq-length s)))))
        (cons elt (k/shuffle (remove elt s))))))
  ;; (defconst k/actual-enemies (-take 2 (k/shuffle k/possible-enemies)))
  (defconst k/actual-enemies '("orca" "octopus" "dolphin" "pelican" "beluga" "horse" "hump" "jelly"))
  )

;;;; Graphics
;;;;; Backgrounds
(defvar k/source-bg
  (u/gba/image-load-pngs-palette
    `( (,(f-join k/base-path "assets/tiles.png") :duplicate t)
       ,(f-join k/base-path "assets/battle.png")
       ,(f-join k/base-path "assets/font.png")
       ,@(--map (f-join k/base-path (format "assets/enemies/%s.png" it)) k/actual-enemies)
       )))
(defvar k/palette-bg (car k/source-bg))
(u/gba/symtab-add! k/syms :data :data-palette-bg 'bytes (u/gba/palette-bytes k/palette-bg))

(defconst k/image-tiles (nth 1 k/source-bg))
(defconst k/tiledata-tiles (--mapcat it (u/gba/image-tiledata k/image-tiles)))
(u/gba/symtab-add! k/syms :data :data-tiledata-tiles 'bytes k/tiledata-tiles)

(defconst k/image-battle (nth 2 k/source-bg))
(defconst k/tiledata-battle (--mapcat it (u/gba/image-tiledata k/image-battle)))
(defconst k/tilemap-battle (--mapcat (list (cdr it) 0) (u/gba/image-cell-indices k/image-battle)))
(u/gba/symtab-add! k/syms :data :data-tiledata-battle 'bytes k/tiledata-battle)
(u/gba/symtab-add! k/syms :data :data-tilemap-battle 'bytes k/tilemap-battle)

(defconst k/image-font (nth 3 k/source-bg))
(defconst k/tiledata-font (--mapcat it (u/gba/image-tiledata k/image-font)))
(defconst k/tilemap-font (--mapcat (list (cdr it) 0) (u/gba/image-cell-indices k/image-font)))
(u/gba/symtab-add! k/syms :data :data-tiledata-font 'bytes k/tiledata-font)
(defconst k/font-order (seq-into "ABCDEFGHIJKLMNOPQRSTUVWXYZ.!?|abcdefghijklmnopqrstuvwxyz 0123456789" 'list))
(defun k/font-index (c)
  "Determine the font index for C."
  (or (--find-index (= it c) k/font-order) (error "Character %s not in font" c)))
(defun k/font-string (str)
  "Convert STR to a list of font indices."
  (-concat (-map #'k/font-index (seq-into str 'list)) (list #xff)))

(defvar k/source-titlescreen (u/gba/image-load-png-palette (f-join k/base-path "assets/titlescreen.png")))
(defconst k/palette-titlescreen (cdr k/source-titlescreen))
(defconst k/image-titlescreen (car k/source-titlescreen))
(defconst k/tiledata-titlescreen (--mapcat it (u/gba/image-tiledata k/image-titlescreen)))
(defconst k/tilemap-titlescreen (--mapcat (list (cdr it) 0) (u/gba/image-cell-indices k/image-titlescreen)))
(u/gba/symtab-add! k/syms :data :data-palette-titlescreen 'bytes (u/gba/palette-bytes k/palette-titlescreen))
(u/gba/symtab-add! k/syms :data :data-tiledata-titlescreen 'bytes k/tiledata-titlescreen)
(u/gba/symtab-add! k/syms :data :data-tilemap-titlescreen 'bytes k/tilemap-titlescreen)

(defvar k/source-gameover (u/gba/image-load-png-palette (f-join k/base-path "assets/gameover.png")))
(defconst k/palette-gameover (cdr k/source-gameover))
(defconst k/image-gameover (car k/source-gameover))
(defconst k/tiledata-gameover (--mapcat it (u/gba/image-tiledata k/image-gameover)))
(defconst k/tilemap-gameover (--mapcat (list (cdr it) 0) (u/gba/image-cell-indices k/image-gameover)))
(u/gba/symtab-add! k/syms :data :data-palette-gameover 'bytes (u/gba/palette-bytes k/palette-gameover))
(u/gba/symtab-add! k/syms :data :data-tiledata-gameover 'bytes k/tiledata-gameover)
(u/gba/symtab-add! k/syms :data :data-tilemap-gameover 'bytes k/tilemap-gameover)

(defvar k/source-youwin (u/gba/image-load-png-palette (f-join k/base-path "assets/youwin.png")))
(defconst k/palette-youwin (cdr k/source-youwin))
(defconst k/image-youwin (car k/source-youwin))
(defconst k/tiledata-youwin (--mapcat it (u/gba/image-tiledata k/image-youwin)))
(defconst k/tilemap-youwin (--mapcat (list (cdr it) 0) (u/gba/image-cell-indices k/image-youwin)))
(u/gba/symtab-add! k/syms :data :data-palette-youwin 'bytes (u/gba/palette-bytes k/palette-youwin))
(u/gba/symtab-add! k/syms :data :data-tiledata-youwin 'bytes k/tiledata-youwin)
(u/gba/symtab-add! k/syms :data :data-tilemap-youwin 'bytes k/tilemap-youwin)

(defconst k/enemies (ht-create))
(defconst k/enemies-by-index (ht-create))
(defmacro k/define-enemy (nm idx)
  "Define an enemy named NM at IDX."
  (let*
    ( (image-sym (intern (format "k/image-enemy-%s" nm)))
      (tiledata-sym (intern (format "k/tiledata-enemy-%s" nm)))
      (tilemap-sym (intern (format "k/tilemap-enemy-%s" nm)))
      (data-tiledata-sym (intern (format ":data-tiledata-enemy-%s" nm)))
      (data-tilemap-sym (intern (format ":data-tilemap-enemy-%s" nm)))
      (data-string-sym (intern (format ":data-string-enemy-%s" nm)))
      (ret
        `(progn
           (defconst ,image-sym (nth ,(+ idx 4) k/source-bg))
           (defconst ,tiledata-sym (--mapcat it (u/gba/image-tiledata ,image-sym)))
           (defconst ,tilemap-sym (--mapcat (list (cdr it) 0) (u/gba/image-cell-indices ,image-sym)))
           (u/gba/symtab-add! k/syms :data ,data-tiledata-sym 'bytes ,tiledata-sym)
           (u/gba/symtab-add! k/syms :data ,data-tilemap-sym 'bytes ,tilemap-sym)
           (u/gba/symtab-add! k/syms :data ,data-string-sym 'bytes (k/font-string ,(format "%s" nm)))
           (ht-set! k/enemies (intern ,nm) (list ,idx ,data-string-sym ,data-tiledata-sym (length ,tiledata-sym) ,data-tilemap-sym (length ,tilemap-sym)))
           (ht-set! k/enemies-by-index ,idx (intern ,nm)))))
    ret))
(defmacro k/add-enemies ()
  "Add all enemies."
  (cons 'progn (--map-indexed `(k/define-enemy ,it ,it-index) k/actual-enemies)))
(k/add-enemies)

;;;;; Sprites
(defvar k/source-player (u/gba/image-load-png-palette (f-join k/base-path "assets/player.png")))

(defconst k/image-player (car k/source-player))
(defconst k/palette-player (cdr k/source-player))
(defconst k/tiledata-player (--mapcat it (u/gba/image-tiledata k/image-player)))
(u/gba/symtab-add! k/syms :data :data-palette-player 'bytes (u/gba/palette-bytes k/palette-player))
(u/gba/symtab-add! k/syms :data :data-tiledata-player 'bytes k/tiledata-player)

;;;; Strings
(u/gba/symtab-add! k/syms :data :data-string-test 'bytes (seq-into "hello computer" 'list))
(u/gba/symtab-add! k/syms :data :data-string-clear 'bytes (k/font-string "                ")) ;; clear one line
(u/gba/symtab-add! k/syms :data :data-string-appears 'bytes (k/font-string "appears!"))
(u/gba/symtab-add! k/syms :data :data-string-bustat 'bytes (k/font-string "bust at         "))
(u/gba/symtab-add! k/syms :data :data-string-totalis 'bytes (k/font-string "total is        "))
(u/gba/symtab-add! k/syms :data :data-string-drew 'bytes (k/font-string "drew            "))

(provide 'kalamari-assets)
;;; kalamari-assets.el ends here
