;;; udc-gba-image --- GBA image assets / tile data -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)

(cl-defstruct (u/gba/palette (:constructor u/gba/make-palette))
  colors)

(defun u/gba/palette-bytes (pal)
  "Given PAL, return the corresponding bytes."
  (--mapcat
   (let ((r (car it)) (g (cadr it)) (b (caddr it)))
     (u/split16le
      (logior
       (lsh (logand #b11111 (lsh b -3)) 10)
       (lsh (logand #b11111 (lsh g -3)) 5)
       (logand #b11111 (lsh r -3)))))
   (u/gba/palette-colors pal)))

(defun u/gba/palette-closest (pal rgb)
  "Return the index of the color in PAL that is closest to RGB."
  (cdr
   (-min-by
    (-on #'> #'car)
    (--map-indexed
     (cons
      (u/rgb-color-distance it rgb)
      it-index)
     (u/gba/palette-colors pal)))))

(defun u/gba/image-quantize-palette (pal)
  "Return a quantize function that will return indices in PAL."
  (lambda (rgb)
    (u/gba/palette-closest pal rgb)))

(defun u/gba/image-quantize-palette-exact (pal)
  "Return a quantize function that will return indices in PAL.
If the colors do not occur exactly in PAL, throw an error."
  (lambda (rgb)
    (let ((res (--find-index (equal it rgb) (u/gba/palette-colors pal))))
      (or res (error "Could not find color in palette: %s" rgb)))))

(defun u/gba/image-load-png-palette (path)
  "Load an image and derive a palette automatically from PATH."
  (let* ( (img (u/load-image-png path))
          (colors (cons '(0 0 0) (-uniq (seq-into (caddr img) 'list))))
          (pal (u/gba/make-palette :colors colors)))
    (cons
      (u/gba/image-tiles (u/gba/image-quantize-palette-exact pal) img)
      pal)))

(cl-defstruct (u/gba/image (:constructor u/gba/make-image))
  width
  height
  tiledata ;; list of lists of bytes, each constituting the pixels of one unique tile
  cell-indices ;; alist mapping tile coordinates to indices in the tile list
  )

(defun u/gba/image-tiles (quantize image)
  "Convert IMAGE (a list of width, height, pixels) into tile data and a tilemap.
QUANTIZE should be a function that converts an RBG pixel to a 4-bit color index."
  (let*
      ((width (car image))
       (height (cadr image))
       (width-tiles (ceiling (/ width 8.0)))
       (height-tiles (ceiling (/ height 8.0)))
       (quantized (seq-into (-map quantize (caddr image)) 'vector)))
    (cl-flet*
        ((getpixel (x y)
           (if (or (< x 0) (>= x width) (< y 0) (>= y height))
               0
             (aref quantized (+ x (* y width)))))
         (getrow (x y)
           (--map (getpixel (+ x it) y) (-iota 8)))
         (gettile (tx ty)
           (let ((x (* tx 8))
                 (y (* ty 8)))
             (-flatten
              (--map
               (getrow x (+ y it))
               (-iota 8))))))
      (let* ((tiles
              (-mapcat
               (lambda (y)
                 (-map
                  (lambda (x)
                    (cons (cons x y) (gettile x y)))
                  (-iota width-tiles)))
               (-iota height-tiles)))
             (unique-tiles (-uniq (-map #'cdr tiles)))
             (tile-indices (--map-indexed (cons it it-index) unique-tiles))
             (cell-indices (--map (cons (car it) (alist-get (cdr it) tile-indices nil nil #'equal)) tiles))
             (cell-indices-populated ;; fill in the "missing" coordinates in a 256x256 map
              (-mapcat
               (lambda (y)
                 (-map
                  (lambda (x)
                    (cons (cons x y) (alist-get (cons x y) cell-indices 0 nil #'equal)))
                  (-iota 32)))
               (-iota 32))))
        (u/gba/make-image
         :width width
         :height height
         :tiledata unique-tiles
         :cell-indices cell-indices-populated)))))

(defun u/gba/tile-4bit-bytes (stile)
  "Convert the list of nibbles STILE into bytes."
  (if (and stile (car stile) (cadr stile))
      (cons
       (logior (car stile) (lsh (cadr stile) 4))
       (u/gba/tile-4bit-bytes (cddr stile)))
    nil))

(provide 'udc-gba-image)
;;; udc-gba-image.el ends here
