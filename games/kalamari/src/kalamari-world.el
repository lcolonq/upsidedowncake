;;; kalamari-world --- Kalamari Dominancy: world and chunks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)

(defconst k/chunk-size 16)

(u/gba/symtab-add! k/syms :data :data-chunk-0 'bytes
  '( 1 1 1 1 2 2 2 2 1 1 1 1 1 1 1 1
     1 1 1 1 2 2 2 2 1 1 1 2 1 1 2 1
     1 1 3 1 1 2 2 1 1 1 1 2 1 1 2 1
     1 1 1 1 1 2 1 1 1 1 1 2 2 2 2 1
     1 1 1 1 1 2 1 1 1 1 1 2 1 1 2 1
     1 2 2 2 2 2 1 1 1 1 1 2 1 1 2 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

(defun k/render-chunk-at (name destx desty)
  "Generate a Thumb function called NAME that loads a chunk to VRAM.
This function loads a 16x16 chunk to DESTX,DESTY in VRAM."
  (u/gba/thumb-function k/syms name ;; chunk addr in r0
    ;; Load a chunk from memory into VRAM
    ;; A chunk is a 32x32=1024-element array of bytes
    ;; Each byte represents a tile index
    (u/gba/claim! 'r1 'r2 'r3)
    (let ( (dx (mod destx 32)) (dy (mod desty 32))
           (quad
             (u/gba/thumb-fresh-constant
               (* 1024
                 (+ (if (>= destx 32) 1 0) (if (>= desty 32) 2 0) )))))
      (u/gba/thumb-for 0 (* k/chunk-size k/chunk-size)
        (lambda (idx)
          (let ( (tmp (u/gba/thumb-fresh-constant 0))
                 (x (u/gba/fresh!))
                 (y (u/gba/fresh!)))
            (u/gba/thumb-constant y (- k/chunk-size 1))
            (u/gba/emit!
              `(mov ,x ,idx) ;; extract x and y from idx
              `(and ,x ,y) `(lsrx ,y ,idx ,(truncate (log k/chunk-size 2)))
              `(inc ,x ,dx) `(inc ,y ,dy))
            (u/gba/emit!
              `(lslx ,y ,y 5) ;; assemble the index in the tilemap
              `(orr ,y ,x)
              `(add ,y ,y ,quad)
              `(lslx ,y ,y 1)) ;; convert that index to a byte offset
            (u/gba/claim! x)
            (u/gba/thumb-get8 k/syms tmp `(r0 . ,idx))
            (u/gba/thumb-set16 k/syms `(:vram-bg-screenblock28 . ,y) tmp)
            ))))))

(-each (-iota 4)
  (lambda (cx)
    (-each (-iota 4)
      (lambda (cy)
        (let ((x (* cx 16)) (y (* cy 16)))
          (k/render-chunk-at (intern (format ":render-chunk-%s-%s" x y)) x y))))))

(provide 'kalamari-world)
;;; kalamari-world.el ends here
