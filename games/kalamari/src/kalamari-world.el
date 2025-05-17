;;; kalamari-world --- Kalamari Dominancy: world and chunks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)
(require 'kalamari-world-gen)

(defconst k/chunk-idx 0)
(defun k/add-chunk (chunk)
  "Add a world CHUNK to the ROM."
  (u/gba/symtab-add! k/syms :data (intern (format ":data-chunk-%s" k/chunk-idx)) 'bytes chunk)
  (cl-incf k/chunk-idx))

(--each (-iota (* k/world-width k/world-height))
  (k/add-chunk (k/extract-chunk it)))
(u/gba/symtab-add! k/syms :data :data-chunk-empty 'bytes (-repeat (* k/chunk-size k/chunk-size) 0))

(defun k/render-chunk-at (name destx desty)
  "Generate a Thumb function called NAME that loads a chunk to VRAM.
This function loads a 16x16 chunk to DESTX,DESTY in VRAM."
  (u/gba/thumb-function k/syms name ;; chunk addr in r0
    ;; Load a chunk from memory into VRAM
    ;; A chunk is a 32x32=1024-element array of bytes
    ;; Each byte represents a tile index
    (u/gba/claim! 'r1 'r2 'r3)
    (let ( (dx (mod destx 32)) (dy (mod desty 32))
           (idx 'r0)
           (tmp (u/gba/thumb-fresh-constant 0))
           (offset (u/gba/fresh!))
           (quad (* 1024 (+ (if (>= destx 32) 1 0) (if (>= desty 32) 2 0) ))))
      (u/gba/thumb-for dy (+ k/chunk-size dy)
        (lambda (y)
          (u/gba/thumb-for dx (+ k/chunk-size dx)
            (lambda (x)
              (u/gba/thumb-constant tmp quad)
              (u/gba/emit!
                `(lslx ,offset ,y 5) ;; assemble the index in the tilemap
                `(orr ,offset ,x)
                `(add ,offset ,offset ,tmp)
                `(lslx ,offset ,offset 1)) ;; convert that index to a byte offset
              (u/gba/thumb-get8 k/syms tmp idx)
              (u/gba/emit!
                `(inc ,idx 1))
              (u/gba/thumb-set16 k/syms `(:vram-bg-screenblock28 . ,offset) tmp))))))))

(-each (-iota 4)
  (lambda (cx)
    (-each (-iota 4)
      (lambda (cy)
        (let ((x (* cx 16)) (y (* cy 16)))
          (k/render-chunk-at (intern (format ":render-chunk-%s-%s" x y)) x y))))))

(u/gba/thumb-function k/syms :chunk-index ;; x in r0, y in r1
  ;; Return the offset from :data-chunk-0 of the chunk at x, y
  ;; Return -1 if the chunk is out of bounds
  (let ( (res (u/gba/thumb-fresh-constant -1))
         (width (u/gba/thumb-fresh-constant k/world-width)))
    (u/gba/emit!
      `(cmpi r0 0) '(blt :end)
      `(cmpi r0 ,k/world-width) '(bge :end)
      `(cmpi r1 0) '(blt :end)
      `(cmpi r1 ,k/world-height) '(bge :end)
      `(mul r1 ,width)
      `(add ,res r1 r0)
      `(lslx ,res ,res 8) ;; multiply index by 256 - the size of one 16x16 chunk!
      :end
      `(mov r0 ,res))))

(defun k/render-current-chunks-offset-helper (name addr adjust)
  "Generate code placing a chunk at NAME.
ADJUST should emit code placing the chunk X and Y coordinates in r0 and r1.
ADDR is a register containing the base chunk address."
  (funcall adjust)
  (u/gba/thumb-call k/syms :chunk-index)
  (u/gba/emit!
    `(cmpi r0 0))
  (u/gba/thumb-if-cond 'ge
    (lambda ()
      (u/gba/emit! `(add r0 r0 ,addr))
      (u/gba/thumb-call k/syms name))
    (lambda ()
      (u/gba/thumb-addr 'r0 k/syms :data-chunk-empty)
      (u/gba/thumb-call k/syms name))))
(u/gba/thumb-function k/syms :render-current-chunks
  ;; Load the 9 chunks surrounding the player's chunk into VRAM
  (u/gba/claim! 'r2 'r3)
  (let ( (cx (u/gba/fresh!)) (cy (u/gba/fresh!))
         (addr (u/gba/thumb-loc k/syms :data-chunk-0)))
    (u/gba/thumb-get32 k/syms cx :var-cx)
    (u/gba/thumb-get32 k/syms cy :var-cy)
    (k/render-current-chunks-offset-helper :render-chunk-0-0 addr
      (lambda () (u/gba/emit! `(sub r0 ,cx 1) `(sub r1 ,cy 1))))
    (k/render-current-chunks-offset-helper :render-chunk-16-0 addr
      (lambda () (u/gba/emit! `(mov r0 ,cx) `(sub r1 ,cy 1))))
    (k/render-current-chunks-offset-helper :render-chunk-32-0 addr
      (lambda () (u/gba/emit! `(add r0 ,cx 1) `(sub r1 ,cy 1))))
    (k/render-current-chunks-offset-helper :render-chunk-0-16 addr
      (lambda () (u/gba/emit! `(sub r0 ,cx 1) `(mov r1 ,cy))))
    (k/render-current-chunks-offset-helper :render-chunk-16-16 addr
      (lambda () (u/gba/emit! `(mov r0 ,cx) `(mov r1 ,cy))))
    (k/render-current-chunks-offset-helper :render-chunk-32-16 addr
      (lambda () (u/gba/emit! `(add r0 ,cx 1) `(mov r1 ,cy))))
    (k/render-current-chunks-offset-helper :render-chunk-0-32 addr
      (lambda () (u/gba/emit! `(sub r0 ,cx 1) `(add r1 ,cy 1))))
    (k/render-current-chunks-offset-helper :render-chunk-16-32 addr
      (lambda () (u/gba/emit! `(mov r0 ,cx) `(add r1 ,cy 1))))
    (k/render-current-chunks-offset-helper :render-chunk-32-32 addr
      (lambda () (u/gba/emit! `(add r0 ,cx 1) `(add r1 ,cy 1))))))

(u/gba/thumb-function k/syms :walkable? ;; x in r0, y in r1
  ;; Return nonzero if the tile at global coordinate x, y is walkable
  (u/gba/claim! 'r2 'r3)
  (let ( (cx (u/gba/fresh!)) (cy (u/gba/fresh!))
         (x (u/gba/fresh!)) (y (u/gba/fresh!))
         (tmp (u/gba/thumb-fresh-constant (- k/chunk-size 1))))
    (message "regs: %s" (u/gba/codegen-regs-available u/gba/codegen))
    (u/gba/emit!
      `(asrx ,cx r0 ,(truncate (log k/chunk-size 2)))
      `(asrx ,cy r1 ,(truncate (log k/chunk-size 2)))
      `(and r0 ,tmp) `(mov ,x r0)
      `(and r1 ,tmp) `(mov ,y r1))
    (u/gba/emit!
      `(mov r0 ,cx) `(mov r1 ,cy))
    (u/gba/thumb-call k/syms :chunk-index)
    (u/gba/emit!
      `(cmpi r0 0))
    (u/gba/thumb-if-cond 'ge
      (lambda ()
        (u/gba/emit!
          `(lslx r1 ,y ,(truncate (log k/chunk-size 2)))
          `(add r1 r1 ,x)
          '(add r0 r0 r1))
        (u/gba/claim! 'r1)
        (u/gba/thumb-get8 k/syms tmp (cons :data-chunk-0 'r0))
        (u/gba/emit!
          `(mov r0 ,tmp)))
      (lambda ()
        (u/gba/thumb-constant 'r0 0)))))

(provide 'kalamari-world)
;;; kalamari-world.el ends here
