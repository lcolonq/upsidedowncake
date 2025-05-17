;;; kalamari-world-gen --- Kalamari Dominancy: world generation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)

(defconst k/world-width 16)
(defconst k/world-height 16)

(defconst k/chunk-size 16)
(defconst k/world (make-vector (* k/world-width k/world-height k/chunk-size k/chunk-size) 128))

(defconst k/room-min 5)
(defconst k/room-max 10)

(defun k/pick-random (xs)
  "Pick a random element of XS."
  (and xs (nth (random (length xs)) xs)))

(defun k/gen-floor ()
  "Return a random floor tile."
  (let ((roll (random 20)))
    (cond
      ((= 0 roll) 132)
      ((< 5 roll) 131)
      ((< 10 roll) 130)
      ((< 15 roll) 129)
      (t 128))))

(defun k/split (x y w h dir)
  "Compute a valid split in DIR for X Y W H.
Return nil if no split exists."
  (let* ( (low (if (eq dir 'horiz) x y))
          (high (if (eq dir 'horiz) (+ x w) (+ y h)))
          (lowbound (+ low k/room-min 1))
          (highbound (- high k/room-min 1))
          (diff (- highbound lowbound)))
    (when (> diff 0)
      (cons dir (+ lowbound (random diff))))))

(defun k/gen-world-tree (x y w h)
  "Generate a world tree in the region X Y W H."
  (let* ( (dirs (if (= 0 (random 2)) '(horiz vert) '(vert horiz)))
          (split (or (k/split x y w h (car dirs)) (k/split x y w h (cadr dirs)))))
    (if split
      (let* ( (dir (car split))
              (sp (cdr split))
              (regions
                (if (eq dir 'horiz)
                  (cons (list x y (- sp x) h) (list sp y (- w (- sp x)) h))
                  (cons (list x y w (- sp y)) (list x sp w (- h (- sp y)))))))
        (list dir
          (apply #'k/gen-world-tree (car regions))
          (apply #'k/gen-world-tree (cdr regions))))
      (list 'region x y w h))))

(defun k/world-get (x y)
  "Retrieve the cell at X, Y."
  (when
    (or
      (< x 0) (>= x (* k/world-width k/chunk-size))
      (< y 0) (>= y (* k/world-height k/chunk-size)))
    (error "World coordinates out of bounds: (%s, %s)" x y))
  (let ((idx (+ (* y k/world-width k/chunk-size) x)))
    (seq-elt k/world idx)))

(defun k/world-set (x y v)
  "Set the cell at X, Y to V."
  (when
    (or
      (< x 0) (>= x (* k/world-width k/chunk-size))
      (< y 0) (>= y (* k/world-height k/chunk-size)))
    (error "World coordinates out of bounds: (%s, %s)" x y))
  (let ((idx (+ (* y k/world-width k/chunk-size) x)))
    (setf (seq-elt k/world idx) v)))

(defun k/valid-door? (dir cs)
  "Check if the coordinates CS represent a valid door placement for DIR."
  (let ((x (car cs)) (y (cdr cs)))
    (cl-case dir
      (horiz (and (>= (k/world-get (- x 1) y) 128) (>= (k/world-get (+ x 1) y) 128)))
      (vert (and (>= (k/world-get x (- y 1)) 128) (>= (k/world-get x (+ y 1)) 128)))
      (t (error "Invalid direction: %s" dir)))))

(-each (-iota (* k/chunk-size k/world-width))
  (lambda (x)
    (-each (-iota (* k/chunk-size k/world-height))
      (lambda (y)
        (k/world-set x y (k/gen-floor))))))
(defun k/gen-world (tree)
  "Generate the world from TREE.
Return a pair of the lists of the horizontal and vertical walls placed."
  (cl-case (car tree)
    (region
      (-let [(x y w h) (cdr tree)]
        (let* ( (horizwalls (--map (cons (+ x it) (+ y (- h 1))) (-iota (- w 1))))
                (vertwalls (--map (cons (+ x (- w 1)) (+ y it)) (-iota (- h 1))))
                (toset (-concat horizwalls vertwalls)))
          (--each toset
            (k/world-set (car it) (cdr it) 0))
          (k/world-set (+ x (- w 1)) (+ y (- h 1)) 0)
          (cons horizwalls vertwalls))))
    (horiz
      (-let ( ((horiz0 . vert0) (k/gen-world (cadr tree)))
              ((horiz1 . vert1) (k/gen-world (caddr tree))))
        (let ((door (k/pick-random (--filter (k/valid-door? 'horiz it) vert0))))
          (k/world-set (car door) (cdr door) (k/gen-floor)))
        (cons (-concat horiz0 horiz1) vert1)))
    (vert
      (-let ( ((horiz0 . vert0) (k/gen-world (cadr tree)))
              ((horiz1 . vert1) (k/gen-world (caddr tree))))
        (let ((door (k/pick-random (--filter (k/valid-door? 'vert it) horiz0))))
          (k/world-set (car door) (cdr door) (k/gen-floor)))
        (cons horiz1 (-concat vert0 vert1))))
    (t (error "Invalid tree node: %s" (car tree)))))

(k/gen-world (k/gen-world-tree 0 0 (* k/world-width k/chunk-size) (* k/world-height k/chunk-size)))

(defun k/extract-chunk (idx)
  "Extract chunk IDX from the world."
  (when (or (< idx 0) (>= idx (* k/world-width k/world-height)))
    (error "Chunk index out of bounds: %s" idx))
  (let ( (x (* k/chunk-size (mod idx k/world-width)))
         (y (* k/chunk-size (/ idx k/world-width)))
         (ret (list)))
    (-each (-iota k/chunk-size)
      (lambda (dy)
        (-each (-iota k/chunk-size)
          (lambda (dx)
            (push (k/world-get (+ x dx) (+ y dy)) ret)))))
    (reverse ret)))

(provide 'kalamari-world-gen)
;;; kalamari-world-gen.el ends here
