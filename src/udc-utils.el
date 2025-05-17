;;; udc-utils --- Compiler Upside-Down Cake - utilities -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'eieio)

;;;; Utility macros
(defmacro u/defstruct (name &rest body)
  "Define a structure with NAME (with the constructor under the u/ namespace).
BODY is passed directly to `cl-defstruct'."
  `(cl-defstruct
       (,name (:constructor ,(intern (s-concat "u/make-" (s-chop-prefix "u/" (symbol-name name)))))
              (:copier nil))
     ,@body))

(defmacro u/. (slot s)
  "Lookup SLOT in the struct S."
  `(eieio-oref ,s (quote ,slot)))

;;;; Structs
(u/defstruct
 u/structdef
 size
 (fields (ht-create)) ;; hash table mapping symbols to offsets
 )
(defun u/struct (align &rest fields)
  "Define a new struct and populate it with FIELDS.
Align all fields to ALIGN.
FIELDS should contain alternating field names and sizes."
  (let ((ret (u/make-structdef))
        (offset 0))
    (cl-labels
        ((go (xs)
           (when (and (car xs) (cadr xs))
             (let ((sz (u/sizeof (cadr xs))))
               (ht-set! (u/structdef-fields ret) (car xs) offset)
               (setf offset (* (/ (+ offset sz (- align 1)) align) align))
               (go (cddr xs))))))
      (go fields)
      (setf (u/structdef-size ret) offset)
      ret)))
(defun u/offsetof (st field)
  "Return the offset in bytes of FIELD in the `u/structdef' ST."
  (ht-get (u/structdef-fields st) field))

;;;; Utility functions
(defun u/sizeof (ty)
  "Return the size of the type TY.
This can be a number of a symbol naming a primitive type or a `u/structdef'.
Or other things, we'll play it by ear."
  (cond
   ((numberp ty) ty) ;; if we pass a number, it's the size in bytes
   ((eq ty 'u8) 1)
   ((eq ty 'u16) 2)
   ((eq ty 'u32) 4)
   ((u/structdef-p ty) (u/structdef-size ty))
   (t (error "Attempted to get the size of unknown type: %s" ty))))

(defun u/split16be (w16)
  "Split the 16-bit W16 into a big-endian list of 8-bit integers."
  (list
   (logand #xff (lsh w16 -8))
   (logand #xff w16)))

(defun u/split16le (w16)
  "Split the 16-bit W16 into a little-endian list of 8-bit integers."
  (list
   (logand #xff w16)
   (logand #xff (lsh w16 -8))))

(defun u/split32le (w32)
  "Split the 32-bit W32 into a little-endian list of 8-bit integers."
  (list
   (logand #xff w32)
   (logand #xff (lsh w32 -8))
   (logand #xff (lsh w32 -16))
   (logand #xff (lsh w32 -24))))

(defun u/split64le (w64)
  "Split the 64-bit W64 into a little-endian list of 8-bit integers."
  (list
   (logand #xff w64)
   (logand #xff (lsh w64 -8))
   (logand #xff (lsh w64 -16))
   (logand #xff (lsh w64 -24))
   (logand #xff (lsh w64 -32))
   (logand #xff (lsh w64 -40))
   (logand #xff (lsh w64 -48))
   (logand #xff (lsh w64 -56))))

(defun u/pad-to (len bytes &optional byte)
  "Pad BYTES to LEN with 0xde (or BYTE).
Truncate BYTES if it is longer than LEN."
  (let* ((taken (-take len bytes)))
    (append taken (-repeat (- len (length taken)) (or byte #xde)))))

(defun u/write! (mem addr bytes)
  "Given a vector MEM and a base ADDR, write BYTES."
  (--each-indexed bytes
    (aset mem (+ addr it-index) it)))

(defun u/aref-u32-be (a idx)
  "Read a big-endian 32-bit integer starting at IDX from A."
  (logior
   (lsh (aref a idx) 24)
   (lsh (aref a (+ idx 1)) 16)
   (lsh (aref a (+ idx 2)) 8)
   (aref a (+ idx 3))))

(defun u/aref-u16-be (a idx)
  "Read a big-endian 16-bit integer starting at IDX from A."
  (logior
   (lsh (aref a idx) 8)
   (aref a (+ idx 1))))

(defun u/load-image-ff (path)
  "Load the Farbfeld image at PATH.
Return a list of the width, height, and pixels of the image."
  (when-let*
      ((data (f-read-bytes path))
       ((s-prefix? "farbfeld" data))
       (width (u/aref-u32-be data 8))
       (height (u/aref-u32-be data 12))
       (pixels
        (--map
         (let ((a (+ 16 (* it 8))))
           (list
            (lsh (u/aref-u16-be data a) -8)
            (lsh (u/aref-u16-be data (+ a 2)) -8)
            (lsh (u/aref-u16-be data (+ a 4)) -8)))
         (-iota (* width height)))))
    (list width height (seq-into pixels 'vector))))

(defun u/load-image-png (path)
  "Load the PNG image at PATH (by converting to Farbfeld first)."
  (let ((tmp (make-temp-file "udcff.ff")))
    (unwind-protect
      (when (= 0 (call-process-shell-command (format "png2ff <'%s' >'%s'" path tmp) nil "*udc-png-error*"))
        (u/load-image-ff tmp))
      (delete-file tmp))))

(defun u/pixel-grayscale (p)
  "Given a list of red, green, and blue bytes P, convert it to one grayscale byte."
  (round
   (+
    (* 0.2989 (car p))
    (* 0.5870 (cadr p))
    (* 0.1140 (caddr p)))))

(defun u/rgb-color-distance (c0 c1)
  "Determine the distance between two RGB colors C0 and C1."
  (let* ((r0 (car c0)) (g0 (cadr c0)) (b0 (caddr c0))
         (r1 (car c1)) (g1 (cadr c1)) (b1 (caddr c1))
         (dr (- r0 r1)) (dg (- g0 g1)) (db (- b0 b1)))
    (sqrt (+ (* dr dr) (* dg dg) (* db db)))))

(provide 'udc-utils)
;;; udc-utils.el ends here
