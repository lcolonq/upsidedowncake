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

(u/defstruct
 u/symtab-entry
 addr
 code)

(defun u/symtab-add! (symtab name entry)
  "Add a mapping from NAME to ENTRY in SYMTAB.
Ensure that the address of ENTRY doesn't fall within any existing regions.
Note that this doesn't exhaustively prevent overlaps.
\(Since we don't necessarily know the length of this symbol)."
  (let* ((addr (u/symtab-entry-addr entry))
         (overlap
          (--first
           (and
            (>= addr (u/symtab-entry-addr (cdr it)))
            (< addr (+ (u/symtab-entry-addr (cdr it)) (length (u/symtab-entry-code (cdr it))))))
           (ht->alist symtab))))
    (if overlap
        (error "Failed to add symbol %s - address within body of %s" name (car overlap))
      (ht-set! symtab name entry))))

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

(defun u/pad-to (len bytes)
  "Pad BYTES to LEN with 0xFF.
Truncate BYTES if it is longer than LEN."
  (let* ((taken (-take len bytes)))
    (append taken (-repeat (- len (length taken)) #xff))))

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

(defun u/pixel-grayscale (p)
  "Given a list of red, green, and blue bytes P, convert it to one grayscale byte."
  (round
   (+
    (* 0.2989 (car p))
    (* 0.5870 (cadr p))
    (* 0.1140 (caddr p)))))

(provide 'udc-utils)
;;; udc-utils.el ends here
