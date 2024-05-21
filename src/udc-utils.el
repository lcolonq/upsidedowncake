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

(defun u/pad-to (len bytes)
  "Pad BYTES to LEN with 0xFF.
Truncate BYTES if it is longer than LEN."
  (let* ((taken (-take len bytes)))
    (append taken (-repeat (- len (length taken)) #xff))))

(defun u/write! (mem addr bytes)
  "Given a vector MEM and a base ADDR, write BYTES."
  (--each-indexed bytes
    (aset mem (+ addr it-index) it)))

(provide 'udc-utils)
;;; udc-utils.el ends here
