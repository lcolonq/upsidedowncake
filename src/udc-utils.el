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

;;;; Symbol tables
(u/defstruct
 u/symtab
 (alignment 1) ;; symbol addresses must be a multiple of this
 (symbols (ht-create)) ;; hash table mapping symbols to entries
 (sections (ht-create)) ;; hash table mapping section name symbols to the current start address
 )

(u/defstruct
 u/symtab-entry
 addr ;; location of this symbol (in machine address space)
 (type 'code) ;; code or bytes or var or const
 data ;; list of instructions if code, or a list of bytes if data, or a size if var, or a function from symbol table and address to list of bytes if const
 )

(defun u/symtab-entry-length (symtab addr entry)
  "Return the length in bytes of ENTRY at ADDR in SYMTAB."
  (let ((data (u/symtab-entry-data entry)))
    (cl-case (u/symtab-entry-type entry)
      (code (* 4 (length data)))
      (bytes (length data))
      (var data)
      (const (length (funcall data symtab addr)))
      (t (error "Unknown symbol table entry type: %s" (u/symtab-entry-type entry))))))

(defun u/symtab-add-section! (symtab name addr)
  "Add a section NAME starting at ADDR in SYMTAB."
  (ht-set! (u/symtab-sections symtab) name addr))

(defun u/symtab-add-entry! (symtab name entry)
  "Add a mapping from NAME to ENTRY in SYMTAB."
  (ht-set! (u/symtab-symbols symtab) name entry))

(defun u/symtab-add! (symtab section name type data)
  "Add a mapping from NAME to DATA of TYPE in SECTION of SYMTAB."
  (let* ((section-offset (ht-get (u/symtab-sections symtab) section))
         (align (u/symtab-alignment symtab))
         (aligned (* (/ (+ section-offset (- align 1)) align) align))
         (entry (u/make-symtab-entry :addr aligned :type type :data data)))
    (unless section-offset
      (error "Could not find section %s when adding symbol %s" section name))
    (u/symtab-add-entry! symtab name entry)
    (ht-set!
     (u/symtab-sections symtab) section
     (+ aligned (u/symtab-entry-length symtab aligned entry)))))

(defun u/symtab-lookup (symtab name)
  "Return the address of NAME in SYMTAB."
  (ht-get (u/symtab-symbols symtab) name))

(defun u/symtab-lookup-relative (symtab base name)
  "Return the word offset of NAME in SYMTAB given the word offset BASE."
  (when-let*
      ((ent (u/symtab-lookup symtab name))
       (addrword (/ (u/symtab-entry-addr ent) 4)))
    (- addrword base 2)))

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

(defun u/pad-to (len bytes)
  "Pad BYTES to LEN with 0xde.
Truncate BYTES if it is longer than LEN."
  (let* ((taken (-take len bytes)))
    (append taken (-repeat (- len (length taken)) #xde))))

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

(defun u/rgb-color-distance (c0 c1)
  "Determine the distance between two RGB colors C0 and C1."
  (let* ((r0 (car c0)) (g0 (cadr c0)) (b0 (caddr c0))
         (r1 (car c1)) (g1 (cadr c1)) (b1 (caddr c1))
         (dr (- r0 r1)) (dg (- g0 g1)) (db (- b0 b1)))
    (sqrt (+ (* dr dr) (* dg dg) (* db db)))))

(provide 'udc-utils)
;;; udc-utils.el ends here
