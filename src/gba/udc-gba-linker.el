;;; udc-gba-linker --- GBA linker and symbol table -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)
(require 'udc-gba-constants)
(require 'udc-gba-arm-assembler)
(require 'udc-gba-thumb-assembler)

;;;; Symbol tables
(cl-defstruct (u/gba/symtab (:constructor u/gba/make-symtab))
  (alignment 4) ;; symbol addresses must be a multiple of this
  (symbols (ht-create)) ;; hash table mapping symbols to entries
  (sections (ht-create)) ;; hash table mapping section name symbols to the current start address
  )

(cl-defstruct (u/gba/symtab-entry (:constructor u/gba/make-symtab-entry))
  addr ;; location of this symbol (in machine address space)
  (type 'arm) ;; arm or thumb or bytes or var or const
  data
  ;; list of instructions if arm or thumb,
  ;; or a list of bytes if bytes,
  ;; or a size if var,
  ;; or a function from symbol table and address to list of bytes if const
  )

(defun u/gba/symtab-entry-length (symtab addr entry)
  "Return the length in bytes of ENTRY at ADDR in SYMTAB."
  (let ((data (u/gba/symtab-entry-data entry)))
    (cl-case (u/gba/symtab-entry-type entry)
      (arm (* 4 (length data)))
      (thumb (* 2 (length data)))
      (bytes (length data))
      (var data)
      (const (length (funcall data symtab addr)))
      (t (error "Unknown symbol table entry type: %s" (u/gba/symtab-entry-type entry))))))

(defun u/gba/symtab-add-section! (symtab name addr)
  "Add a section NAME starting at ADDR in SYMTAB."
  (ht-set! (u/gba/symtab-sections symtab) name addr))

(defun u/gba/symtab-add-entry! (symtab name entry)
  "Add a mapping from NAME to ENTRY in SYMTAB."
  (ht-set! (u/gba/symtab-symbols symtab) name entry))

(defun u/gba/symtab-add! (symtab section name type data)
  "Add a mapping from NAME to DATA of TYPE in SECTION of SYMTAB."
  (let* ((section-offset (ht-get (u/gba/symtab-sections symtab) section))
          (align (u/gba/symtab-alignment symtab))
          (aligned (* (/ (+ section-offset (- align 1)) align) align))
          (entry (u/gba/make-symtab-entry :addr aligned :type type :data data)))
    (unless section-offset
      (error "Could not find section %s when adding symbol %s" section name))
    (u/gba/symtab-add-entry! symtab name entry)
    (ht-set!
      (u/gba/symtab-sections symtab) section
      (+ aligned (u/gba/symtab-entry-length symtab aligned entry)))))

(defun u/gba/symtab-lookup (symtab name)
  "Return the address of NAME in SYMTAB."
  (let ((res (ht-get (u/gba/symtab-symbols symtab) name)))
    (or res (error "Could not find symbol: %s" name))))

(defun u/gba/symtab-lookup-relative (inssz symtab base name)
  "Return the word offset of NAME in SYMTAB given the word offset BASE.
Instructions are sized at INSSZ."
  (when-let*
    ((ent (u/gba/symtab-lookup symtab name))
      (addrword (/ (u/gba/symtab-entry-addr ent) inssz)))
    (message "replacing %s (at word %s) with %s (base %s)" name addrword (- addrword base 2) base)
    (- addrword base 2)))

;;;; Known addresses in most symbol tables
(defun u/gba/initial-symtab ()
  "Create and populate a new symbol table."
  (let ((symtab (u/gba/make-symtab :alignment 4)))
    (u/gba/symtab-add-section! symtab :header u/gba/rom-start)
    (u/gba/symtab-add-section! symtab :code (+ u/gba/rom-start #x1000))
    (u/gba/symtab-add-section! symtab :data (+ u/gba/rom-start #x20000))
    (u/gba/symtab-add-section! symtab :vars #x02000000)

    (u/gba/symtab-add-entry! symtab :reg-ifbios (u/gba/make-symtab-entry :addr #x03007ff8 :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-intaddr (u/gba/make-symtab-entry :addr #x03007ffc :type 'var :data 4))

    (u/gba/symtab-add-entry! symtab :reg-dispcnt (u/gba/make-symtab-entry :addr #x04000000 :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-dispstat (u/gba/make-symtab-entry :addr #x04000004 :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-bg0cnt (u/gba/make-symtab-entry :addr #x04000008 :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-bg1cnt (u/gba/make-symtab-entry :addr #x0400000a :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-keyinput (u/gba/make-symtab-entry :addr #x04000130 :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-ie (u/gba/make-symtab-entry :addr #x04000200 :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-if (u/gba/make-symtab-entry :addr #x04000202 :type 'var :data 2))
    (u/gba/symtab-add-entry! symtab :reg-ime (u/gba/make-symtab-entry :addr #x04000208 :type 'var :data 1))
    (u/gba/symtab-add-entry! symtab :palette-bg (u/gba/make-symtab-entry :addr #x05000000 :type 'var :data 512))
    (u/gba/symtab-add-entry! symtab :palette-sprite (u/gba/make-symtab-entry :addr #x05000200 :type 'var :data 512))

    (u/gba/symtab-add-entry! symtab :vram-bg (u/gba/make-symtab-entry :addr #x06000000 :type 'var :data (* 64 1024)))
    (u/gba/symtab-add-entry! symtab :vram-bg-charblock0 (u/gba/make-symtab-entry :addr #x06000000 :type 'var :data 16384))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock0  (u/gba/make-symtab-entry :addr #x06000000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock1  (u/gba/make-symtab-entry :addr #x06000800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock2  (u/gba/make-symtab-entry :addr #x06001000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock3  (u/gba/make-symtab-entry :addr #x06001800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock4  (u/gba/make-symtab-entry :addr #x06002000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock5  (u/gba/make-symtab-entry :addr #x06002800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock6  (u/gba/make-symtab-entry :addr #x06003000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock7  (u/gba/make-symtab-entry :addr #x06003800 :type 'var :data 2048))

    (u/gba/symtab-add-entry! symtab :vram-bg-charblock1  (u/gba/make-symtab-entry :addr #x06004000 :type 'var :data 16384))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock8  (u/gba/make-symtab-entry :addr #x06004000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock9  (u/gba/make-symtab-entry :addr #x06004800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock10 (u/gba/make-symtab-entry :addr #x06005000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock11 (u/gba/make-symtab-entry :addr #x06005800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock12 (u/gba/make-symtab-entry :addr #x06006000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock13 (u/gba/make-symtab-entry :addr #x06006800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock14 (u/gba/make-symtab-entry :addr #x06007000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock15 (u/gba/make-symtab-entry :addr #x06007800 :type 'var :data 2048))

    (u/gba/symtab-add-entry! symtab :vram-bg-charblock2  (u/gba/make-symtab-entry :addr #x06008000 :type 'var :data 16384))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock16 (u/gba/make-symtab-entry :addr #x06008000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock17 (u/gba/make-symtab-entry :addr #x06008800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock18 (u/gba/make-symtab-entry :addr #x06009000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock19 (u/gba/make-symtab-entry :addr #x06009800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock20 (u/gba/make-symtab-entry :addr #x0600a000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock21 (u/gba/make-symtab-entry :addr #x0600a800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock22 (u/gba/make-symtab-entry :addr #x0600b000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock23 (u/gba/make-symtab-entry :addr #x0600b800 :type 'var :data 2048))

    (u/gba/symtab-add-entry! symtab :vram-bg-charblock3  (u/gba/make-symtab-entry :addr #x0600c000 :type 'var :data 16384))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock24 (u/gba/make-symtab-entry :addr #x0600c000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock25 (u/gba/make-symtab-entry :addr #x0600c800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock26 (u/gba/make-symtab-entry :addr #x0600d000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock27 (u/gba/make-symtab-entry :addr #x0600d800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock28 (u/gba/make-symtab-entry :addr #x0600e000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock29 (u/gba/make-symtab-entry :addr #x0600e800 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock30 (u/gba/make-symtab-entry :addr #x0600f000 :type 'var :data 2048))
    (u/gba/symtab-add-entry! symtab :vram-bg-screenblock31 (u/gba/make-symtab-entry :addr #x0600f800 :type 'var :data 2048))

    (u/gba/symtab-add-entry! symtab :vram-sprite (u/gba/make-symtab-entry :addr #x06010000 :type 'var :data (* 32 1024)))
    (u/gba/symtab-add-entry! symtab :vram-sprite-charblock0 (u/gba/make-symtab-entry :addr #x06010000 :type 'var :data 16384))

    (u/gba/symtab-add-entry! symtab :oam (u/gba/make-symtab-entry :addr #x07000000 :type 'var :data 1024))
    symtab))
;;;; Linker and header generation
(defun u/gba/relocate-ins (inssz symtab idx ins)
  "Replace all keywords in INS by looking up relative addresses in SYMTAB.
We assume that this instruction is at word IDX in memory.
Instructions are sized at INSSZ."
  (--map
    (if (keywordp it)
      (if-let* ((ent (u/gba/symtab-lookup-relative inssz symtab idx it)))
        ent
        (error "Unknown symbol: %s" it))
      it)
    ins))

(defun u/gba/relocate (inssz symtab base prog)
  "Replace all keywords in PROG by looking up relative addresses in SYMTAB.
We assume that PROG starts in memory at word BASE.
Instructions are sized at INSSZ."
  (--map-indexed (u/gba/relocate-ins inssz symtab (+ base it-index) it) prog))

(defun u/gba/link (symtab base size)
  "Convert SYMTAB to a vector of bytes to be placed at address BASE.
SIZE is the length of the resulting vector."
  (condition-case res
    (let* ((mem (make-vector size 0))
            (memwrite
              (lambda (name addr idx bytes)
                (if (and (>= idx 0) (< (+ idx (length bytes)) size))
                  (u/write! mem idx bytes)
                  (warn "Symbol table entry %s at %s is out of bounds" name addr)))))
      (--each (ht->alist (u/gba/symtab-symbols symtab))
        (let* ( (name (car it))
                (entry (cdr it))
                (type (u/gba/symtab-entry-type entry))
                (addr (u/gba/symtab-entry-addr entry))
                (idx (- addr base)))
          (cl-case type
            (arm
              (condition-case err
                (let* ((relocated (u/gba/relocate 4 symtab (/ addr 4) (u/gba/symtab-entry-data entry)))
                        (bytes (u/gba/arm-assemble relocated)))
                  (funcall memwrite name addr idx bytes))
                (error
                  (error "While assembling ARM code at %s\n%s" name (cadr err)))))
            (thumb
              (condition-case err
                (let* ((relocated (u/gba/relocate 2 symtab (/ addr 2) (u/gba/symtab-entry-data entry)))
                        (bytes (u/gba/thumb-assemble relocated)))
                  (funcall memwrite name addr idx bytes))
                (error
                  (error "While assembling Thumb code at %s\n%s" name (cadr err)))))
            (bytes
              (funcall memwrite name addr idx (u/gba/symtab-entry-data entry)))
            (var nil)
            (const
              (funcall
                memwrite name addr idx
                (funcall (u/gba/symtab-entry-data entry) symtab addr)))
            (t (error "Unknown symbol table entry type: %s" (u/gba/symtab-entry-type it))))))
      mem)
    (error
      (error "Error while building ROM!\n%s" (cadr res)))
    (:success
      (message "Successfully linked ROM!")
      res)))

(cl-defstruct (u/gba/header (:constructor u/gba/make-header))
  entry ;; instruction branching to entrypoint
  title ;; game title, max 12 characters
  code ;; game code, 4 characters
  maker ;; make code, 2 characters
  )

(defun u/gba/header (header)
  "Return a function from a symbol table to a HEADER."
  (lambda (symtab addr)
    (-concat
      (u/gba/arm-assemble-ins ;; place an ARM jump to entrypoint at the start
        `(b ,(or (u/gba/symtab-lookup-relative 4 symtab (/ addr 4) (u/gba/header-entry header)) -2)))
      (u/pad-to 156 '()) ;; nintendo logo
      (u/pad-to 12 (seq-into (s-upcase (u/gba/header-title header)) 'list) #x00) ;; game title
      (u/pad-to 4 (seq-into (s-upcase (u/gba/header-code header)) 'list) #x00) ;; game code
      (u/pad-to 2 (seq-into (s-upcase (u/gba/header-maker header)) 'list) #x00) ;; maker code
      '(#x96) ;; fixed value
      '(#x00) ;; main unit code
      '(#x00) ;; device type
      (u/pad-to 7 '() #x00) ;; reserved area
      '(#x00) ;; software version
      '(#x00) ;; header checksum
      (u/pad-to 2 '() #x00) ;; reserved area
      )))

(provide 'udc-gba-linker)
;;; udc-gba-linker.el ends here
