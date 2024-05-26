;;; udc-gb --- Compiler Upside-Down Cake - Game Boy tools -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'muzak)
(require 'udc-utils)

;;;; Constants
;; registers specified as offsets from 0xff00 (for use with ldh)
(defconst u/gb/reg-input #x00)
(defconst u/gb/reg-sound-channel-1-sweep #x10)
(defconst u/gb/reg-sound-channel-1-dutycycle #x11)
(defconst u/gb/reg-sound-channel-1-envelope #x12)
(defconst u/gb/reg-sound-channel-1-periodlo #x13)
(defconst u/gb/reg-sound-channel-1-periodhi #x14)
(defconst u/gb/reg-interrupt-flag #x0f)
(defconst u/gb/reg-sound #x26)
(defconst u/gb/reg-sound-volume #x24)
(defconst u/gb/reg-lcdc #x40)
(defconst u/gb/reg-ly #x44)
(defconst u/gb/reg-bgp #x47)
(defconst u/gb/reg-interrupt #xff)

;;;; Utility functions
(defun u/gb/freq-to-period (freq)
  "Convert FREQ to an 11-bit period value."
  (if (= freq 0)
      0
    (let ((res (round (- 2048.0 (/ 131072.0 freq)))))
      (if (>= res 2048)
          (error "Frequency %s is too high" freq)
        res))))

(defun u/gb/music-from-muzak (inp)
  "Turn the Muzak string INP into a list of bytes appropriate for playbnack."
  (let* ((parsed (muzak/parse inp))
         (track (car parsed))
         (freqs (-map #'muzak/note-to-freq track))
         (periods (-map #'u/gb/freq-to-period freqs)))
    (-flatten (-map #'u/split16le periods))))

(defun u/gb/image-quantize-guess (rgb)
  "Quantize RGB in a somewhat reasonable way.
For images that weren't made for the Game Boy."
  (let ((gs (u/pixel-grayscale rgb)))
    (- 3 (lsh gs -6))))

(defun u/gb/image-quantize-aseprite (rgb)
  "Quantize RGB based on Aseprite's Game Boy palette.
For images that weren't made for the Game Boy."
  (let ((r (car rgb))
        (g (cadr rgb))
        (b (caddr rgb)))
    (cond
     ((and (= r #x9b) (= g #xbc) (= b #x0f)) 0)
     ((and (= r #x8b) (= g #xac) (= b #x0f)) 1)
     ((and (= r #x30) (= g #x62) (= b #x30)) 2)
     ((and (= r #x0f) (= g #x38) (= b #x0f)) 3)
     (t (error "Invalid color: %s" rgb)))))

(defun u/gb/image-tiles (quantize image)
  "Given an RGB IMAGE (a list of width, height, and pixels), produce tile data.
Calls QUANTIZE to convert the RGB color lists to a number from 0 to 3.
Returns a pair of a list of tiles and a list of tile indices (a tilemap)."
  (let* ((width (car image))
         (height (cadr image))
         (width-tiles (ceiling (/ width 8.0)))
         (height-tiles (ceiling (/ height 8.0)))
         (pixels (caddr image))
         (quantized (seq-into (-map quantize pixels) 'vector)))
    (cl-flet*
        ((getpixel (x y)
           (if (or (< x 0) (>= x width) (< y 0) (>= y height))
               0
             (aref quantized (+ x (* y width)))))
         (getrow (x y)
           (let ((pixels (--map (getpixel (+ x it) y) (-iota 8))))
             (list
              (apply #'logior (--map-indexed (lsh (logand #b1 it) (- 7 it-index)) pixels))
              (apply #'logior (--map-indexed (lsh (logand #b1 (lsh it -1)) (- 7 it-index)) pixels)))))
         (gettile (tx ty)
           (let ((x (* tx 8))
                 (y (* ty 8)))
             (-flatten
              (--map
               (getrow x (+ y it))
               (-iota 8))))))
      (let* ((tiles
             (apply
              #'-concat
              (-map
               (lambda (y)
                 (-map
                  (lambda (x)
                    (cons (cons x y) (gettile x y)))
                  (-iota width-tiles)))
               (-iota height-tiles))))
             (unique-tiles (-uniq (-map #'cdr tiles)))
             (tile-indices (--map-indexed (cons it it-index) unique-tiles))
             (coord-indices (--map (cons (car it) (alist-get (cdr it) tile-indices nil nil #'equal)) tiles))
             (tilemap
              (-map
               (lambda (y)
                 (-map
                  (lambda (x)
                    (alist-get (cons x y) coord-indices 0 nil #'equal))
                  (-iota 32)))
               (-iota 32)))
             )
        (cons
         (-flatten unique-tiles)
         (-flatten tilemap))))))

(defun u/gb/replace-symbols (symtab asm)
  "Replace keywords in ASM with values from SYMTAB."
  (-map
   (lambda (ins)
     (--map
      (if (keywordp it)
          (let ((entry (ht-get symtab it 0)))
            (if (u/symtab-entry-p entry) (u/symtab-entry-addr entry) entry))
        it)
      ins))
   asm))

(defun u/gb/assemble-imm8? (x)
  "Return the value for X if X is an 8-bit immediate."
  (cond
   ((and (integerp x) (>= x 0) (<= x 255)) x)
   ((and (integerp x) (>= x -128) (< x 0)) (+ 255 x))
   (t nil)))

(defun u/gb/assemble-imm16? (x)
  "Return the value for X if X is a 16-bit immediate."
  (when (and (integerp x) (>= x 0) (<= x 65535))
    x))

(defun u/gb/assemble-r8? (x)
  "Return the value for X if X represents an r8."
  (cl-case x
    (b 0)
    (c 1)
    (d 2)
    (e 3)
    (h 4)
    (l 5)
    (*hl 6)
    (a 7)
    (otherwise nil)))

(defun u/gb/assemble-r16? (x)
  "Return the value for X if X represents an r16."
  (cl-case x
    (bc 0)
    (de 1)
    (hl 2)
    (sp 3)
    (otherwise nil)))

(defun u/gb/assemble-r16stk? (x)
  "Return the value for X if X represents an r16stk."
  (cl-case x
    (bc 0)
    (de 1)
    (hl 2)
    (af 3)
    (otherwise nil)))

(defun u/gb/assemble-r16mem? (x)
  "Return the value for X if X represents an r16mem."
  (cl-case x
    (*bc 0)
    (*de 1)
    (*hl+ 2)
    (*hl- 3)
    (otherwise nil)))

(defun u/gb/assemble-cond? (x)
  "Return the value for X if X represents a cond."
  (cl-case x
    (nz 0)
    (z 1)
    (nc 2)
    (c 3)
    (otherwise nil)))

(defun u/gb/assemble-ins (ins)
  "Assemble INS to a sequence of bytes.
INS is either:
 - an opcode symbol
 - a list of an opcode symbol followed by operands"
  (let* ((op (if (listp ins) (car ins) ins))
         (args (if (listp ins) (cdr ins) nil))
         (a0 (car args))
         (a1 (cadr args))
         (a0imm8 (and a0 (u/gb/assemble-imm8? a0)))
         (a0imm16 (and a0 (u/gb/assemble-imm16? a0)))
         (a0r8 (and a0 (u/gb/assemble-r8? a0)))
         (a0r16 (and a0 (u/gb/assemble-r16? a0)))
         (a0r16stk (and a0 (u/gb/assemble-r16stk? a0)))
         (a0r16mem (and a0 (u/gb/assemble-r16mem? a0)))
         (a0cond (and a0 (u/gb/assemble-cond? a0)))
         (a1imm8 (and a1 (u/gb/assemble-imm8? a1)))
         (a1imm16 (and a1 (u/gb/assemble-imm16? a1)))
         (a1r8 (and a1 (u/gb/assemble-r8? a1)))
         (a1r16 (and a1 (u/gb/assemble-r16? a1)))
         (a1r16mem (and a1 (u/gb/assemble-r16mem? a1))))
    (cl-case op
      (nop '(#x00)) ;; nop
      (ld
       (cond
        ((and a0r16 a1imm16) ;; ld r16, imm16
         (cons (logior #b00000001 (lsh a0r16 4)) (u/split16le a1imm16)))
        ((and a0imm16 (eq a1 'a)) ;; ld [imm16], a
         (cons #b11101010 (u/split16le a0imm16)))
        ((and (eq a0 'a) a1imm16 (eq (caddr args) 'mem)) ;; ld a, [imm16]
         (cons #b11111010 (u/split16le a1imm16)))
        ((and a0r16mem (eq a1 'a)) ;; ld [r16mem], a
         (list (logior #b00000010 (lsh a0r16mem 4))))
        ((and (eq a0 'a) a1r16mem) ;; ld a, [r16mem]
         (list (logior #b00001010 (lsh a1r16mem 4))))
        ((and a0imm16 (eq a1 'sp)) ;; ld [imm16], sp
         (cons #b00001000 (u/split16le a0imm16)))
        ((and a0r8 a1imm8) ;; ld r8, imm8
         (list (logior #b00000110 (lsh a0r8 3)) a1imm8))
        ((and a0r8 a1r8) ;; ld r8, r8
         (list (logior #b01000000 (lsh a0r8 3) a1r8)))
        ((and (eq a0 'hl) a1imm8 (and (caddr args) (eq (caddr args) 'sp+))) ;; ld hl, sp + imm8
         (list #b11111000 a1imm8))
        ((and (eq a0 'sp) (eq a1 'hl)) ;; ld sp, hl
         '(11111001))
        ))
      (ldh
       (cond
        ((and (eq a0 '*c) (eq a1 'a)) ;; ldh [c], a
         '(#b11100010))
        ((and a0imm8 (eq a1 'a)) ;; ldh [imm8], a
         (list #b11100000 a0imm8))
        ((and (eq a0 'a) (eq a1 '*c)) ;; ldh a, [c]
         '(#b11110010))
        ((and (eq a0 'a) a1imm8) ;; ldh a, [imm8]
         (list #b11110000 a1imm8))
        ))
      (inc
       (cond
        (a0r16 ;; inc r16
         (list (logior #b00000011 (lsh a0r16 4))))
        (a0r8 ;; inc r8
         (list (logior #b00000100 (lsh a0r8 3))))
        ))
      (dec
       (cond
        (a0r16 ;; dec r16
         (list (logior #b00001011 (lsh a0r16 4))))
        (a0r8 ;; dec r8
         (list (logior #b00000101 (lsh a0r8 3))))
        ))
      (add
       (cond
        ((and (eq a0 'hl) a1r16) ;; add hl, r16
         (list (logior #b00001001 (lsh a1r16 4))))
        ((and (eq a0 'a) a1r8) ;; add a, r8
         (list (logior #b10000000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; add a, imm8
         (list #b11000110 a1imm8))
        ((and (eq a0 'sp) a1imm8) ;; add sp, imm8
         (list #b11101000 a1imm8))
        ))
      (adc
       (cond
        ((and (eq a0 'a) a1r8) ;; adc a, r8
         (list (logior #b10001000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; adc a, imm8
         (list #b11001110 a1imm8))
        ))
      (sub
       (cond
        ((and (eq a0 'a) a1r8) ;; sub a, r8
         (list (logior #b10010000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; sub a, imm8
         (list #b11010110 a1imm8))
        ))
      (sbc
       (cond
        ((and (eq a0 'a) a1r8) ;; sbc a, r8
         (list (logior #b10011000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; sbc a, imm8
         (list #b11011110 a1imm8))
        ))
      (and
       (cond
        ((and (eq a0 'a) a1r8) ;; and a, r8
         (list (logior #b10100000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; and a, imm8
         (list #b11100110 a1imm8))
        ))
      (xor
       (cond
        ((and (eq a0 'a) a1r8) ;; xor a, r8
         (list (logior #b10101000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; xor a, imm8
         (list #b11101110 a1imm8))
        ))
      (or
       (cond
        ((and (eq a0 'a) a1r8) ;; or a, r8
         (list (logior #b10110000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; or a, imm8
         (list #b11110110 a1imm8))
        ))
      (cp
       (cond
        ((and (eq a0 'a) a1r8) ;; cp a, r8
         (list (logior #b10111000 a1r8)))
        ((and (eq a0 'a) a1imm8) ;; cp a, imm8
         (list #b11111110 a1imm8))
        ))
      (jp
       (cond
        (a0imm16 ;; jp imm16
         (cons #b11000011 (u/split16le a0imm16)))
        ((and a0cond a1imm16) ;; jp cond, imm16
         (cons (logior #b11000010 (lsh a0cond 3)) (u/split16le a1imm16)))
        ((eq a0 'hl) ;; jp hl
         '(#b11101001))
        ))
      (jr
       (cond
        (a0imm8 ;; jr imm8
         (list #b00011000 a0imm8))
        ((and a0cond a1imm8) ;; jr cond, imm8
         (list (logior #b00100000 (lsh a0cond 3)) a1imm8))
        ))
      (ret
       (cond
        (a0cond ;; ret cond
         (list (logior #b11000000 (lsh a0cond 3))))
        (t '(#b11001001)) ;; ret
        ))
      (call
       (cond
        (a0imm16 ;; call imm16
         (cons #b11001101 (u/split16le a0imm16)))
        ((and a0cond a1imm16) ;; call cond, imm16
         (cons (logior #b11000100 (lsh a0cond 3)) (u/split16le a1imm16)))))
      (rst (list (logior #b11000111 (lsh (logand a0 #b111) 3)))) ;; rst tgt3
      (pop
       (cond
        (a0r16stk ;; pop r16stk
         (list (logior #b11000001 (lsh a0r16stk 4))))))
      (push
       (cond
        (a0r16stk ;; push r16stk
         (list (logior #b11000101 (lsh a0r16stk 4))))))
      (reti '(#b11011001)) ;; reti
      (rlca '(#b00000111)) ;; rlca
      (rrca '(#b00001111)) ;; rrca
      (rla '(#b00010111)) ;; rla
      (rra '(#b00011111)) ;; rra
      (daa '(#b00100111)) ;; daa
      (cpl '(#b00101111)) ;; cpl
      (scf '(#b00110111)) ;; scf
      (ccf '(#b00111111)) ;; ccf
      (stop '(#b00010000)) ;; stop
      (halt '(#b01110110)) ;; halt
      (di '(#b11110011)) ;; di
      (ei '(#b11111011)) ;; ei
      ;; CB prefix:
      (rlc ;; rlc r8
       (when a0r8 (list #xcb (logior #b00000000 a0r8))))
      (rrc ;; rrc r8
       (when a0r8 (list #xcb (logior #b00001000 a0r8))))
      (rl ;; rl r8
       (when a0r8 (list #xcb (logior #b00010000 a0r8))))
      (rr ;; rr r8
       (when a0r8 (list #xcb (logior #b00011000 a0r8))))
      (sla ;; sla r8
       (when a0r8 (list #xcb (logior #b00100000 a0r8))))
      (sra ;; sra r8
       (when a0r8 (list #xcb (logior #b00101000 a0r8))))
      (swap ;; swap r8
       (when a0r8 (list #xcb (logior #b00110000 a0r8))))
      (srl ;; srl r8
       (when a0r8 (list #xcb (logior #b00111000 a0r8))))
      (bit ;; bit b3, r8
       (when a1r8 (list #xcb (logior #b01000000 (lsh (logand a0 #b111) 3) a1r8))))
      (res ;; res b3, r8
       (when a1r8 (list #xcb (logior #b10000000 (lsh (logand a0 #b111) 3) a1r8))))
      (set ;; set b3, r8
       (when a1r8 (list #xcb (logior #b11000000 (lsh (logand a0 #b111) 3) a1r8)))))))

(defun u/gb/assemble (code)
  "Link CODE into a sequence of bytes.
CODE should be a list of instructions."
  (-flatten
   (--map
    (let ((res (u/gb/assemble-ins it)))
      (if (and res (-all? #'integerp res))
          res
        (error "Failed to assemble instruction: %s" it)))
    code)))

(defun u/gb/reserve (bytes)
  "Return a list of BYTES NOP instructions.
This is useful when reserving space for variables."
  (-repeat bytes 'nop))

(defun u/gb/assembly-length (asm)
  "Return the length in bytes of ASM."
  (length (u/gb/assemble (u/gb/replace-symbols (ht-create) asm))))

(defun u/gb/link (symtab base syms)
  "Add SYMS to SYMTAB.
SYMTAB is an existing hash table from keywords to symbol table entries.
SYMS should be an alist mapping keywords to lists of instructions.
BASE specifies the base address where the resulting code will be placed."
  (let* ((lengths ;; compute the lengths of each symbol
          (--map
           (cons
            (car it)
            (u/gb/assembly-length (cdr it)))
           syms))
         ;; then use those lengths to compute the real address of each symbol
         (addrs
          (cons
           (cons (caar lengths) base)
           (cdr
            (--reduce-from
             (let ((off (+ (cddr it) (car acc))))
               (cons off (cons (cons (caar it) off) (cdr acc))))
             (cons base nil)
             (-zip-pair (cdr lengths) lengths))))))
    (--each addrs ;; add entries only containing the addresses to symtab
      (u/symtab-add! ;; (we only need lengths to replace symbols in the code)
       symtab
       (car it)
       (u/make-symtab-entry :addr (cdr it))))
    (--each syms ;; then add all of the code
      (when-let ((entry (ht-get symtab (car it))))
        (setf
         (u/symtab-entry-code entry)
         (u/gb/assemble (u/gb/replace-symbols symtab (cdr it))))))))

(defun u/gb/prepend-header (initial rom)
  "Prepend the 0x150-byte header to ROM.
Place 0x100 bytes of INITIAL at the front."
  (-concat
   (u/pad-to #x100 initial) ;; initial bytes
   (u/pad-to
    4
    (u/gb/assemble ;; entry point - jump to 0x150
     '(nop
       (jp #x150))))
   (list ;; nintendo logo
    #xce #xed #x66 #x66 #xcc #x0d #x00 #x0b #x03 #x73 #x00 #x83 #x00 #x0c #x00 #x0d
    #x00 #x08 #x11 #x1f #x88 #x89 #x00 #x0e #xdc #xcc #x6e #xe6 #xdd #xdd #xd9 #x99
    #xbb #xbb #x67 #x63 #x6e #x0e #xec #xcc #xdd #xdc #x99 #x9f #xbb #xb9 #x33 #x3e)
   (-repeat 16 #x00) ;; title (we don't care)
   (list #x00 #x00) ;; new licensee code
   (list #x00) ;; SGB flag
   (list #x00) ;; cartridge type (currently ROM only)
   (list #x00) ;; ROM size (currently 32KiB)
   (list #x00) ;; RAM size (currently no RAM)
   (list #x00) ;; destination code
   (list #x00) ;; old licensee code
   (list #x00) ;; version number
   (list #x00) ;; header checksum
   (u/split16be #x0000) ;; global checksum
   rom))

(defun u/gb/expression (expr)
  "Convert EXPR to a list of assembly instructions.
The reulting value will be placed in register A."
  (cond
   ((listp expr)
    (let ((res nil))
      (--each (cdr expr)
        (setq
         res
         (append
          res
          (u/gb/expression it)
          '((dec sp)
            (ld sp a))
          )))
      (setq
       res
       (append
        res
        '((ld b 0))))
      (--each (cdr expr)
        (setq
         res
         (append
          res
          (cl-case (car expr)
            (+
             '((ld a sp)
               (inc sp)
               (add a b)
               (ld b a)))))))
      res
      ))
   ((integerp expr)
    `((ld a ,expr)))
   ((and (symbolp expr) (u/gb/assemble-r8? expr))
    `((ld a ,expr)))
   ))

(provide 'udc-gb)
;;; udc-gb.el ends here
