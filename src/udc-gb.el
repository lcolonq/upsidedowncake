;;; udc-gb --- Compiler Upside-Down Cake - Game Boy tools -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)

(defun u/gb/replace-symbols (symtab asm)
  "Replace keywords in ASM with values from the alist SYMTAB."
  (-map
   (lambda (ins)
     (--map (if (keywordp it) (alist-get it symtab) it) ins))
   asm))

(defun u/gb/assemble-imm8? (x)
  "Return the value for X if X is an 8-bit immediate."
  (when (and (integerp x) (> x 0) (<= x 255))
    x))

(defun u/gb/assemble-imm16? (x)
  "Return the value for X if X is a 16-bit immediate."
  (when (and (integerp x) (> x 0) (<= x 65535))
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
 - a list of an opcode symbol followed by arguments"
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
        ((and a0r16mem (eq a1 'a)) ;; ld [r16mem], a
         (list (logior #b00000010 (lsh a0r16mem 4))))
        ((and (eq a0 'a) a1r16mem) ;; ld a, [r16mem]
         (list (logior #b00001010 (lsh a1r16mem 4))))
        ((and a0imm16 (eq a1 'sp)) ;; ld [imm16], sp
         (cons #b00001000 (u/split16le a0imm16)))
        ((and a0imm16 (eq a1 'sp)) ;; ld r8, imm8
         (list (logior #b00000110 (lsh a0r8 3)) a1imm8))
        ((and a0r8 a1r8) ;; ld r8, r8
         (list (logior #b01000000 (lsh a0r8 3) a1r8)))
        ((and a0imm16 (eq a1 'a)) ;; ld [imm16], a
         (cons #b11101010 (u/split16le a0imm16)))
        ((and (eq a0 'a) a1imm16) ;; ld a, [imm16]
         (cons #b11111010 (u/split16le a1imm16)))
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
        (a0cond ;; jr cond, imm8
         (list (logior #b00100000 (lsh a0cond 3)) a0imm8))
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

(defun u/gb/link-one (code)
  "Link CODE into a sequence of bytes.
CODE should be a list of instructions."
  (-flatten (-map #'u/gb/assemble-ins code)))

(defun u/gb/link (base syms)
  "Link SYMS into an alist mapping keywords to sequences of bytes.
SYMS should be an alist mapping keywords to lists of instructions.
BASE specifies the base address where the resulting code will be placed."
  (let* ((presymtab (--map (cons (car it) 0) syms)) ;; use a temporary symbol table mapping each sym to 0
         ;; then use it to compute the lengths of each symbol
         (lengths (--map (cons (car it) (length (u/gb/replace-symbols presymtab (cdr it)))) syms))
         ;; then use those lengths to compute the real address of each symbol
         (symtab
          (cdr
           (--reduce-from
            (let ((off (+ (cdr it) (car acc))))
              (cons off (cons (cons (car it) (+ -1 (cdr it) (car acc))) (cdr acc))))
            (cons base nil)
            lengths)))
         ;; and finallly replace those addresses in the assembly
         (code (--map (cons (car it) (u/gb/replace-symbols symtab (cdr it))) syms)))
    ;; then assemble and concatenate all of the code!
    (--map (cons (car it) (u/gb/link-one (cdr it))) code)))

(defun u/gb/prepend-header (initial rom)
  "Prepend the 0x150-byte header to ROM.
Place 0x100 bytes of INITIAL at the front."
  (-concat
   (u/pad-to #x100 initial) ;; initial bytes
   (u/pad-to
    4
    (u/gb/link-one ;; entry point - jump to 0x150
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
            ('+
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
