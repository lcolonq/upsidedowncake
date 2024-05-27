;;; game --- A simple Game Boy game -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(add-to-list 'load-path (f-canonical "."))
(require 'udc)

;;;; Code generation state
(defconst g/mem-early-length #x150)
(defconst g/mem-early (make-vector g/mem-early-length 0))
(defconst g/mem-length (* 32 1000))
(defconst g/mem (make-vector g/mem-length 0))
(defconst g/symtab (ht-create))
(defconst g/symtab-interrupts (ht-create))

;;;; Assets
(defconst g/font-image (u/load-image-ff "assets/font.ff"))
(defconst g/font-addr #x4000)
(defconst g/font-tiles (car (u/gb/image-tiles #'u/gb/image-quantize-aseprite g/font-image)))
(u/write! g/mem g/font-addr g/font-tiles)

(defconst g/ui-image (u/load-image-ff "assets/ui.ff"))
(defconst g/ui-addr (+ g/font-addr (length g/font-tiles)))
(defconst g/ui-tiles (car (u/gb/image-tiles #'u/gb/image-quantize-aseprite g/ui-image)))
(u/write! g/mem g/ui-addr g/ui-tiles)

(defconst g/battle-image (u/load-image-ff "assets/battle.ff"))
(defconst g/battle-tiledata (u/gb/image-tiles #'u/gb/image-quantize-aseprite g/battle-image))
(defconst g/battle-tiles-addr (+ g/ui-addr (length g/ui-tiles)))
(u/write! g/mem g/battle-tiles-addr (car g/battle-tiledata))
(defconst g/battle-tilemap-addr #x5000)
(u/write!
 g/mem g/battle-tilemap-addr
 (--map
  (+ it (/ (- g/battle-tiles-addr g/font-addr) 16))
  (cdr g/battle-tiledata)))

(defconst g/music
  ;; (u/gb/music-from-muzak "E3E3F3G3/A3F3E3D3/C3C3D3E3/G3E3D3C3///B3B3A3G3/A3G3F3E3/D3D3E3F3/A3F3E3D3///")
  '(229 4 229 4 17 5 99 5 0 0 172 5 17 5 229 4 131 4 0 0 22 4 22 4 131 4
        229 4 0 0 99 5 229 4 131 4 22 4 0 0 0 0 0 0 237 5 237 5 172 5 99
        5 0 0 172 5 99 5 17 5 229 4 0 0 131 4 131 4 229 4 17 5 0 0 172 5
        17 5 229 4 131 4 0 0 0 0 0 0))
(defconst g/music-addr #x6000)
(u/write! g/mem g/music-addr g/music)

;;;; Utility functions
(defun g/unless (cond body)
  "Return assembly to only run BODY if COND is not set."
  `((jr ,cond ,(u/gb/assembly-length body))
    ,@body))

(defun g/ite (cond then else)
  "Return assembly to run THEN if COND is set and ELSE otherwise."
  (let ((elsebody
         `(,@else
           (jr ,(u/gb/assembly-length then)))))
    `((jr ,cond ,(u/gb/assembly-length elsebody))
      ,@elsebody
      ,@then)))

(defun g/while (cond body)
  "Return assembly to repeat BODY while COND is set."
  `(,@body
    (jr ,cond ,(- (+ (u/gb/assembly-length body) 1)))))

(defun g/memset (val dst len)
  "Return assembly setting LEN bytes in DST to VAL."
  `((ld hl ,dst)
    (ld bc ,len)
    ,@(g/while
       'nz
       `((ld a ,val)
         (ld *hl+ a)
         (dec bc)
         (ld a b)
         (or a c)))))

(defun g/memcpy (src dst len)
  "Return assembly copying LEN bytes from SRC to DST."
  `((ld de ,src)
    (ld hl ,dst)
    (ld bc ,len)
    ,@(g/while
       'nz
       '((ld a *de)
         (ld *hl+ a)
         (inc de)
         (dec bc)
         (ld a b)
         (or a c)))))

(defun g/write-cursor (idx &optional erase)
  "Return assembly writing the cursor to entry IDX.
If ERASE is non-nil, erase that cursor instead."
  (let* ((tx 18)
         (ty (+ 1 (* idx 3)))
         (underline-len (+ 3 idx))
         (underline-x (- tx (+ 1 underline-len))))
    `(
      ;; draw cursor
      (ld a ,(+ (/ (length g/font-tiles) 16) (if erase 3 1)))
      (ld ,(+ #x9800 (* ty 32) tx) a)
      (ld a ,(+ (/ (length g/font-tiles) 16) (if erase 3 2)))
      (ld ,(+ #x9800 (* (+ ty 1) 32) tx) a)
      ;; draw underline
      ,@(apply
         #'-concat
         (--map
          `((ld a ,(+ (/ (length g/font-tiles) 16) (if erase 3 0)))
            (ld ,(+ #x9800 (* (+ ty 2) 32) (+ underline-x it)) a))
          (-iota underline-len))))))

(defun g/write-digit (x y)
  "Return assembly writing the digit in the A register to tile X, Y."
  (let ((tx (+ 2 x))
        (ty (+ 12 y)))
    `((add a 16)
      (ld ,(+ #x9800 (* ty 32) tx) a))))

(defun g/write-battle-text (msg)
  "Return assembly writing the string MSG to the battle text box."
  (let ((ret (list))
        (x 2))
    (-each-indexed (s-lines msg)
      (lambda (y line)
        (push `(ld hl ,(+ #x9800 (* (+ y 12) 32) x)) ret)
        (--each (seq-into (s-pad-right 16 " " line) 'list)
          (push `(ld a ,(- it 32)) ret)
          (push '(ld *hl+ a) ret))))
    (reverse ret)
    ))

(defun g/increment-var (addr)
  "Return assembly incrementing the value at ADDR."
  `((ld a ,addr mem)
    (inc a)
    (ld ,addr a)))

(defun g/increment-var-mask (mask addr)
  "Return assembly incrementing the value at ADDR.
The incremented value is masked with MASK."
  `((ld a ,addr mem)
    (inc a)
    (and a ,mask)
    (ld ,addr a)))

(defun g/increment-var-mod (mod addr)
  "Return assembly incrementing the value at ADDR.
The incremented value is kept strictly less than MOD."
  `((ld a ,addr mem)
    (inc a)
    (cp a ,mod)
    ,@(g/unless 'nz '((ld a 0)))
    (ld ,addr a)))

(defun g/decrement-var-mod (mod addr)
  "Return assembly decrementing the value at ADDR.
The decremented value is kept strictly less than MOD."
  `((ld a ,addr mem)
    (dec a)
    (cp a ,mod)
    ,@(g/unless 'c `((ld a ,(- mod 1))))
    (ld ,addr a)))

(defun g/if-button-pressed (button body)
  "Return assembly running BODY if BUTTON was just pressed."
  (if-let* ((bmask
             (alist-get
              button
              '((a . #b1) (b . #b10) (select . #b100) (start . #b1000)
                (right . #b10000) (left . #b100000) (up . #b1000000) (down . #b10000000)))))
      `((ld a :new-keys mem)
        (and a ,bmask)
        ,@(g/unless 'z body))
    (error "Invalid button: %s" button)))
  
(defun g/play-note-1 ()
  "Play the note in BC on channel 1."
  `((ld a #b00000011) (ldh ,u/gb/reg-sound-channel-1-dutycycle a)
    (ld a c) (ldh ,u/gb/reg-sound-channel-1-periodlo a)
    (ld a b) (and a #b111) (or a #b11000000) (ldh ,u/gb/reg-sound-channel-1-periodhi a)))

(defun g/sfx-2 (val)
  "Play VAL on channel 1."
  `((ld a #b00001111) (ldh ,u/gb/reg-sound-channel-2-dutycycle a)
    (ld a ,(logand val #xff)) (ldh ,u/gb/reg-sound-channel-2-periodlo a)
    (ld a ,(logior #b11000000 (logand (ash val -8) #b00000111))) (ldh ,u/gb/reg-sound-channel-2-periodhi a)))

;;;; Implementation
(u/gb/link
 g/symtab
 #xc000 ;; variables in RAM
 `((:audio-tick ,(u/gb/reserve 1))
   (:audio-cursor ,(u/gb/reserve 1))
   (:audio-cursor-hi ,(u/gb/reserve 1))
   (:cur-keys ,(u/gb/reserve 1))
   (:new-keys ,(u/gb/reserve 1))
   (:ui-cursor ,(u/gb/reserve 1))))

(u/gb/link
 g/symtab
 #x0150
 `((:entry-point
    (ld a #b00000001) (ldh ,u/gb/reg-interrupt a) ;; enable vblank interrupt
    (xor a a) (ldh ,u/gb/reg-interrupt-flag a) ;; clear interrupt flag
    (ei)
    (halt)
    (ld a 0) (ldh ,u/gb/reg-lcdc a) ;; turn off screen
    (jp :initialize-vram))

   (:vblank-handler
    ,@(g/increment-var :audio-tick)
    (cp a 12)
    ,@(let* ((end (+ g/music-addr (length g/music)))
             (endbytes (u/split16le end)))
        (g/unless
         'nz
         `((xor a a) (ld :audio-tick a)
           (ld hl :audio-cursor)
           (ld a *hl+) (ld c a)
           (ld a *hl+) (ld b a)
           (ld h b) (ld l c)
           (ld a *hl+) (ld c a)
           (ld a *hl+) (ld b a)
           (ld a l) (ld :audio-cursor a)
           (ld a h) (ld :audio-cursor-hi a)
           (ld a b)
           (or a c)
           ,@(g/unless
              'z
              (g/play-note-1))
           (ld a l) (xor a ,(car endbytes)) (ld b a)
           (ld a h) (xor a ,(cadr endbytes))
           (or a b)
           ,@(g/unless 'nz '((call :reset-music)))
           )))
    (pop hl)
    (pop de)
    (pop bc)
    (pop af)
    (reti))
   (:ret ;; function
    (ret))
   (:update-keys-onenibble ;; function
    (ldh ,u/gb/reg-input a)
    (call :ret)
    (ldh a ,u/gb/reg-input)
    (ldh a ,u/gb/reg-input)
    (ldh a ,u/gb/reg-input)
    (or a #xf0)
    (ret))
   (:update-keys ;; function
    (ld a #b00010000)
    (call :update-keys-onenibble)
    (ld b a)
    (ld a #b00100000)
    (call :update-keys-onenibble)
    (swap a)
    (xor a b)
    (ld b a)
    (ld a #b00110000)
    (ldh ,u/gb/reg-input a)
    (ld a :cur-keys mem)
    (xor a b)
    (and a b)
    (ld :new-keys a)
    (ld a b)
    (ld :cur-keys a)
    (ret))
   (:reset-music ;; function
    (ld hl ,g/music-addr)
    (ld a l)
    (ld :audio-cursor a)
    (ld a h)
    (ld :audio-cursor-hi a)
    (ret))
   (:update-cursor-0 ;; function
    ,@(g/write-cursor 0)
    ,@(g/write-cursor 1 t)
    ,@(g/write-cursor 2 t)
    (ret))
   (:update-cursor-1 ;; function
    ,@(g/write-cursor 0 t)
    ,@(g/write-cursor 1)
    ,@(g/write-cursor 2 t)
    (ret))
   (:update-cursor-2 ;; function
    ,@(g/write-cursor 0 t)
    ,@(g/write-cursor 1 t)
    ,@(g/write-cursor 2)
    (ret))
   (:update-cursor ;; function
    ;; update the tilemap to reflect the UI cursor
    (ld a :ui-cursor mem) (ld b a)
    (xor a #b0) ,@(g/unless 'nz '((call :update-cursor-0)))
    (ld a b) (xor a #b1) ,@(g/unless 'nz '((call :update-cursor-1)))
    (ld a b) (xor a #b10) ,@(g/unless 'nz '((call :update-cursor-2)))
    (ret))
   (:clear-battle-text ;; function
    ,@(g/memset 0 (+ #x9800 (* 12 32) 2) 16)
    ,@(g/memset 0 (+ #x9800 (* 13 32) 2) 16)
    ,@(g/memset 0 (+ #x9800 (* 14 32) 2) 16)
    ,@(g/memset 0 (+ #x9800 (* 15 32) 2) 16)
    (ret))
   (:write-battle-text-from ;; function
    (ld b h) (ld c l) ;; string address
    (ld de ,(+ #x9800 (* 12 32) 2)) ;; tilemap address
    ,@(g/while
       'nz
       `((ld h b) (ld l c) (ld a *hl+) (ld b h) (ld c l) ;; increment string address and load char to a
         (cp a 0) ;; compare a to 0
       (push af)
       ,@(g/unless ;; if the character is nonzero
          'z
          `((ld l a)
            (xor a ,?\n)
            ,@(g/ite ;; if the character is a newline
               'z
               `((ld hl 32) (add hl de) (ld d h) (ld e l) ;; go one tile down in the tilemap (without writing)
                 (ld a e) (and a #b11100000) (inc a) (inc a) (ld e a) ;; reset x coordinate to 2
                 )
               `((ld a l) (sub a 32) ;; otherwise subtract base character to find tile index
                 (halt) (nop) ;; wait for vblank
                 (ld h d) (ld l e) (ld *hl+ a) (ld d h) (ld e l) ;; increment tilemap address and write tile index
                 ))))
       (pop af)))
    (ret))
   (:random-jest ;; function - expects entropy in b
    (ld hl :string-jest0)
    (ret))
   (:random-greet ;; function - expects entropy in b
    (ld a b) (and a #b11)
    (cp a #b00) ,@(g/unless 'nz '((ld hl :string-greet0)))
    (cp a #b01) ,@(g/unless 'nz '((ld hl :string-greet1)))
    (cp a #b10) ,@(g/unless 'nz '((ld hl :string-greet2)))
    (cp a #b11) ,@(g/unless 'nz '((ld hl :string-greet3)))
    (ret))
   (:random-battle ;; function - expects entropy in b
    (ld a b) (and a #b111)
    (cp a #b000) ,@(g/unless 'nz '((ld hl :string-battle0)))
    (cp a #b001) ,@(g/unless 'nz '((ld hl :string-battle1)))
    (cp a #b010) ,@(g/unless 'nz '((ld hl :string-battle2)))
    (cp a #b011) ,@(g/unless 'nz '((ld hl :string-battle3)))
    (cp a #b100) ,@(g/unless 'nz '((ld hl :string-battle4)))
    (cp a #b101) ,@(g/unless 'nz '((ld hl :string-battle5)))
    (cp a #b110) ,@(g/unless 'nz '((ld hl :string-battle6)))
    (cp a #b111) ,@(g/unless 'nz '((ld hl :string-battle7)))
    (ret))

   ;; strings
   (:string-jest0 . "imagine this is\na joke\nhaha\nha...\0")

   (:string-greet0 . "hello computer!\0")
   (:string-greet1 . "good morning!\0")
   (:string-greet2 . "welcome in!\0")
   (:string-greet3 . "what's up?\0")

   (:string-battle0 . "it hits!\nyou take 1 DMG\nouch!\0")
   (:string-battle1 . "you hit!\nit takes 1 DMG\n:(\0")
   (:string-battle2 . "you miss...\0")
   (:string-battle3 . "it misses...\0")
   (:string-battle4 . "it menaces with\nspikes of\nbread\0")
   (:string-battle5 . "you win!\0")
   (:string-battle6 . "you lose...\0")
   (:string-battle7 . "huh?\nwhat was that?\0")

   (:initialize-vram
    ,@(g/memcpy g/font-addr #x8000 (+ (length g/font-tiles) (length g/ui-tiles) (length (car g/battle-tiledata))))
    ,@(g/memcpy g/battle-tilemap-addr #x9800 (length (cdr g/battle-tiledata))))

   (:write-initial-message
    ,@(g/write-battle-text "this is hardly\na game i'm sorry\nit was fun to\nmake though! :)")
    )

   (:initialize-system
    (ld a #b10010001) (ldh ,u/gb/reg-lcdc a) ;; re-enable screen
    (ld a #b11100100) (ldh ,u/gb/reg-bgp a)
    (ld a #b10000000) (ldh ,u/gb/reg-sound a) ;; enable sound
    (ld a #b01110111) (ldh ,u/gb/reg-sound-volume a) ;; max volume
    (ld a #b00000000) (ldh ,u/gb/reg-sound-channel-1-sweep a) ;; no period sweep
    (ld a #b11111000) (ldh ,u/gb/reg-sound-channel-1-envelope a) ;; envelope

    (ld a #b00000000) (ldh ,u/gb/reg-sound-channel-2-dutycycle a) ;; duty cycle
    (ld a #b01111000) (ldh ,u/gb/reg-sound-channel-2-envelope a) ;; envelope
    )

   (:initialize-variables
    (ld a 0) (ld :audio-tick a)
    (call :reset-music)
    (halt) (nop)
    (ld a 0) (ld :ui-cursor a)
    (call :update-cursor)
    )

   (:main-loop
    (halt) (nop) ;; wait for vblank and run interrupt
    (call :update-keys)
    ,@(g/if-button-pressed
       'up
       `(,@(g/sfx-2 (u/gb/freq-to-period 138.59))
         ,@(g/decrement-var-mod 3 :ui-cursor)
         (call :update-cursor)))
    ,@(g/if-button-pressed
       'down
       `(,@(g/sfx-2 (u/gb/freq-to-period 138.59))
         ,@(g/increment-var-mod 3 :ui-cursor)
         (call :update-cursor)))
    ,@(g/if-button-pressed
       'a
       `(,@(g/sfx-2 (u/gb/freq-to-period 783.99))
         (call :clear-battle-text)
         (ld a :audio-tick mem) ;; we use this for "randomness"
         (ld b a)
         (ld a :ui-cursor mem)
         (ld c a)
         (cp a 0) ,@(g/unless 'nz '((call :random-jest)))
         (ld a c) (cp a 1) ,@(g/unless 'nz '((call :random-greet)))
         (ld a c) (cp a 2) ,@(g/unless 'nz '((call :random-battle)))
         (call :write-battle-text-from)
         ))
    (jp :main-loop))))

(u/gb/link
 g/symtab-interrupts
 #x0040 ;; vblank interrupt
 `((:vblank-interrupt
    (push af)
    (push bc)
    (push de)
    (push hl)
    (jp ,(u/symtab-entry-addr (ht-get g/symtab :vblank-handler))))))

(--each (ht-values g/symtab)
  (let ((addr (u/symtab-entry-addr it)))
    (when (< addr g/mem-length)
      (u/write! g/mem addr (u/symtab-entry-code it)))))

(--each (ht-values g/symtab-interrupts)
  (let ((addr (u/symtab-entry-addr it)))
    (if (>= addr g/mem-length)
        (error "Interrupt handler outside early range")
      (u/write! g/mem-early addr (u/symtab-entry-code it)))))

(defconst g/rom-with-header
 (u/pad-to
  g/mem-length
  (u/gb/prepend-header
   (seq-into g/mem-early 'list)
   (-drop g/mem-early-length (seq-into g/mem 'list)))))

(f-write-bytes (apply #'unibyte-string g/rom-with-header) "game.gb")

(provide 'game)
;;; game.el ends here
