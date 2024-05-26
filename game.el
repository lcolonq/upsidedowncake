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
(defconst g/font-image (u/load-image-ff "font.ff"))
(defconst g/font-addr #x4000)
(defconst g/font-tiles (car (u/gb/image-tiles #'u/gb/image-quantize-aseprite g/font-image)))
(u/write! g/mem g/font-addr g/font-tiles)

(defconst g/battle-image (u/load-image-ff "battle.ff"))
(defconst g/battle-tiledata (u/gb/image-tiles #'u/gb/image-quantize-aseprite g/battle-image))
(defconst g/battle-tiles-addr (+ g/font-addr (length g/font-tiles)))
(u/write! g/mem g/battle-tiles-addr (car g/battle-tiledata))
(defconst g/battle-tilemap-addr #x5000)
(u/write!
 g/mem g/battle-tilemap-addr
 (--map
  (+ it (/ (length g/font-tiles) 16))
  (cdr g/battle-tiledata)))

(defconst g/music
  (u/gb/music-from-muzak
   ;; "DEFGA/dcA/D/AGFEDEFGA/GFEDEFEDCE DEFGA/dcA/D/AGFEDEFGA/GFE/F/G/A"
   "F/E/D/E/F/G//A//DDD/////C//////F/E/D/E/F/G//G//G///F///E"
   ))
(defconst g/music-addr #x6000)
(u/write! g/mem g/music-addr g/music)

;;;; Utility functions
(defun g/unless (cond body)
  "Return assembly to only run BODY if COND is not set."
  `((jr ,cond ,(u/gb/assembly-length body))
    ,@body))

(defun g/while (cond body)
  "Return assembly to repeat BODY while COND is set."
  `(,@body
    (jr ,cond ,(- (+ (u/gb/assembly-length body) 1)))))

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
        (--each (seq-into line 'list)
          (push `(ld a ,(- it 32)) ret)
          (push '(ld *hl+ a) ret))))
    (reverse ret)
    ))

(defun g/increment-var (addr)
  "Return assembly incrementing the value at ADDR."
  `((ld a ,addr mem)
    (inc a)
    (ld ,addr a)))

(defun g/trigger-1 ()
  "Trigger channel 1."
  `((ld a #b11000000) (ldh ,u/gb/reg-sound-channel-1-periodhi a)))

(defun g/play-note-1 ()
  "Play the note in BC on channel 1."
  `((ld a c) (ldh ,u/gb/reg-sound-channel-1-periodlo a)
    (ld a b) (and a #b10000111) (ldh ,u/gb/reg-sound-channel-1-periodhi a)))

(defun g/test-note-1 (val)
  "Play VAL on channel 1."
  `((ld a ,(logand val #xff)) (ldh ,u/gb/reg-sound-channel-1-periodlo a)
    (ld a ,(logand (ash val -8) #b111)) (ldh ,u/gb/reg-sound-channel-1-periodhi a)))

;;;; Implementation
(u/gb/link
 g/symtab
 #xc000 ;; variables in RAM
 `((:audio-tick ,(u/gb/reserve 1))
   (:audio-cursor ,(u/gb/reserve 1))
   (:audio-cursor-hi ,(u/gb/reserve 1))
   (:cur-keys ,(u/gb/reserve 1))
   (:new-keys ,(u/gb/reserve 1))))

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

   (:initialize-vram
    ,@(g/memcpy g/font-addr #x9000 (+ (length g/font-tiles) (length (car g/battle-tiledata))))
    ,@(g/memcpy g/battle-tilemap-addr #x9800 (length (cdr g/battle-tiledata))))

   (:write-test-message
    ,@(g/write-battle-text "hello computer\ntest"))

   (:initialize-system
    (ld a #b10000001) (ldh ,u/gb/reg-lcdc a) ;; re-enable screen
    (ld a #b11100100) (ldh ,u/gb/reg-bgp a)
    (ld a #b10000000) (ldh ,u/gb/reg-sound a) ;; enable sound
    (ld a #b01110111) (ldh ,u/gb/reg-sound-volume a) ;; max volume
    (ld a #b00000000) (ldh ,u/gb/reg-sound-channel-1-sweep a) ;; no period sweep
    (ld a #b00000111) (ldh ,u/gb/reg-sound-channel-1-dutycycle a) ;; duty cycle
    (ld a #b11111000) (ldh ,u/gb/reg-sound-channel-1-envelope a) ;; envelope
    ,@(g/trigger-1) ;; start playing channel 1
    )

   (:initialize-variables
    (ld a 0) (ld :audio-tick a)
    (call :reset-music))

   (:main-loop
    (halt) (nop) ;; wait for vblank and run interrupt
    ;; (call :update-keys)
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
