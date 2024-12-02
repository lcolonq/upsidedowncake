;;; cave --- A simple Game Boy Advance game -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(add-to-list 'load-path (f-canonical "../.."))
(add-to-list 'load-path (f-canonical "../../src"))
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc)

(defconst g/symtab (u/make-symtab :alignment 4))

;;;; Symbol table setup and platform constants
(u/symtab-add-section! g/symtab :header u/gba/rom-start)
(u/symtab-add-section! g/symtab :code (+ u/gba/rom-start #x1000))
(u/symtab-add-section! g/symtab :data (+ u/gba/rom-start #x20000))
(u/symtab-add-section! g/symtab :vars #x02000000)
(u/symtab-add-section! g/symtab :ioram #x04000000)

(u/symtab-add-entry! g/symtab :reg-ifbios (u/make-symtab-entry :addr #x03007ff8 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-intaddr (u/make-symtab-entry :addr #x03007ffc :type 'var :data 4))

(u/symtab-add-entry! g/symtab :reg-dispcnt (u/make-symtab-entry :addr #x04000000 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-dispstat (u/make-symtab-entry :addr #x04000004 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-bg0cnt (u/make-symtab-entry :addr #x04000008 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-bg1cnt (u/make-symtab-entry :addr #x0400000a :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-keyinput (u/make-symtab-entry :addr #x04000130 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-ie (u/make-symtab-entry :addr #x04000200 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-if (u/make-symtab-entry :addr #x04000202 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :reg-ime (u/make-symtab-entry :addr #x04000208 :type 'var :data 1))
(u/symtab-add-entry! g/symtab :palette-bg (u/make-symtab-entry :addr #x05000000 :type 'var :data 512))
(u/symtab-add-entry! g/symtab :palette-sprite (u/make-symtab-entry :addr #x05000200 :type 'var :data 512))

(u/symtab-add-entry! g/symtab :vram-bg (u/make-symtab-entry :addr #x06000000 :type 'var :data (* 64 1024)))
(u/symtab-add-entry! g/symtab :vram-bg-charblock0 (u/make-symtab-entry :addr #x06000000 :type 'var :data 16384))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock0  (u/make-symtab-entry :addr #x06000000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock1  (u/make-symtab-entry :addr #x06000800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock2  (u/make-symtab-entry :addr #x06001000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock3  (u/make-symtab-entry :addr #x06001800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock4  (u/make-symtab-entry :addr #x06002000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock5  (u/make-symtab-entry :addr #x06002800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock6  (u/make-symtab-entry :addr #x06003000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock7  (u/make-symtab-entry :addr #x06003800 :type 'var :data 2048))

(u/symtab-add-entry! g/symtab :vram-bg-charblock1  (u/make-symtab-entry :addr #x06004000 :type 'var :data 16384))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock8  (u/make-symtab-entry :addr #x06004000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock9  (u/make-symtab-entry :addr #x06004800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock10 (u/make-symtab-entry :addr #x06005000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock11 (u/make-symtab-entry :addr #x06005800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock12 (u/make-symtab-entry :addr #x06006000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock13 (u/make-symtab-entry :addr #x06006800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock14 (u/make-symtab-entry :addr #x06007000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock15 (u/make-symtab-entry :addr #x06007800 :type 'var :data 2048))

(u/symtab-add-entry! g/symtab :vram-bg-charblock2  (u/make-symtab-entry :addr #x06008000 :type 'var :data 16384))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock16 (u/make-symtab-entry :addr #x06008000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock17 (u/make-symtab-entry :addr #x06008800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock18 (u/make-symtab-entry :addr #x06009000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock19 (u/make-symtab-entry :addr #x06009800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock20 (u/make-symtab-entry :addr #x0600a000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock21 (u/make-symtab-entry :addr #x0600a800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock22 (u/make-symtab-entry :addr #x0600b000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock23 (u/make-symtab-entry :addr #x0600b800 :type 'var :data 2048))

(u/symtab-add-entry! g/symtab :vram-bg-charblock3  (u/make-symtab-entry :addr #x0600c000 :type 'var :data 16384))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock24 (u/make-symtab-entry :addr #x0600c000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock25 (u/make-symtab-entry :addr #x0600c800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock26 (u/make-symtab-entry :addr #x0600d000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock27 (u/make-symtab-entry :addr #x0600d800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock28 (u/make-symtab-entry :addr #x0600e000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock29 (u/make-symtab-entry :addr #x0600e800 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock30 (u/make-symtab-entry :addr #x0600f000 :type 'var :data 2048))
(u/symtab-add-entry! g/symtab :vram-bg-screenblock31 (u/make-symtab-entry :addr #x0600f800 :type 'var :data 2048))

(u/symtab-add-entry! g/symtab :vram-sprite (u/make-symtab-entry :addr #x06010000 :type 'var :data (* 32 1024)))
(u/symtab-add-entry! g/symtab :vram-sprite-charblock0 (u/make-symtab-entry :addr #x06010000 :type 'var :data 16384))

(u/symtab-add-entry! g/symtab :oam (u/make-symtab-entry :addr #x07000000 :type 'var :data 1024))

;;;; Data structures
(defconst g/map-dim 20)
(defconst g/player-speed 1)
(defconst g/player-jump-height 10)
(defconst
  g/struct-playerdata
  (u/struct
   1
   :x 1
   :y 1
   :pad 1
   :jump 1
   :vx 4 ;; current x movement offset (since this can't change during the ascent of a jump)
   ))
(defconst
  g/struct-room-layout
  (u/struct
   4
   :terrain (* g/map-dim g/map-dim)))

(defconst
  g/struct-room
  (u/struct
   4
   :layout g/struct-room-layout))

(defun g/tile-from-char (c)
  "Convert the character C into a tile index."
  (cl-case c
    (?. 0)
    (?# 2)
    (?@ 3)
    (t 0)))

(defun g/room-from-string (str)
  "Convert the string STR into a room."
  (let* ((trimmed (s-join "" (s-split "\n" (s-trim str))))
         (chars (seq-into trimmed 'list))
         (clen (length chars))
         (rlen (* g/map-dim g/map-dim))
         )
    (unless (= clen rlen)
      (error "Failed to build room from string: length was %s rather than %s" clen rlen))
    (-map #'g/tile-from-char chars)))

(defconst g/room-test
  (g/room-from-string "
....................
....................
....................
....@...............
.............#......
....#........#......
....##########......
....................
......@@@...........
....................
..#....#.....#......
..#....#.....#......
..#....#.....#......
..#....#.....#......
..######.....#......
..#....#.....#......
..#....#.....#......
..#....#.....#......
..#....#.....#......
....................
"))

;;;; Assets
(defconst
  g/palette
  (u/gba/make-palette
   :colors
   '((#xff #x00 #xe7) ;; transparency
     (#x08 #x14 #x1e) ;; normal nyx8 palette
     (#x0f #x2a #x3f)
     (#x20 #x39 #x4f)
     (#xf6 #xd6 #xbd)
     (#xc3 #xa3 #x8a)
     (#x99 #x75 #x77)
     (#x81 #x62 #x71)
     (#x4e #x49 #x5f)
     (#x27 #xbf #x03) ;; special green highlight
     )))

(defconst
  g/palette-sprite
  (u/gba/make-palette
   :colors
   '((#xff #x00 #xe7) ;; transparency
     (#xff #xff #xff)
     (#x00 #x00 #x00)
     (#xff #x00 #x00)
     (#x00 #xff #x00)
     (#x00 #x00 #xff)
     (#xaf #xaf #xaf)
     (#x5a #x5a #x5a)
     (#x32 #x32 #x32)
     (#xaf #x4a #x4a)
     (#x72 #xc7 #x70)
     (#x76 #x73 #xd2)
     )))

(defvar g/image-source-ui (u/load-image-ff "./assets/ui.ff"))
(defconst g/image-ui
  (u/gba/image-tiles (u/gba/image-quantize-palette-exact g/palette) g/image-source-ui))
(defconst g/image-ui-tiledata-bytes
  (--mapcat (u/gba/tile-s-bytes it) (u/gba/image-tiledata g/image-ui)))
(defconst g/image-ui-screenblock-bytes
  (--mapcat
   (list (cdr it) 0)
   (u/gba/image-cell-indices g/image-ui)))

(defvar g/image-source-cave (u/load-image-ff "./assets/cave.ff"))
(defconst g/image-cave
  (u/gba/image-tiles (u/gba/image-quantize-palette-exact g/palette) g/image-source-cave))
(defconst g/image-cave-tiledata-bytes
  (--mapcat (u/gba/tile-s-bytes it) (u/gba/image-tiledata g/image-cave)))

(defvar g/image-source-player (u/load-image-ff "./assets/player.ff"))
(defconst g/image-player
  (u/gba/image-tiles (u/gba/image-quantize-palette-exact g/palette-sprite) g/image-source-player))
(defconst g/image-player-tiledata-bytes
  (--mapcat (u/gba/tile-s-bytes it) (u/gba/image-tiledata g/image-player)))

(defconst g/image-sprite-tiledata-bytes
  (-concat
   g/image-player-tiledata-bytes))

;;;; Constants in ROM
(u/symtab-add! g/symtab :data :data-palette 'bytes (u/gba/palette-bytes g/palette))
(u/symtab-add! g/symtab :data :data-palette-sprite 'bytes (u/gba/palette-bytes g/palette-sprite))
(u/symtab-add! g/symtab :data :data-tiles-ui 'bytes g/image-ui-tiledata-bytes)
(u/symtab-add! g/symtab :data :data-tiles-cave 'bytes g/image-cave-tiledata-bytes)
(u/symtab-add! g/symtab :data :data-tiles-sprite 'bytes g/image-sprite-tiledata-bytes)
(u/symtab-add! g/symtab :data :data-screenblock-ui 'bytes g/image-ui-screenblock-bytes)
(u/symtab-add! g/symtab :data :data-rooms 'bytes g/room-test)

;;;; Variables in RAM
(u/symtab-add! g/symtab :vars :var-test0 'var 4)
(u/symtab-add! g/symtab :vars :var-test1 'var 2)
(u/symtab-add! g/symtab :vars :var-test2 'var 1)
(u/symtab-add! g/symtab :vars :var-player 'var (u/sizeof g/struct-playerdata))
(u/symtab-add! g/symtab :vars :var-current-room 'var u/gba/size-word) ;; pointer to current room

;;;; Code
(u/symtab-add!
 g/symtab
 :header :header 'const
 (u/gba/header
  (u/gba/make-header :entry :main :title "gbatest" :code "test" :maker "lq")))

(u/symtab-add!
 g/symtab
 :code :interrupt-handler 'code
 (u/gba/gen
  (u/gba/emit!
   '(mov r0 1) ;; vblank
   (u/gba/set16 g/symtab :reg-if 'r0)
   (u/gba/get16 g/symtab 'r1 :reg-ifbios)
   '(orr r0 r0 r1)
   (u/gba/set16 g/symtab :reg-ifbios 'r0)
   `(bx ,u/gba/lr))))

(u/symtab-add!
 g/symtab
 :code :main 'code
 (u/gba/gen
  (u/gba/emit!
   '(bl :hide-all-sprites)

   (u/gba/set32 g/symtab :var-test0 #xdeadbeef)
   (u/gba/set16 g/symtab :var-test1 #xcafe)
   (u/gba/set8 g/symtab :var-test2 #x42)
   (u/gba/set8 g/symtab '(:var-test2 . 1) #x42)
   (u/gba/set8 g/symtab '(:var-test2 . 4) #x42)
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :x)) #xbb)
   '(mov r1 #xdd)
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :y)) 'r1)
   (u/gba/set32 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :big)) #xfefebabe)

   ;; load palette into background palette
   '(mov r1 8)
   (u/gba/addr 'r2 g/symtab :data-palette)
   (u/gba/addr 'r3 g/symtab :palette-bg)
   '(bl :wordcopy)

   ;; load palette into sprite palette
   '(mov r1 8)
   (u/gba/addr 'r2 g/symtab :data-palette-sprite)
   (u/gba/addr 'r3 g/symtab :palette-sprite)
   '(bl :wordcopy)

   ;; load ui tile data into background charblock 0
   `(mov r1 ,(/ (length g/image-ui-tiledata-bytes) u/gba/size-word))
   (u/gba/addr 'r2 g/symtab :data-tiles-ui)
   (u/gba/addr 'r3 g/symtab :vram-bg-charblock0)
   '(bl :wordcopy)

   ;; load terrain tile data into background charblock 1
   `(mov r1 ,(/ (length g/image-cave-tiledata-bytes) u/gba/size-word))
   (u/gba/addr 'r2 g/symtab :data-tiles-cave)
   (u/gba/addr 'r3 g/symtab :vram-bg-charblock1)
   '(bl :wordcopy)

   ;; write ui tile indices to screenblock 30
   (u/gba/constant 'r1 (/ (length g/image-ui-screenblock-bytes) u/gba/size-word))
   (u/gba/addr 'r2 g/symtab :data-screenblock-ui)
   (u/gba/addr 'r3 g/symtab :vram-bg-screenblock30)
   '(bl :wordcopy)

   ;; load sprite tile data into sprite charblock 0
   `(mov r1 ,(/ (length g/image-sprite-tiledata-bytes) u/gba/size-word))
   (u/gba/addr 'r2 g/symtab :data-tiles-sprite)
   (u/gba/addr 'r3 g/symtab :vram-sprite-charblock0)
   '(bl :wordcopy)

   ;; set video mode
   (u/gba/set16 g/symtab :reg-dispcnt #b0001001101000000) ;; turn on BG0, BG1, and sprites, mode 0
   ;; set BG0 control flags to render background starting at screenblock 30 from charblock 0
   (u/gba/set16 g/symtab :reg-bg0cnt #b0001111000000000)
   ;; set BG1 control flags to render background starting at screenblock 31 from charblock 1
   (u/gba/set16 g/symtab :reg-bg1cnt #b0001111100000101)

   (u/gba/set16 g/symtab '(:vram-bg-screenblock31 . 12) 1)
   (u/gba/set16 g/symtab '(:vram-bg-screenblock31 . 16) 2)
   (u/gba/set16 g/symtab '(:vram-bg-screenblock31 . 24) 2)

   (u/gba/addr 'r1 g/symtab :data-rooms)
   (u/gba/set32 g/symtab :var-current-room 'r1)

   '(bl :render-room) ;; called when moving to a new room to populate the background

   '(b :enable-interrupts))))

(u/symtab-add!
 g/symtab
 :code :enable-interrupts 'code
 (u/gba/gen
  (u/gba/emit!
   (u/gba/set32 g/symtab :reg-intaddr (u/symtab-entry-addr (u/symtab-lookup g/symtab :interrupt-handler)))
   (u/gba/set16 g/symtab :reg-dispstat #b0000000000001000) ;; turn on vblank interrupt
   (u/gba/set16 g/symtab :reg-ie #b0000000000000001) ;; only enable vblank interrupt
   (u/gba/set32 g/symtab :reg-ime 1) ;; enable interrupts

   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :x)) 32)
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :y)) 0)
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :jump)) 0)
   (u/gba/set32 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :vx)) 0)
   '(mov r7 0)
   '(b :mainloop))))

(u/symtab-add!
 g/symtab
 :code :mainloop 'code
 (u/gba/gen
  (u/gba/emit!
   ;; update state
   '(bl :update-player)

   `(swi ,(lsh #x05 16)) ;; VBlankIntrWait BIOS function, remember to shift in ARM!

   ;; "render" / update vram
   '(bl :render-player)

   '(b :mainloop))))

(u/symtab-add!
 g/symtab
 :code :wordcopy 'code
 (u/gba/gen ;; len in r1, src in r2, dst in r3
  (u/gba/function-header)
  (u/gba/push 'r4)
  (u/gba/emit!
   :begin
   '(sub s r1 r1 1)
   '(b lt :end)
   '(ldr post wb r4 r2 4)
   '(str post wb r4 r3 4)
   '(b :begin)
   :end)
  (u/gba/pop 'r4)
  (u/gba/function-footer)))

(u/symtab-add!
 g/symtab
 :code :hide-all-sprites 'code
 (u/gba/gen
  (u/gba/function-header)
  (u/gba/emit!
   (u/gba/addr 'r0 g/symtab :oam)
   '(mov r1 r0)
   '(add r0 r0 1 11) ;; + 1024
   :begin
   (u/gba/set16 g/symtab 'r1 #b0000001000000000)
   '(add r1 r1 8)
   '(cmp s r0 r1 r0)
   '(b lt :begin))
  (u/gba/function-footer)))

(u/symtab-add!
 g/symtab
 :code :update-player 'code
 (u/gba/gen
  (u/gba/function-header)
  (u/gba/emit!
   '(mov r0 0) '(mov r1 0)
   (u/gba/get8 g/symtab 'r6 `(:var-player . ,(u/offsetof g/struct-playerdata :x)))
   (u/gba/get8 g/symtab 'r7 `(:var-player . ,(u/offsetof g/struct-playerdata :y)))

   '(mov r10 1)
   '(add r0 r6 0) '(add r1 r7 16) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :post-grounded)
   '(add r0 r6 7) '(add r1 r7 16) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :post-grounded)
   '(mov r10 0) ;; set r10 to nonzero if we are grounded
   :post-grounded

   (u/gba/get8 g/symtab 'r0 `(:var-player . ,(u/offsetof g/struct-playerdata :jump)))

   '(mov r8 0) '(mov r9 0) ;; x and y offsets
   `(cmp s r0 r0 0)
   '(b ne :skip-input) ;; move/jump only if we're not already in the ascent of the jump
   (u/gba/button-pressed? g/symtab 'left `(sub r8 r8 ,g/player-speed)) ;; move left if holding left
   (u/gba/button-pressed? g/symtab 'right `(add r8 r8 ,g/player-speed)) ;; move right if holding right
   (u/gba/set32 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :vx)) 'r8) ;; save this, so when we enter the ascent it stays the same
   `(cmp s r0 r10 0)
   '(b eq :skip-input)
   (u/gba/button-pressed? ;; jump if we're pressing up, only if we're on the ground (didn't move vertically last frame)
    g/symtab 'up
    (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :jump)) g/player-jump-height))
   :skip-input
   (u/gba/get32 g/symtab 'r8 `(:var-player . ,(u/offsetof g/struct-playerdata :vx))) ;; load saved horizontal offset

   (u/gba/get8 g/symtab 'r0 `(:var-player . ,(u/offsetof g/struct-playerdata :jump)))
   '(cmp s r0 r0 0) ;; if we're jumping
   '(b eq :done-jumping)
   '(sub r9 r9 4) ;; move up instead of down
   '(sub r0 r0 1) ;; decrement the jump counter
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :jump)) 'r0)
   :done-jumping
   '(add r9 r9 2) ;; by default, move down
   
   ;; actually apply the computed offsets
   '(add r7 r7 r9)
   '(bl :player-colliding) '(cmp s r0 r2 0) '(b ne :skip-y-movement)
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :y)) 'r7)
   '(b :after-y-movement)
   :skip-y-movement ;; if we collided with something during vertical movement, we aren't jumping anymore
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :jump)) '0)
   :after-y-movement

   (u/gba/get8 g/symtab 'r7 `(:var-player . ,(u/offsetof g/struct-playerdata :y))) ;; reset r7 to the previous value during horizontal movement
   '(add r6 r6 r8)
   '(bl :player-colliding) '(cmp s r0 r2 0) '(b ne :skip-x-movement)
   (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :x)) 'r6)
   '(b :after-x-movement)
   :skip-x-movement ;; if we collided with something during horizontal movement, we also aren't jumping anymore
   ;; (u/gba/set8 g/symtab `(:var-player . ,(u/offsetof g/struct-playerdata :jump)) '0)
   :after-x-movement
   )
  (u/gba/function-footer)))

(u/symtab-add!
 g/symtab
 :code :player-colliding 'code
 (u/gba/gen ;; x in r6, y in r7. r2 will be nonzero if the point is colliding with anything
  (u/gba/function-header)
  (u/gba/push 'r0 'r1)
  (u/gba/emit!
   '(mov r0 0) '(mov r1 0)
   '(add r0 r6 0) '(add r1 r7 0) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :colliding)
   '(add r0 r6 7) '(add r1 r7 0) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :colliding)
   '(add r0 r6 0) '(add r1 r7 7) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :colliding)
   '(add r0 r6 7) '(add r1 r7 7) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :colliding)
   '(add r0 r6 0) '(add r1 r7 15) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :colliding)
   '(add r0 r6 7) '(add r1 r7 15) '(bl :point-colliding) '(cmp s r0 r2 0) '(b ne :colliding)
   '(mov r2 0)
   '(b :end))
  (u/gba/emit!
   :colliding
   '(mov r2 1)
   :end)
  (u/gba/pop 'r0 'r1)
  (u/gba/function-footer)))

(u/symtab-add!
 g/symtab
 :code :point-oob 'code
 (u/gba/gen ;; x in r0, y in r1. r2 will be nonzero if the point is out of bounds
  (u/gba/function-header)
  (u/gba/emit!
   ;; bounds check
   '(cmp s r0 r0 160)
   '(b cs :end-collision)
   '(cmp s r0 r1 160)
   '(b cs :end-collision)
   '(mov r2 0)
   '(b eq :end)
   :end-collision
   '(mov r2 1)
   :end)
  (u/gba/function-footer)))

(u/symtab-add!
 g/symtab
 :code :point-colliding 'code
 (u/gba/gen ;; x in r0, y in r1. r2 will be nonzero if the point is colliding with anything
  (u/gba/function-header)
  (u/gba/push 'r4 'r5 'r6 'r7 'r8 'r9 'r10)
  (u/gba/emit!
   (u/gba/get32 g/symtab 'r8 :var-current-room) ;; r8 holds room address
   `(add r8 r8 ,(u/offsetof g/struct-room :layout)) ;; r8 holds layout address
   `(add r8 r8 ,(u/offsetof g/struct-room-layout :terrain)) ;; r8 holds terrain address

   '(mov r7 r0 (lsr 3)) ;; x tile coordinate
   '(mov r6 r1 (lsr 3)) ;; y tile coordinate
   `(mov r9 ,g/map-dim) ;; map dim
   '(mul r9 r9 r6)
   '(add r9 r9 r7) ;; tile offset
   (u/gba/get8 g/symtab 'r9 '(r8 . r9))
   '(cmp r0 r9 0)
   '(mov r2 0)
   '(b eq :end)

   :end-collision
   '(mov r2 1)
   :end)
  (u/gba/pop 'r4 'r5 'r6 'r7 'r8 'r9 'r10)
  (u/gba/function-footer)))

(u/symtab-add!
 g/symtab
 :code :render-room 'code
 (u/gba/gen
  (u/gba/function-header)
  (u/gba/emit!
   (u/gba/get32 g/symtab 'r1 :var-current-room) ;; r1 holds room address
   `(add r1 r1 ,(u/offsetof g/struct-room :layout)) ;; r1 holds layout address
   `(add r1 r1 ,(u/offsetof g/struct-room-layout :terrain)) ;; r1 holds terrain address
   '(mov r2 19) ;; y offset
   :loop-start-y
   '(mov r3 19) ;; x offset
   :loop-start-x
   `(mov r7 ,g/map-dim)
   '(mul r7 r7 r2)
   '(add r7 r7 r3)
   (u/gba/get8 g/symtab 'r8 '(r1 . r7))
   '(mov r7 32)
   '(mul r7 r7 r2)
   '(add r7 r7 r3)
   '(add r7 r7 5)
   '(mov r7 r7 (lsl 1))
   (u/gba/set16 g/symtab '(:vram-bg-screenblock31 . r7) 'r8)
   '(sub s r3 r3 1)
   '(b ge :loop-start-x)
   '(sub s r2 r2 1)
   '(b ge :loop-start-y))
  (u/gba/function-footer)))

(u/symtab-add!
 g/symtab
 :code :render-player 'code
 (u/gba/gen
  (u/gba/function-header)
  (u/gba/emit!
   (u/gba/get8 g/symtab 'r2 `(:var-player . ,(u/offsetof g/struct-playerdata :y)))
   (u/gba/get8 g/symtab 'r1 `(:var-player . ,(u/offsetof g/struct-playerdata :y)))
   (u/gba/constant 'r0 #b1000000000000000)
   '(orr r0 r0 r1)
   (u/gba/set16 g/symtab :oam 'r0)
   (u/gba/get8 g/symtab 'r1 `(:var-player . ,(u/offsetof g/struct-playerdata :x)))
   '(add r1 r1 40)
   (u/gba/set16 g/symtab '(:oam . 2) 'r1))
  (u/gba/function-footer)))

;;;; Link and emit ROM
;; populate autocomple for symbol names in debugger
(setq
 colonq/c-gdb-symbols
 (--map
  (cons (format "%s" (car it)) (format "0x%x" (u/symtab-entry-addr (cdr it))))
  (ht->alist (u/symtab-symbols g/symtab))))

(defconst g/linked (u/gba/link g/symtab u/gba/rom-start #x25000))
(defconst g/rom
  (seq-mapcat
   #'byte-to-string
   g/linked
   'string))

(f-write-bytes g/rom "cave.gba")

(provide 'cave)
;;; cave.el ends here
