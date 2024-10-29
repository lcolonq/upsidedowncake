;;; gbatest --- Game Boy Advance testing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(add-to-list 'load-path (f-canonical "../.."))
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

(u/symtab-add-entry! g/symtab :oam (u/make-symtab-entry :addr #x07000000 :type 'var :data 1024))

;;;; Data structures
(defconst
  g/struct-playerdata
  (u/struct
   1
   :x 1
   :y 1
   :hp 1
   :padding 1
   :big 4
   ))

(defconst
  g/struct-room-layout
  (u/struct
   4
   :terrain (* 20 20)))

(defconst
  g/struct-room
  (u/struct
   4
   :layout g/struct-room-layout))

;;;; Assets
(defconst
  g/palette
  (u/gba/make-palette
   :colors
   '((#xff #x00 #xe7)
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

(defvar g/image-source-cave (u/load-image-ff "./assets/cave.ff"))
(defconst g/image-cave
  (u/gba/image-tiles (u/gba/image-quantize-palette-exact g/palette) g/image-source-cave))
(defconst g/image-cave-tiledata-bytes
  (--mapcat (u/gba/tile-s-bytes it) (u/gba/image-tiledata g/image-cave)))

(defvar g/image-source-ui (u/load-image-ff "./assets/ui.ff"))
(defconst g/image-ui
  (u/gba/image-tiles (u/gba/image-quantize-palette-exact g/palette) g/image-source-ui))
(defconst g/image-ui-tiledata-bytes
  (--mapcat (u/gba/tile-s-bytes it) (u/gba/image-tiledata g/image-ui)))
(defconst g/image-ui-screenblock-bytes
  (--mapcat
   (list (cdr it) 0)
   (u/gba/image-cell-indices g/image-ui)))

;;;; Constants in ROM
(u/symtab-add! g/symtab :data :data-palette 'bytes (u/gba/palette-bytes g/palette))
(u/symtab-add! g/symtab :data :data-tiles-ui 'bytes g/image-ui-tiledata-bytes)
(u/symtab-add! g/symtab :data :data-tiles-cave 'bytes g/image-cave-tiledata-bytes)
(u/symtab-add! g/symtab :data :data-screenblock-ui 'bytes g/image-ui-screenblock-bytes)

;;;; Variables in RAM
(u/symtab-add! g/symtab :vars :var-test0 'var 4)
(u/symtab-add! g/symtab :vars :var-test1 'var 2)
(u/symtab-add! g/symtab :vars :var-test2 'var 1)
(u/symtab-add! g/symtab :vars :var-player 'var (u/sizeof g/struct-playerdata))

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
   (u/gba/addr 'r2 g/symtab :data-palette)
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

   ;; write tile index to screenblock
   (u/gba/constant 'r1 (/ (length g/image-ui-screenblock-bytes) u/gba/size-word))
   (u/gba/addr 'r2 g/symtab :data-screenblock-ui)
   (u/gba/addr 'r3 g/symtab :vram-bg-screenblock30)
   '(bl :wordcopy)

   ;; write some data to sprite charblock 0
   (u/gba/set32 g/symtab :vram-sprite #xDDDDDDDD)

   ;; set video mode
   (u/gba/set16 g/symtab :reg-dispcnt #b0001001100000000) ;; turn on BG0, BG1, and sprites, mode 0
   ;; set BG0 control flags to render background starting at screenblock 30 from charblock 0
   (u/gba/set16 g/symtab :reg-bg0cnt #b0001111000000000)
   ;; set BG1 control flags to render background starting at screenblock 31 from charblock 1
   (u/gba/set16 g/symtab :reg-bg1cnt #b0001111100000101)

   (u/gba/set16 g/symtab '(:vram-bg-screenblock31 . 12) 1)
   (u/gba/set16 g/symtab '(:vram-bg-screenblock31 . 16) 2)

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

   '(mov r7 0)
   '(b :mainloop))))

(u/symtab-add!
 g/symtab
 :code :mainloop 'code
 (u/gba/gen
  (u/gba/emit!
   `(swi ,(lsh #x05 16)) ;; VBlankIntrWait BIOS function, remember to shift in ARM!

   ;; set sprite 0 position in OAM
   '(add r7 r7 1)
   '(and r7 r7 #x3f)
   (u/gba/addr 'r0 g/symtab :oam)
   '(mov r1 r7)
   '(mov r1 r1 (lsl 16))
   '(orr r1 r1 r7)
   '(str r1 r0)

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
 :code :render-room 'code
 (u/gba/gen ;; room address in r1
  (u/gba/function-header)
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

(f-write-bytes g/rom "test.gba")

(provide 'gbatest)
;;; gbatest.el ends here
