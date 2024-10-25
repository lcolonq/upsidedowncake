;;; gbatest --- Game Boy Advance testing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(add-to-list 'load-path (f-canonical "../.."))
(require 'udc)

(defconst
  g/palette
  (u/gba/make-palette
   :colors
   '((0 0 0)
     (63 63 63)
     (127 127 127)
     (191 191 191)
     (255 255 255)
     (63 0 0)
     (127 0 0)
     (255 0 0)
     (0 63 0)
     (0 127 0)
     (0 255 0)
     (0 0 63)
     (0 0 127)
     (0 0 255))))

(defconst g/image (u/gba/image-tiles (u/gba/image-quantize-palette g/palette) (u/load-image-ff "~/src/upsidedowncake/assets/test.ff")))

(defconst g/symtab (u/make-symtab))

(u/symtab-add-section! g/symtab :header u/gba/rom-start)
(u/symtab-add-section! g/symtab :code (+ u/gba/rom-start #x1000))
(u/symtab-add-section! g/symtab :data (+ u/gba/rom-start #x20000))
(u/symtab-add-section! g/symtab :ioram #x4000000)

(u/symtab-add-entry! g/symtab :reg-dispcnt (u/make-symtab-entry :addr #x4000000 :type 'var :data 4))
(u/symtab-add-entry! g/symtab :reg-bg0cnt (u/make-symtab-entry :addr #x4000008 :type 'var :data 2))
(u/symtab-add-entry! g/symtab :vram (u/make-symtab-entry :addr #x6000000 :type 'var :data (* 96 1024)))
(u/symtab-add-entry! g/symtab :vram-screenblock8 (u/make-symtab-entry :addr #x6004000 :type 'var :data 2048))
(u/symtab-add-entry!
 g/symtab :palette-bg
 (u/make-symtab-entry :addr #x5000000 :type 'var :data 512))

(u/symtab-add!
 g/symtab :data :data-palette
 'bytes
 (u/gba/palette-bytes g/palette))

(u/symtab-add!
 g/symtab :data :data-tiles
 'bytes
 (--mapcat (u/gba/tile-s-bytes (cdr it)) g/image))

(u/symtab-add!
 g/symtab
 :header :header 'const
 (u/gba/header
  (u/gba/make-header :entry :main :title "gbatest" :code "test" :maker "lq")))

(u/symtab-add!
 g/symtab
 :code :main 'code
 `(
   ;;,@(u/gba/addr 'r0 g/symtab :reg-dispcnt)
   ;;,@(u/gba/constant 'r1 #x0403)
   ;;(str r1 r0)
   ;; ,@(u/gba/write-pixel 80 80 0 31 0)
   ;; ,@(u/gba/write-pixel 82 80 0 31 0)
   ;; ,@(u/gba/write-pixel 79 82 31 31 31)
   ;; ,@(u/gba/write-pixel 80 83 31 31 31)
   ;; ,@(u/gba/write-pixel 81 83 31 31 31)
   ;; ,@(u/gba/write-pixel 82 83 31 31 31)
   ;; ,@(u/gba/write-pixel 83 82 31 31 31)
   ;; (mov r1 10)
   ;; (bl :fact)

   (mov r1 8)
   ,@(u/gba/addr 'r2 g/symtab :data-palette)
   ,@(u/gba/addr 'r3 g/symtab :palette-bg)
   (bl :wordcopy)

   (mov r1 32)
   ,@(u/gba/addr 'r2 g/symtab :data-tiles)
   ,@(u/gba/addr 'r3 g/symtab :vram)
   (bl :wordcopy)

   ;; write some colors to background palette
   ;; ,@(u/gba/addr 'r0 g/symtab :palette-bg)
   ;; ,@(u/gba/constant 'r1 #b00000000000111110000001111100000)
   ;; (str r1 r0)

   ;; write some tile data to charblock
   ;; ,@(u/gba/addr 'r0 g/symtab :vram)
   ;; (mov r1 #b00000000000000000000000000010000)
   ;; (str r1 r0)

   ;; write tile index to screenblock
   ,@(u/gba/addr 'r0 g/symtab :vram-screenblock8)
   ,@(u/gba/constant 'r1 #x00010000)
   (str r1 r0)
   ,@(u/gba/constant 'r1 #x00030002)
   (str r1 r0 64)

   ;; set video mode
   ,@(u/gba/addr 'r0 g/symtab :reg-dispcnt)
   ,@(u/gba/constant 'r1 #x0100) ;; turn on BG0, mode 0
   (str r1 r0)

   ;; set BG0 control flags
   ,@(u/gba/addr 'r0 g/symtab :reg-bg0cnt)
   ,@(u/gba/constant 'r1 #b0000100000000000)
   (str r1 r0)

   (b :main)))

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
 :code :fact 'code
 (u/gba/gen
  (u/gba/function-header)
  (u/gba/push 'r1)
  (u/gba/emit!
   '(mov r0 1)
   '(sub s r1 r1 1)
   '(bl ne :fact))
  (u/gba/pop 'r1)
  (u/gba/emit!
   '(mul r0 r0 r1))
  (u/gba/function-footer)
  ))

(defconst g/linked (u/gba/link g/symtab u/gba/rom-start #x25000))
(defconst g/rom
  (seq-mapcat
   #'byte-to-string
   g/linked
   'string))

(f-write-bytes g/rom "test.gba")

(provide 'gbatest)
;;; gbatest.el ends here
