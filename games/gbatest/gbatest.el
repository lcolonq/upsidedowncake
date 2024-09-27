;;; gbatest --- Game Boy Advance testing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(add-to-list 'load-path (f-canonical "../.."))
(require 'udc)

(defconst g/symtab (u/make-symtab))
(defconst g/rom-start #x8000000)

(u/symtab-add-section! g/symtab :header g/rom-start)
(u/symtab-add-section! g/symtab :code (+ g/rom-start #x1000))
(u/symtab-add-section! g/symtab :data (+ g/rom-start #x20000))
(u/symtab-add-section! g/symtab :ioram #x4000000)
(u/symtab-add-section! g/symtab :vram #x6000000)

(u/symtab-add-entry! g/symtab :reg-dispcnt (u/make-symtab-entry :addr #x4000000 :type 'var :data 4))
(u/symtab-add-entry! g/symtab :bitmap (u/make-symtab-entry :addr #x6000000 :type 'var :data 76800))

(u/symtab-add!
 g/symtab
 :header :header 'const
 (u/gba/header
  (u/gba/make-header :entry :main :title "gbatest" :code "test" :maker "lq")))

(u/symtab-add!
 g/symtab
 :code :main 'code
 `(,@(u/gba/constant 'r0 #x04000000)
   ,@(u/gba/constant 'r1 #x0403)
   (str r1 r0)
   ,@(u/gba/write-pixel 80 80 0 31 0)
   ,@(u/gba/write-pixel 82 80 0 31 0)
   ,@(u/gba/write-pixel 79 82 31 31 31)
   ,@(u/gba/write-pixel 80 83 31 31 31)
   ,@(u/gba/write-pixel 81 83 31 31 31)
   ,@(u/gba/write-pixel 82 83 31 31 31)
   ,@(u/gba/write-pixel 83 82 31 31 31)
   (mov r1 10)
   (bl :fact)
   (b :main)))

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

(defconst g/linked (u/gba/link g/symtab g/rom-start #x25000))
(defconst g/rom
  (seq-mapcat
   #'byte-to-string
   g/linked
   'string))

(f-write-bytes g/rom "test.gba")

(provide 'gbatest)
;;; gbatest.el ends here
