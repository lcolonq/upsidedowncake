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

(u/symtab-add!
 g/symtab
 :header :header 'const
 (u/gba/header
  (u/gba/make-header :entry :foo :title "gbatest" :code "test" :maker "lq")))

(u/symtab-add!
 g/symtab
 :code :foo 'code
 '((add r0 r0 1)
   (b :bar)))

(u/symtab-add!
 g/symtab
 :code :bar 'code
 '((mul r0 r0 r0)
   (b :foo)))

(defconst g/linked (u/gba/link g/symtab g/rom-start 5000))
(defconst g/rom
  (seq-mapcat
   #'byte-to-string
   g/linked
   'string))

(f-write-bytes g/rom "test.gba")

(provide 'gbatest)
;;; gbatest.el ends here
