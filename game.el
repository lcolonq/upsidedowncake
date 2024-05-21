;;; game --- A simple Game Boy game -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)
(require 'f)
(add-to-list 'load-path (f-canonical "."))
(require 'udc)

(defvar g/symtab
  (u/gb/link
   #x150
   '((:start
      (jp :start)
      ))))

(defvar g/rom
  (-concat
   (-flatten (-map #'cdr g/symtab))
   ))

(defvar g/rom-with-header
  (u/pad-to
   (* 32 1024)
   (u/gb/prepend-header
    (list)
    g/rom)))

(f-write-bytes (apply #'unibyte-string g/rom-with-header) "game.gb")

(provide 'game)
;;; game.el ends here
