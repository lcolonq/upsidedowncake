;;; gbatest --- Game Boy Advance testing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(add-to-list 'load-path (f-canonical "../.."))
(require 'udc)

(defconst g/mem-length (* 32 1000))
(defconst g/mem (make-vector g/mem-length 0))
(defconst g/symtab (ht-create))

(f-write-bytes (apply #'unibyte-string g/rom-with-header) "game.gb")

(provide 'gbatest)
;;; gbatest.el ends here
