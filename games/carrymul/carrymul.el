;;; carrymul --- Multiplication carry tests -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(require 's)
(require 'cl-lib)
(add-to-list 'load-path (f-canonical "./src"))
(add-to-list 'load-path (f-canonical "../.."))
(add-to-list 'load-path (f-canonical "../../src"))
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc)

(defconst cm/syms (u/gba/initial-symtab))
(defconst cm/base-path (if load-file-name (f-parent load-file-name) default-directory))

(u/gba/arm-toplevel cm/syms :main
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (u/gba/arm-constant 'r13 #x5c6aa0bc)
  (u/gba/arm-constant 'r8  #x00ed3f65)
  `(mul s r1 r13 r8)
  `(b :main))

(u/gba/symtab-add! cm/syms :header :header 'const
  (u/gba/header (u/gba/make-header :entry :main :title "carrymul" :code "cmul" :maker "lq")))
(defconst cm/linked (u/gba/link cm/syms u/gba/rom-start))
(f-write-bytes
  (apply #'unibyte-string (seq-into cm/linked 'list))
  (f-join cm/base-path "carrymul.gba"))

(u/gba/insert-checksum! cm/linked)

(provide 'carrymul)
;;; carrymul.el ends here
