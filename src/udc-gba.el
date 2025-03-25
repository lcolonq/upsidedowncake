;;; udc-gba --- Compiler Upside-Down Cake - Game Boy Advance tools -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'udc-utils)

(add-to-list 'load-path (f-canonical "./gba/"))
(add-to-list 'load-path (f-canonical "./gba/arm/"))
(add-to-list 'load-path (f-canonical "./gba/thumb/"))
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc-gba-constants)
(require 'udc-gba-arm)
(require 'udc-gba-thumb)
(require 'udc-gba-linker)
(require 'udc-gba-codegen)
(require 'udc-gba-image)

(setf testsyms (u/gba/initial-symtab))
(u/gba/toplevel testsyms :code :test 'arm (u/gba/emit! '(:const r0 #xdeadbeef)))

(provide 'udc-gba)
;;; udc-gba.el ends here
