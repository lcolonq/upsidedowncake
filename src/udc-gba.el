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

(require 'udc-gba-constants)
(require 'udc-gba-arm)
(require 'udc-gba-thumb)
(require 'udc-gba-linker)
(require 'udc-gba-codegen)
(require 'udc-gba-image)
(require 'udc-gba-ir)

(provide 'udc-gba)
;;; udc-gba.el ends here
