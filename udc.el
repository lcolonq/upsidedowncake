;;; udc --- Compiler Upside-Down Cake - utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports

(require 'f)

;;;; Load all features
(add-to-list 'load-path (f-canonical "./src/"))
(require 'udc-utils)
(require 'udc-gb)
(require 'udc-gba)

(provide 'udc)
;;; udc.el ends here
