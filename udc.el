;;; udc --- Compiler Upside-Down Cake - utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports

(require 'f)

;;;; Load all features
(defconst udc/base-dir (or (and load-file-name (f-dirname load-file-name)) default-directory))
(add-to-list 'load-path (f-join udc/base-dir "src/"))
(add-to-list 'load-path (f-join udc/base-dir "src/gba/"))
(add-to-list 'load-path (f-join udc/base-dir "src/gba/arm/"))
(add-to-list 'load-path (f-join udc/base-dir "src/gba/thumb/"))
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc-utils)
(require 'udc-gb)
(require 'udc-gba)

(provide 'udc)
;;; udc.el ends here
