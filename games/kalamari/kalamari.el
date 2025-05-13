;;; kalamari --- Kalamari Dominancy -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
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

(defun k/generate ()
  "Generate the ROM."
  (interactive)
  (defconst k/libraries
    '( "kalamari-syms"
       "kalamari-assets"
       "kalamari-state"
       "kalamari-library"
       "kalamari-world"
       "kalamari-engine"
       ))
  (-each k/libraries #'load-library)
  (u/gba/symtab-add! k/syms :header :header 'const
    (u/gba/header (u/gba/make-header :entry :main :title "kalamari" :code "klmr" :maker "lq")))
  (setq
    c/c-gdb-symbols
    (--map
      (cons (format "%s" (car it)) (format "0x%x" (u/gba/symtab-entry-addr (cdr it))))
      (ht->alist (u/gba/symtab-symbols k/syms))))
  (defconst k/linked (u/gba/link k/syms u/gba/rom-start #x25000))
  (f-write-bytes
    (apply #'unibyte-string (seq-into k/linked 'list))
    (f-join k/base-path "kalamari.gba")))

(k/generate)

(provide 'kalamari)
;;; kalamari.el ends here
