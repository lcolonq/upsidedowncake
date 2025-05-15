;;; kalamari-state --- Kalamari Dominancy: global state -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'f)
(require 'udc)
(require 'kalamari-syms)

;;;; State
(u/gba/symtab-add! k/syms :vars :var-x 'var 4) ;; player global coordinates
(u/gba/symtab-add! k/syms :vars :var-y 'var 4)
;; on rom:
;; world - array of chunks
;; chunk - 32x32 array of tiles
;; in memory:
;; keep 9 chunks loaded
;; function to make VRAM reflect the 9 loaded chunks
;; various player attributes

(provide 'kalamari-state)
;;; kalamari-state.el ends here
