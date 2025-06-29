;;; kalamari-syms --- Kalamari Dominancy: symbol table -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)

;;;; Populate symbol table
(defconst k/syms (u/gba/initial-symtab))

;;;; Constant definitions
(defconst k/MODE-TITLESCREEN 1)
(defconst k/MODE-GAME 2)
(defconst k/MODE-GAMEOVER 3)
(defconst k/MODE-YOUWIN 4)

(provide 'kalamari-syms)
;;; kalamari-syms.el ends here
