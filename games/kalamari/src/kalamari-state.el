;;; kalamari-state --- Kalamari Dominancy: global state -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'f)
(require 'udc)
(require 'kalamari-syms)

;;;; State
(u/gba/symtab-add! k/syms :vars :var-mode 'var 4) ;; current mode
(u/gba/symtab-add! k/syms :vars :var-keys-last 'var 2) ;; previous value of :reg-keyinput
(u/gba/symtab-add! k/syms :vars :var-keys-new 'var 2) ;; keys that were newly set this frame
(u/gba/symtab-add! k/syms :vars :var-x 'var 4) ;; player global coordinates
(u/gba/symtab-add! k/syms :vars :var-y 'var 4)
(u/gba/symtab-add! k/syms :vars :var-cx 'var 4) ;; player chunk coordinates - which chunk is currently in VRAM?
(u/gba/symtab-add! k/syms :vars :var-cy 'var 4)

(u/gba/thumb-function k/syms :initialize-state
  (u/gba/thumb-set32 k/syms :var-mode k/MODE-GAME)
  (u/gba/thumb-set16 k/syms :var-keys-last 0) (u/gba/thumb-set16 k/syms :var-keys-new 0)
  (u/gba/thumb-set32 k/syms :var-x 0) (u/gba/thumb-set32 k/syms :var-y 0)
  (u/gba/thumb-set32 k/syms :var-cx 1) (u/gba/thumb-set32 k/syms :var-cy 1))

(provide 'kalamari-state)
;;; kalamari-state.el ends here
