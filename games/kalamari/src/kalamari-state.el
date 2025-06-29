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
(u/gba/symtab-add! k/syms :vars :var-battle-draw 'var 4) ;; the player's last draw
(u/gba/symtab-add! k/syms :vars :var-battle-total 'var 4) ;; the player's current total
(u/gba/symtab-add! k/syms :vars :var-battle-bust 'var 4) ;; the player's bust level
(u/gba/symtab-add! k/syms :vars :var-battle-target 'var 4) ;; number to beat if we're in a battle, 0 otherwise
(u/gba/symtab-add! k/syms :vars :var-battle-nm 'var 4) ;; address of the current opponent's name
(u/gba/symtab-add! k/syms :vars :var-battle-nm-len 'var 4) ;; length of the current opponent's name
(u/gba/symtab-add! k/syms :vars :var-rng 'var 4) ;; current rng state
(u/gba/symtab-add! k/syms :vars :var-cursor 'var 4) ;; menu cursor

(provide 'kalamari-state)
;;; kalamari-state.el ends here
