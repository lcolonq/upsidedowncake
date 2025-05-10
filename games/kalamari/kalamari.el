;;; kalamari --- Kalamari Dominancy -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(require 's)
(require 'cl-lib)
(add-to-list 'load-path (f-canonical "../.."))
(add-to-list 'load-path (f-canonical "../../src"))
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc)

;;;; Populate symbol table
(defconst k/syms (u/gba/initial-symtab))

;;;; Assets
(defvar k/image-tiles-source (u/gba/image-load-png-palette "./assets/tiles.png"))
(defconst k/image-tiles (car k/image-tiles-source))
(defconst k/image-tiles-thesample (--mapcat it (u/gba/image-tiledata k/image-tiles)))
(defconst k/palette-tiles (cdr k/image-tiles-source))

;;;; State
(u/gba/symtab-add! k/syms :vars :var-test 'var 4)
;; on rom:
;; world - array of chunks
;; chunk - 32x32 array of tiles
;; in memory:
;; keep 9 chunks loaded
;; function to make VRAM reflect the 9 loaded chunks
;; various player attributes

;;;; Subroutines
;;;;; "Engine" code
(u/gba/arm-toplevel k/syms :interrupt-handler
  (u/gba/claim! 'r2)
  '(mov r0 1) ;; vblank
  (u/gba/arm-set16 k/syms :reg-if 'r0)
  (u/gba/arm-get16 k/syms 'r1 :reg-ifbios)
  '(orr r0 r0 r1)
  (u/gba/arm-set16 k/syms :reg-ifbios 'r0)
  `(bx ,u/gba/arm-lr))

(u/gba/arm-function k/syms :enable-interrupts
  (u/gba/arm-set32 k/syms :reg-intaddr (u/gba/symtab-entry-addr (u/gba/symtab-lookup k/syms :interrupt-handler)))
  (u/gba/arm-set16 k/syms :reg-dispstat #b0000000000001000) ;; turn on vblank interrupt
  (u/gba/arm-set16 k/syms :reg-ie #b0000000000000001) ;; only enable vblank interrupt
  (u/gba/arm-set32 k/syms :reg-ime 1) ;; enable interrupts
  )

(u/gba/thumb-toplevel k/syms :mainloop
  `(bl0 :update) `(bl1 :update)
  ;; (u/gba/arm-call k/syms :update) ;; update game state
  ;; `(swi ,(ash #x05 16)) ;; VBlankIntrWait BIOS function, remember to shift in ARM!
  `(swi #x05) ;; VBlankIntrWait BIOS function, remember to shift in ARM!
  ;; (u/gba/arm-call k/syms :render) ;; reflect game state in VRAM immediately after vblank
  '(b :mainloop))

(u/gba/arm-toplevel k/syms :main
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  (u/gba/arm-call k/syms :enable-interrupts)
  (let ((r (u/gba/arm-loc k/syms :mainloop)))
    (u/gba/emit!
      `(add ,r ,r 1)
      `(bx ,r))))

;;;;; Game logic
(u/gba/thumb-function k/syms :update
  '(inc r6 1)
  )

(u/gba/arm-function k/syms :render
  nil)

;;;;; Generate ROM
(u/gba/symtab-add! k/syms :header :header 'const
  (u/gba/header (u/gba/make-header :entry :main :title "kalamari" :code "klmr" :maker "lq")))
(setq
  colonq/c-gdb-symbols
  (--map
    (cons (format "%s" (car it)) (format "0x%x" (u/gba/symtab-entry-addr (cdr it))))
    (ht->alist (u/gba/symtab-symbols k/syms))))
(defconst k/linked (u/gba/link k/syms u/gba/rom-start #x25000))
(defconst k/rom (apply #'unibyte-string (seq-into k/linked 'list)))
(f-write-bytes k/rom "kalamari.gba")

(provide 'kalamari)
;;; kalamari.el ends here
