;;; test --- scratchpad -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'ht)
(require 'f)
(require 's)
(require 'cl-lib)

(add-to-list 'load-path (f-canonical "../../src"))
(require 'udc)

(load-file "shim.so")

(defconst test/syms (u/gba/make-symtab :alignment 4))
(u/gba/symtab-add-section! test/syms :start u/gba/rom-start)
(u/gba/toplevel test/syms :start :main 'arm
  '(mov r0 10)
  '(mov r2 1)
  :loop
  '(add r2 r2 r2)
  '(sub s r0 r0 1)
  '(b ne :loop))
(defconst test/program (u/gba/link test/syms u/gba/rom-start))

(defun test/test ()
  "Test the emulator."
  (u/gba/c-emulate test/program 100))
(test/test)

(provide 'test)
;;; test.el ends here
