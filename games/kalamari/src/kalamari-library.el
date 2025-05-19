;;; kalamari-library --- Kalamari Dominancy: libc-ish library functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)

;;;; Library functions
(u/gba/thumb-function k/syms :wordcpy ;; dest in r0, src in r1, len in r2
  ;; Copy LEN 32-bit words from SRC to DEST
  (u/gba/thumb-for 0 'r2
    (lambda (idx)
      (let ( (tmp (u/gba/fresh!))
             (mulidx (u/gba/fresh!)))
        (u/gba/emit!
          `(lslx ,mulidx ,idx 2)
          `(ldr ,tmp r1 ,mulidx)
          `(str ,tmp r0 ,mulidx))))))

(u/gba/thumb-function k/syms :wordset ;; dest in r0, srcword in r1, len in r2
  ;; Set LEN 32-bit words in DEST to SRC
  (u/gba/thumb-for 0 'r2
    (lambda (idx)
      (let ((mulidx (u/gba/fresh!)))
        (u/gba/emit!
          `(lslx ,mulidx ,idx 2)
          `(str r1 r0 ,mulidx))))))

(u/gba/thumb-function k/syms :random
  ;; https://en.wikipedia.org/wiki/Xorshift
  (let ((tmp (u/gba/fresh!)))
    (u/gba/thumb-get32 k/syms 'r0 :var-rng)
    (u/gba/emit!
      `(lslx ,tmp r0 13) `(eor r0 ,tmp)
      `(lsrx ,tmp r0 17) `(eor r0 ,tmp)
      `(lslx ,tmp r0 5) `(eor r0 ,tmp))
    (u/gba/thumb-set32 k/syms :var-rng 'r0)))

(u/gba/thumb-function k/syms :hide-all-sprites
  (u/gba/thumb-for 0 1024
    (lambda (idx)
      (u/gba/thumb-set16 k/syms (cons :oam idx) #b0000001000000000)
      (u/gba/emit!
        `(inc ,idx 7)))))

(u/gba/thumb-function k/syms :debug-enable
  ;; Enable MGBA debugging features
  (u/gba/thumb-set16 k/syms :reg-debug-enable #xc0de)
  (u/gba/thumb-get16 k/syms (u/gba/fresh!) :reg-debug-enable))

(u/gba/thumb-function k/syms :debug-print ;; len in r0, buf in r1
  ;; Enable MGBA debugging features
  (u/gba/claim! 'r2 'r3)
  (let ( (idxreg)
         (outidx (u/gba/fresh!)))
    (u/gba/thumb-for 0 'r0
      (lambda (idx)
        (setf idxreg idx)
        (let ((tmp (u/gba/fresh!)))
          (u/gba/thumb-get8 k/syms tmp (cons 'r1 idx))
          (u/gba/thumb-set8 k/syms (cons :reg-debug-string idx) tmp))))
    (u/gba/emit! `(mov ,outidx ,idxreg))
    (u/gba/thumb-set8 k/syms (cons :reg-debug-string outidx) 0))
  (u/gba/thumb-set16 k/syms :reg-debug-flags (logior 3 #x100)))

(provide 'kalamari-library)
;;; kalamari-library.el ends here
