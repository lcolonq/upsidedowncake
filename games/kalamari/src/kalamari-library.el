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

(provide 'kalamari-library)
;;; kalamari-library.el ends here
