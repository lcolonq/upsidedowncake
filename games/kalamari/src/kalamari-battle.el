;;; kalamari-battle --- Kalamari Dominancy: battles -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'udc)
(require 'kalamari-syms)
(require 'kalamari-assets)

(u/gba/thumb-function k/syms :game-over
  (u/gba/thumb-set32 k/syms :var-mode k/MODE-GAMEOVER)
  (u/gba/thumb-call k/syms :wordcpy :palette-bg :data-palette-gameover 128)
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-charblock1 :data-tiledata-gameover (/ (length k/tiledata-gameover) 4))
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-screenblock27 :data-tilemap-gameover (/ (length k/tilemap-gameover) 4))
  (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg1))

(u/gba/thumb-function k/syms :game-over
  (u/gba/thumb-set32 k/syms :var-mode k/MODE-GAMEOVER)
  (u/gba/thumb-call k/syms :wordcpy :palette-bg :data-palette-gameover 128)
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-charblock1 :data-tiledata-gameover (/ (length k/tiledata-gameover) 4))
  (u/gba/thumb-call k/syms :wordcpy :vram-bg-screenblock27 :data-tilemap-gameover (/ (length k/tilemap-gameover) 4))
  (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg1))

(u/gba/thumb-function k/syms :render-text ;; x in r0, y in r1, text address in r2
  ;; Render a 0xFF-terminated string to the text background at cell X, Y
  (let ( (tmp (u/gba/fresh!))
         (loc (u/gba/thumb-loc k/syms :vram-bg-screenblock15)))
    (u/gba/emit!
      `(lslx ,tmp r1 5)
      `(add ,tmp ,tmp r0)
      `(lslx ,tmp ,tmp 1)
      `(add ,loc ,loc ,tmp)
      :start)
    (u/gba/thumb-get8 k/syms tmp 'r2)
    (u/gba/emit! `(cmpi ,tmp #xff))
    (u/gba/thumb-if-cond 'ne
      (lambda ()
        (u/gba/thumb-set16 k/syms loc tmp)
        (u/gba/emit!
          '(inc r2 1)
          `(inc ,loc 2)
          '(b :start))))))

(u/gba/thumb-function k/syms :render-number ;; x in r0, y in r1, val in r2
  ;; Render a two-digit decimal number to the text background at cell X, Y
  (u/gba/claim! 'r3)
  (let ( (const (u/gba/thumb-fresh-constant 6554))
         (quot 'r2)
         (tmp (u/gba/fresh!))
         (rem (u/gba/fresh!))
         (loc (u/gba/thumb-loc k/syms :vram-bg-screenblock15)))
    (u/gba/emit! ;; compute address to write characters
      `(lslx ,tmp r1 5)
      `(add ,tmp ,tmp r0)
      `(lslx ,tmp ,tmp 1)
      `(add ,loc ,loc ,tmp))
    (u/gba/emit! ;; horrible tricks to compute quotient and remainder by 10
      `(mov ,rem r2)
      `(mul ,quot ,const)
      `(lsrx ,quot ,quot 16)
      `(mov ,const ,quot)
      `(:const ,tmp ,10)
      `(mul ,const ,tmp)
      `(sub ,rem ,rem ,const))
    (u/gba/emit!
      `(cmpi ,quot 0))
    (u/gba/thumb-if-cond 'gt
      (lambda ()
        (u/gba/emit! `(inc ,quot ,(k/font-index ?0)))
        (u/gba/thumb-set16 k/syms loc quot)))
    (u/gba/emit!
      `(inc ,loc 2)
      `(inc ,rem ,(k/font-index ?0)))
    (u/gba/thumb-set16 k/syms loc rem)))

(u/gba/thumb-function k/syms :random-draw
  ;; Get a random number between 1 and 10 (inclusive)
  (u/gba/claim! 'r2 'r3)
  (let ( (const (u/gba/thumb-fresh-constant 6554))
         (quot (u/gba/fresh!))
         (tmp (u/gba/thumb-fresh-constant 10))
         (rem (u/gba/fresh!)))
    (u/gba/thumb-call k/syms :random)
    (u/gba/thumb-constant quot #xff)
    (u/gba/emit! ;; the same horrible trick
      `(and r0 ,quot)
      `(mov ,rem r0)
      `(mov ,quot r0)
      `(mul ,quot ,const)
      `(lsrx ,quot ,quot 16)
      `(mov ,const ,quot)
      `(mul ,const ,tmp)
      `(sub ,rem ,rem ,const)
      `(inc ,rem 1) ;; 0-9 -> 1-10
      `(mov r0 ,rem))))

(u/gba/thumb-function k/syms :battle-hit
  ;; Draw a new card and render the menu to reflect it
  (let ((draw (u/gba/fresh!)))
    (u/gba/thumb-call k/syms :render-text 3 14 :data-string-drew)
    (u/gba/thumb-call k/syms :render-text 3 15 :data-string-totalis)
    (u/gba/thumb-call k/syms :random-draw)
    (u/gba/emit!
      '(mov r2 r0)
      `(mov ,draw r0))
    (u/gba/thumb-set32 k/syms :var-battle-draw draw)
    (u/gba/thumb-call k/syms :render-number 8 14) ;; drew
    (let ((total (u/gba/fresh!)))
      (u/gba/thumb-get32 k/syms total :var-battle-total)
      (u/gba/emit! `(add ,total ,total ,draw))
      (u/gba/thumb-set32 k/syms :var-battle-total total)
      (u/gba/emit! `(mov r2 ,total))
      (u/gba/thumb-call k/syms :render-number 12 15) ;; total
      (u/gba/thumb-get32 k/syms draw :var-battle-bust)
      (u/gba/emit! `(cmp ,total ,draw))
      (u/gba/thumb-if-cond 'gt
        (lambda ()
          (u/gba/thumb-call k/syms :game-over))))))

(u/gba/thumb-function k/syms :battle-stand
  (let ( (target (u/gba/fresh!))
         (total (u/gba/fresh!)))
    (u/gba/thumb-get32 k/syms target :var-battle-target)
    (u/gba/thumb-get32 k/syms total :var-battle-total)
    (u/gba/emit!
      `(cmp ,total ,target))
    (u/gba/thumb-if-cond 'lt
      (lambda ()
        (u/gba/thumb-call k/syms :game-over))
      (lambda ()
        (u/gba/thumb-get32 k/syms total :var-battle-bust)
        (u/gba/emit! `(cmpi ,total ,21))
        (u/gba/thumb-if-cond 'lt
          (lambda ()
            (u/gba/emit! `(inc ,total 1))
            (u/gba/thumb-set32 k/syms :var-battle-bust total)))
        (u/gba/thumb-call k/syms :end-battle)))))

(u/gba/thumb-function k/syms :battle-flee
  (let ((bust (u/gba/thumb-fresh-constant #b1)))
    (u/gba/thumb-get32 k/syms bust :var-battle-bust)
    (u/gba/emit!
      `(cmpi ,bust 0))
    (u/gba/thumb-if-cond 'gt
      (lambda ()
        (u/gba/emit!
          `(dec ,bust 1))
        (u/gba/thumb-set32 k/syms :var-battle-bust bust)
        (u/gba/thumb-call k/syms :end-battle))
      (lambda ()
        (u/gba/thumb-call k/syms :game-over)))))

(u/gba/thumb-function k/syms :end-battle
  (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg0 :sprites) ;; disable layers
  (u/gba/thumb-set32 k/syms :var-battle-target 0) ;; no longer in battle, can move again
  )

(defun k/render-enemy (e)
  "Generate a function rendering the enemy E."
  (u/gba/thumb-function k/syms (intern (format ":render-enemy-%s" e))
    (-let* ( ((_ nm tiledata tiledata-len tilemap tilemap-len) (ht-get k/enemies e))
             (np (u/gba/symtab-entry-addr (u/gba/symtab-lookup k/syms nm)))
             (s (format "%s" e)))
      (u/gba/thumb-call k/syms :wordcpy :vram-bg-charblock2 tiledata (/ tiledata-len 4))
      (u/gba/thumb-call k/syms :wordcpy :vram-bg-screenblock14 tilemap (/ tilemap-len 4))
      (u/gba/thumb-set32 k/syms :var-battle-nm np)
      (u/gba/thumb-set32 k/syms :var-battle-nm-len (length s))
      (u/gba/thumb-call k/syms :render-text 3 12 :data-string-clear)
      (u/gba/thumb-call k/syms :render-text 3 12 np)
      (u/gba/thumb-call k/syms :render-text (+ 3 (length s) 1) 12 :data-string-appears)
      (u/gba/thumb-call k/syms :render-text 3 13 :data-string-bustat)
      (u/gba/thumb-get32 k/syms 'r2 :var-battle-bust)
      (u/gba/thumb-call k/syms :render-number 11 13) ;; bust at
      (u/gba/thumb-call k/syms :render-text 3 14 :data-string-clear)
      (u/gba/thumb-call k/syms :render-text 3 15 :data-string-clear)
      (u/gba/thumb-call k/syms :random-draw)
      (u/gba/thumb-set32 k/syms :var-battle-target 'r0)
      (u/gba/thumb-set32 k/syms :var-battle-draw 0)
      (u/gba/thumb-set32 k/syms :var-battle-total 0)
      (u/gba/thumb-dispcnt k/syms :videomode0 :object1d :bg0 :bg1 :bg2 :bg3 :sprites) ;; enable layers
      )))
(--each (ht-keys k/enemies)
  (k/render-enemy it))

(u/gba/thumb-function k/syms :random-encounter
  (u/gba/thumb-call k/syms :random)
  (let ((tmp (u/gba/thumb-fresh-constant #b111)))
    (u/gba/emit!
      `(and ,tmp r0))
    (--each (-iota (length k/actual-enemies))
      (u/gba/emit! `(cmpi ,tmp ,it))
      (u/gba/thumb-if-cond 'eq
        (lambda ()
          (u/gba/thumb-call k/syms (intern (format ":render-enemy-%s" (ht-get k/enemies-by-index it)))))))))

(u/gba/thumb-function k/syms :cursor-up
  (let ((tmp (u/gba/fresh!)))
    (u/gba/thumb-get32 k/syms tmp :var-cursor)
    (u/gba/thumb-if tmp
      (lambda () (u/gba/emit! `(dec ,tmp 1)))
      (lambda () (u/gba/emit! `(movi ,tmp 2))))
    (u/gba/thumb-set32 k/syms :var-cursor tmp)))

(u/gba/thumb-function k/syms :cursor-down
  (let ((tmp (u/gba/fresh!)))
    (u/gba/thumb-get32 k/syms tmp :var-cursor)
    (u/gba/emit!
      `(inc ,tmp 1)
      `(cmpi ,tmp 2))
    (u/gba/thumb-if-cond 'gt
      (lambda () (u/gba/emit! `(movi ,tmp 0))))
    (u/gba/thumb-set32 k/syms :var-cursor tmp)))

(defun k/tilemap-offset (x y)
  "Compute the tilemap offset (in bytes) for X, Y."
  (* 2 (+ x (* y 32))))
(u/gba/thumb-function k/syms :render-cursor
  (u/gba/claim! 'r2 'r3)
  (let* ( (off (u/gba/fresh!))
          (base (u/gba/fresh!))
          (addr (u/gba/thumb-loc k/syms :vram-bg-screenblock27))
          (clearline
            (lambda (y)
              (u/gba/thumb-constant off (k/tilemap-offset 19 y))
              (u/gba/thumb-set16 k/syms (cons addr off) 5))))
    (funcall clearline 12)
    (funcall clearline 13)
    (funcall clearline 14)
    (funcall clearline 15)
    (funcall clearline 16)
    (funcall clearline 17)
    (u/gba/thumb-get32 k/syms base :var-cursor)
    (u/gba/emit!
      `(lslx ,base ,base 1)
      `(inc ,base 12)
      `(lslx ,off ,base 5)
      `(inc ,off 19)
      `(lslx ,off ,off 1))
    (u/gba/thumb-set16 k/syms (cons addr off) 10) ;; FIXME: gross hardcoding of tiles
    (u/gba/emit!
      `(inc ,base 1)
      `(lslx ,off ,base 5)
      `(inc ,off 19)
      `(lslx ,off ,off 1))
    (u/gba/thumb-set16 k/syms (cons addr off) 15)))

(provide 'kalamari-battle)
;;; kalamari-battle.el ends here
