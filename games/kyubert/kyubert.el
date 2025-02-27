;;; kyubert --- Kyu*bert -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'dash)
(require 'ht)
(require 'f)
(add-to-list 'load-path (f-canonical "../.."))
(add-to-list 'load-path (f-canonical "../../src"))
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc)

;;;; Populate symbol table
(defconst kyu/syms (u/gba/make-symtab))
(u/symtab-add-section! kyu/syms :header u/gba/rom-start)
(u/symtab-add-section! kyu/syms :code (+ u/gba/rom-start #x1000))
(u/symtab-add-section! kyu/syms :data (+ u/gba/rom-start #x20000))
(u/symtab-add-section! kyu/syms :vars #x02000000)

;;;; Assets
(defconst kyu/palette-map ;; base palette used to parse the map (not necessarily used at runtime)
  (u/gba/make-palette
    :colors
    '( (#x00 #x00 #x00) ;; aseprite apple2 palette
       (#x6c #x29 #x40)
       (#x40 #x35 #x78)
       (#xd9 #x3c #xf0)
       (#x13 #x57 #x40)
       (#x80 #x80 #x80)
       )))

(defconst kyu/palette-map-yellow
  (u/gba/make-palette
    :colors
    '( (#x00 #x00 #x00)
       (#xf5 #xff #xbd)
       (#xeb #xdc #x4a)
       (#xde #xb8 #x06)
       (#xe1 #xeb #x5a)
       (#x97 #x9d #x49)
       )))

(defvar kyu/image-source-test (u/load-image-png "./assets/map.png"))
(defconst kyu/image-test
  (u/gba/image-tiles (u/gba/image-quantize-palette-exact kyu/palette-map) kyu/image-source-test))
(defconst kyu/image-tiledata-test
  (--mapcat (u/gba/tile-4bit-bytes it) (u/gba/image-tiledata kyu/image-test)))
(defconst kyu/image-screenblock-test
  (--mapcat
    (list (cdr it) 0)
    (u/gba/image-cell-indices kyu/image-test)))

(defvar kyu/image-source-pal-thesample
  (u/gba/image-load-png-palette "./assets/guy.png"))
(defconst kyu/image-thesample (car kyu/image-source-pal-thesample))
(defconst kyu/image-tiledata-thesample (--mapcat it (u/gba/image-tiledata kyu/image-thesample)))
(defconst kyu/palette-thesample (cdr kyu/image-source-pal-thesample))

;;;; Data
(u/symtab-add! kyu/syms :data :data-palette-map 'bytes
  (u/gba/palette-bytes kyu/palette-map))
(u/symtab-add! kyu/syms :data :data-tiledata-test 'bytes
  kyu/image-tiledata-test)
(u/symtab-add! kyu/syms :data :data-screenblock-test 'bytes
  kyu/image-screenblock-test)
(u/symtab-add! kyu/syms :data :data-palette-thesample 'bytes
  (u/gba/palette-bytes kyu/palette-thesample))
(u/symtab-add! kyu/syms :data :data-tiledata-thesample 'bytes
  kyu/image-tiledata-thesample)

;;;; Variables
(u/symtab-add! kyu/syms :vars :var-test 'var 4)

;;;; Subroutines
;;;;; "Engine" code
(u/symtab-add! kyu/syms :code :interrupt-handler 'code
  (u/gba/toplevel
    (u/gba/claim! 'r2)
    (u/gba/emit! '(mov r0 1)) ;; vblank
    (u/gba/set16 kyu/syms :reg-if 'r0)
    (u/gba/get16 kyu/syms 'r1 :reg-ifbios)
    (u/gba/emit! '(orr r0 r0 r1))
    (u/gba/set16 kyu/syms :reg-ifbios 'r0)
    (u/gba/emit! `(bx ,u/gba/lr))))

(u/symtab-add! kyu/syms :code :enable-interrupts 'code
  (u/gba/function
    (u/gba/set32 kyu/syms :reg-intaddr (u/symtab-entry-addr (u/symtab-lookup kyu/syms :interrupt-handler)))
    (u/gba/set16 kyu/syms :reg-dispstat #b0000000000001000) ;; turn on vblank interrupt
    (u/gba/set16 kyu/syms :reg-ie #b0000000000000001) ;; only enable vblank interrupt
    (u/gba/set32 kyu/syms :reg-ime 1) ;; enable interrupts
    ))

(u/symtab-add! kyu/syms :code :main 'code
  (u/gba/toplevel
    (u/gba/claim! 'r0 'r1 'r2 'r3)

    (u/gba/call kyu/syms :wordcpy 8
      :data-palette-map :palette-bg)
    (u/gba/call kyu/syms :wordcpy (/ (length kyu/image-tiledata-test) 4)
      :data-tiledata-test :vram-bg-charblock0)

    (u/gba/call kyu/syms :wordcpy 128
      :data-palette-thesample :palette-sprite)
    (u/gba/call kyu/syms :wordcpy (/ (length kyu/image-tiledata-thesample) 4)
      :data-tiledata-thesample :vram-sprite-charblock0)
    (u/gba/call kyu/syms :hide-all-sprites)
    (u/gba/set16 kyu/syms (u/gba/sprite-attr1 0) #b0010000000000000)
    (u/gba/set-sprite-coords kyu/syms 0 20 20)
    ;; (u/gba/call kyu/syms :wordcpy (/ (length kyu/image-screenblock-test) 4) :data-screenblock-test :vram-bg-screenblock30)

    (u/gba/set16 kyu/syms :reg-dispcnt #b0001000101000000) ;; 1D object mapping, mode 0, BG0, sprites
    ;; set BG0 control flags to render background starting at screenblock 30 from charblock 0
    (u/gba/set16 kyu/syms :reg-bg0cnt #b0001111000000000)
    (u/gba/call kyu/syms :enable-interrupts)
    (u/gba/emit! '(b :mainloop))))

(u/symtab-add! kyu/syms :code :mainloop 'code
  (u/gba/toplevel
    (u/gba/call kyu/syms :update) ;; update game state
    (u/gba/emit! `(swi ,(ash #x05 16))) ;; VBlankIntrWait BIOS function, remember to shift in ARM!
    (u/gba/call kyu/syms :render) ;; reflect game state in VRAM immediately after vblank
    (u/gba/emit! '(b :mainloop))))

;;;;; Helpers
(u/symtab-add! kyu/syms :code :hide-all-sprites 'code
 (u/gba/function
  (u/gba/emit!
   (u/gba/addr 'r0 kyu/syms :oam)
   '(mov r1 r0)
   '(add r0 r0 1 11) ;; + 1024
   :begin
   (u/gba/set16 kyu/syms 'r1 #b0000001000000000)
   '(add r1 r1 8)
   '(cmp s r0 r1 r0)
   '(b lt :begin))))

(u/symtab-add! kyu/syms :code :memcpy 'code
  (u/gba/function ;; len in r0, src in r1, dst in r2
    (u/gba/do-times 'r0
      (lambda (_)
        (let ((tmp (u/gba/fresh!)))
          (u/gba/emit!
            `(ldr byte post wb ,tmp r1 1)
            `(str byte post wb ,tmp r2 1)))))))

(u/symtab-add! kyu/syms :code :wordcpy 'code
  (u/gba/function ;; len in r0, src in r1, dst in r2
    (u/gba/do-times 'r0
      (lambda (_)
        (let ((tmp (u/gba/fresh!)))
          (u/gba/emit!
            `(ldr post wb ,tmp r1 4)
            `(str post wb ,tmp r2 4)))))))

;;;;; Game logic
(u/symtab-add! kyu/syms :code :update 'code
  (u/gba/function
    (let ((r (u/gba/fresh!)))
      (u/gba/get32 kyu/syms r :var-test)
      (u/gba/emit! `(eor ,r ,r 1))
      (u/gba/set32 kyu/syms :var-test r)
      )))

(defun kyu/draw-cube (offset mode &optional upleft upright)
  "Draw a cube at OFFSET. MODE determines the top color.
UPLEFT and UPRIGHT are non-nil when there is a cube in that position."
  (u/gba/emit!
    (u/gba/scope
      (let* ((base (+ 7 (* 12 mode)))
              (bottom (+ base 8))
              (addr (u/gba/fresh!)))
        (u/gba/emit! (u/gba/addr addr kyu/syms :vram-bg-screenblock30))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 0)) (+ base 0 (if upleft 4 0)))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 2)) (+ base 1 (if upleft 4 0)))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 4)) (+ base 2 (if upright 4 0)))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 6)) (+ base 3 (if upright 4 0)))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 0 (* 64 1))) (+ bottom 0))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 2 (* 64 1))) (+ bottom 1))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 4 (* 64 1))) (+ bottom 2))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 6 (* 64 1))) (+ bottom 3))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 0 (* 64 2))) 1)
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 2 (* 64 2))) 1)
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 4 (* 64 2))) 2)
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 6 (* 64 2))) 2)
        ))))
(defun kyu/draw-cube-bottom-left (offset)
  "Draw the bottom of a cube at OFFSET."
  (u/gba/emit!
    (u/gba/scope
      (let* ((addr (u/gba/fresh!)))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 0 (* 64 3))) 3)
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 2 (* 64 3))) 4)))))
(defun kyu/draw-cube-bottom-right (offset)
  "Draw the bottom of a cube at OFFSET."
  (u/gba/emit!
    (u/gba/scope
      (let* ((addr (u/gba/fresh!)))
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 4 (* 64 3))) 5)
        (u/gba/set16 kyu/syms `(,addr . ,(+ offset 6 (* 64 3))) 6)))))
(defun kyu/cube-offset (x y)
  (let* ((row (+ x y))
          (col (- (+ x 8) y)))
    (+ (* col 4) (* (* row 3) 64))))

(u/symtab-add! kyu/syms :code :render 'code
  (u/gba/function
    (u/gba/emit!
      (u/gba/scope
        (let ((r (u/gba/fresh!)))
          (u/gba/get16 kyu/syms r :var-test)
          (u/gba/emit! `(add ,r ,r ,r))
          (u/gba/set16 kyu/syms `(:oam . 4) r))))
    (-each (-iota 5)
      (lambda (y)
        (-each (-iota 5)
          (lambda (x)
            (kyu/draw-cube
              (kyu/cube-offset x y)
              (cond
                ((and (= x 2) (= y 2)) 1)
                ((and (= x 0) (= y 1)) 2)
                (t 0))
              (not (= x 0)) (not (= y 0)))
            (when (= x 4)
              (kyu/draw-cube-bottom-right (kyu/cube-offset x y)))
            (when (= y 4)
              (kyu/draw-cube-bottom-left (kyu/cube-offset x y)))))))))

;;;;; Generate ROM
(u/symtab-add! kyu/syms :header :header 'const
  (u/gba/header (u/gba/make-header :entry :main :title "kyubert" :code "kbrt" :maker "lq")))
(setq
 c/c-gdb-symbols
 (--map
  (cons (format "%s" (car it)) (format "0x%x" (u/symtab-entry-addr (cdr it))))
  (ht->alist (u/symtab-symbols kyu/syms))))
(defconst kyu/linked (u/gba/link kyu/syms u/gba/rom-start #x25000))
(defconst kyu/rom (seq-mapcat #'byte-to-string kyu/linked 'string))
(f-write-bytes kyu/rom "kyubert.gba")

(provide 'kyubert)
;;; kyubert.el ends here
