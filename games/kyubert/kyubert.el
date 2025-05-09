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
(defconst kyu/syms (u/gba/initial-symtab))

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
(u/gba/symtab-add! kyu/syms :data :data-palette-map 'bytes
  (u/gba/palette-bytes kyu/palette-map))
(u/gba/symtab-add! kyu/syms :data :data-tiledata-test 'bytes
  kyu/image-tiledata-test)
(u/gba/symtab-add! kyu/syms :data :data-screenblock-test 'bytes
  kyu/image-screenblock-test)
(u/gba/symtab-add! kyu/syms :data :data-palette-thesample 'bytes
  (u/gba/palette-bytes kyu/palette-thesample))
(u/gba/symtab-add! kyu/syms :data :data-tiledata-thesample 'bytes
  kyu/image-tiledata-thesample)

;;;; Variables
(u/gba/symtab-add! kyu/syms :vars :var-test 'var 4)

;;;; Subroutines
;;;;; "Engine" code
(u/gba/arm-toplevel kyu/syms :interrupt-handler
  (u/gba/claim! 'r2)
  '(mov r0 1) ;; vblank
  (u/gba/arm-set16 kyu/syms :reg-if 'r0)
  (u/gba/arm-get16 kyu/syms 'r1 :reg-ifbios)
  '(orr r0 r0 r1)
  (u/gba/arm-set16 kyu/syms :reg-ifbios 'r0)
  `(bx ,u/gba/arm-lr))

(u/gba/arm-function kyu/syms :enable-interrupts
  (u/gba/arm-set32 kyu/syms :reg-intaddr (u/gba/symtab-entry-addr (u/gba/symtab-lookup kyu/syms :interrupt-handler)))
  (u/gba/arm-set16 kyu/syms :reg-dispstat #b0000000000001000) ;; turn on vblank interrupt
  (u/gba/arm-set16 kyu/syms :reg-ie #b0000000000000001) ;; only enable vblank interrupt
  (u/gba/arm-set32 kyu/syms :reg-ime 1) ;; enable interrupts
  )

(u/gba/toplevel kyu/syms :code :thumb-test 'thumb
  '(inc r7 1)
  '(b :thumb-test))

;; (u/gba/thumb-function kyu/syms :ir-test
(u/gba/toplevel kyu/syms :code :ir-test 'thumb
  ;; (u/gba/burn! 'r4 'r5 'r6)
  (u/gba/claim! 'r4 'r5)
  (u/gba/ir-gen-cfg (u/gba/make-ir-gen) test-bb-cfg))

(u/gba/arm-toplevel kyu/syms :main
  (u/gba/claim! 'r0 'r1 'r2 'r3)
  '(:const r0 #xdeadbeef)
  '(:const r1 #xcafebabe)

  (let ((r (u/gba/arm-loc kyu/syms :ir-test)))
    (u/gba/emit!
      `(add ,r ,r 1)
      `(bx ,r)))

  (u/gba/arm-call kyu/syms :wordcpy 8
    :data-palette-map :palette-bg)
  (u/gba/arm-call kyu/syms :wordcpy (/ (length kyu/image-tiledata-test) 4)
    :data-tiledata-test :vram-bg-charblock0)

  (u/gba/arm-call kyu/syms :wordcpy 128
    :data-palette-thesample :palette-sprite)
  (u/gba/arm-call kyu/syms :wordcpy (/ (length kyu/image-tiledata-thesample) 4)
    :data-tiledata-thesample :vram-sprite-charblock0)
  (u/gba/arm-call kyu/syms :hide-all-sprites)
  (u/gba/arm-set16 kyu/syms (u/gba/sprite-attr1 0) #b0010000000000000)
  (u/gba/arm-set-sprite-coords kyu/syms 0 20 20)
  ;; (u/gba/call kyu/syms :wordcpy (/ (length kyu/image-screenblock-test) 4) :data-screenblock-test :vram-bg-screenblock30)

  (u/gba/arm-set16 kyu/syms :reg-dispcnt #b0001000101000000) ;; 1D object mapping, mode 0, BG0, sprites
  ;; set BG0 control flags to render background starting at screenblock 30 from charblock 0
  (u/gba/arm-set16 kyu/syms :reg-bg0cnt #b0001111000000000)
  (u/gba/arm-call kyu/syms :enable-interrupts)
  '(b :mainloop))

(u/gba/arm-toplevel kyu/syms :mainloop
  (u/gba/arm-call kyu/syms :update) ;; update game state
  `(swi ,(ash #x05 16)) ;; VBlankIntrWait BIOS function, remember to shift in ARM!
  (u/gba/arm-call kyu/syms :render) ;; reflect game state in VRAM immediately after vblank
  '(b :mainloop))

;;;;; Helpers
(u/gba/arm-function kyu/syms :hide-all-sprites
  (u/gba/arm-addr 'r0 kyu/syms :oam)
  '(mov r1 r0)
  '(add r0 r0 1 11) ;; + 1024
  :begin
  (u/gba/arm-set16 kyu/syms 'r1 #b0000001000000000)
  '(add r1 r1 8)
  '(cmp s r0 r1 r0)
  '(b lt :begin))

(u/gba/arm-function kyu/syms :memcpy ;; len in r0, src in r1, dst in r2
  (u/gba/arm-do-times 'r0
    (lambda (_)
      (let ((tmp (u/gba/fresh!)))
        (u/gba/emit!
          `(ldr byte post wb ,tmp r1 1)
          `(str byte post wb ,tmp r2 1))))))

(u/gba/arm-function kyu/syms :wordcpy ;; len in r0, src in r1, dst in r2
  (u/gba/arm-do-times 'r0
    (lambda (_)
      (let ((tmp (u/gba/fresh!)))
        (u/gba/emit!
          `(ldr post wb ,tmp r1 4)
          `(str post wb ,tmp r2 4))))))

;;;;; Game logic
(u/gba/arm-function kyu/syms :update
  (let ((r (u/gba/fresh!)))
    (u/gba/arm-get32 kyu/syms r :var-test)
    (u/gba/emit! `(eor ,r ,r 1))
    (u/gba/arm-set32 kyu/syms :var-test r)
    ))

(defun kyu/draw-cube (offset mode &optional upleft upright)
  "Draw a cube at OFFSET. MODE determines the top color.
UPLEFT and UPRIGHT are non-nil when there is a cube in that position."
  (u/gba/emit!
    (u/gba/scope
      (let* ((base (+ 7 (* 12 mode)))
              (bottom (+ base 8))
              (addr (u/gba/fresh!)))
        (u/gba/emit! (u/gba/arm-addr addr kyu/syms :vram-bg-screenblock30))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 0)) (+ base 0 (if upleft 4 0)))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 2)) (+ base 1 (if upleft 4 0)))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 4)) (+ base 2 (if upright 4 0)))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 6)) (+ base 3 (if upright 4 0)))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 0 (* 64 1))) (+ bottom 0))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 2 (* 64 1))) (+ bottom 1))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 4 (* 64 1))) (+ bottom 2))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 6 (* 64 1))) (+ bottom 3))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 0 (* 64 2))) 1)
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 2 (* 64 2))) 1)
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 4 (* 64 2))) 2)
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 6 (* 64 2))) 2)
        ))))
(defun kyu/draw-cube-bottom-left (offset)
  "Draw the bottom of a cube at OFFSET."
  (u/gba/emit!
    (u/gba/scope
      (let* ((addr (u/gba/fresh!)))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 0 (* 64 3))) 3)
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 2 (* 64 3))) 4)))))
(defun kyu/draw-cube-bottom-right (offset)
  "Draw the bottom of a cube at OFFSET."
  (u/gba/emit!
    (u/gba/scope
      (let* ((addr (u/gba/fresh!)))
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 4 (* 64 3))) 5)
        (u/gba/arm-set16 kyu/syms `(,addr . ,(+ offset 6 (* 64 3))) 6)))))
(defun kyu/cube-offset (x y)
  "Return the offset in VRAM of the cube at (X, Y)."
  (let* ((row (+ x y))
          (col (- (+ x 8) y)))
    (+ (* col 4) (* (* row 3) 64))))

(u/gba/arm-function kyu/syms :render
  (u/gba/emit!
    (u/gba/scope
      (let ((r (u/gba/fresh!)))
        (u/gba/arm-get16 kyu/syms r :var-test)
        (u/gba/emit! `(add ,r ,r ,r))
        (u/gba/arm-set16 kyu/syms `(:oam . 4) r))))
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
            (kyu/draw-cube-bottom-left (kyu/cube-offset x y))))))))

;;;;; Generate ROM
(u/gba/symtab-add! kyu/syms :header :header 'const
  (u/gba/header (u/gba/make-header :entry :main :title "kyubert" :code "kbrt" :maker "lq")))
(setq
  c/c-gdb-symbols
  (--map
    (cons (format "%s" (car it)) (format "0x%x" (u/gba/symtab-entry-addr (cdr it))))
    (ht->alist (u/gba/symtab-symbols kyu/syms))))
(defconst kyu/linked (u/gba/link kyu/syms u/gba/rom-start #x25000))
(defconst kyu/rom (seq-mapcat #'byte-to-string kyu/linked 'string))
(f-write-bytes kyu/rom "kyubert.gba")

(provide 'kyubert)
;;; kyubert.el ends here
