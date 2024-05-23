;;; game --- A simple Game Boy game -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)
(require 'f)
(add-to-list 'load-path (f-canonical "."))
(require 'udc)

(defconst g/mem-length (* 32 1024))
(defvar g/mem)
(setq g/mem (make-vector g/mem-length 0))

(defconst g/reg-sound #xff26)
(defconst g/reg-lcdc #xff40)
(defconst g/reg-ly #xff44)
(defconst g/reg-bgp #xff47)

(defvar g/background-image)
(setq g/background-image (u/load-image-ff "mrgreenbig.ff"))
(defvar g/background-tiledata)
(setq g/background-tiledata (u/gb/image-tiles g/background-image))

(defconst g/tiles-addr #x4000)
(defvar g/tiles)
(setq g/tiles (car g/background-tiledata))
(u/write! g/mem g/tiles-addr g/tiles)

(defconst g/tilemap-addr #x5000)
(defvar g/tilemap)
(setq g/tilemap (cdr g/background-tiledata))
(u/write! g/mem g/tilemap-addr g/tilemap)

(defconst g/symtab-addr #x150)
(defvar g/symtab)
(setq g/symtab
      (u/gb/link
       g/symtab-addr
       `((:entry-point
          (ld a 0)
          (ld ,g/reg-sound a))
         (:wait-vblank
          (ld a ,g/reg-ly mem)
          (cp a 144)
          (jp c :wait-vblank)
          (ld a 0)
          (ld ,g/reg-lcdc a))
         (:load-to-vram
          (ld de ,g/tiles-addr)
          (ld hl #x9000)
          (ld bc ,(length g/tiles)))
         (:copy-tiles
          (ld a *de)
          (ld *hl+ a)
          (inc de)
          (dec bc)
          (ld a b)
          (or a c)
          (jp nz :copy-tiles)
          (ld de ,g/tilemap-addr)
          (ld hl #x9800)
          (ld bc ,(length g/tilemap)))
         (:copy-tilemap
          (ld a *de)
          (ld *hl+ a)
          (inc de)
          (dec bc)
          (ld a b)
          (or a c)
          (jp nz :copy-tilemap))
         (:turn-on-screen
          (ld a #b10000001)
          (ld ,g/reg-lcdc a)
          (ld a #b11100100)
          (ld ,g/reg-bgp a))
         (:done
          (jp :done))
         )))
(u/write! g/mem g/symtab-addr (-flatten (-map #'cdr g/symtab)))

(defvar g/rom)
(setq g/rom (-drop #x150 (seq-into g/mem 'list)))

(defvar g/rom-with-header)
(setq
 g/rom-with-header
 (u/pad-to
  (* 32 1024)
  (u/gb/prepend-header
   (list)
   g/rom)))

(f-write-bytes (apply #'unibyte-string g/rom-with-header) "game.gb")

(provide 'game)
;;; game.el ends here
