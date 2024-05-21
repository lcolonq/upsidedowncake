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

(defconst g/tiles-addr #x4000)
(defvar g/tiles
  '(     #x00 #xff  #x00 #xff  #x00 #xff  #x00 #xff  #x00 #xff  #x00 #xff  #x00 #xff  #x00 #xff
         #x00 #xff  #x00 #x80  #x00 #x80  #x00 #x80  #x00 #x80  #x00 #x80  #x00 #x80  #x00 #x80
         #x00 #xff  #x00 #x7e  #x00 #x7e  #x00 #x7e  #x00 #x7e  #x00 #x7e  #x00 #x7e  #x00 #x7e
         #x00 #xff  #x00 #x01  #x00 #x01  #x00 #x01  #x00 #x01  #x00 #x01  #x00 #x01  #x00 #x01
         #x00 #xff  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00
         #x00 #xff  #x00 #x7f  #x00 #x7f  #x00 #x7f  #x00 #x7f  #x00 #x7f  #x00 #x7f  #x00 #x7f
         #x00 #xff  #x03 #xfc  #x00 #xf8  #x00 #xf0  #x00 #xe0  #x20 #xc0  #x00 #xc0  #x40 #x80
         #x00 #xff  #xc0 #x3f  #x00 #x1f  #x00 #x0f  #x00 #x07  #x04 #x03  #x00 #x03  #x02 #x01
         #x00 #x80  #x00 #x80  #x7f #x80  #x00 #x80  #x00 #x80  #x7f #x80  #x7f #x80  #x00 #x80
         #x00 #x7e  #x2a #x7e  #xd5 #x7e  #x2a #x7e  #x54 #x7e  #xff #x00  #xff #x00  #x00 #x00
         #x00 #x01  #x00 #x01  #xff #x01  #x00 #x01  #x01 #x01  #xfe #x01  #xff #x01  #x00 #x01
         #x00 #x80  #x80 #x80  #x7f #x80  #x80 #x80  #x00 #x80  #xff #x80  #x7f #x80  #x80 #x80
         #x00 #x7f  #x2a #x7f  #xd5 #x7f  #x2a #x7f  #x55 #x7f  #xff #x00  #xff #x00  #x00 #x00
         #x00 #xff  #xaa #xff  #x55 #xff  #xaa #xff  #x55 #xff  #xfa #x07  #xfd #x07  #x02 #x07
         #x00 #x7f  #x2a #x7f  #xd5 #x7f  #x2a #x7f  #x55 #x7f  #xaa #x7f  #xd5 #x7f  #x2a #x7f
         #x00 #xff  #x80 #xff  #x00 #xff  #x80 #xff  #x00 #xff  #x80 #xff  #x00 #xff  #x80 #xff
         #x40 #x80  #x00 #x80  #x7f #x80  #x00 #x80  #x00 #x80  #x7f #x80  #x7f #x80  #x00 #x80
         #x00 #x3c  #x02 #x7e  #x85 #x7e  #x0a #x7e  #x14 #x7e  #xab #x7e  #x95 #x7e  #x2a #x7e
         #x02 #x01  #x00 #x01  #xff #x01  #x00 #x01  #x01 #x01  #xfe #x01  #xff #x01  #x00 #x01
         #x00 #xff  #x80 #xff  #x50 #xff  #xa8 #xff  #x50 #xff  #xa8 #xff  #x54 #xff  #xa8 #xff
         #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80
         #xff #x00  #xff #x00  #xff #x00  #xab #x7e  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e
         #xff #x01  #xfe #x01  #xff #x01  #xfe #x01  #xff #x01  #xfe #x01  #xff #x01  #xfe #x01
         #x7f #x80  #xff #x80  #x7f #x80  #xff #x80  #x7f #x80  #xff #x80  #x7f #x80  #xff #x80
         #xff #x00  #xff #x00  #xff #x00  #xaa #x7f  #xd5 #x7f  #xaa #x7f  #xd5 #x7f  #xaa #x7f
         #xf8 #x07  #xf8 #x07  #xf8 #x07  #x80 #xff  #x00 #xff  #xaa #xff  #x55 #xff  #xaa #xff
         #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #xff #x80  #x7f #x80  #xff #x80
         #xd5 #x7f  #xaa #x7f  #xd5 #x7f  #xaa #x7f  #xd5 #x7f  #xaa #x7f  #xd5 #x7f  #xaa #x7f
         #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xeb #x3c
         #x54 #xff  #xaa #xff  #x54 #xff  #xaa #xff  #x54 #xff  #xaa #xff  #x54 #xff  #xaa #xff
         #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x00 #xff
         #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #x2a #xff
         #xff #x01  #xfe #x01  #xff #x01  #xfe #x01  #xff #x01  #xfe #x01  #xff #x01  #x80 #xff
         #x7f #x80  #xff #x80  #x7f #x80  #xff #x80  #x7f #x80  #xff #x80  #x7f #x80  #xaa #xff
         #xff #x00  #xff #x00  #xff #x00  #xff #x00  #xff #x00  #xff #x00  #xff #x00  #x2a #xff
         #xff #x01  #xfe #x01  #xff #x01  #xfe #x01  #xfe #x01  #xfe #x01  #xfe #x01  #x80 #xff
         #x7f #x80  #xff #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x7f #x80  #x00 #xff
         #xfe #x01  #xfe #x01  #xfe #x01  #xfe #x01  #xfe #x01  #xfe #x01  #xfe #x01  #x80 #xff
         #x3f #xc0  #x3f #xc0  #x3f #xc0  #x1f #xe0  #x1f #xe0  #x0f #xf0  #x03 #xfc  #x00 #xff
         #xfd #x03  #xfc #x03  #xfd #x03  #xf8 #x07  #xf9 #x07  #xf0 #x0f  #xc1 #x3f  #x82 #xff
         #x55 #xff  #x2a #x7e  #x54 #x7e  #x2a #x7e  #x54 #x7e  #x2a #x7e  #x54 #x7e  #x00 #x7e
         #x01 #xff  #x00 #x01  #x01 #x01  #x00 #x01  #x01 #x01  #x00 #x01  #x01 #x01  #x00 #x01
         #x54 #xff  #xae #xf8  #x50 #xf0  #xa0 #xe0  #x60 #xc0  #x80 #xc0  #x40 #x80  #x40 #x80
         #x55 #xff  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00
         #x55 #xff  #x6a #x1f  #x05 #x0f  #x02 #x07  #x05 #x07  #x02 #x03  #x03 #x01  #x02 #x01
         #x54 #xff  #x80 #x80  #x00 #x80  #x80 #x80  #x00 #x80  #x80 #x80  #x00 #x80  #x00 #x80
         #x55 #xff  #x2a #x1f  #x0d #x07  #x06 #x03  #x01 #x03  #x02 #x01  #x01 #x01  #x00 #x01
         #x55 #xff  #x2a #x7f  #x55 #x7f  #x2a #x7f  #x55 #x7f  #x2a #x7f  #x55 #x7f  #x00 #x7f
         #x55 #xff  #xaa #xff  #x55 #xff  #xaa #xff  #x55 #xff  #xaa #xff  #x55 #xff  #x00 #xff
         #x15 #xff  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00  #x00 #x00
         #x55 #xff  #x6a #x1f  #x0d #x07  #x06 #x03  #x01 #x03  #x02 #x01  #x03 #x01  #x00 #x01
         #x54 #xff  #xa8 #xff  #x54 #xff  #xa8 #xff  #x50 #xff  #xa0 #xff  #x40 #xff  #x00 #xff
         #x00 #x7e  #x2a #x7e  #xd5 #x7e  #x2a #x7e  #x54 #x7e  #xab #x76  #xdd #x66  #x22 #x66
         #x00 #x7c  #x2a #x7e  #xd5 #x7e  #x2a #x7e  #x54 #x7c  #xff #x00  #xff #x00  #x00 #x00
         #x00 #x01  #x00 #x01  #xff #x01  #x02 #x01  #x07 #x01  #xfe #x03  #xfd #x07  #x0a #x0f
         #x00 #x7c  #x2a #x7e  #xd5 #x7e  #x2a #x7e  #x54 #x7e  #xab #x7e  #xd5 #x7e  #x2a #x7e
         #x00 #xff  #xa0 #xff  #x50 #xff  #xa8 #xff  #x54 #xff  #xa8 #xff  #x54 #xff  #xaa #xff
         #xdd #x62  #xbf #x42  #xfd #x42  #xbf #x40  #xff #x00  #xff #x00  #xf7 #x08  #xef #x18
         #xff #x00  #xff #x00  #xff #x00  #xab #x7c  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e
         #xf9 #x07  #xfc #x03  #xfd #x03  #xfe #x01  #xff #x01  #xfe #x01  #xff #x01  #xfe #x01
         #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7e  #xd5 #x7e  #xab #x7c
         #xf7 #x18  #xeb #x1c  #xd7 #x3c  #xeb #x3c  #xd5 #x3e  #xab #x7e  #xd5 #x7e  #x2a #xff
         #xff #x01  #xfe #x01  #xff #x01  #xfe #x01  #xff #x01  #xfe #x01  #xff #x01  #xa2 #xff
         #x7f #xc0  #xbf #xc0  #x7f #xc0  #xbf #xe0  #x5f #xe0  #xaf #xf0  #x57 #xfc  #xaa #xff
         #xff #x01  #xfc #x03  #xfd #x03  #xfc #x03  #xf9 #x07  #xf0 #x0f  #xc1 #x3f  #x82 #xff
         #x55 #xff  #x2a #xff  #x55 #xff  #x2a #xff  #x55 #xff  #x2a #xff  #x55 #xff  #x00 #xff
         #x45 #xff  #xa2 #xff  #x41 #xff  #x82 #xff  #x41 #xff  #x80 #xff  #x01 #xff  #x00 #xff
         #x54 #xff  #xaa #xff  #x54 #xff  #xaa #xff  #x54 #xff  #xaa #xff  #x54 #xff  #x00 #xff
         #x15 #xff  #x2a #xff  #x15 #xff  #x0a #xff  #x15 #xff  #x0a #xff  #x01 #xff  #x00 #xff
         #x01 #xff  #x80 #xff  #x01 #xff  #x80 #xff  #x01 #xff  #x80 #xff  #x01 #xff  #x00 #xff))
(u/write! g/mem g/tiles-addr g/tiles)

(defconst g/tilemap-addr #x5000)
(defvar g/tilemap
  '(      #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x01  #x02  #x03  #x01  #x04  #x03  #x01  #x05  #x00  #x01  #x05  #x00  #x06  #x04  #x07  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x08  #x09  #x0a  #x0b  #x0c  #x0d  #x0b  #x0e  #x0f  #x08  #x0e  #x0f  #x10  #x11  #x12  #x13  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x14  #x15  #x16  #x17  #x18  #x19  #x1a  #x1b  #x0f  #x14  #x1b  #x0f  #x14  #x1c  #x16  #x1d  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x1e  #x1f  #x20  #x21  #x22  #x23  #x24  #x22  #x25  #x1e  #x22  #x25  #x26  #x22  #x27  #x1d  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x01  #x28  #x29  #x2a  #x2b  #x2c  #x2d  #x2b  #x2e  #x2d  #x2f  #x30  #x2d  #x31  #x32  #x33  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x08  #x34  #x0a  #x0b  #x11  #x0a  #x0b  #x35  #x36  #x0b  #x0e  #x0f  #x08  #x37  #x0a  #x38  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x14  #x39  #x16  #x17  #x1c  #x16  #x17  #x3a  #x3b  #x17  #x1b  #x0f  #x14  #x3c  #x16  #x1d  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x1e  #x3d  #x3e  #x3f  #x22  #x27  #x21  #x1f  #x20  #x21  #x22  #x25  #x1e  #x22  #x40  #x1d  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x41  #x42  #x43  #x44  #x30  #x33  #x41  #x45  #x43  #x41  #x30  #x43  #x41  #x30  #x33  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00   0 0 0 0 0 0 0 0 0 0 0 0
          ))
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
