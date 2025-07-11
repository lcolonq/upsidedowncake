;;; test --- scratchpad -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq backtrace-on-error-noninteractive nil)

(require 'dash)
(require 'ht)
(require 'f)
(require 's)
(require 'cl-lib)

(add-to-list 'load-path (f-canonical "../.."))
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

(defun test/build-loads (ts)
  "Construct a vector of load vector from TS."
  (seq-mapcat
    (lambda (tr)
      (-let [(&hash "kind" "size" "addr" "data") tr]
        (when (= kind 1)
          (list (vector addr data size)))))
    ts
    'vector))
(defun test/build-state (opcode initial)
  "Construct an initial emulator test state from OPCODE and INITIAL."
  (-let* ( ((&hash "R" "R_fiq" "R_irq" "R_svc" "R_abt" "R_und" "CPSR" "SPSR") initial)
           ([SPSR_fiq SPSR_svc SPSR_abt SPSR_irq SPSR_und] SPSR))
    (vector
      opcode
      (vector R R_fiq R_irq R_svc R_abt R_und)
      CPSR
      (vector SPSR_fiq SPSR_irq SPSR_svc SPSR_abt SPSR_und))))
(defun test/cpsr-n (x) "Extract N from X." (logand (lsh x -31) #b1))
(defun test/cpsr-z (x) "Extract Z from X." (logand (lsh x -30) #b1))
(defun test/cpsr-c (x) "Extract C from X." (logand (lsh x -29) #b1))
(defun test/cpsr-v (x) "Extract V from X." (logand (lsh x -28) #b1))
(defun test/cpsr-dnm (x) "Extract DNM from X." (logand (lsh x 8) #b11111111111111111111))
(defun test/cpsr-i (x) "Extract I from X." (logand (lsh x 7) #b1))
(defun test/cpsr-f (x) "Extract F from X." (logand (lsh x 6) #b1))
(defun test/cpsr-t (x) "Extract T from X." (logand (lsh x 5) #b1))
(defun test/cpsr-mode (x) "Extract mode from X." (logand x #b11111))
(defun test/compare-field (nm expected actual)
  "Compare EXPECTED and ACTUAL and error with NM if not equal."
  (unless (= actual expected)
    (error "Mismatch in field %s - expected 0x%08x, got 0x%08x" nm expected actual)))
(defun test/compare-cpsr (nm e a)
  "Compare E and A for CPSR NM."
  (condition-case err
    (progn
      (test/compare-field "N (negative)" (test/cpsr-n e) (test/cpsr-n a))
      (test/compare-field "Z (zero)" (test/cpsr-z e) (test/cpsr-z a))
      (test/compare-field "C (carry)" (test/cpsr-c e) (test/cpsr-c a))
      (test/compare-field "V (overflow)" (test/cpsr-v e) (test/cpsr-v a))
      (test/compare-field "CPSR DNM" (test/cpsr-dnm e) (test/cpsr-dnm a))
      (test/compare-field "I (IRQ interrupts)" (test/cpsr-i e) (test/cpsr-i a))
      (test/compare-field "F (FIQ interrupts)" (test/cpsr-f e) (test/cpsr-f a))
      (test/compare-field "T (THUMB execution)" (test/cpsr-t e) (test/cpsr-t a))
      (test/compare-field "mode" (test/cpsr-mode e) (test/cpsr-mode a)))
    (error (error "While comparing %s:\n%s" nm (cadr err)))))
(defun test/compare-state (disasm state final)
  "Compare STATE with FINAL for instruction DISASM."
  (-let* ( ((&hash "R" "R_fiq" "R_irq" "R_svc" "R_abt" "R_und" "CPSR" "SPSR") final)
           ([SPSR_fiq SPSR_svc SPSR_abt SPSR_irq SPSR_und] SPSR)
           ([_ [aR aR_fiq aR_irq aR_svc aR_abt aR_und] aCPSR aSPSR] state))
    (condition-case err
      (progn
        (--each (-iota 16) (let ((nm (format "r%s" it))) (test/compare-field nm (seq-elt R it) (seq-elt aR it))))
        (--each (-iota 7) (let ((nm (format "r%s_FIQ" (+ it 8)))) (test/compare-field nm (seq-elt R_fiq it) (seq-elt aR_fiq it))))
        (--each (-iota 2) (let ((nm (format "r%s_IRQ" (+ it 13)))) (test/compare-field nm (seq-elt R_irq it) (seq-elt aR_irq it))))
        (--each (-iota 2) (let ((nm (format "r%s_SVC" (+ it 13)))) (test/compare-field nm (seq-elt R_svc it) (seq-elt aR_svc it))))
        (--each (-iota 2) (let ((nm (format "r%s_ABT" (+ it 13)))) (test/compare-field nm (seq-elt R_abt it) (seq-elt aR_abt it))))
        (--each (-iota 2) (let ((nm (format "r%s_UND" (+ it 13)))) (test/compare-field nm (seq-elt R_und it) (seq-elt aR_und it))))
        (test/compare-cpsr "CPSR" CPSR aCPSR)
        (test/compare-cpsr "SPSR_FIQ" SPSR_fiq (seq-elt aSPSR 0))
        (test/compare-cpsr "SPSR_IRQ" SPSR_irq (seq-elt aSPSR 1))
        (test/compare-cpsr "SPSR_SVC" SPSR_svc (seq-elt aSPSR 2))
        (test/compare-cpsr "SPSR_ABT" SPSR_abt (seq-elt aSPSR 3))
        (test/compare-cpsr "SPSR_UND" SPSR_und (seq-elt aSPSR 4)))
      (error (error "While checking instruction:\n%s\n%s" disasm (cadr err))))))
(defun test/compare-transaction (idx expected actual)
  "Compare EXPECTED with ACTUAL at IDX."
  (-let ( ((&hash "kind" "size" "addr" "data") expected)
          ([akind aaddr asize adata] actual))
    (condition-case err
      (progn
        (test/compare-field "Kind" kind akind)
        (test/compare-field "Address" addr aaddr)
        (test/compare-field "Size" size asize)
        (test/compare-field "Data" data adata))
      (error (error "While comparing transaction %s:\n%s" idx (cadr err))))))
(defun test/compare-transactions (disasm expected actual)
  "Compare EXPECTED transactions with ACTUAL for DISASM."
  (-let ( (es (--filter (not (= 0 (ht-get it "kind"))) (seq-into expected 'list)))
          (as (seq-into actual 'list)))
    (condition-case err
      (progn
        (--each-indexed (-zip-pair es as)
          (test/compare-transaction it-index (car it) (cdr it))))
      (error (error "While checking transactions for instruction:\n%s\n%s" disasm (cadr err))))))
  
(defun test/to-binary (x)
  "Convert X to a binary string."
  (apply #'s-concat
    (reverse
      (--map
        (if (= 1 (logand (lsh x (- it)) #b1)) "1" "0")
        (-iota 32)))))
(defun test/pretty-mode (m)
  "Pretty-print M."
  (cl-case m
    (#b10000 "USR")
    (#b10001 "FIQ")
    (#b10010 "IRQ")
    (#b10011 "SVC")
    (#b10111 "ABT")
    (#b11011 "UND")
    (#b11111 "SYS")
    (t "<unknown>")))
(defun test/pretty-state (state)
  "Pretty-print STATE."
  (-let [[_ [aR aR_fiq aR_irq aR_svc aR_abt aR_und] aCPSR aSPSR] state]
    (s-concat
      (s-join " " (--map (format "r%s=0x%08x" it (seq-elt aR it)) (-iota 16)))
      " | " (s-join " " (--map (format "r%s_fiq=0x%08x" (+ it 8) (seq-elt aR_fiq it)) (-iota 7)))
      " | " (s-join " " (--map (format "r%s_irq=0x%08x" (+ it 13) (seq-elt aR_irq it)) (-iota 2)))
      " | " (s-join " " (--map (format "r%s_svc=0x%08x" (+ it 13) (seq-elt aR_svc it)) (-iota 2)))
      " | " (s-join " " (--map (format "r%s_abt=0x%08x" (+ it 13) (seq-elt aR_abt it)) (-iota 2)))
      " | " (s-join " " (--map (format "r%s_und=0x%08x" (+ it 13) (seq-elt aR_und it)) (-iota 2)))
      (format " | CPSR=0x%08x" aCPSR)
      (format " | SPSR_FIQ=0x%08x" (seq-elt aSPSR 0))
      (format " SPSR_IRQ=0x%08x" (seq-elt aSPSR 1))
      (format " SPSR_SVC=0x%08x" (seq-elt aSPSR 2))
      (format " SPSR_ABT=0x%08x" (seq-elt aSPSR 3))
      (format " SPSR_UND=0x%08x" (seq-elt aSPSR 4))
      (format " | mode=%s" (test/pretty-mode (logand aCPSR #b11111))))))

(defun test/case (c)
  "Test the emulator on C."
  (-let* ( ((&hash "initial" "final" "opcode" "transactions") c)
           (state (test/build-state opcode initial))
           (initstr (test/pretty-state state))
           (expectstr (test/pretty-state (test/build-state opcode final)))
           (loads (test/build-loads transactions))
           ((disasm . trans) (u/gba/c-emulate-test-arm state loads))
           (insnm (format "%s (%s)" disasm (test/to-binary opcode))))
    (condition-case err
      (progn
        (test/compare-state insnm state final)
        (test/compare-transactions insnm transactions trans))
      (error
        (error
          "%s\nInitial: %s\nActual:  %s\nExpect:  %s\n\nExpected Transactions:\n%s"
          (cadr err) initstr (test/pretty-state state) expectstr
          (test/pretty-transactions transactions)
          )))
    (message "%s passed" (s-pad-right 40 " " disasm))
    ))
(defun test/cases (cs)
  "Test all of CS."
  (seq-each
    (lambda (c)
      (test/case c))
    cs))
(defconst test/files
  '(
     ;;"arm_data_proc_immediate"
     ;;"arm_data_proc_immediate_shift"
     ;;"arm_data_proc_register_shift"
     ;;"arm_mul_mla"
     ;;"arm_bx"
     ;;"arm_b_bl"
     ;;"arm_ldr_str_immediate_offset"
     ;;"arm_ldr_str_register_offset"
     ;;"arm_ldrh_strh"
     ;;"arm_ldrsb_ldrsh"
     "arm_ldm_stm"
     ;; "arm_swp"
     ))
(--each test/files
  (message "Testing file: %s" it)
  (test/cases (json-parse-string (f-read-text (format "~/src/singlesteptests/ARM7TDMI/v1/%s.json" it)))))
(message "All tests passed!")

(provide 'test)
;;; test.el ends here
