;;; udc-gba-ir --- Intermediate representation -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 's)
(require 'f)
(require 'dash)
(require 'ht)
(require 'queue)
(require 'udc-utils)
(require 'udc-gba-codegen)

;;;; Intermediate representation for the GBA
;; We use a simple three-address code intermediate representation for higher-level codegen
;; A "normal" instruction looks like: (op d args...), where op is a symbol opcode
;;  d is the destination (an integer register index)
;;  args are the arguments (also integer register indices)
;; There are some "special" instructions shaped like: (sp args...) where sp is a keyword opcode
;;  As these can also have register destinations/arguments, they must be handled specially
(defun u/gba/ir-ins-op (ins) "Return the opcode of INS." (car ins))
(defun u/gba/ir-ins-dest (ins)
  "Return the destination register of INS, if it exists."
  (let ((op (u/gba/ir-ins-op ins)))
    (cond
      ((keywordp op)
        (cl-case op
          (:const (cadr ins))
          (:jmp nil)
          (:br nil)
          (:return nil)
          (:phi (cadr ins))
          (t (error "Unknown special instruction: %s" ins))))
      ((symbolp op) (cadr ins))
      (t (error "Malformed instruction: %s" ins)))))
(defun u/gba/ir-ins-dests (ins)
  "Return a singleton or nil list of destination registers of INS."
  (when-let* ((dest (u/gba/ir-ins-dest ins)))
    (list dest)))
(defun u/gba/ir-ins-args (ins)
  "Return the argument registers of INS."
  (let ((op (u/gba/ir-ins-op ins)))
    (cond
      ((keywordp op)
        (cl-case op
          (:const nil)
          (:jmp nil)
          (:br (list (cadr ins)))
          (:return (cdr ins))
          (:phi (-map #'cdr (cddr ins)))
          (t (error "Unknown special instruction: %s" ins))))
      ((symbolp op) (cddr ins))
      (t (error "Malformed instruction: %s" ins)))))

;; The normal instructions; remember, these are all shaped like (op d args...):
;;   add, sub, mul (arithmetic, binary)
;;   and, or, xor (logical/bitwise, binary)
;;   lsl, asr, lsr (shifts, binary)
;;   mov, not (unary)

;; The special instructions:
;;   (:const dest x) writes the constant integer x to dest (an integer register index)
;;   (:jmp label) jumps to a basic block label (an integer)
;;   (:br cond then else) jumps to then if register cond (an integer register index) contains a nonzero value
;;     otherwise, it jumps to else
;;   (:return x) returns from the function with return value in x (an integer register index)
;;   (:phi dest pairs...) where each pair looks like (label . reg)
;;     each label is a basic block label integer, each reg is a register index integer
;;     phi moves from one of the regs in pairs into dest, based on which branch was evaluated last

;; Basic blocks consist of a body of normal instructions followed by a terminator special instruction
;;  (the valid terminators are :jmp, :br, and :return)
(cl-defstruct (u/gba/ir-bb (:constructor u/gba/make-ir-bb))
  (code nil) ;; list of three-address code instructions
  (term '(:unspecified-terminator))
  )

(defun u/gba/ir-bb-next-use (bb idx v)
  "Determine the position in BB where V is used after IDX.
Return nil if it is not used."
  (let ((rc
          (-find-index
              (lambda (ins)
                (-contains? (u/gba/ir-ins-args ins) v))
            (-drop idx (u/gba/ir-bb-code bb)))))
    (cond
      (rc rc)
      ((-contains? (u/gba/ir-ins-args (u/gba/ir-bb-term bb)) v)
        (length (u/gba/ir-bb-code bb)))
      (t nil))))

;; Control-flow graphs
(cl-defstruct (u/gba/ir-cfg (:constructor u/gba/make-ir-cfg))
  (entry 0) ;; label of entry block
  (next-block 0) ;; label of next block to add
  (blocks (ht-create)) ;; map from labels to basic blocks
  )

(defun u/gba/ir-cfg-bb-label-keyword (b)
  "Convert the integer B to a label keyword."
  (intern (format ":bb%s" b)))

(defun u/gba/ir-cfg-add-bb (cfg bb)
  "Add BB to CFG and return its label."
  (let ((idx (u/gba/ir-cfg-next-block cfg)))
    (ht-set! (u/gba/ir-cfg-blocks cfg) idx bb)
    (cl-incf (u/gba/ir-cfg-next-block cfg))
    idx))

(defun u/gba/ir-cfg-successors (cfg b)
  "Return the successor labels for the block labeled B in CFG."
  (let ((bb (or (ht-get (u/gba/ir-cfg-blocks cfg) b)
              (error "Block %s does not exist in CFG" b))))
    (when-let* ((term (u/gba/ir-bb-term bb)))
      (cl-case (u/gba/ir-ins-op term)
        (:jmp (list (cadr term)))
        (:br (cddr term))
        (:return nil)
        (t (error "Unknown terminator instruction %s for block %s" term b))))))

(defun u/gba/ir-cfg-predecessors (cfg b)
  "Return the predecessor labels for the block labeled B in CFG."
  (--filter
    (-contains? (u/gba/ir-cfg-successors cfg it) b)
    (ht-keys (u/gba/ir-cfg-blocks cfg))))

(defun u/gba/ir-cfg-all-regs (cfg)
  "Return all of the register indices that are used in CFG."
  (let ((ret nil))
    (-each (ht-values (u/gba/ir-cfg-blocks cfg))
      (lambda (bb)
        (-each (u/gba/ir-bb-code bb)
          (lambda (ins)
            (--each (u/gba/ir-ins-dests ins)
              (push it ret))))))
    ret))

;; Data flow analysis
(cl-defstruct (u/gba/ir-dataflow-analysis (:constructor u/gba/make-ir-dataflow-analysis))
  (initial nil) ;; initial value before the first block
  (transfer nil) ;; function given a basic block label and a pre-value, return a post-value from that block
  (merge nil) ;; function given a list of values (produced by all the predecessors of a block), return a value
  (equals nil) ;; function given two values, return non-nil if they are the same
  )

(defun u/gba/ir-dataflow (cfg exclude inedges outedges a)
  "Apply a data flow analysis A to CFG.
Return a hashtable mapping each basic block label to its output value from A.
INEDGES and OUTEDGES are used to control the direction of the analysis.
Exclude EXCLUDE from the analysis, beyond their initial values."
  (let ( (out (ht-create))
         (in (ht-create))
         (worklist (ht-keys (u/gba/ir-cfg-blocks cfg)))
         (init (u/gba/ir-dataflow-analysis-initial a))
         (transfer (u/gba/ir-dataflow-analysis-transfer a))
         (merge (u/gba/ir-dataflow-analysis-merge a))
         (equals (u/gba/ir-dataflow-analysis-equals a)))
    (--each worklist
      (ht-set! in it init)
      (ht-set! out it init))
    (while worklist
      (let ((b (pop worklist)))
        (unless (-contains? exclude b)
          (let* ((preds (funcall inedges cfg b))
                  (inb (funcall merge (--map (ht-get out it) preds)))
                  (outb (funcall transfer b inb)))
            (ht-set! in b inb)
            (unless (funcall equals (ht-get out b) outb)
              (ht-set! out b outb)
              (setf worklist (append (funcall outedges cfg b) worklist)))))))
    (cons in out)))
(defun u/gba/ir-dataflow-forward (cfg a)
  "Apply a forward data flow analysis A to CFG.
Return a hashtable mapping each basic block label to its output value from A."
  (cdr (u/gba/ir-dataflow cfg nil #'u/gba/ir-cfg-predecessors #'u/gba/ir-cfg-successors a)))
(defun u/gba/ir-dataflow-backward (cfg a)
  "Apply a backward data flow analysis A to CFG.
Return a hashtable mapping each basic block label to its input value from A."
  (cdr (u/gba/ir-dataflow cfg nil #'u/gba/ir-cfg-successors #'u/gba/ir-cfg-predecessors a)))

;; Specific analysis passes
;; the usual liveness analysis
(defun u/gba/ir-cfg-liveness (cfg)
  "Determine variable liveness on CFG."
  (u/gba/ir-dataflow-backward
    cfg
    (u/gba/make-ir-dataflow-analysis
      :initial nil
      :merge (lambda (xs) (-reduce-from #'seq-union nil xs))
      :equals #'equal
      :transfer
      ;; given the live variables at the end of a block, what are the live variables at the start?
      ;; well, we need to add all of the variables that are used in this block,
      ;; and remove all of the variables that are declared in this block
      (lambda (b x)
        (let ((bb (ht-get (u/gba/ir-cfg-blocks cfg) b)))
          (seq-difference
            (seq-union x (--mapcat (u/gba/ir-ins-args it) (cons (u/gba/ir-bb-term bb) (u/gba/ir-bb-code bb))))
            (--mapcat (u/gba/ir-ins-dests it) (u/gba/ir-bb-code bb))))))))

;; find dominators - this gives us a map from every basic block to a list of other basic blocks
;; that must traveled through in order to reach this block - we say that those other blocks "dominate" the block
;; this facilitates loop detection: a "backedge" forming a loop is an edge x->y where y dominates x
;; intuitively, this tells us that we have a jump to somewhere we already were (since we had to pass y to reach x)
(defun u/gba/ir-cfg-dominators (cfg)
  "Determine the dominators of every basic block in CFG."
  (u/gba/ir-dataflow-forward
    cfg
    (u/gba/make-ir-dataflow-analysis
      :initial (list (u/gba/ir-cfg-entry cfg))
      :merge
      (lambda (xs)
        (when xs
          (-reduce #'seq-intersection xs)))
      :equals #'equal
      :transfer (lambda (b x) (seq-union (list b) x)))))
;; using the dominators, let's find the backedges!
(defun u/gba/ir-cfg-backedges (cfg)
  "Return a mapping from each basic block label in CFG to a list of its backedges."
  (let ((doms (u/gba/ir-cfg-dominators cfg)))
    (ht<-alist
      (--map
        (cons it (-filter (lambda (s) (-contains? (ht-get doms it) s)) (u/gba/ir-cfg-successors cfg it)))
        (ht-keys (u/gba/ir-cfg-blocks cfg))))))

;; an augmented liveness analysis as described in:
;; Braun, Matthias, and Sebastian Hack. "Register spilling and live-range splitting for SSA-form programs."
;; instead of computing live sets for each block, we instead compute a map from each variable to next use
;; (this is equivalent to live sets: if a variable *has* a finite next use, it is live)
(defun u/gba/ir-cfg-nextuse-block-use (bb v)
  "Determine the position in BB where V is used.
Return nil if it is not used."
  (u/gba/ir-bb-next-use bb 0 v))
(defun u/gba/ir-cfg-nextuse-block-def (bb v)
  "Determine the position in BB where V is defined.
Return nil if it is not defined."
  (-find-index
    (lambda (ins)
      (-contains? (u/gba/ir-ins-dests ins) v))
    (u/gba/ir-bb-code bb)))
(defun u/gba/ir-cfg-nextuse-union (x y)
  "Find the union between two hash tables X and Y.
Take the minimum value for keys present in both."
  (let ((allkeys (seq-union (ht-keys x) (ht-keys y))))
    (ht<-alist
      (--map
        (let ((vx (ht-get x it)) (vy (ht-get y it)))
          (cons it
            (cond
              ((and vx vy) (min vx vy))
              (vx vx) (vy vy)
              (t nil))))
        allkeys))))
(defun u/gba/ir-cfg-nextuse (cfg)
  "Determine next variable use on CFG.
Return two hashmaps - one of uses after each block, one of uses before."
  (let ((all (u/gba/ir-cfg-all-regs cfg)))
    (u/gba/ir-dataflow
      cfg
      nil #'u/gba/ir-cfg-successors #'u/gba/ir-cfg-predecessors
      (u/gba/make-ir-dataflow-analysis
        :initial (ht-create)
        :merge (lambda (xs) (-reduce-from #'u/gba/ir-cfg-nextuse-union (ht-create) xs))
        :equals #'ht-equal?
        :transfer
        (lambda (b a)
          (let ((bb (ht-get (u/gba/ir-cfg-blocks cfg) b)))
            (ht<-alist
              (--map
                (let ( (use (u/gba/ir-cfg-nextuse-block-use bb it))
                       (def (u/gba/ir-cfg-nextuse-block-def bb it))
                       (ait (ht-get a it))
                       (blen (length (u/gba/ir-bb-code bb))))
                  (cons it
                    (cond
                      ((and def (or (not use) (< def use))) nil)
                      (use use)
                      ((not ait) nil)
                      (t (+ blen ait)))))
                ;; todo: weight loops according to braun09cc
                ;; they weight edges that exit loops more highly
                ;; in order to not spill variables used within loops
                ;; we know how to compute this for a given edge probably,
                ;; but how do we do this for a given block?
                ;; this is glossed over in the paper
                all))))))))

;; Spillage

;; Code generation
(cl-defstruct (u/gba/ir-gen (:constructor u/gba/make-ir-gen))
  (op (lambda (o dreg aregs) (u/gba/emit! `(,o ,dreg ,@aregs)))) ;; function that emits code for an opcode
  (regs (ht-create)) ;; mapping from virtual register indices to either symbols (real registers) or integers (spills)
  (next-use (ht-create)) ;; estimate of global next used location for each variable. higher = spill first
  (next-use-bb-out nil) ;; next-use map at the end of each basic block
  (all-vars nil) ;; list of all variables defined in the CFG
  )

(defun u/gba/ir-gen-next-spill (g)
  "Return the next spill index of G."
  (-let ((idxs (-filter #'integerp (ht-values (u/gba/ir-gen-regs g)))))
    (--first (not (-contains? idxs it))
      (-iota 100)))) ;; arbitrary number that's probably way bigger than maximum spill

(defun u/gba/ir-gen-get-next-use (g x)
  "Return the next-use score for X in G."
  (ht-get (u/gba/ir-gen-next-use g) x))

(defun u/gba/ir-gen-pick-unused (g vars)
  "Select a variable in a register from G that is not present in VARS.
This variable should be heuristically most suited to be spilled to memory.
Return a pair of that variable and whether or not it needs to be spilled.
For example variables that are not used in the future need not be spilled."
  (let* ( (in-regs (-map #'car (--filter (symbolp (cdr it)) (ht->alist (u/gba/ir-gen-regs g)))))
          (candidates (seq-difference in-regs vars))
          (nextuse (u/gba/ir-gen-next-use g))
          (tagged (--map (cons it (ht-get nextuse it)) candidates)))
    ;; picking any choice in candidates should yield working code
    ;; however, picking the candidate that is used the furthest in the future is likely the best
    ;; this corresponds to the variable with the entry in the next-use map that is greatest (or nil)
    (if-let* ((unused (--first (not (cdr it)) tagged)))
      unused
      (car (-sort (-on #'> #'cdr) (-filter #'cdr tagged))))))

(defun u/gba/ir-gen-ensure (g vars)
  "Ensure VARS are within a register in G; return those registers."
  (-map
    (lambda (x)
      (let ( (cur (ht-get (u/gba/ir-gen-regs g) x))
             (free (u/gba/codegen-regs-available (u/gba/codegen))))
        (cond
          ((null cur) (error "Variable %s is not necessarily defined before use" x))
          ((symbolp cur) cur) ;; x is already in register cur, return it
          ((and (integerp cur) free)
            ;; if this register is in a local and also there is a free register available, load it
            (let ((r (u/gba/fresh!)))
              (print "getting local")
              (u/gba/emit! `(:get-local ,r ,cur))
              (ht-set! (u/gba/ir-gen-regs g) x r)
              r))
          ((integerp cur)
            ;; otherwise, this is a local and we don't have any free registers
            ;; we'll need to spill. use a heuristic (right now, just pick the first one) to find another
            ;; register that we don't need right now (i.e., vars aren't currently using it), and then
            ;; swap the value in that register into the lowest unused spill index
            (-let* ( ((victim . spill) (u/gba/ir-gen-pick-unused g vars))
                     (vreg (ht-get (u/gba/ir-gen-regs g) victim)))
              (unless victim (error "Out of fresh registers and no victims to spill!"))
              (unless (symbolp vreg) (error "Selected spill victim is not in a register: %s at %s" victim vreg))
              (if spill
                (progn ;; if the victim is alive, we need to swap victim and x
                  (u/gba/emit!
                    `(:swap-local ,cur ,vreg))
                  (ht-set! (u/gba/ir-gen-regs g) victim cur))
                (progn ;; otherwise, the victim is no longer available
                  (print "getting local 2")
                  (u/gba/emit!
                    `(:get-local ,cur ,vreg))
                  (ht-remove! (u/gba/ir-gen-regs g) victim)))
              (ht-set! (u/gba/ir-gen-regs g) x vreg)
              vreg))
          (t (error "Malformed variable: %s" x)))))
    vars))

(defun u/gba/ir-gen-fresh (g var)
  "Return an available register in G, possibly spilling some variable.
VAR will not be spilled, and will be recorded as being stored in that register."
  (print `(fresh ,var ,(ht->alist (u/gba/ir-gen-next-use g))))
  (let ((free (u/gba/codegen-regs-available (u/gba/codegen))))
    (cond
      (free ;; if there are registers free, easy! take one!
        (let ((r (u/gba/fresh!)))
          (ht-set! (u/gba/ir-gen-regs g) var r) ;; but remember that var is stored there
          r))
      (t ;; otherwise, we need to spill
        ;; unlike in the above case where we're swapping out spilled arguments, this is a new variable
        (-let* ( ((victim . spill) (u/gba/ir-gen-pick-unused g (list var)))
                 (vreg (ht-get (u/gba/ir-gen-regs g) victim))
                 (cur (u/gba/ir-gen-next-spill g)))
          (unless victim (error "Out of fresh registers and no victims to spill!"))
          (unless (symbolp vreg) (error "Selected spill victim is not in a register: %s at %s" victim vreg))
          (if spill
            (progn ;; spill the victim if necessary
              (u/gba/emit!
                `(:set-local ,cur ,vreg))
              (ht-set! (u/gba/ir-gen-regs g) victim cur)) ;; victim is now in a local
            (ht-remove! (u/gba/ir-gen-regs g) victim)) ;; otherwise forget about it
          (ht-set! (u/gba/ir-gen-regs g) var vreg) ;; var will be in the victim's register
          vreg)))))

(defun u/gba/ir-gen-ins-update-next-use (g cfg b idx search)
  "Update next-use data for G given we just executed IDX in B in CFG.
Search forward and update the variables in SEARCH"
  (let* ( (bb (ht-get (u/gba/ir-cfg-blocks cfg) b))
          (len (seq-length (u/gba/ir-bb-code bb)))
          (end (ht-get (u/gba/ir-gen-next-use-bb-out g) b)) ;; map from vars -> next use at end of b
          (nextuse (u/gba/ir-gen-next-use g))) ;; map from variables to next-use distances - update this!
    (print `(next-use ,search ,(ht->alist nextuse)))
    (-each (ht-keys nextuse)
      (lambda (var)
        (let ((old (ht-get nextuse var)))
          (if old
            (ht-set! nextuse var (- old 1))
            (ht-remove! nextuse var)))))
    (-each search
      (lambda (var)
        (let* ( (next (u/gba/ir-bb-next-use bb idx var))
                (postblock (ht-get end var))
                (new
                  (cond
                    (next next)
                    (postblock (+ postblock (- len idx)))
                    (t nil))))
          (ht-set! nextuse var new))))
    (-each (ht-keys nextuse)
      (lambda (var)
        (let ((old (ht-get nextuse var)))
          (when (and old (< old 0))
            (error "Variable %s has negative next-use (%s) in basic block %s (this is a bug)" var old b)))))))

(defun u/gba/ir-gen-ins (g cfg b idx)
  "Emit code for the instruction IDX in B in CFG in the context of G.
It is assumed that we are within a code generation context."
  (let* ( (bb (ht-get (u/gba/ir-cfg-blocks cfg) b))
          (ins (seq-elt (u/gba/ir-bb-code bb) idx))
          (op (u/gba/ir-ins-op ins)))
    (print `(gen-ins ,b ,idx ,ins ,(ht->alist (u/gba/ir-gen-regs g))))
    (cond
      ((keywordp op)
        (cl-case op
          (:const
            (let* ( (dest (u/gba/ir-ins-dest ins))
                    (dreg (u/gba/ir-gen-fresh g dest)))
              (print `(const ,dest ,dreg))
              (u/gba/emit! `(:const ,dreg ,(caddr ins)))))
          (:jmp (error "Found jmp instruction in block body"))
          (:br (error "Found br instruction in block body"))
          (:return (error "Found return instruction in block body"))
          (:phi nil)
          (t (error "Unknown special instruction: %s" ins)))
        (u/gba/ir-gen-ins-update-next-use g cfg b (+ idx 1) (u/gba/ir-ins-args ins)))
      ((symbolp op)
        (let* ( (args (u/gba/ir-ins-args ins))
                (dest (u/gba/ir-ins-dest ins))
                ;; before we generate code for the instruction, ensure all args are in registers
                (aregs (u/gba/ir-gen-ensure g args))
                (_ (u/gba/ir-gen-ins-update-next-use g cfg b (+ idx 1) (u/gba/ir-ins-args ins)))
                ;; we also need a place to put the result, so get a register for that too (maybe spilling)
                (dreg (u/gba/ir-gen-fresh g dest)))
          (funcall (u/gba/ir-gen-op g) op dreg aregs) ;; actually produce code
          ))
      (t (error "Malformed instruction: %s" ins)))))

(defun u/gba/ir-gen-bb (g cfg b)
  "Emit code for the basic block B in CFG in G.
It is assumed that we are within a code generation context."
  (let ((bb (ht-get (u/gba/ir-cfg-blocks cfg) b)))
    (setf (u/gba/ir-gen-next-use g) (ht-create))
    (u/gba/ir-gen-ins-update-next-use g cfg b 0 (u/gba/ir-gen-all-vars g))
    (-each-indexed (u/gba/ir-bb-code bb)
      (lambda (idx _)
        (u/gba/ir-gen-ins g cfg b idx)
        ))))

(defun u/gba/ir-gen-bb-successor-correct (g cfg b ex)
  "Emit code correcting register placements in B in CFG in G.
EX in a map from basic blocks to maps from variables to locations."
  (print `(correct ,(ht->alist ex)))
  (let ((current (u/gba/ir-gen-regs g)))
    (-each (u/gba/ir-cfg-successors cfg b) ;; for every successor to this block
      (lambda (sb)
        (when-let* ((allexpected (ht-get ex sb)))
          (-each (ht->alist allexpected) ;; for every variable that we originally expected for this successor
            (-lambda ((v . expected))
              ;; we only need parts of the variable layout that we'll actually use
              (when (ht-get (u/gba/ir-gen-next-use g) v)
                (let ( (actual (or (ht-get current v) (error "Need to fix variable %s, but we forgot it!" v)))
                       (vex (car (ht-find (lambda (_ it) (equal it expected)) current))))
                  (print `(correction ,v (actual ,actual) ,vex (expected ,expected)))
                  (ht-set! current v expected)
                  (ht-set! current vex actual)
                  (cond ;; now swap the actual and expected locations
                    ;; TODO: we can optimize this further by checking next-use for vex
                    ((equal actual expected) nil)
                    ((symbolp actual)
                      (cond
                        ((symbolp expected)
                          (u/gba/emit!
                            `(:swap ,expected ,actual)))
                        ((integerp expected)
                          (u/gba/emit!
                            `(:swap-local ,expected ,actual)))))
                    ((integerp actual)
                      (cond
                        ((symbolp expected)
                          (u/gba/emit!
                            `(:swap-local ,actual ,expected)))
                        ((integerp expected)
                          (ht-remove! current vex)
                          (u/gba/emit!
                            `(:swap-local ,actual r0)
                            `(:set-local ,expected r0)
                            `(:swap-local ,actual r0)))))))))))))))

(defun u/gba/ir-gen-term (g cfg term follow)
  "Emit code for TERM in CFG in G.
Assume that the next block to be generated is labeled FOLLOW."
  (ignore cfg)
  (print `(gen-term ,term))
  (let* ( (op (u/gba/ir-ins-op term))
          (args (cdr term)))
    (cond
      ((keywordp op)
        (cl-case op
          (:jmp ;; jumps are relatively simple: check if the target follows us immediately. if no, emit a jump
            (let ((target (car args)))
              (unless (= follow target)
                (u/gba/emit! `(:jmp ,(u/gba/ir-cfg-bb-label-keyword target))))))
          (:br ;; we need to translate branches into single-target "jmptrue"s, closer to the hardware instruction
            (-let* ( ((c then else) args)
                     (creg (car (u/gba/ir-gen-ensure g (list c)))))
              (cond
                ((= then follow)
                  ;; if the then block follows us, we need to jump to the else block if the condition is false
                  ;; (otherwise, we fall through to the then case)
                  (u/gba/emit! `(:jmpfalse ,creg ,(u/gba/ir-cfg-bb-label-keyword else))))
                ((= else follow)
                  ;; if the else block follows, we need to jump to the then block if the condition is true
                  (u/gba/emit! `(:jmptrue ,creg ,(u/gba/ir-cfg-bb-label-keyword then))))
                (t ;; if neither block follows us, we need to generate multiple jumps
                  (u/gba/emit!
                    `(:jmptrue ,creg ,(u/gba/ir-cfg-bb-label-keyword then))
                    `(:jmpfalse ,creg ,(u/gba/ir-cfg-bb-label-keyword else)))))))
          (:return ;; returns always just emit a return
            (let ((rval (car (u/gba/ir-gen-ensure g args))))
              (u/gba/emit! `(:return ,rval))))
          (t (error "Unknown terminator: %s" term))))
      (t (error "Malformed terminator: %s" term)))))

(defun u/gba/ir-gen-cfg (g cfg)
  "Emit code for CFG in G.
It is assumed that we are within a code generation context."
  (setf (u/gba/ir-gen-next-use-bb-out g)
    (car (u/gba/ir-cfg-nextuse cfg)))
  (-each (ht-values (u/gba/ir-cfg-blocks cfg))
    (lambda (bb)
      (-each (u/gba/ir-bb-code bb)
        (lambda (ins)
          (--each (u/gba/ir-ins-dests ins)
            (push it (u/gba/ir-gen-all-vars g)))))))
  (let ( (upcoming (make-queue))
         (processed (ht-create))
         (expected (ht-create))
         (pterm nil))
    (queue-enqueue upcoming (cons (u/gba/ir-cfg-entry cfg) (ht-create)))
    (while (not (queue-empty upcoming))
      (-let [(b . regs) (queue-dequeue upcoming)]
        (setf (u/gba/ir-gen-regs g) regs)
        (unless (ht-contains? processed b) ;; only emit each block once
          (ht-set! processed b t) ;; when we first generate a block, save the registers we expected
          (when pterm ;; if the previous block set a terminator
            (u/gba/ir-gen-term g cfg pterm b)
            (setf pterm nil))
          (u/gba/emit! (u/gba/ir-cfg-bb-label-keyword b)) ;; label for the block
          (u/gba/ir-gen-bb g cfg b) ;; main block code (terminator will be emitted on the next iteration)
          (setf pterm (u/gba/ir-bb-term (ht-get (u/gba/ir-cfg-blocks cfg) b)))
          ;; finally, we need to "correct" the placement of variables if we've already generated any of the
          ;; successors (and therefore have existing expectations re: what should be where)
          (u/gba/ir-gen-bb-successor-correct g cfg b expected)
          (--each (u/gba/ir-cfg-successors cfg b) ;; try to generate all successors
            (-let [sregs (ht-copy (u/gba/ir-gen-regs g))]
              (ht-set! expected it sregs)
              (queue-enqueue upcoming (cons it sregs)))))))
    (when pterm ;; if the final block set a terminator
      (u/gba/ir-gen-term g cfg pterm -1))))

;; Test code
;; our algorithm is fundamentally broken
;; we really don't handle branching well
;; right now, we pre-emptively discard variables when they aren't visible in the next use
;; this is a problem because of a mismatch between the next-use info and the regs variable
;; regs is updated /linearly/, as we write successive basic blocks
;; while next-use info relates to the graph structure of the blocks
;; we could do something like e.g. attaching the current regs variable to the block queue (we tried this)
;; but this is totally incorrect - regs is bookkeeping meant to reflect the actual locations of variables,
;; and if a block has multiple predecessors, it is arbitrary which will place the block in the queue first
;; (and therefore leaving it with incorrect regs bookkeeping)
;; we need to study the literature a bit more to determine how to resolve this, maybe.
;; one easy and promising prospect is perhaps to just never discard variables, and instead always spill to a local?
;; and then we can strip out unnecessary spills afterward
;; this is probably guaranteed to be correct, at least?
;; fundamentally spilling everything is correct; the problem here is entirely that we're aggressively discarding
;; variables that we think we no longer need
;; honestly we should really just implement the "proper" thing with the W^entry sets in 4.3 of braun09cc
;; an idea: let's do that "greedily"!
;; the first time we encounter an edge to the block, copy the register map
;; when we subsequently encounter an edge to that block, take the intersection between that map and the new map,
;; and generate code to ensure that the shared variables are in the same registers? this probably works
(progn
  (setf test-bb-cfg (u/gba/make-ir-cfg))
  (u/gba/ir-cfg-add-bb test-bb-cfg
    (u/gba/make-ir-bb
      :code
      '( (:const 5 30)
         (:const 6 40)
         (:const 8 20)
         )
      :term
      '(:br 6 1 2)))
  (u/gba/ir-cfg-add-bb test-bb-cfg
    (u/gba/make-ir-bb
      :code
      '( (:const 0 10)
         (:const 1 20)
         (sub 2 1 0)
         (add 3 1 5)
         )
      :term
      '(:jmp 2)))
  (u/gba/ir-cfg-add-bb test-bb-cfg
    (u/gba/make-ir-bb
      :code
      '( (add 7 6 5)
         )
      :term
      '(:jmp 0)))
  )

(defun c/test-gen ()
  (let ((u/gba/codegen (u/gba/make-codegen :type 'thumb))
         (gen (u/gba/make-ir-gen)))
    (u/gba/claim! 'r4 'r5)
    (u/gba/ir-gen-cfg gen test-bb-cfg)
    (reverse (u/gba/codegen-instructions (u/gba/codegen)))))

(defun c/test-gen-extract ()
  (let ((u/gba/codegen (u/gba/make-codegen :type 'thumb))
         (gen (u/gba/make-ir-gen)))
    (u/gba/claim! 'r4 'r5)
    (u/gba/ir-gen-cfg gen test-bb-cfg)
    (u/gba/codegen-extract)))

(progn
  (setf test-cfg (u/gba/make-ir-cfg))
  (u/gba/ir-cfg-add-bb test-cfg
    (u/gba/make-ir-bb
      :code
      '( (:const 0 47)
         (:const 5 3)
         (:const 1 42))
      :term
      '(:br TODOCOND 1 2)))
  (u/gba/ir-cfg-add-bb test-cfg
    (u/gba/make-ir-bb
      :code
      '( (:const 1 1)
         (sub 2 5 1))
      :term
      '(:jmp 3)))
  (u/gba/ir-cfg-add-bb test-cfg
    (u/gba/make-ir-bb
      :code
      '( (:const 0 2)
         (:const 2 10))
      :term
      '(:jmp 3)))
  (u/gba/ir-cfg-add-bb test-cfg
    (u/gba/make-ir-bb
      :code
      '((sub 3 0 2))
      :term
      '(:return foo))))

(progn
  (setf test-cfg2 (u/gba/make-ir-cfg))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 0
    (u/gba/make-ir-bb
      :term
      '(:jmp 1)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 1
    (u/gba/make-ir-bb
      :term
      '(:br foo 2 5)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 2
    (u/gba/make-ir-bb
      :term
      '(:jmp 3)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 3
    (u/gba/make-ir-bb
      :term
      '(:br bar 4 1)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 4
    (u/gba/make-ir-bb
      :term
      '(:return baz)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 5
    (u/gba/make-ir-bb
      :term
      '(:br quux 6 7)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 6
    (u/gba/make-ir-bb
      :term
      '(:jmp 8)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 7
    (u/gba/make-ir-bb
      :term
      '(:jmp 8)))
  (u/gba/ir-cfg-add-bb test-cfg2 ;; 8
    (u/gba/make-ir-bb
      :term
      '(:jmp 3))))

(provide 'udc-gba-ir)
;;; udc-gba-ir.el ends here
