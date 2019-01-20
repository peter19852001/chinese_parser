;;;
;;; A straight-forward Earley parser, mainly following 
;;;  http://loup-vaillant.fr/tutorials/earley-parsing/empty-rules

(defpackage :simple-earley-parser
  (:use :common-lisp)
  (:export :earley-parser :earley-recognizer
           :x-args :x0 :x1 :x2 :x3 :x4 :x5 :x6 :x1-or-id
           :xyz-yxz :xList :rxList :val-cdr :fn
           :t1 :t2 :t3 :t4 :t5 :t6 :tag-identity
           :L1 :L2 :L3 :L4 :L5 :L6 :args :lens :tags
           :tn :tfn
           :internalize-grammar :convert-single-str
           :*score-lower-threshold*
           :match-penalty-func :hamming-penalty-func
           :*penalty-func*
           :debug-parse :for-tags-of-partial-parse
           :*CTB-output*)
  )

(in-package :simple-earley-parser)

;;;;;
;;; first the external form of the grammar, which is a list of rules,
;;; and each rule is a list of things like (A -> B C D ...),
;;; where the head can only be symbols, and the body could be symbols or other items.
;;; All symbols appearing as the head (the first symbol) of any rule is a non-terminal
;;; Later may add convenient forms such as alaternatives

;;; After parsing, each rule gives a value, for a rule like (A -> B C
;;; D ...), the default value is (A vB vC vD ...), where vB is the
;;; value for B, vC is the value for C and so on. If either of B, C or
;;; D is terminal, the value is itself, otherwise it is the value from
;;; the corresponding rule. This value can be modified by using the
;;; :val attribute of the rule.
;; For the external grammar, each rule body can have :val following by
;; a value v at the end; if v is a function (as judged by functionp),
;; then it is applied with the arguments and the lengths of each sub-part;
;; if v is not a function, it is used as the value.

;; In the external grammar, each rule body can also have :com
;; following by a value c at the end; where c is a score penalty upon
;; completion of the rule. Later this c could be extended to a
;; function.

;; Different arrow (->, :>, =>) in the rule represent different default
;; :val.  For ->, the default is to give the args. For :>, the
;; default is #'x1, which takes the second item in the list, i.e. the
;; value of the first terminal or non-terminal in the rule body.

;;; The start symbol of the grammar is specified when running the parser.
(declaim (inline x-args x0 x1 x2 x3 x4 x5 x1-or-id
                 xyz-yxz xList rxList val-cdr
                 t1 t2 t3 t4 t5 t6))

(defun x-args (ls &optional tags lens)
  (declare (ignore tags lens))
  ls)

(defun x0 (ls &optional tags lens)
  (declare (ignore tags lens))
  (first ls))
(defun x1 (ls &optional tags lens)
  (declare (ignore tags lens))
  (second ls))
(defun x2 (ls &optional tags lens)
  (declare (ignore tags lens))
  (third ls))
(defun x3 (ls &optional tags lens)
  (declare (ignore tags lens))
  (fourth ls))
(defun x4 (ls &optional tags lens)
  (declare (ignore tags lens))
  (fifth ls))
(defun x5 (ls &optional tags lens)
  (declare (ignore tags lens))
  (sixth ls))

(defparameter *take-first-p* t)
(defun x1-or-id (ls &optional tags lens)
  (declare (ignore tags lens))
  (if *take-first-p* (x1 ls) ls))

(defun xyz-yxz (ls &optional tags lens)
  (declare (ignore tags lens))
  (list (x2 ls) (x1 ls) (x3 ls)))

(defun xList (ls &optional tags lens)
  (declare (ignore tags lens))
  (cons (x1 ls) (x2 ls)))
(defun rxList (ls &optional tags lens)
  (declare (ignore tags lens))
  (cons (x2 ls) (x1 ls)))

(defun val-cdr (args &optional tags lens)
  (declare (ignore tags lens))
  (cdr args))

(defmacro fn (&body body)
  ;; to help succinctly define the transformation function
  ;; x0 up to x6
  ;; the whole list of arguments is args
  ;; the individual arguments are x0 up to x6
  `#'(lambda (args lens)
       (declare (ignorable args lens))
       (let ((x0 (first args))
             (x1 (second args))
             (x2 (third args))
             (x3 (fourth args))
             (x4 (fifth args))
             (x5 (sixth args))
             (x6 (seventh args))
             ;;
             (L1 (first lens))
             (L2 (second lens))
             (L3 (third lens))
             (L4 (fourth lens))
             (L5 (fifth lens))
             (L6 (sixth lens)))
         (declare (ignorable x0 x1 x2 x3 x4 x5 x6
                             L1 L2 L3 L4 L5 L6))
         ,@body)))
;;; analogous helpers for tag function and tag penalty function
;;; The only difference is that these are 1-based instead of 0-based.
(defun t1 (ls &optional lens)
  (declare (ignore lens))
  (first ls))
(defun t2 (ls &optional lens)
  (declare (ignore lens))
  (second ls))
(defun t3 (ls &optional lens)
  (declare (ignore lens))
  (third ls))
(defun t4 (ls &optional lens)
  (declare (ignore lens))
  (fourth ls))
(defun t5 (ls &optional lens)
  (declare (ignore lens))
  (fifth ls))
(defun t6 (ls &optional lens)
  (declare (ignore lens))
  (sixth ls))

(defun tag-identity (tags &optional lens)
  (declare (ignore lens))
  tags)

(defmacro tn (&body body)
  ;; to help succinctly define the transformation function
  ;; t1 up to t6
  ;; the whole list of arguments is tags
  ;; the individual arguments are t1 up to t6
  `#'(lambda (tags lens)
       (declare (ignorable tags lens))
       (let ((t1 (first tags))
             (t2 (second tags))
             (t3 (third tags))
             (t4 (fourth tags))
             (t5 (fifth tags))
             (t6 (sixth tags))
             ;;
             (L1 (first lens))
             (L2 (second lens))
             (L3 (third lens))
             (L4 (fourth lens))
             (L5 (fifth lens))
             (L6 (sixth lens)))
         (declare (ignorable t1 t2 t3 t4 t5 t6
                             L1 L2 L3 L4 L5 L6))
         ,@body)))

(defmacro tfn (&body body)
  ;; to help succinctly define the CTB transformation function, which
  ;; has access to both the subtrees and the tags.
  ;; x0 is the head
  ;; x1 to x6 are the subtrees.
  ;; t1 to t6 are the tags corresponding to the subtrees.
  ;; the whole list of subtrees is args.
  ;; the whole list of tags is tags.
  `#'(lambda (args tags lens)
       (declare (ignorable args tags lens))
       (let ((x0 (first args))
             (x1 (second args))
             (x2 (third args))
             (x3 (fourth args))
             (x4 (fifth args))
             (x5 (sixth args))
             (x6 (seventh args))
             ;;
             (t1 (first tags))
             (t2 (second tags))
             (t3 (third tags))
             (t4 (fourth tags))
             (t5 (fifth tags))
             (t6 (sixth tags))
             ;;
             (L1 (first lens))
             (L2 (second lens))
             (L3 (third lens))
             (L4 (fourth lens))
             (L5 (fifth lens))
             (L6 (sixth lens)))
         (declare (ignorable x0 x1 x2 x3 x4 x5 x6
                             t1 t2 t3 t4 t5 t6
                             L1 L2 L3 L4 L5 L6))
         ,@body)))

;;;
;;; example grammar for testing purpose
;;(defparameter *sample-grammar*
;;  `((sum -> sum + product :val ,#'xyz-yxz)
;;    (sum -> sum - product :val ,#'xyz-yxz)
;;    (sum :> product)
;;    (product -> product * factor :val ,#'xyz-yxz)
;;    (product -> product / factor :val ,#'xyz-yxz)
;;    (product :> factor)
;;    (factor -> < sum > :val ,(fn x2))
;;    (factor :> number)
;;    (number :> 5)))
;;
;;(defparameter *sample-grammar2*
;;  `((S :> E)
;;    (E -> E Q F :val ,#'xyz-yxz)
;;    (E :> F)
;;    (F :> a)
;;    (F -> a M F :val ,#'xyz-yxz)
;;    (F -> < E > :val ,#'x2)
;;    (M :> *)
;;    (M :> /)
;;    (M -> :val *) ;; allow multiplication to be omitted
;;    (Q :> +)
;;    (Q :> -)))
;;
;;(defparameter *sample-grammar3*
;;  ;; an ambiguous grammar
;;  '((S -> S S)
;;    (S -> x)))
;;;
(defun non-terminals (g)
  ;; g is grammar in external form
  (remove-duplicates (mapcar #'first g)))

;;; not strictly necessary, but for later processing, convert the
;;; external grammar into internal form.

;;; Internal grammar has non-terminals and rules, and the whole
;;; grammar is in a hash table indexing the symbol to non-terminals.
;;; Each non-terminals also records whether it is nullable, i.e. can
;;; generate empty string.

;;; NOTE: with our approach of handling empty rules, if a non-terminal
;;; is nullable but without a rule with empty body (i.e. it has only
;;; non-empty rules, but at least one rule consist solely of nullable
;;; non-terminals), the advance prediction for this nullable
;;; non-terminal may not work correctly.

(defstruct non-terminal
  id ;; integer for easy hashing
  name
  rules ;; list of ruless

  ;; nullable? is previously t or nil to indicate whether this
  ;; non-terminal cna generate empty string. But now is extended to
  ;; contain the list of nullable rules for the non-terminal.
  nullable?)

(defstruct rule
  id ;; integer for easy hashing, suitably spaced so that each position of body also has an id
  head ;; non-terminal
  body ;; list of non-terminal or terminals (anything other than non-terminal)
  val ;; If a non-nil function, it is called with the symbol of head,
  ;; and the values of the body (either terminal, or the value of
  ;; non-terminal), to give the value of the non-terminal using this rule.
  ;; If a non-function, then is used to give the value of this rule
  tval ;; similar to val, but intend to output in format compatible
  ;; with CTB. If a function, it should have two parameters, one
  ;; for the subtrees similar to that for val, the other for the
  ;; list of corresponding tags for the subtrees.
  com ;; score penalty to be added to the total score for this rule.
  
  tag ;; If a non-nil function, it is called with the tags and the
  ;; lengths of the children, to give the tag of the non-terminal
  ;; using this rule. Tag is synthesized attribute as in attribute
  ;; grammar.  If a non-function, it is used as the tag for this rule.
  
  stag ;; If a non-nil function, it is called with the tags and the
  ;; lengths of the children, to give the score penalty of a
  ;; combination of children tags. And the best score is preferred,
  ;; and therefore preferring some combination of children.  If a
  ;; non-function, it is used as the score penalty of the tag.  NOTE:
  ;; for pruning, we assume this function gives non-positive value.
  )

(defmethod print-object ((nt non-terminal) out)
  (format out "#<~A~A>"
          (non-terminal-name nt)
          (if (nullable-non-terminal? nt) " *" "")))

(defmethod print-object ((r rule) out)
  (format out "#<~A -> ~{~A~^ ~}>"
          (rule-head r)
          (rule-body r)))


(defun get-non-terminal (n G)
  (gethash n G))

(defun null-rule? (r) (null (rule-body r)))

;; A rule is considered "context-free" if neither the tag and tag
;; penalty are calculated, so that the combination of tags of children
;; are all acceptable, and that the tag penalty is just another
;; additive score to the overall score.

;; In contrast, if either the tag or the tag penalty is a function,
;; the rule is "locally context sensitive", because the tag penalty
;; can bias towards certain combinations of tags of children, or even
;; forbidden certain combinations by returning nil. If the tag is not
;; a constant, then different combination of children tags may produce
;; different tags for this rule, which may affect preferences at upper
;; parts of the parse tree.

;; However, note that this is before the tag penalty is applied and
;; therefore bias the children combinations, the children are treated
;; as in the context free case, i.e. for each rule, from a start to
;; the current position, only the alteratives with the best score are
;; kept for further consideration. Therefore, if alternative A of a
;; child has lower score than alternative B, it will not be selected
;; even if the tag penalty of A is far better than B. Hence, this
;; "local context sensitivity" is more useful in doing consistency
;; checks such as agreement between nouns and verbs, between units and
;; nouns, etc. If it is desirable to consider many alternatives of a
;; child, they should be given the same score, but this may not be
;; easy, and the efficiency of parsing may suffer.
(defun context-free? (r)
  (not (or (functionp (rule-tag r))
           (functionp (rule-stag r)))))


(defun nullable-non-terminal? (x)
  (and (non-terminal-p x) (non-terminal-nullable? x)))
(defun nullable-body? (r)
  ;; will be true if the body is empty
  (every #'nullable-non-terminal? (rule-body r)))
(defun calculate-nullability (G)
  ;; G is grammar in internal form
  ;; use a naive strategy
  (let ((new? nil))
    (loop
       (setf new? nil)
       (loop for nt being the hash-values in G :do
            (let ((new-nullable
                   (remove-if-not #'nullable-body?
                                  (non-terminal-rules nt))))
              (when (> (length new-nullable)
                       (length (non-terminal-nullable? nt)))
                ;; has more nullable rules
                (setf (non-terminal-nullable? nt) new-nullable
                      new? t))))
       (unless new? (return)))))
;;
(defun convert-rule-body (r nt nts)
  (let* ((arrow (if (keywordp (second r))
                    (second r)
                    (intern (symbol-name (second r))
                            :simple-earley-parser)))
         ;; get the string and convert back to symbol in the this
         ;; package, so that the user need not use our version of
         ;; arrow.
         (default-val (case arrow
                        ((:> =>) #'x1-or-id)
                        (t #'x-args)))
         (default-tag (ccase arrow
                        ((=1> =>) #'t1)
                        ((=2>) #'t2)
                        ((=3>) #'t3)
                        ((=4>) #'t4)
                        ((=5>) #'t5)
                        ((=6>) #'t6)
                        ((-> :>) nil)))
         (body (cddr r))
         (pv (position :val body :from-end t))
         (ptv (position :tval body :from-end t))
         (pc (position :com body :from-end t))
         (pt (position :tag body :from-end t))
         (ps (position :stag body :from-end t))
         (bL (length body))
         (end (min (or pv bL) (or ptv bL) (or pc bL) (or pt bL) (or ps bL)))
         (rb (if (< end bL) (subseq body 0 end) body)))
    (make-rule :head nt
               :body (mapcar #'(lambda (x)
                                 (or (gethash x nts) x))
                             rb)
               :val (if pv (nth (1+ pv) body) default-val)
               :tval (if ptv (nth (1+ ptv) body) default-val)
               :com (if pc (nth (1+ pc) body) 0)
               :tag (if pt (nth (1+ pt) body) default-tag)
               :stag (if ps (nth (1+ ps) body) 0))))

(defun internalize-grammar (g)
  ;; g is grammar in external form
  ;; assume the grammar is in correct form
  ;; returns (num-rules-ids . G)
  (let ((out (make-hash-table :test 'eq))
        (ns (non-terminals g))
        (n-id -1)
        (r-id 0))
    ;; create non-terminals and assign id
    (dolist (n ns)
      (setf (gethash n out) (make-non-terminal :id (incf n-id)
                                               :name n
                                               :rules nil
                                               :nullable? nil)))
    ;; convert the rules and assign rule id
    (dolist (r g)
      (let* ((nt (gethash (first r) out))
             (new-r (convert-rule-body r nt out)))
        (push new-r (non-terminal-rules nt))
        (setf (rule-id new-r) r-id)
        (incf r-id (1+ (length (rule-body new-r))))))
    ;;
    (calculate-nullability out)
    ;; some sanity check
    (loop for nt being the hash-values in out :do
         (let ((null-rs (non-terminal-nullable? nt)))
           (when (and null-rs
                      (every #'(lambda (r) (not (null (rule-body r))))
                             null-rs))
             (warn "Nullable non-terminal ~A has no empty rules, advance prediction may not work properly: ~A"
                   (non-terminal-name nt) null-rs))))
    ;; done
    (cons r-id out)))

(defun print-internal-grammar (G)
  (loop for nt being the hash-values in G :do
       (format t "~{~A~%~}" (non-terminal-rules nt))))

(defun convert-single-str-G (G)
  ;; make strings of one character into a character
  (loop for nt being the hash-values in G :do
       (dolist (r (non-terminal-rules nt))
         (setf (rule-body r)
               (mapcar #'(lambda (x)
                           (if (and (stringp x)
                                    (= (length x) 1))
                               (coerce x 'character)
                               x))
                       (rule-body r))))))
(defun convert-single-str (iG)
  (convert-single-str-G (cdr iG)))
;;;
;;(defparameter *sample-G* (internalize-grammar *sample-grammar*))
;;(defparameter *sample-G2* (internalize-grammar *sample-grammar2*))
;;(defparameter *sample-G3* (internalize-grammar *sample-grammar3*))

;;;

;;; the parser uses partial parses, which indicate the starting
;;; position and the matched portion of a rule.
;;; partial parse is commonly referred to as "Earley item"

(defparameter *score-lower-threshold* -4
  "Lower threshold of score, below which that parse is ignored.")

;; TODO: currently each terminal match can have only one tag. may change to allow multiple tags
(defun match-penalty-func (grammar-word input-words)
  "Gives the penalty of matching grammar-word from the grammar to input-words (a list) from the input.
nil means missing word, i.e. insertion/deletion.
Returning nil means such matching is prohibited.
Returning a single number means the penalty for matching the grammar-word to (car input-words).
Returning a list of (penalty . n-consumed) or (penalty n-consumed . tag) means each is a match with the penalty and consumed n-consumed words from input-words, with the tag."
  ;; ignore case when comparing strings
  (let ((input-word (car input-words)))
    (and grammar-word input-word (equalp grammar-word input-word) 0)))

(defun hamming-penalty-func (grammar-word input-words)
  ;; penalty of -1 for both mismatch, insertion and deletion
  (let ((input-word (car input-words)))
    (cond ((null grammar-word) -1)
          ((null input-word) -1)
          ((equalp grammar-word input-word) 0)
          (t -1))))

;; Note that the penalty need not be symmetric.
;; TODO: extend to allow different penalty function in different grammar rules
(defparameter *penalty-func* #'match-penalty-func)

;; called when two words match, to select the one reported
(defun select-grammar-word (grammar-word input-word)
  (if (keywordp grammar-word) input-word grammar-word))

(defparameter *select-func* #'select-grammar-word) ;; *** Temporarily not used

(defmacro aif (con then &optional else)
  `(let ((it ,con))
     (if it ,then ,else)))

(defmacro awhen (con &body body)
  `(let ((it ,con))
     (when it ,@body)))

;;; given a string (as a list of things), position i is just after the i-th item.
;;; i.e. position 0 is before anything, position 1 is after the first item.
(defstruct partial-parse
  seq-id ;; used mainly for debug purpose
  rule
  upto ;; point to sublist in the body of the rule
  start

  ;; the number of items matched so far, i.e. current position - start
  len
  
  ;; list of back links from partial parses for the previous term.
  ;; each link is a cons cell (from . through)
  prev

  ;; processed? could be either:
  ;; nil: not processed once
  ;; to-reprocess: processed at least once, and re-inserted to be processed again
  ;; processed: processed at least once
  ;; shadowed: should be ignored because there is an alternative with a higher score
  processed?

  ;; the score is decreased by penalties, e.g. due to mismatch, insertion/deletion.
  ;; the score includes any completion penalty and tag score, if the rule is completed, i.e. upto is nil.
  (score 0)
  ;;;;
  ;; tag is nil for non-completed partial-parse.
  ;; For completed partial-parse (i.e. upto is nil):
  ;;
  ;; If the rule of this partial-parse is context-free, the tag
  ;; contains the tag of the rule.

  ;; Otherwise, the tag is a list of (tag . alternatives) where the
  ;; alternatives are the combination of children (from left to right)
  ;; that result in tag from the tag function. And these tags have the
  ;; same best score.

  ;; Each alternative is a list of either
  ;;  1. partial-parse, which means a non-terminal (context-free or not),
  ;;  2. or (partial-parse tag . alternatives), which uses a
  ;;  particular tag of a non-terminal, with its equal score
  ;;  alternatives in the tag,
  ;;  3. or matched-terminal, which contains the terminal itself and the tag

  ;; Each accepted tag can be traced to give the desired parse
  ;; subtree. The link to partial-parse is kept for applying the rule
  ;; action function to transform the parse tree into desired form.
  tag
  
  ;;;; the following are for optimization
  pos ;; the 0-based of position of upto in body of the rule
  next ;; used in parses to chain non-empty and empty partial parses
  )

(declaim (inline partial-parse-penalty next-seq-id
                 make-link link-from link-through
                 partial-parse-head next-term
                 make-empty-parses))

(defparameter *seq-id* 0)
(defun next-seq-id () (incf *seq-id*))

(defstruct matched-terminal
  ;; to represent terminal in the parse tree
  tag ;; the tag for this terminal, resulted from the matching function
  val ;; the terminal itself
  len ;; the matched length of the terminal
  )

(defun partial-parse-penalty (p)
  (partial-parse-score p))

(defun make-link (from through) (cons from through))
(defun link-from (L) (car L))
(defun link-through (L) (cdr L))

(defun add-link (p from through)
  ;; returns the new link, if added.
  ;; returns nil otherwise
  (if (find-if #'(lambda (L) (and (eq from (car L))
                                  (eq through (cdr L))))
               (partial-parse-prev p))
      nil
      (let ((Lk (make-link from through)))
        (push Lk (partial-parse-prev p))
        Lk)))

(defun extend-partial-parse-score (p penalty)
  ;; return nil not extensible by the penalty.
  ;; return the score after the penalty
  (let ((s (cond ((null penalty) nil) ;; prohibited
                 ((null p) penalty)
                 (t (+ (partial-parse-score p) penalty)))))
    (if (and s (>= s *score-lower-threshold*)) s nil)))

(defun partial-parse-head (p)
  (rule-head (partial-parse-rule p)))

(defmethod print-object ((obj partial-parse) out)
  (format out "#<~A: ~A ->"
          (partial-parse-seq-id obj)
          (non-terminal-name (partial-parse-head obj)))
  (do ((q (rule-body (partial-parse-rule obj)) (cdr q)))
      ((null q))
    (when (eq q (partial-parse-upto obj))
      (format out " @"))
    (format out " ~A"
            (if (non-terminal-p (car q))
                (non-terminal-name (car q))
                (car q))))
  (format out " [~A]>" (partial-parse-start obj)))
(defun debug-print-partial-parse (p &optional (out *standard-output*))
  ;; some further information in addition to the above print-object
  (format out "** score: ~A~%** len: ~A~%"
          (partial-parse-score p) (partial-parse-len p))
  (format out "** prev:~%")
  (dolist (L (partial-parse-prev p))
    (let* ((from (link-from L))
           (through (link-through L)))
      (format out " from #~A through ~A~%"
              (if from (partial-parse-seq-id from) from)
              (if (partial-parse-p through)
                  (partial-parse-seq-id through)
                  through))
      (terpri out)))
  ;;
  (format out "** tags:~%")
  (if (tag-partial-parse? p)
      (dolist (entry (partial-parse-tag p))
        (format out " tag (~A alts): ~A~%"
                (length (cdr entry))
                (car entry)))
      (format out " ~A~%" (partial-parse-tag p)))
  (terpri out))

(defun for-tags-of-partial-parse (p func)
  ;; call func on each of the tags of p
  (if (context-free? (partial-parse-rule p))
      ;; only one tag for context free rule
      (funcall func (partial-parse-tag p))
      ;; maybe more than one
      (dolist (ts (partial-parse-tag p))
        ;; ts is (tag . alternatives)
        (funcall func (car ts)))))

(defun next-term (p)
  (car (partial-parse-upto p)))

;;; at each position, the partial parses are kept in a queue.
;;; here we simply use a list to represent a queue, and when adding to
;;; the queue, we need to check whether it is already in the queue.
;;; we only add to the end of the queue if it is not already there.
;;; NOTE: this is changed, see below
(defun same-partial-parse (rule upto start p)
  (and (eq rule (partial-parse-rule p))
       (eq upto (partial-parse-upto p))
       (= start (partial-parse-start p))))

;;;;
;;; a queue-like data structure to hold the partial parses at each position
(defstruct parses
  ;; those already processed are only indicated in partial-parse-processed?
  all ;; hash of all partial parses using rule id and start position as key

  ;; hash of partial parses waiting a non-terminal to complete, with
  ;; non-terminal id as key
  waiting

  ;; hash of whether a non-terminal at this position has been
  ;; predicted, using the non-terminal name as key
  predicted
  
  ;; list of completed partial-parse, this should not have (eq)
  ;; duplicates, because this is pushed to only in parses-insert-new,
  ;; and not in parses-re-insert. Seems this accumulates very fast
  ;; after adding the shadowing mechanism, and a large number of these
  ;; are shadowed, which contains links to possibly other shadowed
  ;; partial-parses. This seems to be causing memory issue, so will
  ;; remove the shadowed ones after each position is processed.
  completed

  ;; to record the best score for a non-terminal in a given span, so
  ;; that in a given span, for a non-terminal, only the (possibly
  ;; different) rule(s) with the same best score can survive, other
  ;; will be shadowed. (calculated-key => best-score)
  best-completed
  
  ;; to count the number of completed non-terminals, including those
  ;; rules processed multiple times. non-terminal-name => count
  count-completed

  ;; the following are processed in the order: to-be-completed => non-empty => empty.
  to-be-completed ;; those with non-empty body, but upto is null, chained with partial-parse-next
  non-empty ;; those with non-empty body and upto is not nil, chained with partial-parse-next
  empty ;; those with empty body, chained with partial-parse-next
  )

(defun make-empty-parses ()
  (make-parses :all (make-hash-table)
               :waiting (make-hash-table)
               :predicted (make-hash-table)
               :best-completed (make-hash-table)
               :count-completed (make-hash-table)))

;; For the purpose of indexing all partial parses in parses, we use
;; the rule-id (accounting for position of upto) and its starting
;; position as key. It is possible to use (rule-id . start) as a key
;; in hash table, but a single integer is preferred as key.
;; Therefore we need the maximum unused rule id to calculate the key as follows.
(defparameter *max-unused-rule-id* nil
  ;; should be assigned to the car of output of internalize-grammar before doing the parse
  )

(declaim (inline partial-parse-key partial-parse-key-of))
(defun partial-parse-key (rule-id start)
  (+ (* start *max-unused-rule-id*) rule-id))
(defun partial-parse-key-of (p)
  ;; p is a partial-parse
  (partial-parse-key (+ (partial-parse-pos p)
                        (rule-id (partial-parse-rule p)))
                     (partial-parse-start p)))

(defparameter *max-unused-non-terminal-id* nil
  ;; should be assigned to the count of the internalize-grammar before
  ;; doing the parse, because the internalize-grammar uses the
  ;; non-terminal names as keys, and the non-terminal id start from 0
  )

(declaim (inline rule-span-key best-non-terminal-score
                 worse-non-terminal-score-helper
                 worse-non-terminal-score
                 parses-waiting-for
                 parses-predicted-non-terminal?
                 parses-predict-for-non-terminal
                 parses-may-add-waiting
                 parses-find parses-re-insert))
(defun rule-span-key (rule start)
  ;; gives a hash key for its (non-terminal, start)
  (+ (* start *max-unused-non-terminal-id*)
     (non-terminal-id (rule-head rule))))
(defun best-non-terminal-score (ps rule start)
  (let* ((k (rule-span-key rule start))
         (h (parses-best-completed ps))
         (best-score (gethash k h)))
    (values best-score k h)))
(defun worse-non-terminal-score-helper (cur-score cur-best-score key hash)
  (cond ((or (null cur-best-score)
             (> cur-score cur-best-score))
         (setf (gethash key hash) cur-score)
         nil)
        ((< cur-score cur-best-score) t)
        ;; (= cur-score best-score)
        (t nil)))
(defun worse-non-terminal-score (cur-score ps rule start)
  ;; returns true if cur-score is worse than the existing score for
  ;; the non-terminal and the span.
  ;; Update the best score if cur-score is better
  (let* ((k (rule-span-key rule start))
         (h (parses-best-completed ps))
         (best-score (gethash k h)))
    (worse-non-terminal-score-helper cur-score best-score k h)))

(defun parses-waiting-for (ps nt)
  ;; return those partial parses waiting for non-terminal nt
  (gethash (non-terminal-id nt) (parses-waiting ps)))

(defun parses-predicted-non-terminal? (ps nt)
  ;; nt is the non-terminal struct, or a symbol
  (gethash (if (symbolp nt) nt (non-terminal-name nt))
           (parses-predicted ps)))
(defun parses-predict-for-non-terminal (ps nt)
  ;; nt is the non-terminal struct, or a symbol
  (setf (gethash (if (symbolp nt) nt (non-terminal-name nt))
                 (parses-predicted ps))
        t))

(defun parses-may-add-waiting (ps p)
  ;; add to waiting if appropriate
  (let ((nt (next-term p)))
    (when (non-terminal-p nt)
      (push p (gethash (non-terminal-id nt) (parses-waiting ps))))))

(defun parses-find (ps rule upto-pos start)
  ;; upto-pos is 0-based position of upto in rule-body
  (gethash (partial-parse-key (+ (rule-id rule) upto-pos) start)
           (parses-all ps)))

(defun parses-next (ps)
  ;; take out one partial parse if available, detach and return it
  (let ((c nil))
    (cond ((parses-to-be-completed ps)
           (setf c (parses-to-be-completed ps)
                 (parses-to-be-completed ps) (partial-parse-next c)))
          ((parses-non-empty ps)
           (setf c (parses-non-empty ps)
                 (parses-non-empty ps) (partial-parse-next c)))
          ((parses-empty ps)
           (setf c (parses-empty ps)
                 (parses-empty ps) (partial-parse-next c))))
    (when c
      (setf (partial-parse-next c) nil))
    c))

(defun parses-insert-into (ps p)
  ;; helper function
  (cond ((null-rule? (partial-parse-rule p))
         (setf (partial-parse-next p) (parses-empty ps)
               (parses-empty ps) p))
        ((null (partial-parse-upto p))
         (setf (partial-parse-next p) (parses-to-be-completed ps)
               (parses-to-be-completed ps) p))
        (t (setf (partial-parse-next p) (parses-non-empty ps)
                 (parses-non-empty ps) p))))

(defun parses-insert-new (ps p)
  ;; Since a partial parse may be processed more than once, but should
  ;; be inserted only once, so we check for waiting non-terminal and
  ;; completed parse for insertion into parses-waiting and
  ;; parses-completed, to avoid duplicate entries
  (parses-may-add-waiting ps p)
  ;; check for completed parse. 
  (when (null (partial-parse-upto p))
    (push p (parses-completed ps)))
  ;; index all partial parses at this position
  (setf (gethash (partial-parse-key-of p) (parses-all ps)) p)
  ;;
  (parses-insert-into ps p))

(defun parses-re-insert (ps p)
  ;; p is already processed, re-insert so that it will be re-processed
  ;; NOTE: need to make sure re-inserting does not mess up the list of completed parses
  (setf (partial-parse-processed? p) 'to-reprocess)
  (parses-insert-into ps p))

;;;;
;;;

(defun trace-tags-prev (links output tags tag-links lens)
  ;; Accumulate the terminal itself for tag-links such that the
  ;; parse tree could be re-constructed.
  (if (null links)
      (funcall output tags tag-links lens)
      (dolist (L links)
        (trace-tags (link-from L) (link-through L) output
                    tags tag-links lens))))
(defun trace-tags (p through output tags tag-links lens)
  ;; p is the partial parse, through is either terminal or
  ;; non-terminal with which p leads to the next partial parse.
  ;; tags accumulates the list of tags of a path.
  ;; tag-links accumulates the list of containers for back tracing.
  ;; Whenever a combination of children tags has been accumulated,
  ;; call (output tags tag-links lens).
  ;; Note that likely the tails of tags and separately tag-links could
  ;; be shared, so the function output should not destructively
  ;; modified the given list.

  ;; The accumulated tag-links is a list of either a terminal, a
  ;; partial-parse for context-free rule, or a list of alternatives of
  ;; a particular tag for locally sensitive rule.

  ;; Note: changed, previously for nullable rules, the 'through' may be nil
  ;; Now 'through' should not be nil?
  (assert (not (null through)))
  
  (cond ((null p)
         ;; done one combination
         (funcall output tags tag-links lens))
        ;;
        ((matched-terminal-p through)
         ;; Through a terminal. Use the tag (may be nil)
         (trace-tags-prev (partial-parse-prev p) output
                          (cons (matched-terminal-tag through) tags)
                          (cons through tag-links)
                          (cons (matched-terminal-len through) lens)))
        ;; through a partial-parse, see what type
        ((context-free? (partial-parse-rule through))
         ;; has a single tag only, accumulate the partial parse to
         ;; tag-links for reconstruction of the parse tree.
         (trace-tags-prev (partial-parse-prev p) output
                          (cons (partial-parse-tag through) tags)
                          (cons through tag-links)
                          (cons (partial-parse-len through) lens)))
        (t ;; locally context sensitive, try the tags
         (dolist (x (partial-parse-tag through))
           ;; x is (tag . alternatives)
           (trace-tags-prev (partial-parse-prev p) output
                            (cons (car x) tags)
                            (cons (cons through x) tag-links)
                            (cons (partial-parse-len through) lens))))))

;; TODO: should use custom function to compare equality of tags,
;; instead of equalp
(defun same-alt-component (a b)
  (or (eq a b) ;; covers matched-terminal and partial-parse
      (and (consp a) (consp b)
           ;; (partial-parse tag . alternatives) case
           (eq (car a) (car b))
           (equalp (cadr a) (cadr b)))))
(defun same-alt-combination (alt-a alt-b)
  ;; both a and b are alternatives, see partial-parse-tag for details of alternatives
  (every #'same-alt-component alt-a alt-b))

;; Note: this should not be needed now
;;(defun ignored-alt-p (alt)
;;  ;; this alternative contains NIL, which is due to advancing nullable
;;  ;; non-terminals.
;;  (some #'null alt))

(defun add-tag-alternatives (new-alts tag-alternatives)
  ;; tag-alternatives is (tag . alternatives)
  ;; new-alts is a list of alternatives for the tag
  (dolist (alt new-alts)
    (pushnew alt (cdr tag-alternatives) :test #'same-alt-combination)))

(defun update-tags-and-penalty (prev one-link-from one-link-through tag-func penalty-func best-score best-tags)
  ;; prev is a list of back links in partial-parse.
  ;; one-link-from and one-link-through allow to update only one link (if either non-nil)
  ;; Return updated best-score and best-tags.
  ;; Either tag-func or penalty-func may be constant, but at least one
  ;; should be a function for non-context-free rule.
  (let ((output
         #'(lambda (tags tag-links lens)
             (let ((penalty (if (functionp penalty-func)
                                (funcall penalty-func tags lens)
                                penalty-func)))
               (when (and penalty (or (null best-score)
                                      (>= penalty best-score)))
                 ;; ignore if this combination forbidden
                 ;; only calculate the tag if the penalty is at least as good
                 (let ((tag (if (functionp tag-func)
                                (funcall tag-func tags lens)
                                tag-func)))
                   (cond ((or (null best-score)
                              (> penalty best-score))
                          ;; first score or better score, replace
                          (setf best-score penalty
                                best-tags (list (list tag tag-links))))
                         ((= best-score penalty)
                          ;; equal score, add to existing tags
                          ;; TODO: may use different equality test for
                          ;; tags, so that re-computation of the same
                          ;; tag combinations do not give different
                          ;; alternatives.
                          (aif (assoc tag best-tags :test #'equalp)
                               ;; NOTE: Now, there should not no
                               ;; alternatives with nil
                               (pushnew tag-links (cdr it)
                                        :test #'same-alt-combination)
                               (push (list tag tag-links)
                                     best-tags))))))))))
    ;; trace tags
    (when (or one-link-from one-link-through)
      (trace-tags one-link-from one-link-through output nil nil nil))
    (when prev
      (trace-tags-prev prev output nil nil nil)))
  ;; done
  (values best-score best-tags))

;;;;
(defun union-tags-to-partial-parse (best-tags p)
  ;; return true if there are new tags and p is non-context-free,
  ;; return false otherwise
  (cond ((context-free? (partial-parse-rule p))
         (setf (partial-parse-tag p) best-tags)
         nil)
        (t ;; non-context-free, best-tags is a list of (tag . alternatives), where the tags should be distinct
         ;; since the tags may be composite objects, so use equalp
         (let ((new-tag? nil))
           (dolist (x best-tags new-tag?)
             (aif (assoc (car x) (partial-parse-tag p) :test #'equalp)
                  (add-tag-alternatives (cdr x) it)
                  (progn
                    (setf new-tag? t)
                    (push x (partial-parse-tag p)))))))))

(defun cal-completion-penalty-and-tag (rule from through)
  ;; return the completion penalty + tag score, and the tags
  (let ((completion-penalty (rule-com rule)))
    (cond ((context-free? rule)
           ;; simple for context-free rule
           (values (+ completion-penalty (rule-stag rule))
                   (rule-tag rule)))
          (t ;; not context-free
           (multiple-value-bind (best-score best-tags)
               ;; get the tags for this link
               (update-tags-and-penalty nil from through
                                        (rule-tag rule) (rule-stag rule) nil nil)
             ;; it is possible that all combinations are forbidden
             (values (and best-score (+ completion-penalty best-score))
                     best-tags))))))

(defun add-partial-parse-to-array (rule upto upto-pos start from through s i penalty)
  ;; return the (new or existing) partial parse.
  ;; from the the previous partial parse leading to this one, if non-nil.
  ;; through is either the completed partial parse as completion, or
  ;; terminal for scanning, which leads to the current partial parse.
  ;; penalty is for adding 'through' to 'from', and any additional penalty (tag score) is not yet added.

  ;; new => insert-new, add-link
  ;; existing
  ;;   better score => re-insert, re-process, add-link
  ;;   same score =>
  ;;     add-link
  ;;     union the tags (only for non-context-free completion partial-parse)
  ;;       if has new tags => re-insert, re-process
  ;;   worse score => not insert, ignore

  ;; Note: currently, score is used to replace partial-parse (not up
  ;; to completion yet), which means if a prefix section of a rule has
  ;; a higher score (but with lower suffix score or completion score),
  ;; it gets advantage, and will shadow one with lower score.
  
  ;; Note: It is possible to modify the strategy to only consider
  ;; score on completed partial-parse, but need to modify the
  ;; structure of the partial parse to keep more possibilities.

  (let ((score (extend-partial-parse-score from penalty))
        (best-tag nil))
    (and
     ;;; not prohibited so far
     score
     ;;; account for completion penalties including tag score
     (progn
       (when (null upto)
         ;; before calculating the full completion score, check if we
         ;; can already prune it.

         ;; NOTE: this assumes the stag function gives non-positive value.
         (multiple-value-bind (cur-best-score key hash)
             (best-non-terminal-score (aref s i) rule start)
           ;; NOTE: can only prune, but NOT update the score yet,
           ;; because the completion tag score may forbidden this
           ;; completion entirely.
           
           ;; TODO: thought: let stag be a list of functions whose
           ;; values (each non-positive) are to be added, so that we
           ;; can prune early, like a shortcut
           (if (and cur-best-score
                    ;; can add also the rule-com penalty because it is
                    ;; a constant value
                    (< (+ score (rule-com rule))
                       cur-best-score))
               ;; pruned
               (setf score nil)
               ;; not pruned yet, calculate the full completion score
               (multiple-value-bind (completion-penalty c-tag)
                   (cal-completion-penalty-and-tag rule from through)
                 (setf score (and completion-penalty
                                  (+ score completion-penalty))
                       best-tag c-tag)
                 ;; for completion, check if the non-terminal has better score already
                 (when (and score
                            (worse-non-terminal-score-helper
                             score cur-best-score key hash))
                   (setf score nil))
                 ))))
       score)
     ;;; now score is correct for comparison purpose
     ;;; if not prohibited by tag score or completion penalty
     (let* ((queue (aref s i))
            (p (parses-find queue rule upto-pos start)))
       (cond ((null p)
              ;; new one, the tag may be nil, or a constant, or a list of tags
              (setf p (make-partial-parse :seq-id (next-seq-id)
                                          :rule rule
                                          :upto upto
                                          :start start
                                          :len (- i start)
                                          :score score
                                          :pos upto-pos
                                          :tag best-tag))
              (parses-insert-new queue p))
             ((> score (partial-parse-score p))
              ;; existing, but better new score

              ;; Since this instance may be part of other rules (at
              ;; the end of rule) and held in one of the alternative
              ;; of its tag. But the updated instance may not lead to
              ;; better score of the outer rule, so would not update
              ;; its score, but part of its alternative would now be
              ;; inconsistent if we mutate this instance!!

              ;; Therefore, mutate it if it has not been processed
              ;; even once. In other cases, simply create a new one to
              ;; be safe.

              ;; NOTE: if we want to be more precise, could add a flag
              ;; to partial-parse, and mark it as 'held' if it is used
              ;; either as 'from' or 'through' in a link.

              ;; NOTE: after some thoughts, it seems we need only be
              ;; careful for the completion parses, because only
              ;; completion parses and terminals would be the
              ;; 'through' in alternatives in context sensitive
              ;; rules. For context-free rules, the 'through's do not
              ;; matter because the tag is constant.

              ;; NOTE: Alternatively, we could prefer to process
              ;; completion parses first by inserting them before
              ;; those non-completion parses, to achive similar
              ;; effects. Because then the non-completion parses will
              ;; be unprocessed when some completion parses lead it to
              ;; have better score, and can be safely overridden and
              ;; reused.
              (if (partial-parse-processed? p)
                  ;; processed at least once, to be safe, create a new
                  ;; one instead of mutating the old one.
                  (progn
                    ;; shadow the old one, so that it need not be
                    ;; processed when encountered. TODO: actually mark
                    ;; it as shadowed, and should be ignored in
                    ;; searching for parses-waiting-for in completion
                    (setf (partial-parse-processed? p) 'shadowed)
                    ;;
                    (setf p (make-partial-parse :seq-id (next-seq-id)
                                          :rule rule
                                          :upto upto
                                          :start start
                                          :len (- i start)
                                          :score score
                                          :pos upto-pos
                                          :tag best-tag))
                    (parses-insert-new queue p))
                  ;; p has not been processed, safe to mutate and
                  ;; reuse the partial-parse. And we need not
                  ;; re-insert it.
                  (progn
                    (setf (partial-parse-score p) score
                          (partial-parse-prev p) nil
                          ;; if not completion, best-tag is nil anyway
                          (partial-parse-tag p) best-tag))))
             ((= score (partial-parse-score p))
              ;; existing, and same score
              ;; union tags, only for completion partial-parse
              (when (and (null upto)
                         ;; use upto to decide whether it is completion
                         (union-tags-to-partial-parse best-tag p))
                ;; new tags, re-insert, re-process
                (when (eq (partial-parse-processed? p)
                          'processed)
                  ;; re-insert, so that it will be re-processed
                  (parses-re-insert queue p)))))
	;;;;
       (when (and from through (>= score (partial-parse-score p)))
         ;; add new link, even for other cases
         (add-link p from through))
       ;;
       p))))

;;;;
;; s is the array of sets of partial parses
;; completed at position i
(defun completed (p s i)
  ;; p is completed parse
  ;; any completion penalty and tag score is already calculated and is in the score,
  ;; simply add this completed partial-parse to anything waiting for it
  
  (cond ((null (rule-body (partial-parse-rule p)))
         ;; with our way of handling empty rules, they are essentially
         ;; completed at prediction, so need not complete them again,
         ;; lest there be spurious duplicate results.
         nil)
        ((worse-non-terminal-score
          (partial-parse-score p) (aref s i)
          (partial-parse-rule p) (partial-parse-start p))
         ;; its non-terminal has better score for the span, ignore and
         ;; shadow this
         (setf (partial-parse-processed? p) 'shadowed))
        (t
         ;; accumulate the completion counts
         (incf (gethash (non-terminal-name (partial-parse-head p))
                        (parses-count-completed (aref s i))
                        0))
         ;; complete as usual
         (dolist (x (parses-waiting-for (aref s (partial-parse-start p))
                                        (partial-parse-head p)))
           ;; ignored shadowed partial parse
           (unless (eq (partial-parse-processed? x) 'shadowed)
             (add-partial-parse-to-array
              (partial-parse-rule x)
              (cdr (partial-parse-upto x))
              (1+ (partial-parse-pos x))
              (partial-parse-start x)
              x p s i
              (partial-parse-penalty p)))))))

(defun predict-non-terminal (nt s i)
  ;; NOTE: add-partial-parse-to-array need to insert empty rules at
  ;; the end of the queue
  (let ((ps (aref s i)))
    (unless (parses-predicted-non-terminal? ps nt)
      ;; mark it predicted first, so that it is not recursively
      ;; predicted below, even for left-recursive rules
      (parses-predict-for-non-terminal ps nt)
      ;;
      (dolist (r (non-terminal-rules nt))
        (add-partial-parse-to-array r (rule-body r) 0 i nil nil s i 0)))))

(defun predict (p upto s i)
  ;; upto has a non-terminal
  (let ((nt (car upto)))
    (predict-non-terminal nt s i)
    ;; in order to handle empty rules, when predicting, predict also
    ;; the non-terminal after the (possibly many) nullable
    ;; non-terminals

    ;; The current strategy works only for one level of empty rule.
    ;; When encountering nullable non-terminal, advance one position,
    ;; and actually get the partial-parse for the empty rule(s).

    ;; Therefore, the handling of empty rule is special: they are
    ;; completed as they are predicted and inserted into the queue,
    ;; and completion can ignore empty rules.
    (dolist (r (non-terminal-nullable? nt))
      ;; this loop is empty if the non-terminal is not nullable
      ;; only advance for the empty rules
      (when (null (rule-body r))
        (let ((ep (add-partial-parse-to-array r nil 0 i nil nil s i 0)))
          (when ep
            (add-partial-parse-to-array
             (partial-parse-rule p) (cdr upto) (1+ (partial-parse-pos p))
             (partial-parse-start p) p ep s i (partial-parse-penalty ep))))))
    ;;
    ))
  
(defun matching-word (terminal words)
  ;; return in a uniform way: list of (penalty n-consumed . tag) or (penalty . n-consumed)
  (let ((m (funcall *penalty-func* terminal words)))
    (cond ((null m) nil)
          ((numberp m) (list (cons m 1)))
          (t m))))
(defun take-matched-word (words matched-length)
  (if (= matched-length 1)
      (car words)
      (subseq words 0 matched-length)))
(defun scan (p terminal words s i)
  ;; p is the partial parse
  ;; word is the input just after position i
  (dolist (m (matching-word terminal words))
    ;; m is (penalty n-consumed . tag) or (penalty . n-consumed)
    (let ((penalty (car m))
          (n-consumed nil)
          (tag nil))
      (if (numberp (cdr m))
          (setf n-consumed (cdr m))
          (setf n-consumed (cadr m)
                tag (cddr m)))
      (add-partial-parse-to-array
       (partial-parse-rule p)
       (cdr (partial-parse-upto p))
       (1+ (partial-parse-pos p))
       (partial-parse-start p)
       p
       ;; currently always take the input words, may change later
       (make-matched-terminal :tag tag
                              :val (take-matched-word words n-consumed)
                              :len n-consumed)
       s (+ i n-consumed) penalty))))

(defun earley-helper (start-nt input)
  ;; start-nt is non-terminal of the start symbol
  ;; input is a list of things
  ;; returns the array of list of partial parses at each position
  (let* ((*seq-id* 0) ;; reset the sequential id
         (L (length input))
         (s (make-array (1+ L) :initial-element nil)))
    ;; s[i] is the set of partial parses at position i
    ;; for i from 0 to L
    ;; empty sets of parses
    (dotimes (i (1+ L)) (setf (aref s i) (make-empty-parses)))
    ;; initialize, all rules for the start symbol
    (let ((q (aref s 0)))
      (dolist (r (non-terminal-rules start-nt))
        (parses-insert-new q (make-partial-parse
                              :seq-id (next-seq-id)
                              :rule r :upto (rule-body r)
                              :start 0 :len 0 :pos 0))))
    ;; loop through
    (do ((i 0 (1+ i))
         (str input (cdr str)))
        ((> i L))
      (let ((ps (aref s i)))
        (do ((p (parses-next ps) (parses-next ps)))
            ((null p))
          ;; note that (aref s i) may change during the loop
          (unless (member (partial-parse-processed? p)
                          '(processed shadowed))
            (let ((upto (partial-parse-upto p)))
              (cond ((null upto)
                     (completed p s i))
                    ((non-terminal-p (car upto))
                     (predict p upto s i))
                    ((< i L) ;; terminal
                     (scan p (car upto) str s i)))
              (unless (eq (partial-parse-processed? p) 'shadowed)
                (setf (partial-parse-processed? p) 'processed)))))
        ;; remove the shadowed completed parses, to relief memory problem
        (setf (parses-completed ps)
              (delete-if #'(lambda (p) (eq 'shadowed (partial-parse-processed? p)))
                         (parses-completed ps)))
        ))
    ;; done
    s))

(defun whole-match? (start-nt p)
  (and (= (partial-parse-start p) 0)
       (null (partial-parse-upto p))
       (eq start-nt (partial-parse-head p))))

(defun earley-recognizer (iG start-sym input)
  ;; iG is the internalized grammar (as returned by internalize-grammar)
  ;; start-sym is the start symbol
  ;; input is a list of things
  ;; returns whether the start symbol can generate the input

  ;; important to set *max-unused-rule-id*, so that partial-parse-key
  ;; work properly. important to set *max-unused-non-terminal-id*, so
  ;; that partial-parse-head-key-of work properly
  (let* ((*max-unused-rule-id* (car iG))
         (*max-unused-non-terminal-id* (hash-table-count (cdr iG)))
         (start-nt (gethash start-sym (cdr iG)))
         (s (earley-helper start-nt input)))
    (some #'(lambda (p)
              (whole-match? start-nt p))
          (parses-completed (aref s (- (length s) 1))))))

;;; (earley-recognizer (get-non-terminal 'S *sample-G2*) '(a - a + a))

;;;
(declaim (inline single? tag-partial-parse?
                 may-have-alternatives))
(defun single? (x) (null (cdr x)))
(defun tag-partial-parse? (p)
  (and (null (partial-parse-upto p))
       (not (context-free? (partial-parse-rule p)))))
(defun may-have-alternatives (ps)
  (if (single? ps) (car ps) (cons :@ ps)))

;;
(defparameter *CTB-output* nil) ;; to use tval instead of val, to give output compatible with CTB
;; for outputing more details
(defparameter *subtree-completed-parse* nil)
(defparameter *output-detail* nil) ;; give more information of the partial parse
(defparameter *debug-parse* nil)
(defstruct detail-info
  name
  penalty
  tag)
(defun output-sub-parse-head (p)
  (if *output-detail*
      (make-detail-info
       :name (non-terminal-name (partial-parse-head p))
       :penalty (partial-parse-penalty *subtree-completed-parse*)
       :tag (if (tag-partial-parse? *subtree-completed-parse*)
                (mapcar #'car (partial-parse-tag *subtree-completed-parse*))
                (partial-parse-tag *subtree-completed-parse*)))
      (non-terminal-name (partial-parse-head p))))
(defun one-parse-subtree (p partial tags lens output)
  ;; p is a partial-parse, to get the rule action and head
  ;; partial is the accumulated subtrees
  ;; lens is the list of lengths of spans of the subtrees
  ;; tags is the corresponding tags for the subtrees, could be nil if *CTB-output* is nil.
  (let* ((act (if *CTB-output*
                  (rule-tval (partial-parse-rule p))
                  (rule-val (partial-parse-rule p))))
         (sub (cons (output-sub-parse-head p)
                    partial))
         (sub-val (cond ((eq act #'identity) sub)
                        ((functionp act)
                         (if *CTB-output*
                             (funcall act sub tags lens)
                             (funcall act sub lens)))
                        (t act))))
    (when *debug-parse*
      (format t "~%*** Debug: ~A~%" p)
      (debug-print-partial-parse p)
      (format t "~A~%" sub-val))
    (funcall output sub-val)))

(defun trace-tag-alternative (p alt output)
  ;; one alternative of one tag.
  ;; see defstruct of partial-parse for the structure of one alternative.
  (let ((*subtree-completed-parse* p))
    (one-parse-subtree
     p
     (mapcar #'(lambda (x)
                 (cond ((partial-parse-p x)
                        (trace-sub-parse x))
                       ((matched-terminal-p x)
                        (matched-terminal-val x))
                       ((partial-parse-p (car x))
                        ;; (partial-parse tag . alternatives)
                        (let ((ps nil))
                          (dolist (y (cddr x)) ;; sub-alternatives
                            (trace-tag-alternative (car x)
                                                   y
                                                   #'(lambda (r) (push r ps))))
                          (may-have-alternatives ps)))
                       (t ;; should not happen
                        x)))
             alt)
     (if *CTB-output*
         (mapcar #'(lambda (x)
                     (cond ((partial-parse-p x)
                            ;; should have only one tag, if at all
                            (partial-parse-tag x))
                           ((matched-terminal-p x)
                            (matched-terminal-tag x))
                           ((partial-parse-p (car x))
                            ;; (partial-parse tag . alternatives)
                            (cadr x))
                           (t ;; should not happen
                            nil)))
                 alt)
         nil)
     (mapcar #'(lambda (x)
                 (cond ((partial-parse-p x)
                        ;; should have only one tag, if at all
                        (partial-parse-len x))
                       ((matched-terminal-p x)
                        (matched-terminal-len x))
                       ((partial-parse-p (car x))
                        ;; (partial-parse tag . alternatives)
                        (partial-parse-len (car x)))
                       (t ;; should not happen
                        0)))
             alt)
     output)))
(defun trace-tag-parses (p output)
  ;; p is the completion partial parse which is not context-free
  ;; traces through its allowed (recorded) tag combinations
  (when *debug-parse*
    (format t "~%*** trace-tag-parses: Debug: ~A~%" p)
    (debug-print-partial-parse p)
    (terpri))
  (dolist (x (partial-parse-tag p)) ;; (tag . alternatives)
    (assert (not (null (cdr x))) () "Not alternatives in trace-tag-parses")
    (dolist (alt (cdr x)) ;; alternatives
      (trace-tag-alternative p alt output))))

(defun trace-sub-parse (p)
  ;; p is a partial parse representing a non-terminal, trace it from the end
  ;; and output a list of (:@ subtree1 subtree2 ...) if there are more than one alternative.
  ;; If there is only one alternative, simply output that alternative
  (when *debug-parse*
    (format t "~%*** trace-sub-parses: Debug: ~A~%" p)
    (debug-print-partial-parse p)
    (terpri))
  (let ((ps nil))
    (trace-parses p #'(lambda (r) (push r ps)))
    (may-have-alternatives ps)))
(defun trace-CF-parses (p partial lens output)
  ;; Traces the context-free node p
  ;; partial is the current tail already traced
  (when *debug-parse*
    (format t "~%*** trace-CF-parses: Debug: ~A~%" p)
    (debug-print-partial-parse p)
    (terpri))
  (let ((prev (partial-parse-prev p)))
    (if (null prev)
        ;; done the (sub) parse
        (one-parse-subtree p partial nil lens output)
        (dolist (L prev)
          (trace-CF-parses (link-from L)
                           (cons (if (partial-parse-p (link-through L))
                                     ;; through completion, trace its sub-parses
                                     (trace-sub-parse (link-through L))
                                     ;; through matched-terminal, use the terminal in it
                                     (matched-terminal-val (link-through L)))
                                 partial)
                           (cons (if (partial-parse-p (link-through L))
                                     ;; through completion, get its length of span
                                     (partial-parse-len (link-through L))
                                     ;; through matched-terminal, use the terminal in it
                                     (matched-terminal-len (link-through L)))
                                 lens)
                           output)))))

(defun trace-parses (p output)
  ;; This always gives one parse, and if there are alternatives at any
  ;; subtree, it would be contained in (:@ subtree1 subtree2 ...)

  ;; TODO: avoid getting into infinite loop because of circular unit rules, e.g. A -> A
  ;; p is the partial parse
  ;; output is the function to output a parse for p
  (when *debug-parse*
      (format t "~%*** trace-parses: Debug: ~A~%" p)
      (debug-print-partial-parse p)
      (terpri))
  (let ((*subtree-completed-parse* p))
    (if (tag-partial-parse? p)
        (trace-tag-parses p output)
        (trace-CF-parses p nil nil output))))

(defparameter *show-all-parses* nil)
(defun earley-parser (iG start-sym input)
  ;; iG is the internalized grammar (as returned by internalize-grammar)
  ;; start-sym is the start symbol
  ;; input is a list of things
  ;; returns the list of parses where the start symbol generates the input, and nil if none exists

  ;; important to set *max-unused-rule-id*, so that partial-parse-key
  ;; work properly. important to set *max-unused-non-terminal-id*, so
  ;; that partial-parse-head-key-of work properly
  (let* ((*max-unused-rule-id* (car iG))
         (*max-unused-non-terminal-id* (hash-table-count (cdr iG)))
         (start-nt (gethash start-sym (cdr iG)))
         (s (earley-helper start-nt input))
         (ps nil)
         (best-score nil))
    ;; trace the parses, by following the prev pointers, and use
    ;; matched pointers to get the sub parts of the completed
    ;; non-terminals.
    ;; find the maximum score first
    (dolist (p (parses-completed (aref s (- (length s) 1))))
      (when (whole-match? start-nt p)
        (setf best-score
              (if (null best-score)
                  (partial-parse-score p)
                  (max (partial-parse-score p) best-score)))))
    ;;
    (when best-score
      (dolist (p (parses-completed (aref s (- (length s) 1))))
        (when (and (whole-match? start-nt p)
                   (or *show-all-parses*
                       (= best-score (partial-parse-score p))))
          (trace-parses p #'(lambda (r) (push r ps))))))
    ;;
    (values (may-have-alternatives ps) best-score s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun earley-parses (iG start-sym input)
  ;; iG is the internalized grammar (as returned by internalize-grammar)
  ;; start-sym is the start symbol
  ;; input is a list of things
  ;; returns the array of partial parses, for debug purpose
  (let* ((*max-unused-rule-id* (car iG)) ;; important to set this, so that partial-parse-key work properly
         (start-nt (gethash start-sym (cdr iG))))
    (earley-helper start-nt input)))

(defmacro debug-parse (&body body)
  `(let (;;(*take-first-p* nil)
         ;;(*output-detail* t)
         (*debug-parse* t))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (earley-parser *sample-G2* 'S '(a - a + a))
;;; (earley-parser *sample-G3* 'S '(x x))
;;; (earley-parser *sample-G2* 'S '(a - a * a / a))
;;; (earley-parser *sample-G2* 'S '(a - a  a / a))

;; for sbcl when loading utf-8 encoded file
(setf sb-impl::*default-external-format* :utf-8)

;;
;; in score, nil means prohibited, similar to -inf
