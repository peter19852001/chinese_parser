(in-package :chinese-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For CTB output
(defun single? (x) (null (cdr x)))

(defmacro def-pos-tag (tag-name pos-tag)
  `(defparameter ,tag-name (t-str ',pos-tag)))

(def-pos-tag *pos-SB* SB)
(def-pos-tag *pos-VA* VA)
(def-pos-tag *pos-VV* VV)
(def-pos-tag *pos-JJ* JJ)
(def-pos-tag *pos-M* M)
(def-pos-tag *pos-AD* AD)
(def-pos-tag *pos-PN* PN)
(def-pos-tag *pos-PU* PU)
(def-pos-tag *pos-AS* AS)
(def-pos-tag *pos-SP* SP)
(def-pos-tag *pos-DEV* DEV)
(def-pos-tag *pos-MSP* MSP)
(def-pos-tag *pos-LC* LC)
(def-pos-tag *pos-LB* LB)
(def-pos-tag *pos-NR* NR)
(def-pos-tag *pos-NT* NT)
(def-pos-tag *pos-NN* NN)
(def-pos-tag *pos-CS* CS)
(def-pos-tag *pos-P* P)
(def-pos-tag *pos-CD* CD)
(defparameter *pos-CD-or-QP*
  ;; the thing may be already a QP
  (tfn (if (headed-by x1 'QP)
           x1
           (tag-as 'CD args))))

(def-pos-tag *pos-OD* OD)
(def-pos-tag *pos-CC* CC)
(def-pos-tag *pos-ON* ON)
(def-pos-tag *pos-IJ* IJ)
(def-pos-tag *pos-FW* FW)
(def-pos-tag *pos-DT* DT)

(defparameter *pos-ADVP-AD*
  (tfn `(ADVP ,(tag-as 'AD args))))
(defparameter *pos-ADVP-CS*
  (tfn `(ADVP ,(tag-as 'CS args))))

(defparameter *pos-ADJP-JJ*
  (tfn `(ADJP ,(tag-as 'JJ args))))
(defparameter *pos-NP-NN*
  (tfn `(NP ,(tag-as 'NN args))))
(defparameter *pos-CLP-M*
  (tfn `(CLP ,(tag-as 'M args))))

(defparameter *pos-NN-or-NR*
  (tfn (if (and (consp x1)
                (eq (car x1) 'NR))
           x1
           (list 'NN (collapse-str args)))))
(defun mono-p (x) (= (strs-len x) 1))
(defun empty-p (x) (= (strs-len x) 0))
(defun both-mono-p (x1 x2)
  (and (= (strs-len x1) 1)
       (= (strs-len x2) 1)))

(defun headed-by (x tag)
  (and (consp x) (eq (car x) tag)))
(defun headed-by-any (x &rest tags)
  (and (consp x) (member (car x) tags)))
(defun as-head (x tag)
  (cond ((headed-by x tag) x)
        ((headed-by x :splice) (cons tag (cdr x)))
        (t (list tag x))))
(defun as-lst-head (lst tag)
  (if (null (cdr lst))
      (as-head (car lst) tag)
      (cons tag lst)))

(defun spliced-car (lst)
  ;; lst is a list, possibly with (:splice ...) which should be treated as if it is at the same level as the enclosing list
  (let ((x (if (headed-by lst :splice)
               (second lst)
               (car lst))))
    (if (headed-by x :splice)
        (spliced-car x)
        x)))

(defun spliced-headed-by (x tag)
  (if (headed-by x :splice)
      (headed-by (spliced-car x) tag)
      (headed-by x tag)))

(defun spliced-lst (lst)
  (if (headed-by lst :splice)
      (cdr lst)
      lst))

(defun spliced-some (pred lst)
  ;; analogous to some, but reduced
  ;; lst is a list, possibly with (:splice ...) which should be treated as if it is at the same level as the enclosing list
  (some #'(lambda (x)
            (if (headed-by x :splice)
                (spliced-some pred (cdr x))
                (funcall pred x)))
        (spliced-lst lst)))

(defun spliced-every (pred lst)
  ;; analogous to every, but reduced
  ;; lst is a list, possibly with (:splice ...) which should be treated as if it is at the same level as the enclosing list
  (every #'(lambda (x)
            (if (headed-by x :splice)
                (spliced-every pred (cdr x))
                (funcall pred x)))
        (spliced-lst lst)))

(defun spliced-for-each (func lst)
  ;; lst is a list, possibly with (:splice ...) which should be treated as if it is at the same level as the enclosing list
  (dolist (x (if (headed-by lst :splice)
                 (cdr lst)
                 lst))
    (if (headed-by x :splice)
        (spliced-for-each func (cdr x))
        (funcall func x))))

(defun spliced-mapcar (func lst)
  ;; analogous to mapcar, but reduced
  ;; lst is a list, possibly with (:splice ...) which should be treated as if it is at the same level as the enclosing list
  ;; return a flat list
  (let ((res nil))
    (labels ((walk (ls)
               (dolist (x (spliced-lst ls))
                 (if (headed-by x :splice)
                     (walk (cdr x))
                     (push (funcall func x) res)))))
      (walk lst)
      (nreverse res))))

(defparameter *may-be-NR*
  (tfn (list (if (typep t1 'nr-name)
                 'NR
                 'NN)
             (collapse-str args))))

(defun NP-head-p (x)
  (and (consp x) (eq (car x) 'NP)))

(defun head-of (x)
  (if (headed-by x :splice)
      (head-of (car (last x)))
      x))
(defun head-tag-of (x)
  ;; the head should be (tag ...)
  (car (head-of x)))
(defun phrase-level-head (x &optional default-head)
  (case x
    ((ADJP ADVP CLP DP DNP DVP FRAG LCP LST
           NP PP PRN QP UCP VP IP)
     ;; already phrase level
     x)
    ;; simple cases
    ((JJ) 'ADJP)
    ((AD) 'ADVP)
    ((M) 'CLP)
    ((DT) 'DP) ;; when DT is used alone
    ((DEG) 'DNP)
    ((DEV) 'DVP)
    ((LC) 'LCP)
    ((VV VA) 'VP)
    ((CD) 'QP)
    ((NR NN NT PN) 'NP)
    ;; not sure how to handle other cases
    (t (or default-head x))))
(defun as-phrase-level (x &optional default-head)
  (let* ((h (head-tag-of x))
         (ph (phrase-level-head h default-head)))
    (if (eq (car x) ph)
        x ;; already phrase level
        `(,ph ,x))))

(defun ctb-JJ-to-VA (x)
  (sublis '((JJ . VA) (ADJP . VP))
          x
          :test #'eq))

(defun ctb-VA-to-JJ (x)
  (sublis '((VA . JJ) (VP . ADJP))
          x
          :test #'eq))

(defun ctb-VP-to-NP (x)
  ;; TODO: may need to handle compound verb
  (sublis '((VP . NP) (VA . NN) (VV . NN))
          x
          :test #'eq))

(defun ctb-VP-to-IP (x)
  ;; leave other heads alone
  (if (headed-by x 'VP)
      (as-head x 'IP)
      x))

(defun ctb-contract-chain-head (x)
  ;; e.g. (IP (IP xxx)) -> (IP xxx)
  (if (and (single? x)
           (single? (second x))
           (eq (car x) (car (second x))))
      (second x)
      x))
(defun ctb-IP-VP-JJ-to-NP (x)
  (ctb-contract-chain-head
   (sublis '((VP . NP) (VA . NN) (VV . NN)
             (ADJP . NP) (JJ . NN)
             (IP . NP))
           x
           :test #'eq)))

(defun ctb-SP-to-IJ (x)
  (sublis '((SP . IJ))
          x
          :test #'eq))

(defun NP-with-DNP-p (x)
  (and (headed-by x 'NP)
       (headed-by (second x) 'DNP)))
(defun as-head-tag-or-phrase-level (x head-tag &optional (default-head 'NP))
  (if (eq (head-tag-of x) head-tag)
      x
      (as-phrase-level x default-head)))
(defparameter *ctb-mod-noun*
  ;; to decide how to output CTB format for mod-noun constructs
  (tfn ;; a subtree is regarded as monosyllabic if it has length one
    (cond ((and (typep t1 'nr-name)
                (not (typep t1 'dynasty-name))
                (typep t2 'ind))
           (list 'NR (collapse-str args)))
          ;; 全区 should be (NP (DP (DT "全")) (NP (NN "区")))
          ;; 全省 should be (NP (DP (DT "全")) (NP (NN "省")))
          ((and (typep t1 'p-pronoun)
                (typep t2 'place)
                (not (typep t2 'country-ind)))
           `(NP ,(as-phrase-level x1) ,(as-phrase-level x2 'NP)))
          ;; not collapse if x1 is number, e.g. '两国' gives '(QP (CD "两")) (NP (NN "国"))'
          ((or (numberp t1) (typep t1 'num) (headed-by x1 'CD))
           `(:splice ,(as-head x1 'QP) ,(as-head x2 'NP)))
          ;; need to collapse, e.g. 上海 + 市 ==> 上海市
          ((or (and
                (typep t2 'ind)
                (or (typep t2 'noun-suffix)
                    (typep t2 'human-suffix)
                    (typep t2 'occupation-suffix)
                    (typep t2 'abstract-suffix)
                    (typep t2 'animate-suffix)))
               (and (typep t2 'ind)
                    (not (or (typep t1 'pronoun)
                             (typep t1 'q-pronoun)
                             (typep t1 'p-pronoun)
                             ;; e.g. '被偷拍的人' should not be one word
                             (typep t1 'de-noun-mod)
                             (typep t1 'unit)))
                    (mono-p x2)))
           (list 'NN (collapse-str args)))
          ;;
          ((typep t1 'adj)
           (if (and (= L1 1) (= L2 1))
               ;; X+N -> one word if X is non-predicate adjective
               ;; and both X and N are monosyllabic
               (list 'NN (collapse-str args))
               `(:splice ,(as-head (ctb-VA-to-JJ x1) 'ADJP) ,(as-head x2 'NP))))
          ;;
          ((or (and (<= L1 2)
                    (typep t1 'verb))
               (headed-by x1 'VV))
           ;; convert short verb mod to NN
           `(:splice ,(tag-as 'NN x1) ,x2))
          ;;
          ((typep t1 'dir) ;; LC+N
           (if (and (= L1 1) (= L2 1))
               (list 'NN (collapse-str args))
               (cons :splice (cdr args))))
          ;;
          ((typep t1 'adj-has-de)
           `(:splice ,x1 ,@(if (NP-with-DNP-p x2)
                               (cdr x2)
                               (list (as-head x2 'NP)))))
          ((or (conn-thing-p t1)
               (headed-by-any x1 'ADJP 'PP))
           `(:splice ,(as-phrase-level x1 'NP)
                     ,(as-head x2 'NP)))
          ;;
          ((or (typep t1 'unit) ;; including num-unit
               (headed-by-any x1 'QP 'CP))
           `(:splice ,x1
                     ;; if second one as CP or other mod already, just add QP to the front.
                     ,(as-head-tag-or-phrase-level x2 'NP 'NP)))
          ;;
          ((and (> (trait-value t2 'n-base-nouns 1) 1)
                (trait-value t1 'has-etc))
           `(:splice ,(as-head x1 'NP) ,(as-head x2 'NP)))
          ;;
          ((headed-by-any x1 'VP 'IP)
           ;; verb-x or subj-pred as mod
           `(:splice (CP ,(as-head x1 'IP))
                     ,(as-head-tag-or-phrase-level x2 'NP 'NP)))
          ;;
          ((eq (head-tag-of x2) 'NP)
           ;; second one at NP, promote the first
           `(:splice ,(as-phrase-level x1 'NP) ,x2))
          ((eq (head-tag-of x1) 'NP)
           ;; first at NP, promote the second
           `(:splice ,(as-phrase-level x1 'NP)
                     ,(as-head x2 'NP)))
          ;;
          (t (cons :splice (cdr args))))))

(defparameter *ctb-mod-de-noun*
  ;; x1 zhi x3
  (tfn (let ((tail (if (NP-with-DNP-p x3)
                       (cdr x3)
                       (list (as-head-tag-or-phrase-level x3 'NP 'NP)))))
         (cond ((NP-with-DNP-p x1)
                (let ((head-nn (car (last x1))))
                  `(,@(butlast x1)
                      (DNP ,(as-phrase-level head-nn 'NP)
                           ,x2)
                      ,@tail)))
               ((headed-by-any x1 'VP 'IP)
                `(NP
                  (CP ,(as-phrase-level x1 'IP)
                      ,(tag-as 'DEC x2))
                  ,@tail))
               (t
                ;; x1 is simple NP
                `(NP (DNP ,(as-phrase-level x1 'NP)
                          ,x2)
                     ,@tail))))))
(defun ctb-simple-noun-p (x)
  (headed-by-any x 'NN 'NR 'NT))
(defun ctb-single-name-p (x)
  (or (headed-by x 'NR)
      (and (headed-by x 'NP)
           (null (cddr x))
           (headed-by (second x) 'NR))))
(defun ctb-all-simple-noun-p (x)
  (and (headed-by-any x :splice 'NP)
       (spliced-every #'ctb-simple-noun-p (cdr x))))
(defun ctb-simple-NNs (x)
  ;; one or more simple noun's
  (or (ctb-simple-noun-p x)
      (ctb-all-simple-noun-p x)))
(defun compound-NP-p (x)
  (and (headed-by x 'NP)
       (not (ctb-simple-noun-p (spliced-car (cdr x))))))

(defun as-NP-or-NNs (x)
  (if (ctb-simple-NNs x)
      x
      (as-phrase-level x 'NP)))

(defun without-head (x)
  (cond ((and (consp x)
              (symbolp (car x)))
         (cdr x))
        (t x)))
(defun ctb-mod-noun-p (x1 x2 &optional t1 t2)
  ;; TODO: write this in more organized way?
  (cond ((or (conn-thing-p t1)
             (conn-thing-p t2))
         `(NP ,(as-phrase-level x1 'NP)
              ,(as-phrase-level x2 'NP)))
        ((and (ctb-single-name-p x1)
              (ctb-all-simple-noun-p x2))
         ;; merge them
         (if (headed-by x1 'NR)
             ;; (NR ?) and (NP (NN ?) ...)
             `(NP ,x1 ,@(without-head x2))
             ;; (NP (NR ?)) and (NP (NN ?) ...)
             (append x1 (without-head x2))))
        ;;
        (t (let ((tail (cond ((compound-NP-p x2)
                              (cdr x2))
                             ((headed-by x2 'DP)
                              ;; "今年 全年" => (NP (NP (NT "去年")) (DP (DT "全") (CLP (M "年"))))
                              (list x2))
                             (t (list (as-head x2 'NP))))))
             (if (and (compound-NP-p x1)
                      (not (and (headed-by x1 'NP)
                                (headed-by (second x1) 'DP))))
                 (append x1 tail)
                 ;; x1 is simple NP, or should be kept as a whole, e.g. (NP (DP (DT "全")) (NP (NN "区")))
                 `(NP ,(as-phrase-level
                        (cond ((headed-by x1 'VV)
                               (ctb-VP-to-NP x1))
                              ((headed-by x1 'VA)
                               (ctb-VA-to-JJ x1))
                              (t x1)))
                      ,@tail))))))
(defparameter *ctb-mod-noun-p*
  ;; x1 x2
  (tfn (ctb-mod-noun-p x1 x2 t1 t2)))

(defparameter *ctb-splice-to-NP*
  (tfn (if (headed-by x1 :splice)
		   (as-head x1 'NP)
		   x1)))
(defparameter *ctb-adv-num-unit*
  (tfn (if (and (headed-by x2 'QP)
                (spliced-headed-by (second x2) 'ADVP))
           `(QP ,(as-head x1 'ADVP) ,@(cdr x2))
           `(QP ,(as-head x1 'ADVP) ,x2))))
(defparameter *ctb-splice*
  (tfn (cons :splice (cdr args))))
(defparameter *ctb-long-LB*
  (tfn (cons 'LB (cdr args))))
(defparameter *ctb-append-etc*
  (tfn (append x1 (list x2))))
(defparameter *ctb-N-CC-N*
  (tfn (ctb-N-CC-N x1)))
(defparameter *ctb-NP*
  (tfn (as-lst-head (cdr args) 'NP)))
(defparameter *ctb-as-NP*
  (tfn (as-head x1 'NP)))
(defparameter *ctb-ADJP*
  (tfn (as-head (ctb-VA-to-JJ x1) 'ADJP)))
(defparameter *ctb-ADVP*
  (tfn (as-head x1 'ADVP)))
(defparameter *ctb-PP*
  (tfn (as-lst-head (cdr args) 'PP)))
(defparameter *ctb-as-VP*
  (tfn (as-head x1 'VP)))
(defparameter *ctb-VP*
  (tfn (as-lst-head (cdr args) 'VP)))
(defparameter *ctb-DVP*
  (tfn (as-lst-head (cdr args) 'DVP)))
(defparameter *ctb-IP*
  (tfn (as-lst-head (cdr args) 'IP)))
(defparameter *ctb-as-IP*
  (tfn (as-head x1 'IP)))
(defparameter *ctb-LST*
  (tfn (as-lst-head (cdr args) 'LST)))
(defparameter *ctb-PRN*
  (tfn (as-lst-head (cdr args) 'PRN)))
(defparameter *ctb-CP*
  (tfn (as-lst-head (cdr args) 'CP)))
(defparameter *ctb-LCP*
  (tfn (as-lst-head (cdr args) 'LCP)))

(defparameter *pp-verb-mod-t1-t2*
  (tn (make-pp-verb-mod :pp t1 :object t2)))
(defparameter *pp-verb-mod-t1-t2-prefer-pause*
  (tn (make-pp-verb-mod :pp t1 :object t2 :prefer-pause t)))
(defparameter *may-pp-verb-mod-t1-t2*
  (tn (if t1 (make-pp-verb-mod :pp t1 :object t2) nil)))

(defun one-pred-not-prefer-conn (v)
  (+ (not-prefer-trait v 'has-clause-supp *c3*)
     (not-prefer v 'conn-thing *c-close-suffix*)))
(defparameter *one-pred-not-prefer-conn-t2*
  ;; to encourage flatter structure of conn-thing
  (tn (one-pred-not-prefer-conn t2)))
(defparameter *one-pred-not-prefer-conn-t3*
  ;; to encourage flatter structure of conn-thing
  (tn (one-pred-not-prefer-conn t3)))

(defun walk-tree-nodes (s f)
  ;;; apply f to every node of the tree s,
  ;;; where node means a cons cell with the car being a symbol
  (when (consp s)
    (when (symbolp (car s))
      (funcall f s))
    (dolist (x s)
      (walk-tree-nodes x f))))
(defun ctb-frag-pieces (lst)
  (let ((ls nil))
      (walk-tree-nodes
       lst
       #'(lambda (x)
           (when (stringp (second x))
             (push x ls))))
      `(FRAG ,@(nreverse ls))))
(defparameter *ctb-frag*
  ;; give a linear list of simple parts (e.g. NN, NR, NT, VV, ...), basically anyting (symbol "str")
  (tfn (ctb-frag-pieces (cdr args))))
(defparameter *ctb-as-frag*
  (tfn (as-lst-head (cdr args) 'FRAG)))
(defparameter *ctb-quote-as-phrase*
  ;; L x R
  ;; PRN for "(",
  ;; use the phrase level head of x otherwise
  (tfn (as-lst-head (cdr args)
                    (if (eq t1 'L-paren)
                        'PRN
                        (phrase-level-head x2 'NP)))))
(defparameter *ctb-quote-splice*
  ;; L x R
  ;; PRN for "(",
  ;; splice otherwise
  (tfn (as-lst-head (cdr args)
                    (if (eq t1 'L-paren)
                        'PRN
                        :splice))))

(defun chain-VP-p (x)
  ;; (VP (VP ..) ..)
  (and (headed-by x 'VP)
	   (headed-by (second x) 'VP)))
(defparameter *ctb-VP-VP*
  ;; x1 x2
  ;; chain simple VPs together into one VP
  (tfn (let ((tail (if (chain-VP-p x2)
					   (cdr x2)
					   (list (as-head x2 'VP)))))
		 (if (chain-VP-p x1)
			 (append x1 tail)
			 `(VP ,(as-head x1 'VP)
				  ,@tail)))))

(defparameter *ctb-place-conn*
  ;; only one x1
  (tfn (if (headed-by x1 :splice)
		   (let ((h-tag (car (spliced-car x1))))
			 (as-head x1 (if (eq h-tag 'LCP)
							 'LCP
							 'NP)))
		   x1)))

(defun promote-CC-segments (lst)
  ;; lst is spliced, with segments separated by CC's or PU's.
  ;; collect and promote each segment to phrase level.
  (let ((body nil)
        (ph nil)
        (SS nil))
    (spliced-for-each
     #'(lambda (x)
         (cond ((headed-by-any x 'CC 'PU)
                ;; done one segment
                (when SS
                  ;; if only one item, use it to promote to ph,
                  ;; otherwise, use head for the reverse list.
                  (push (as-lst-head (nreverse SS) ph)
                        body)
                  (setf SS nil
                        ph nil))
                (push x body))
               ((null SS)
                ;; first one in a segment
                (setf ph (phrase-level-head (car x) 'NP))
                (push x SS))
               (t
                ;; continue a segment
                (push x SS))))
     lst)
    ;;
    (when SS
      ;; clear accrued segment
      (push (as-lst-head (nreverse SS) ph)
            body)
      (setf SS nil
            ph nil))
    ;;
    `(:splice ,@(nreverse body)))
  )

(defparameter *ctb-maybe-UCP*
  ;; x1 maybe a splice of conn, if they have different CTB heads in
  ;; conn, use UCP, otherwise use the common head.

  ;; If it is splice, but not conn, still just give the splice, this
  ;; is the case for 'noun-p-a -> p-pronoun-qi2-zhong1 num-unit'.
  (tfn (if (headed-by x1 :splice)
		   (let ((h nil)
                 (ph nil) ;; the phrase-level head of h
                 (need-promotion? nil)
                 (different-heads? nil)
                 (has-conn-or-PU-p nil))
			 (spliced-for-each
			  #'(lambda (x)
				  (cond ((or (eq (car x) 'PU)
							 (eq (car x) 'CC))
                         (setf has-conn-or-PU-p t)
						 ;; ignore the middle connectives
						 )
						((null h)
						 (setf h (car x)
                               ph (phrase-level-head (car x) 'NP)))
                        ((and
                          (not (eq h (car x)))
                          (eq ph (car x)))
                         ;; has both phrase level and base level head,
                         ;; need to promote all others to phrase
                         ;; level.
                         (setf need-promotion? t))
						((not (eq ph
                                  (phrase-level-head (car x) 'NP)))
                         ;; different phrase level heads
						 (setf different-heads? t))))
			  x1)
             ;;
             (if has-conn-or-PU-p
                 (as-head (if need-promotion?
                              (promote-CC-segments x1)
                              x1)
                          (if (or (null h) different-heads?)
                              'UCP
                              ph))
                 x1))
           ;;
		   x1)))

(defun ctb-N-CC-N (lst)
  ;; lst may not even have CC
  ;; some N may be NP, if so, make all of them NP
  (cond ((not (headed-by lst :splice))
         lst)
        ((not (spliced-some #'(lambda (x)
                                (headed-by x 'CC))
                            lst))
         lst)
        ((spliced-some #'NP-head-p lst)
         `(:splice
           ,(spliced-mapcar
             #'(lambda (x)
                 (if (or (headed-by x 'CC)
                         (headed-by x 'NP))
                     x
                     (as-head x 'NP)))
             lst)))
        (t lst)))

(defparameter *ctb-NN-chain-to-NP*
  ;; x1 is possibly a chain of things, some of which are NN, NR or NT
  ;; properly segment them
  (tfn (cond ((not (headed-by x1 :splice))
              ;; a single noun
              x1)
             ((spliced-some #'(lambda (x)
                                (not (ctb-simple-noun-p x)))
                            x1)
              ;; segment properly, promote to phrase level as needed
              ;; group NNs together into one NP
              (let ((NP-body nil)
                    (NNs nil))
                (spliced-for-each
                 #'(lambda (x)
                     (cond ((ctb-simple-noun-p x)
                            (push x NNs))
                           ;; either things like DNP, ADJP, QP, or NP
                           (t (when NNs
                                ;; clear accrued NNs
                                (push `(NP ,@(nreverse NNs)) NP-body)
                                (setf NNs nil))
                              (push (as-phrase-level x) NP-body))))
                 x1)
                ;;
                (when NNs
                  ;; clear accrued NNs
                  (push `(NP ,@(nreverse NNs)) NP-body)
                  (setf NNs nil))
                ;;
                `(NP ,@(nreverse NP-body))))
             (t ;; simple chain of NNs
              (as-head x1 'NP)))))

(defparameter *place-with-dir*
  (tfn (if (and (mono-p x1)
                (or (typep t2 'dir-opposite-pair)
                    (mono-p x2)))
           (list 'NN (collapse-str args))
           (cons 'LCP (cdr args)))))

(defparameter *time-with-F-B*
  (tfn (if (both-mono-p x1 x2)
           (list 'NT (collapse-str args))
           `(LCP ,(as-phrase-level x1)
				 ,x2))))

(defparameter *time-with-ind-suffix*
  (tfn `(LCP ,(as-phrase-level x1) ,x2)))
(defparameter *time-with-ind-prefix*
  (tfn `(PP ,x1 ,(as-phrase-level x2))))

(defparameter *ctb-verb-verb*
  (tfn (cond ((and (not (typep t2 'v-pass))
                   (not (typep t2 'verb-close-supp-zhe))
                   (or (typep t2 'dir-verbs)
                       (typep t2 'verb-close-supp)
                       (typep t2 'v-der)
                       (typep t2 'v-goto)
                       (typep t2 'v-give))
                   (mono-p x2)
                   (mono-p x1))
              (tag-as 'VV x1 x2))
             ((typep t2 'adj)
              `(VRD ,x1 ,(tag-as 'VA x2)))
             ((or (typep t2 'v-goto)
                  (typep t2 'dir-verbs)
                  (typep t2 'v-cheng2-wei4)
                  (typep t2 'v-for)
                  (typep t2 'v-give))
              `(VRD ,x1 ,x2))
             ;; TODO: VSB?
             ((headed-by x2 'VC)
              `(VCP ,x1 ,x2))
             ((or (headed-by x2 'DER)
                  (headed-by x2 'AS))
              `(:splice ,x1 ,x2))
             ;; TODO: more cases for VCD?
             (t `(VCD ,x1 ,x2)))))

(defparameter *ctb-verb-mod-verb-p*
  (tfn
    (cond ((or (headed-by x1 'BA)
               (headed-by x1 'LB))
           ;; e.g. (BA (BA 把) noun-p)
           ;; e.g. (LB (LB 被) noun-p)
           ;; e.g. (LB (LB 被) noun-p (MSP 所))
           `(VP ,(second x1)
                (IP ,(third x1)
                    ,(if (fourth x1)
                         `(VP ,(fourth x1)
                              ,x2)
                         x2))))
          ((headed-by x1 :verb-mod-cross)
           `(VP ,(second x1)
                (VP ,(third x1) ,x2)))
          ((and (spliced-headed-by x1 'PP)
                (headed-by (spliced-car x2) 'PP))
           ;; stacks of PP are on the same level in CTB!
           ;; Note that time could also be PP, so check this before checking for time
           (cons :splice (cdr args)))
          ((typep t1 'time-name)
           ;; time as verb-mod, turn the second into VP, then just piece the time (may have pause) at the front
           (list :splice x1 (as-head x2 'VP)))

          ((headed-by x1 'SB)
           (as-lst-head (cdr args) 'VP))
          ((headed-by x1 'VP)
           ;; some verb-mod's gives VP
           (list :splice x1 (as-head x2 'VP)))
          (t (cons :splice (cdr args))))))

(defun digits-to-int (digits)
  ;; digits is a (potentially nested) tree of digits (0 to 9)
  ;; treat it as the digits of an integer and calculate its value,
  ;; also return the number of digits
  (let ((v 0)
        (L 0))
    (labels ((walk (ds)
               (cond ((consp ds)
                      (walk (car ds))
                      (walk (cdr ds)))
                     ((numberp ds)
                      (incf L)
                      ;; assume 0 to 9
                      (setf v (+ (* v 10) ds)))
                     ;; may have separator, ignore
                     (t nil))))
      (walk digits)
      (values v L))))

(defun digits-to-fraction (digits)
  ;; the digits are after the decimal point, calculate it as a fraction
  (multiple-value-bind (v L)
      (digits-to-int digits)
    (/ v (expt 10 L))))

;;;
(defparameter *x1-mul-x2M*
  ;; x2 is a number
  (fn (if (numberp x1)
          (* x1 x2)
          (cdr args))))
(defparameter *t1-mul-t2M*
  ;; t2 is a number
  (tn (if (numberp t1)
          (* t1 t2)
          tags)))

;; for the case of 'num ge 億', 'num ge 兆'
(defparameter *x1-mul-x3M*
  ;; x3 is a number
  (fn (if (numberp x1)
          (* x1 x3)
          (cdr args))))
(defparameter *t1-mul-t3M*
  ;; t3 is a number
  (tn (if (numberp t1)
          (* t1 t3)
          tags)))
;;
(defparameter *Mx1-add-x2*
  ;; x1 is a number
  (fn (if (numberp x2)
          (+ x1 x2)
          (cdr args))))
(defparameter *Mt1-add-t2*
  ;; t1 is a number
  (tn (if (numberp t2)
          (+ t1 t2)
          tags)))
;;
(defun x-mul-M-add-y-else (x M y else-val)
  ;; M is assumed to be a number
  (if (and (numberp x)
           (numberp y))
      (+ (* x M) y)
      else-val))
(defparameter *x1-mul-x2M-add-x3*
  ;; x2 is a number
  (fn (x-mul-M-add-y-else x1 x2 x3 (cdr args))))
(defparameter *t1-mul-t2M-add-t3*
  ;; t2 is a number
  (tn (x-mul-M-add-y-else t1 t2 t3 tags)))
;;
(defparameter *x1-mul-x2M-add-x4*
  ;; x2 is a number
  (fn (x-mul-M-add-y-else x1 x2 x4 (cdr args))))
(defparameter *t1-mul-t2M-add-t4*
  ;; t2 is a number
  (tn (x-mul-M-add-y-else t1 t2 t4 tags)))
;;;
(defun fraction-a-val (numerator denominator args)
  (if (and (numberp numerator)
           (numberp denominator)
           (/= denominator 0))
      (/ numerator denominator)
      args))
(defun fraction-a-stag (numerator denominator)
  (cond ((not (and (numberp numerator)
                   (numberp denominator)))
         *c-prefer*)
        ((< numerator denominator) *c-prefer*)
        (t *c-long-sep*)))
(defun fraction-a-tag (numerator denominator tags)
  (if (and (numberp numerator)
           (numberp denominator)
           (/= denominator 0))
      (/ numerator denominator)
      tags))
;;;
(defun compare-order-of-magnitude (x1 x2)
  (let* ((L1 (if (= x1 0) 0 (floor (log x1 10))))
         (L2 (if (= x2 0) 0 (floor (log x2 10))))
         (D1 (floor x1 (expt 10 L1)))
         (D2 (floor x2 (expt 10 L2))))
    (values (= L1 L2) (abs (- D1 D2)))))
(defun compare-num-structure (x1 x2)
  (let ((same-order t))
    (labels ((walk (a b)
               (cond ((and (numberp a)
                           (numberp b))
                      (unless (compare-order-of-magnitude a b)
                        (setf same-order nil))
                      t)
                     ((and (consp a)
                           (consp b))
                      (and (walk (car a) (car b))
                           (walk (cdr a) (cdr b))))
                     ;; may need to compare strings
                     (t (equalp a b)))))
      (values (walk x1 x2) same-order))))
(defun same-order-of-magnitude (n1 n2)
  ;; n1 and n2 may be integer, or nested list as constructed as tag of
  ;; int.
  (cond ((and (numberp n1) (numberp n2))
         (multiple-value-bind (same-order leading-diff)
             (compare-order-of-magnitude n1 n2)
           (if same-order
               (if (<= leading-diff 1)
                   *c-close-sep*
                   (+ *c-close-sep*
                      *c-close-sep*))
               *c-long-sep*)))
        ((or (numberp n1) (numberp n2))
         *c-close-suffix*)
        ;; both num-structure
        (t (multiple-value-bind (same-struct same-order)
               (compare-num-structure n1 n2)
             (if same-struct
                 (if same-order
                     *c-close-sep*
                     (+ *c-close-sep*
                        *c-close-sep*))
                 *c-long-sep*)))))

;; num now records the value, if any
(defun as-num (v)
  (typecase v
    (num v)
    (noun v)
    (number (make-instance 'num :val v))
    (t (make-instance 'num))))

(defun num-value (v)
  (typecase v
    (number v)
    (num (num-val v))
    (t nil)))

(defun prefer-int-num (x &optional (penalty *c-rare*))
  (let ((v (num-value x)))
    (cond ((and (numberp v) (not (integerp v)))
           penalty)
          ((numberp v) 0)
          (t *c-close-sep*))))

(defmacro prefer-if-num (x var cond &optional (penalty '*c-rare*)
                                  (secondary-penalty '*c-close-suffix*))
  `(let ((,var (num-value ,x)))
     (if ,var
         (if ,cond 0 ,penalty)
         ,secondary-penalty)))

(defun matching-quote-p (L R)
  (or (and (eq L 'L-paren) (eq R 'R-paren))
	  (and (eq L 'L-bracket) (eq R 'R-bracket))
	  (and (eq L 'L-quote) (eq R 'R-quote))
	  (and (eq L 'double-quote) (eq R 'double-quote))
	  (and (eq L 'single-quote) (eq R 'single-quote))
	  (and (eq L 'back-quote) (eq R 'single-quote))
	  (and (eq L 'L-c-quote) (eq R 'R-c-quote))
      (and (eq L 'one-space) (eq R 'one-space))))

(defparameter *matching-quote*
  ;; L x R
  (tn (if (matching-quote-p t1 t3)
		  *c-prefer*
		  *c-rare*)))

(defun allow-quote-h (in-name cost-level tval-func)
  ;; to reduce typing the rule for quotes, because quotes can appear in many terms
  `((,in-name =2> open-quote ,in-name close-quote
			  :com ,(* cost-level *c-close-sep*)
              ;; add trait to indicate it is quoted, so that some
              ;; verbs can have preference
              :tag ,(tn (more-trait t2 'quoted t))
			  :stag ,*matching-quote*
			  :tval ,tval-func)
    ;; TODO: whether to keep this dash thing?
    ;;(,in-name =2> dash ,in-name
	;;		  :com ,(* cost-level *c-close-sep*)
	;;		  :tval ,*ctb-PRN*)))
    ))

(defun allow-quote (in-name &optional (cost-level 1))
  ;; to reduce typing the rule for quotes, because quotes can appear in many terms
  (allow-quote-h in-name cost-level *ctb-quote-as-phrase*))

(defun allow-quote-splice (in-name &optional (cost-level 1))
  ;; to reduce typing the rule for quotes, because quotes can appear in many terms
  (allow-quote-h in-name cost-level *ctb-quote-splice*))

(defun ctb-vnv (v v-POS-tag tree)
  ;; give CTB format, which may split v-not-v etc.
  (let ((rep-lens (trait-value v 'rep-lens))
        (rep-matched-char (trait-value v 'rep-matched-char))
        (str (collapse-str tree)))
    (if rep-lens
        ;; has rep pattern
        (destructuring-bind (L1 L2 L3) rep-lens
          (declare (ignorable L3))
          `(VNV (,v-POS-tag ,(subseq str 0 L1))
                (,(case rep-matched-char
                    ((yi1) 'CD)
                    ((le) 'AS)
                    ;; bu4
                    (t 'AD))
                  ,(subseq str L1 (+ L1 L2)))
                (,v-POS-tag ,(subseq str (+ L1 L2)))))
        (list v-POS-tag str))))


(defparameter *make-conn-t1-t3*
  (tn (make-conn-thing
       :first t1
       :second t3
       :sep t2)))

(defparameter *make-conn-t1-t2*
  ;; for this, the sep is nil
  (tn (make-conn-thing
       :first t1
       :second t2)))

(defparameter *make-conn-t2-t4*
  ;; for this, the sep is also nil
  (tn (make-conn-thing
       :first t2
       :second t4)))

(defparameter *tag-human-name*
  (make-instance 'human-name))

(defparameter *tag-human*
  (make-instance 'human))

(defparameter *tag-place*
  (make-instance 'place))

(defparameter *tag-place-with-noun-suffix*
  (more-trait-to (make-instance 'place)
                 'with-noun-suffix t))

(defparameter *stag-not-prefer-pause-or-seps*
  (tn (+
       (conn-not-prefer-seps t1 *c-close-sep*
                             '(:comma :semi-colon))
       (not-prefer-trait t1 'verb-mod-has-pause
                         *c-close-suffix*))))

(defparameter *stag-not-prefer-pause-or-seps-nested-VP*
  (tn (+
       (not-prefer-trait t1 'verb-OP *c-close-sep*)
       (not-prefer-trait t1 'verb-VP *c-close-sep*)
       (not-prefer-trait t1 'verb-SP *c-close-sep*)
       ;; for penalizing e.g. '(表示，女性朋友在遇到类似事件) 时'
       ;; NOTE: update this check accordingly if the tag of verb-c2 is changed
       (not-prefer-trait t1 'verb-with-pause *c-common*)
       ;; prefer '在' as time-at-to rather than adv when used with verb-p to form time
       (not-prefer-trait t1 'verb-mod-zai4)
       (conn-not-prefer-seps t1 *c-close-sep*
                             '(:comma :semi-colon))
       (not-prefer-trait t1 'verb-mod-has-pause
                         *c-close-suffix*))))

(defparameter *allowed-verb-mod-as-noun-mod*
  ;; may need more complicated filtering?
  (tn (if (pp-verb-mod-p t1)
          (case (pp-verb-mod-pp t1)
            ((:relative-related-to :after) *c-prefer*)
            ((:relative-to :surrounding)
             *c-long-sep*)
            (t nil))
          nil)))
;;
(defun clause-a0-stag-penalty (v)
  (+ (not-prefer v 'adj *c-common*)
     ;; to encourage '(VP1 並 VP2)' and not '並 VP2' alone
     (not-prefer-trait v 'verb-mod-bing4 *c-long-sep*)
     (not-prefer-conn v 'verb0 *c-close-suffix*)
     (not-prefer v 'adj-has-de *c-common*)))
(defparameter *clause-a0-stag*
  (tn (clause-a0-stag-penalty t1)))

(defparameter *clause-a00-tag*
  (tn (let ((n-vals nil))
        (typecase t1
          (place
           (setf n-vals (cons2 'has-place t
                               n-vals)))
          (time-name
           (setf n-vals (cons2 'has-time t
                               n-vals)))
          (t nil))
        (when t2
          (setf n-vals (cons2 'has-front-pause t
                              n-vals)))
        (more-trait-with t3 n-vals))))
(defparameter *clause-a00-stag*
  (tn (let ((penalty 0))
        (typecase t1
          (place
           (when (trait-value t3 'has-place)
             (incf penalty *c-close-suffix*)))
          (time-name
           (when (trait-value t3 'has-time)
             (incf penalty *c-close-suffix*)))
          (pp-verb-mod
           (when (and (not t2)
                      (eq (pp-verb-mod-pp t1)
                          :following)
                      (typep (pp-verb-mod-object t1)
                             'non-verb-noun))
             ;; prefer pause for '据 NP'
             (incf penalty *c-common*)))
          (t nil))
        (when (and (not t2)
                   (trait-value t3 'has-front-pause))
          (incf penalty *c-close-suffix*))
        penalty)))
;;;
;; testing grammar to decompose into words
(defparameter *chinese-grammar2*
  `((S -> r-words :val ,(fn (if (null (cdr x1))
                                (reverse x1)
                                (list (cons 'S (reverse x1)))))
       :tval ,(tfn (reverse x1)))
    (r-words -> item :val ,#'val-cdr
             :tval ,#'val-cdr)
    (r-words -> r-words item :val ,#'rxList
             :tval ,#'rxList)
    (item => word :com ,*c-item*)
    ;; for tval, usually we collapse the string at appropriate point.
    
    ;; For the purpose of parsing nearly consecutive numbers without
    ;; pause or space, e.g. 十五 十六, 五萬三千 五萬四千, we also try
    ;; to calculate the value as tag for constraints. If the number is
    ;; not determined, use a nested list to represent its pattern.
    (d-zero -> )
    ,@(strs 'd-zero '("零" "0") :> :val 0 :tag 0)
    ,@(strs 'digit '("一" "1" "壹" "幺") :> :val 1 :tag 1)
    ,@(strs 'digit '("二" "2" "貳" "兩") :> :val 2 :tag 2)
    ,@(strs 'digit '("三" "3" "叄" "仨") :> :val 3 :tag 3)
    ,@(strs 'digit '("四" "4" "肆") :> :val 4 :tag 4)
    ,@(strs 'digit '("五" "5" "伍") :> :val 5 :tag 5)
    ,@(strs 'digit '("六" "6") :> :val 6 :tag 6)
    ,@(strs 'digit '("陸") :> :val 6 :tag 6 :com *c-prep*) ;; '陸' could mean land
    ,@(strs 'digit '("七" "7" "柒") :> :val 7 :tag 7)
    ,@(strs 'digit '("八" "8" "捌") :> :val 8 :tag 8)
    ,@(strs 'digit '("九" "9" "玖") :> :val 9 :tag 9)
    ;; in composing an integer, basically many parts that require a
    ;; digit could be replaced with a unknown "what" or "how many"
    ;; t-digit: used alone and after '十', '百', '千', '萬', '億' etc
    (t-digit => digit)
    (t-digit :> ask-amount)
    (t-digit :> consecutive-digit :com ,*c-close-sep*)
    ;; "幾" already represent ask-amount, but it could also mean
    ;; approx amount. But we cannot easily distinguish them here, so
    ;; just mix them.
    ;;,@(strs 't-digit '("數" "多" "來" "餘") :>)
    ;; h-digit: used at the front of '十', '百', '千', '萬', '億' etc
    (h-digit => digit)
    (h-digit :> ask-amount)
    (h-digit :> "數")
    ,@(strs 'h-digit '("好幾" "若干") '->)
    (h-digit => digits-as-int)
    (h-digit :> consecutive-digit :com ,*c-close-sep*)
    (h-digit => decimal :com ,*c-close-sep*)
    ;;
    (xdigit => digit)
    (xdigit :> "零" :val 0 :tag 0)
    (xdigit :> "0" :val 0 :tag 0)
    ;; the tag for digits constructs a list of single digits
    (only-digits -> xdigit :val ,#'val-cdr
                 :tag ,(tn (list t1)))
    (only-digits -> xdigit only-digits :val ,#'xList
                 :tag ,(tn (cons t1 t2)))
    ;; at least one "-" separator
    ;; need to balance the penalty with possible date representation
    (serial-digits-x -> digits "-" digits
                     :val ,#'val-cdr
                     :tag ,#'tag-identity)
    (serial-digits-x -> digits "-" serial-digits-x
                     :val ,#'val-cdr
                     :tag ,#'tag-identity)
    (serial-digits =1> serial-digits-x :com ,*c-serial*
                   ;; serial numbers are tagged as CD in CTB
                   :tval ,*pos-CD*)
    ;; at least two "."
    (serial-digits-x2 -> digits "." digits "."
                      :val ,#'val-cdr
                      :tag ,#'tag-identity)
    (serial-digits-x2 -> digits "." digits "." digits
                      :val ,#'val-cdr
                      :tag ,#'tag-identity)
    (serial-digits-x2 -> digits "." serial-digits-x2
                      :val ,#'val-cdr
                      :tag ,#'tag-identity)
    (serial-digits =1> serial-digits-x2 :com ,*c-serial*
                   ;; serial numbers are tagged as CD in CTB
                   :tval ,*pos-CD*)
    ;; ad-hoc serial number, maybe phone number
    ,@(strs 'serial-digits '("妖妖灵") '->
            :com *c-serial*
            :tval *pos-CD*)
    ;;
    (hex-digit => xdigit)
    (hex-digit -> "a" :val 10 :tag 10)
    (hex-digit -> "b" :val 11 :tag 11)
    (hex-digit -> "c" :val 12 :tag 12)
    (hex-digit -> "d" :val 13 :tag 13)
    (hex-digit -> "e" :val 14 :tag 14)
    (hex-digit -> "f" :val 15 :tag 15)
    ;;
    (hex-digits-x -> hex-digit :val ,#'val-cdr
                  :tag ,(tn (list t1)))
    (hex-digits-x -> hex-digit hex-digits-x :val ,#'xList
                  :tag ,(tn (cons t1 t2)))
    ;;
    (hex-digits =1> hex-digits-x :com ,*c-serial*
                :stag ,(tn (if (and (some #'(lambda (z)
                                              (< z 10))
                                          t1)
                                    (some #'(lambda (z)
                                              (>= z 10))
                                          t1))
                               *c-prefer*
                               *c-common*))
                :tval ,*pos-CD*)
    ;; some sort of score? at least two "/"
    (score-num-x -> only-digits "/" only-digits "/" only-digits)
    (score-num-x -> only-digits "/" score-num-x)
    (score-num :> score-num-x :com ,(+ *c-serial* *c-long-sep*)
               :tval ,*pos-CD*)
    ;;
    (digits => only-digits)
    ;; sequence of digits with thousand separator
    (digits => sep-digits)
    ;; the tag for sep-digits constructs a nested list of single digits
    (sep-digits-x -> "," xdigit xdigit xdigit
                  :tag ,#'tag-identity)
    (sep-digits-xx => sep-digits-x)
    (sep-digits-xx -> sep-digits-x sep-digits-xx
                   :tag ,#'tag-identity)
    (sep-digits -> digit sep-digits-xx
                :tag ,#'tag-identity)
    (sep-digits -> digit xdigit sep-digits-xx
                :tag ,#'tag-identity)
    (sep-digits -> digit xdigit xdigit sep-digits-xx
                :tag ,#'tag-identity)
    ;;
    (digits-as-int -> digits
                   :com ,*c-digits-as-integer*
                   ;; not prefer a leading zero
                   :stag ,(tn (if (eql 0 (car t1))
                                  *c-close-sep*
                                  *c-prefer*))
                   :tag ,(tn (digits-to-int t1))
                   :val ,(fn (digits-to-int x1)))
    (only-digits-as-int -> only-digits
                        :com ,*c-digits-as-integer*
                        :tag ,(tn (digits-to-int t1))
                        :val ,(fn (digits-to-int x1)))
    ;; consecutive digit as an approximate number.
    ;; some are more ambiguous and we skip those
    ,@(strs 'consecutive-digit
            '("一兩" "兩三" "三四" "四五" "五六" "六七" "七八" "八九"))
    ;;
    ;; num10: < 100
    ,@(strs 'ten '("十" "拾") :> :val 10 :tag 10)
    (num10 => t-digit)
    (num10 => ten :com ,*c-long-sep*)
    (num10 -> h-digit ten
           :val ,*x1-mul-x2M*
           :tag ,*t1-mul-t2M*)
    (num10 -> ten t-digit
           :val ,*Mx1-add-x2*
           :tag ,*Mt1-add-t2*)
    (num10 -> h-digit ten t-digit
           :val ,*x1-mul-x2M-add-x3*
           :tag ,*t1-mul-t2M-add-t3*)
    (twenty :> "廿" :val 20 :tag 20)
    (num10 => twenty)
    (num10 -> twenty t-digit
           :val ,*Mx1-add-x2*
           :tag ,*Mt1-add-t2*)
    (thirty :> "卅" :val 30 :tag 30)
    (num10 => thirty)
    (num10 -> thirty t-digit
           :val ,*Mx1-add-x2*
           :tag ,*Mt1-add-t2*)
    ;; some commonly used roman numerals, some have already character variations
    ,@(strs 'num10 '("I" "one")      '-> :val 1  :tag 1 :com *c-close-sep*)
    ,@(strs 'num10 '("II" "two" "Ⅱ")     '-> :val 2  :tag 2 :com *c-close-sep*)
    ,@(strs 'num10 '("III" "three" "Ⅲ")  '-> :val 3  :tag 3 :com *c-close-sep*)
    ,@(strs 'num10 '("IV" "four" "Ⅳ")    '-> :val 4  :tag 4 :com *c-close-sep*)
    ,@(strs 'num10 '("V" "five")     '-> :val 5  :tag 5 :com *c-close-sep*)
    ,@(strs 'num10 '("VI" "six" "Ⅵ")     '-> :val 6  :tag 6 :com *c-close-sep*)
    ,@(strs 'num10 '("VII" "seven" "Ⅶ")  '-> :val 7  :tag 7 :com *c-close-sep*)
    ,@(strs 'num10 '("VIII" "eight" "Ⅷ") '-> :val 8  :tag 8 :com *c-close-sep*)
    ,@(strs 'num10 '("IX" "nine" "Ⅸ")    '-> :val 9  :tag 9 :com *c-close-sep*)
    ,@(strs 'num10 '("X" "ten")      '-> :val 10 :tag 10 :com *c-close-sep*)
    ,@(strs 'num10 '("Xi" "eleven" "Ⅺ")  '-> :val 11 :tag 11 :com *c-close-sep*)
    ,@(strs 'num10 '("Xii" "twelve" "Ⅻ") '-> :val 12 :tag 12 :com *c-close-sep*)
    ;; digits-as-int could in fact give an integer larger than num10
    (num10 => digits-as-int)
    (num10 => fraction :com ,*c-close-sep*)
    (num10 => num-b-x :com ,*c-close-sep*)
    (num10 => pre-num-approx :com ,*c-close-sep*)
    (num10 :> "數" :com ,*c-close-sep*)
    ;; num100: < 1000
    ,@(strs 'hundred '("百" "佰") :> :val 100 :tag 100)
    (num100 => num10)
    (h-num10 => h-digit)
    (h-num10 => decimal :com ,*c-long-sep*)
    ;; for X百, usually only a digit is used, but other are allowed though very rare
    (h-num10 => num10 :com ,*c-long-sep*)
    (x-num100 => hundred :com ,*c-long-sep*)
    (x-num100 -> h-num10 hundred
              :val ,*x1-mul-x2M*
              :tag ,*t1-mul-t2M*)
    (num100 => x-num100)
    (num100 -> h-num10 hundred d-zero num10
            :stag ,(tn (prefer-int-num t4))
            :val ,*x1-mul-x2M-add-x4*
            :tag ,*t1-mul-t2M-add-t4*)
    ;; num1000: < 10000
    ,@(strs 'thousand '("千" "仟") :> :val 1000 :tag 1000)
    (num1000 => num100)
    (h-num100 => h-digit)
    (h-num100 => decimal :com ,*c-long-sep*)
    ;; e.g. 三十七千, though uncommon, but possible
    (h-num100 => num100 :com ,*c-long-sep*)
    (x-num1000 => thousand :com ,*c-long-sep*)
    (x-num1000 -> h-num100 thousand
               :val ,*x1-mul-x2M*
               :tag ,*t1-mul-t2M*)
    (num1000 => x-num1000)
    (num1000 -> h-num100 thousand d-zero num100
             :stag ,(tn (prefer-int-num t4))
             :val ,*x1-mul-x2M-add-x4*
             :tag ,*t1-mul-t2M-add-t4*)
    ;; num10m: < 100m
    (ten-thousand :> "萬" :val 10000 :tag 10000)
    ;; w is sometimes used as shorthand for 萬, but to avoid too much
    ;; ambiguity, only allow this when it is used with numbers
    (x-ten-thousand => ten-thousand)
    (x-ten-thousand -> "w" :val 10000 :tag 10000)
    (num10m => num1000)
    (h-num1000 => num1000)
    (h-num1000 => decimal :com ,*c-long-sep*)
    (h-num1000 => rough-ten-power :com ,*c-long-sep*)
    (x-num10m => ten-thousand :com ,*c-long-sep*)
    (x-num10m -> h-num1000 x-ten-thousand
              :val ,*x1-mul-x2M*
              :tag ,*t1-mul-t2M*)
    (num10m => x-num10m)
    (num10m -> h-num1000 x-ten-thousand d-zero num1000
            :stag ,(tn (prefer-int-num t4))
            :val ,*x1-mul-x2M-add-x4*
            :tag ,*t1-mul-t2M-add-t4*)
    ;; num1000g: integer below 100M * 100M
    (hundred-million :> "億" :val 100000000 :tag 100000000)
    (num1000g => num10m)
    (h-num10m => num10m)
    (h-num10m => decimal :com ,*c-long-sep*)
    (h-num10m => rough-ten-power :com ,*c-long-sep*)
    (x-num1000g => hundred-million :com ,*c-long-sep*)
    (x-num1000g -> h-num10m hundred-million
                :val ,*x1-mul-x2M*
                :tag ,*t1-mul-t2M*)
    (x-num1000g -> h-num10m ge hundred-million
                :tval ,(tfn `(QP (QP ,(tag-as 'CD x1)
                                     (CLP ,x2))
                                 (QP ,(tag-as 'CD x3))))
                :val ,*x1-mul-x3M*
                :tag ,*t1-mul-t3M*)
    (num1000g => x-num1000g)
    (num1000g -> h-num10m hundred-million d-zero num10m
              :stag ,(tn (prefer-int-num t4))
              :val ,*x1-mul-x2M-add-x4*
              :tag ,*t1-mul-t2M-add-t4*)
    (num1000g -> h-num10m ge hundred-million d-zero num10m
              :stag ,(tn (prefer-int-num t5))
              :val ,(fn (x-mul-M-add-y-else x1 x3 x5 (cdr args)))
              :tag ,(tn (x-mul-M-add-y-else t1 t3 t5 tags)))
    ;; int: integer below (100M)^4
    (hundred-trillion :> "兆"
                      :val ,(expt 10 16) :tag ,(expt 10 16))
    (int => num1000g)
    (h-num1000g => num1000g)
    (h-num1000g => decimal :com ,*c-long-sep*)
    (h-num1000g => rough-ten-power :com ,*c-long-sep*)
    (x-int => hundred-trillion :com ,*c-long-sep*)
    (x-int -> h-num1000g hundred-trillion
           :val ,*x1-mul-x2M*
           :tag ,*t1-mul-t2M*)
    (x-int -> h-num1000g ge hundred-trillion
           :tval ,(tfn `(QP (QP ,(tag-as 'CD x1)
                                (CLP ,x2))
                            (QP ,(tag-as 'CD x3))))
           :val ,*x1-mul-x3M*
           :tag ,*t1-mul-t3M*)
    (int => x-int)
    (int -> h-num1000g hundred-trillion d-zero num1000g
         :stag ,(tn (prefer-int-num t4))
         :val ,*x1-mul-x2M-add-x4*
         :tag ,*t1-mul-t2M-add-t4*)
    (int -> h-num1000g ge hundred-trillion d-zero num1000g
         :stag ,(tn (prefer-int-num t5))
         :val ,(fn (x-mul-M-add-y-else x1 x3 x5 (cdr args)))
         :tag ,(tn (x-mul-M-add-y-else t1 t3 t5 tags)))
    ;; some informal usage: xMy means (x*M) + (y*M/10),
    ;; and xM半 means xM5, which means (x*M) + (M/2)
    ;; where M could be 百, 千, 萬, 億, 兆
    (xMy-digit => digit)
    (xMy-digit :> "半" :val 5 :tag 5)
    (int -> x-num100 xMy-digit ;; 百/10 = 10
         :com ,*c-long-sep*
         :val ,(fn (x-mul-M-add-y-else x2 10 x1 (cdr args)))
         :tag ,(tn (x-mul-M-add-y-else t2 10 t1 tags)))
    (int -> x-num1000 xMy-digit ;; 千/10 = 100
         :com ,*c-long-sep*
         :val ,(fn (x-mul-M-add-y-else x2 100 x1 (cdr args)))
         :tag ,(tn (x-mul-M-add-y-else t2 100 t1 tags)))
    (int -> x-num10m xMy-digit ;; 萬/10 = 1000
         :com ,*c-long-sep*
         :val ,(fn (x-mul-M-add-y-else x2 1000 x1 (cdr args)))
         :tag ,(tn (x-mul-M-add-y-else t2 1000 t1 tags)))
    (int -> x-num1000g xMy-digit ;; 億/10 = 10,000,000
         :com ,*c-long-sep*
         :val ,(fn (x-mul-M-add-y-else x2 10000000 x1 (cdr args)))
         :tag ,(tn (x-mul-M-add-y-else t2 10000000 t1 tags)))
    (int -> x-num1000g xMy-digit ;; 兆/10 = 10^15
         :com ,*c-long-sep*
         :val ,(fn (x-mul-M-add-y-else x2 (expt 10 15) x1 (cdr args)))
         :tag ,(tn (x-mul-M-add-y-else t2 (expt 10 15) t1 tags)))
    ;;
    (integer =1> int :com ,*c-num*
             :tval ,*pos-CD-or-QP*)
    (word => integer :com ,*c-integer-alone*)
    ;;
    (quote-number => t-number)
    (quote-number =2> "“" t-number "”")
    (quote-number =2> "\"" t-number "\"")
    (quote-number =2> "'" t-number "'")
    (quote-number =2> "`" t-number "'")
    (quote-number =2> "「" t-number "」")
    (t-ordinal -> "第" quote-number)
    (t-ordinal -> "頭" quote-number)
    ,@(strs 't-ordinal '("首" "末" "初" "頭" "尾"
                         "冠" "亞" "季" "殿"
                         "第N" "頭N")
            '->
            :com *c-num*
            :tag (make-instance 'ordinal))
	,@(strs 'A-Z '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
				   "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
				   "u" "v" "w" "x" "y" "z")
			:>
            :com *c-num*
            :tag (make-instance 'ordinal-A-Z))
	(t-ordinal => A-Z)
    ;; 甲, 乙, 丙, 丁, ...
    (t-ordinal => abstract-a :stag ,(tn (must-be t1 'counting-name)))
    (t-ordinal =2> "~" t-ordinal :com ,*c-close-sep*)
    ;; (ordinal -> "首" t-number) ;; TODO: valid?
    (ordinal => t-ordinal :tval ,*pos-OD*)
    (word => ordinal :com ,*c-ordinal-alone*)
    ;;
    ,@(strs 'fraction-word '("分" "分之"))
    (fraction-word -> "之" :com ,*c-close-sep*)
    (fraction-a -> "半" :com ,*c-fraction-half* :tag 1/2)
    (fraction-a -> "一" "半" :tag 1/2)
    (fraction-numerator => int)
    (fraction-numerator => decimal)
    (fraction-a -> int fraction-word fraction-numerator
                :val ,(fn (fraction-a-val x3 x1 args))
                ;; prefer fraction smaller than one so that 三分之一萬
                ;; is grouped as (三分之一)萬 instead of 三分之(一萬),
                ;; although the two values are the same
                :stag ,(tn (fraction-a-stag t3 t1))
                :tag ,(tn (fraction-a-tag t3 t1 tags)))
    (fraction-a -> int "/" int
                ;; alternative form
                :val ,(fn (fraction-a-val x1 x3 args))
                :stag ,(tn (fraction-a-stag t1 t3))
                :tag ,(tn (fraction-a-tag t1 t3 tags)))
    (fraction => fraction-a :com ,*c-num*)
    (fraction -> integer "又" fraction-a
              :com ,*c-num*
              :val ,(fn (if (and (numberp x1)
                                 (numberp x3))
                            (+ x1 x3)
                            args))
              :stag ,(tn (cond ((not (numberp t3))
                                *c-prefer*)
                               ((< t3 1) *c-prefer*)
                               (t *c-long-sep*)))
              :tag ,(tn (if (and (numberp t1)
                                 (numberp t3))
                            (+ t1 t3)
                            tags)))
    ;;
    (decimal-int => integer
                 :stag ,(tn (if (and (numberp t1)
                                     (not (integerp t1)))
                                *c-rare*
                                *c-prefer*)))
    (decimal-int :> "零" :val 0 :tag 0)
    ,@(strs 'decimal-pt '("點" "." "。") :>)
    ,@(strs 'decimal-pt '("點兒") '->)
    (decimal -> decimal-int decimal-pt digits :com ,*c-decimal*
             ;; avoid floating point, use fraction
             :tag ,(tn (if (numberp t1)
                           (+ t1 (digits-to-fraction t3))
                           tags)))
    (decimal =1> decimal-int decimal-pt
             :com ,(+ *c-decimal* *c-long-sep*))
    ;;
    (t-number => integer)
    (t-number => fraction)
    (t-number => decimal)
    (percentage -> t-number "%"
                :val ,(fn (if (numberp x1)
                              (/ x1 100)
                              args))
                :tag ,(tn (if (numberp t1)
                              (/ t1 100)
                              tags)))
    (percentage-mul :> "成" :val 1/10 :tag 1/10)
    (percentage-mul :> "分" :val 1/10 :tag 1/10
                    ;; confused with time
                    :com ,*c-noun-suffix*)
    ;; 厘 is a measure word in CTB
    ;;(percentage-mul :> "厘" :val 1/100 :tag 1/100)
    (percentage -> t-number percentage-mul
                :val ,(fn (x-mul-M-add-y-else x1 x2 0 args))
                :tag ,(tn (x-mul-M-add-y-else t1 t2 0 tags)))
    (percentage-digit => xMy-digit)
    (percentage-digit :> digits
                      :com ,*c-close-sep*
                      :tag ,(tn (* 10 (digits-to-fraction t1)))
                      :val ,(fn (* 10 (digits-to-fraction x1))))
    (percentage -> t-number percentage-mul percentage-digit
                :val ,(fn (if (and (numberp x1)
                                   (numberp x3))
                              (* (+ x1 (/ x3 10)) x2)
                              args))
                :tag ,(tn (if (and (numberp t1)
                                   (numberp t3))
                              (* (+ t1 (/ t3 10)) t2)
                              tags)))
    ;;
    (number => t-number :tval ,*pos-CD-or-QP*)
    (number => percentage :tval ,*pos-CD*
            :tag ,(tn (more-trait (as-num t1) 'num-is-percentage t)))
    ;;
    (word :> sep :com ,*c-sep-alone*)
    ,@(strs 'sep '("，" "：" "。" "！" "“" "”" "？" "、" "")
            '->
            :tval *pos-PU*)
    (word :> end :com ,*c-end-alone*)
    ,@(str-tags 'end
                '((:full-stop "。")
                  (:exclaimation-mark "！")
                  (:question-mark "？"))
                '->
                :tval *pos-PU*)
    (may-pause -> :tval (:splice))
    (may-pause => pause)

    ;;
    (prefer-long-pause :> pause
                       :stag ,(tn (case t1
                                    ((:comma :semi-colon) *c-prefer*)
                                    (t *c-common*))))
    ;; allow pause but with penalty
    (not-prefer-pause -> :tval (:splice))
    (not-prefer-pause :> pause
                      :tag :pause
                      ;; reduced from previous *c-common*
                      :com ,*c2*)
    (word :> pause :com ,*c-pause-alone*)
    ,@(str-tags 'pause
                '((:comma ",")
                  (:short-comma "、")
                  (:colon "：")
                  (:semi-colon ";"))
                '->
                :tval *pos-PU*)
    ;;
    ,@(str-tags 'short-pause
                '((:comma ",")
                  (:colon "："))
                '->
                :com *c-rare*
                :tval *pos-PU*)
    (short-pause -> "、"
                 :tag :short-comma
                 :tval ,*pos-PU*)
	;; for the strange construct in CTB called PRN
	,@(strs 'L-prn-a '("（" "“" "｛" "＜" "［" "「" "《" "＂")
			:>
			:tval *pos-PU*)
	,@(strs 'L-prn-a '("&lt;") '->
			:tval *pos-PU*)
	,@(strs 'R-prn-a '("）" "”" "｝" "＞" "］" "」" "》" "＂")
			:>
			:tval *pos-PU*)
	,@(strs 'R-prn-a '("&gt;") '->
			:tval *pos-PU*)
	,@(strs 'N-prn-a '("…" "..." "。。" "。。。" "／"
					   "---" "----" "-----" "------")
			'->
			:com *c-close-sep*
			:tval *pos-PU*)
    ,@(strs 'N-prn-a '("-" "--")
			'->
            ;; '-' and '--' could also be used as conn, so give this
            ;; higher penalty
			:com *c-dash*
			:tval *pos-PU*)
	(L-prn :> L-prn-a)
	(L-prn :> R-prn-a :com ,*c-close-suffix*)
	(L-prn :> N-prn-a)

	(R-prn :> R-prn-a)
	(R-prn :> L-prn-a :com ,*c-close-suffix*)
	(R-prn :> N-prn-a)

	(N-prn :> N-prn-a)
	(N-prn :> L-prn-a :com ,*c-close-suffix*)
	;; TODO: things that need PRN and not handled yet:
	;;   (DP UCP CP IP)
	;;   FW CLP NR URL VV CD NT LCP ;; less frequent
    
    ;; NOTE: CTB is not consistent for quotes, sometimes it is PRN,
    ;; sometimes not! TODO: Whether to keep these rules, when we have
    ;; allow-quote? Seems best to remove these, but not decided yet.

	,@(loop :for thing :in
		 '(num-unit verb-mod adj-p
		   animate-p inanimate-p abstract-p place-p time)
		 :collect
		 `(,thing -> L-prn ,thing R-prn
                  :com ,*c-common*
				  :tval ,*ctb-PRN*)
		 :collect
		 `(,thing -> N-prn ,thing
				  :tval ,*ctb-PRN*
				  :com ,*c-close-suffix*))
	;; TODO: UCP: coordination of different categories. Maybe promote each to the 'largest' category?
	;;
    (unknown =1> name-x)
    ;;(unknown -> unknown-x)
    ;;(unknown-x :> :xx :val ,#'val-cdr)
    ;;(unknown-x -> :xx any :val ,#'xList)
    ;;(any -> :x :val ,#'val-cdr)
    ;;(any -> :x any :val ,#'xList)
    (word :> unknown :com ,*c-unknown-alone*)
    ;; used for connecting nouns and adjectives
    ;; "之" is used more in ancient Chinese,
    ;; and "的" used more in modern Chinese
    ,@(strs 'zhi '("的" "之") :> :tval (t-str 'DEG))
    ,@(strs 'dec '("的" "之") :> :tval (t-str 'DEC))
	,@(strs 'sp-de '("的") :> :tval (t-str 'SP))
    ;; noun modifier, applicable to basically all the noun categories
    ;; some noun-mod are not very suitable to act as noun
    ;; use its tag to penalize
    (noun-mod-a => adj :com ,*c-adj-as-noun-mod*)
    (noun-mod-a => adj-p :com ,*c-adj-p-as-noun-mod*)
    ;; some noun-mod are more suitable to act as noun
    ;;(noun-mod-a => sound :com ,*c-long-sep*) ;; x, already in abstract-a
    ;; TODO: whether to add num?
    (noun-mod-a => num-unit
                ;; rough-amount could become noun-mod either by
                ;; 'num-unit -> noun-mod' and 'num -> noun-b', so
                ;; prefer it used as noun-mod through num-unit,
                ;; without penalizing it used as noun.
                :stag ,(tn (prefer t1 'rough-amount *c-close-sep*)))
    (noun-mod-a => q-pronoun) ;; NOTE, only those without not-single as noun
    (noun-mod-a => p-pronoun) ;; NOTE, only those without not-single as noun
    ;;(noun-mod-a =2> dir-suffix-place p-pronoun) ;; x, can be composed of noun
    (pronoun-unit =2> p-pronoun unit
                :tval ,(tfn `(DP ,x1 ,(as-head x2 'CLP))))
    (pronoun-unit =2> q-pronoun unit
                :tval ,(tfn `(DP ,x1 ,(as-head x2 'CLP))))
    ;; TODO: add (pronoun-unit =2> pronoun unit) ? Used mainly in Cantonese.
    (noun-mod-a => pronoun-unit)
    ;;(noun-mod-a =3> dir-suffix-place p-pronoun unit) ;; x, can be composed
    (noun-mod-a => verb-x :com ,*c-verb-x-as-noun-mod*) ;; TODO: check CTB
    ;;
    (noun-mod =1> subj-pred :com ,*c-subj-pred-as-noun-mod*
              ;; e.g. mod in '((黔江 地区) 所辖) (行政 区域)'
              ;; currently only for 'NN (所 VV)'
              :stag ,(tn (if (and (a-subj-pred-p t1)
                                  (trait-value (a-subj-pred-pred t1)
                                               :verb-mod-suo3))
                             (+ (not-prefer-trait t1 'has-time *c-long-sep*)
                                (not-prefer-trait t1 'has-place *c-long-sep*))
                             nil))
              :tval ,(tfn (as-head x1 'IP)))
    ;;
    (noun-mod-a -> p-word-dui4 noun-b
                ;; NOTE: for constructs such as '大连 (对韩 出口)'.
                ;; Not prefer this as noun, but currently noun-mod cannot form a noun by itself.
                ;; TODO: Give proper constraints when modifying nouns.
                
                ;; Should make this less preferred to the noun-zhi-mod-a rule of p-word-dui4.
                :com ,(+ *c-dui-noun-p-as-noun-mod* *c-long-sep*)
                :tag ,(tn (make-dui4-noun :noun t2))
                :tval ,(tfn `(PP ,x1 ,(as-head x2 'NP))))
    ;;(noun-mod-a => verb-p :com ,*c-verb-p-as-noun-mod*
    ;;            :tval ,(tfn (as-head x1 'IP))) ;; x, verb-p does not usually modify noun without zhi
    ;; noun-mod's may be separated by pauses
    ;;(noun-mod-b0 => noun-mod-a) ;; x, conn includes short-pause
    ;;(noun-mod-b0 -> noun-mod-a short-pause noun-mod-b0
	;;			 :tval ,*ctb-splice*
	;;			 :tag ,*make-conn-t1-t3*
	;;			 :stag ,(tn (similar-noun-mod t1 t3))) ;; x, conn includes short-pause
	;;(noun-mod-b => noun-mod-b0
	;;			:tval ,*ctb-maybe-UCP*) ;; x, conn includes short-pause
    ;;
    (noun-mod-b-conn => noun-mod-a)
    (noun-mod-b-conn -> noun-mod-b-conn conn noun-mod-a
                     ;; TODO: whether to use noun-conn?
					 :tval ,*ctb-splice*
                     :com ,*c-close-sep*
                     :tag ,*make-conn-t1-t3*
                     :stag ,(tn (similar-noun-mod t1 t3)))
    ;;
    (noun-mod => noun-mod-b-conn
			  :tval ,*ctb-maybe-UCP*)
    (noun-mod => noun-zhi-mod) ;; ok
    ;; TODO: tidy up noun-mod and noun-zhi-mod
    ;; noun-mod are split into those with zhi, and those without
    (noun-zhi-mod-a =1> adj-p zhi :com ,*c-adj-p-as-noun-mod-zhi*
                    :tval ,(tfn (cond ((headed-by-any x1 'VP 'IP)
                                       `(CP ,(as-head x1 'IP)
                                            ,(tag-as 'DEC x2)))
                                      (t `(DNP ,x1 ,x2))))
                    :stag ,(tn (if (= L1 1) (not-prefer t1 'adj-no-mono-de) 0))
                    :tag ,(tn (make-adj-has-de :the-adj t1))) ;; ?? as noun?
    ;;(noun-zhi-mod-a -> sound zhi)
    ;;(noun-zhi-mod-a -> "所" verb zhi)
    ;;(noun-zhi-mod-a -> time zhi)
    ;;(noun-zhi-mod-a =1> num-unit zhi)
    ;;(noun-zhi-mod-a =1> q-pronoun zhi)
    ;;(noun-zhi-mod-a =1> p-pronoun zhi)
    ;;(noun-zhi-mod-a =2> p-pronoun unit zhi)
    (noun-zhi-mod-a =1> verb-p dec :com ,*c-verb-p-as-noun-mod-zhi*
                    :tval ,(tfn `(CP ,(as-head x1 'IP)
                                     ,x2))
                    :stag ,(tn
                            (min-over-also-conn
                             t1
                             #'(lambda (v)
                                 (+ (not-prefer-trait v 'has-time-pause *c-common*)
                                    (not-prefer-trait v 'has-place-pause *c-common*)
                                    (not-prefer-trait v 'has-subj-pause *c-common*)
                                    (not-prefer-trait v 'verb-mod-has-pause *c-common*)
                                    (conn-not-prefer-seps v *c2*
                                                          '(:comma :semi-colon))))))
                    :tag ,(tn (make-instance 'de-noun-mod :pred t1))) ;; x, as noun
    ;; TODO: record more info in a-subj-pred for consistency checking when modifying noun
    ;; e.g. '(subj verb obj) zhi place' makes more sense if the subj-pred does not include place information.
    ;; e.g. similarly '(subj verb obj) zhi time' makes more sense if the subj-pred does not include place information.
    ;; On the other hand, '(subj verb obj) zhi noun' makes less sense if the verb normally takes only one object.
    (noun-zhi-mod-a =1> subj-pred dec :com ,*c-subj-pred-as-noun-mod*
                    :tval ,(tfn `(CP ,(as-head x1 'IP)
                                     ,x2))
                    ;; the seps give a lot of trouble!!
                    :stag ,(tn (not-prefer-body-pause-seps t1))
                    :tag ,(tn (make-instance 'de-noun-mod :pred t1))) ;; x, as noun
    ;;
    (noun-zhi-mod-a =1> n-verb-mod zhi
                    :com ,*c-verb-mod-as-noun-mod-zhi*
                    ;; e.g. '(有关 当地空气污染) 的 照片'
                    ;; Only some verb-mod are allowed as noun-zhi-mod
                    :stag ,*allowed-verb-mod-as-noun-mod*
                    :tval ,(tfn `(DNP ,x1 ,x2))
                    )
    (noun-zhi-mod-a =1> n-verb-mod-front zhi
                    :com ,*c-verb-mod-as-noun-mod-zhi*
                    ;; Only some verb-mod are allowed as noun-zhi-mod
                    :stag ,*allowed-verb-mod-as-noun-mod*
                    :tval ,(tfn `(DNP ,x1 ,x2))
                    )
    ;;
    ,@(strs 'p-word-dui4 '("對") :> :tval *pos-P*)
    (noun-zhi-mod-a -> p-word-dui4 noun-p zhi :com ,*c-dui-noun-p-as-noun-mod*
                    ;; TODO: whether to give dui4-noun as tag, as the noun-mod-a case above?
                    :tval ,(tfn `(DNP (PP ,x1
                                          ,x2)
                                      ,x3)))
    ;;(noun-zhi-mod-a -> p-word-dui4 pronoun zhi :com ,*c-dui-pronoun-as-noun-mod*
    ;;                :tval ,(tfn `(DNP (PP ,x1
    ;;                                      ,(as-head x2 'NP)
    ;;                                      ,x3)))) ;; x, duplicate
    (noun-zhi-mod-a -> noun-b zhi
                    ;; e.g. '我的', '他的', without explicitly mention what
                    :stag ,(tn (prefer-animate-like t1))
                    :tval ,(tfn `(DNP ,(as-head x1 'NP) ,x2))
                    :tag ,(tn (make-instance 'de-noun-mod :pred t1))
                    :com ,*c-noun-p-as-mod*) ;; fallback
    ;;
    ;;(noun-zhi-mod-b0 => noun-zhi-mod-a)
    ;;(noun-zhi-mod-b0 -> noun-zhi-mod-a pause noun-zhi-mod-b0
	;;				 :tval ,*ctb-splice*
	;;				 :tag ,*make-conn-t1-t3*
	;;				 :stag ,(tn (similar-noun-mod t1 t3))) ;; x, conn includes pause
	;;(noun-zhi-mod-b => noun-zhi-mod-b0
	;;				:tval ,*ctb-maybe-UCP*)
    ;;
    (noun-zhi-mod-b-conn => noun-zhi-mod-a)
    (noun-zhi-mod-b-conn -> noun-zhi-mod-b-conn conn noun-zhi-mod-a
						 :tval ,*ctb-splice*
                         :com ,*c-close-sep*
                         :tag ,*make-conn-t1-t3*
                         :stag ,(tn (similar-noun-mod t1 t3)))
    ;;
    (noun-zhi-mod => noun-zhi-mod-b-conn
				  :tval ,*ctb-maybe-UCP*)
    ;;(noun-zhi-mod -> place-p zhi)
    ;;;;; for use in verb-x
    (noun-a => animate-a)
    (noun-a => inanimate-a)
    (noun-a => abstract-a)
    (noun-a => place-b)
    (noun-a => p-pronoun
            ;; NOTE: also let noun-mod to have p-pronoun, but without
            ;; *c-close-sep* and no restriction on not-single, so that
            ;; it is the preferred usage of p-pronoun
            :com ,*c-close-sep*
            ;; restrict to those can be standalone
            :stag ,(tn (not-prefer t1 'not-single nil)))
    (noun-a => q-pronoun
            ;; NOTE: also let noun-mod to have q-pronoun, but without
            ;; *c-close-sep* and no restriction on not-single, so that
            ;; it is the preferred usage of q-pronoun
            :com ,*c-close-sep*
            ;; restrict to those can be standalone
            :stag ,(tn (not-prefer t1 'not-single nil)))
    (noun-a => pronoun)
    ;; NOTE: "dir" can be obtained with noun-b => dir-suffix-place => dir
    ;; NOTE: the 'noun' rules are replaced by noun-b and noun-t
    ;;
    ;; noun categories have different restrictions when connected by "的"
    ;;(noun => t-animate :com ,*c-animate-as-noun*)
    ;;(noun => inanimate :com ,*c-inanimate-as-noun*)
    ;;(noun => abstract-p :com ,*c-abstract-as-noun*)
    ;;(noun => place :com ,*c-place-as-noun*)
    ;;(noun => p-pronoun :com ,*c-p-pronoun-as-noun*
    ;;      ;; restrict to those can be standalone
    ;;      :stag ,(tn (not-prefer t1 'not-single nil)))
    ;;(noun => pronoun :com ,*c-pronoun-as-noun*)
    ;;(noun =1> time :com ,*c-time-as-noun*
    ;;      :tval ,#'x1)
    ;;(noun =1> num :com ,*c-num-as-noun*
    ;;      :tval ,#'x1)
    ;;(noun =1> serial-digits :com ,*c-num-as-noun*
    ;;      :tval ,#'x1)
    ;;(noun =1> hex-digits :com ,*c-num-as-noun*
    ;;      :tval ,#'x1)
    ;;(noun =1> score-num :com ,*c-num-as-noun*
    ;;      :tval ,#'x1)
    ;;(noun =1> verb-x
    ;;      :tval ,#'x1
    ;;      ;; (Vx N) slightly less penalty as (V N)
    ;;      :stag ,(tn (if (typep t1 'verb-n)
    ;;                     (- *c2*
    ;;                        *c-verb-x-mod-noun*
    ;;                        *c-close-sep*)
    ;;                     *c-verb-as-noun*)))
    ;;(noun =1> adj-p :com ,*c-adj-p-as-noun*
    ;;      :tval ,(tfn (ctb-IP-VP-JJ-to-NP x1)))
    ;;(noun => name)
    ;; testing merging the different rules for nouns
    ,@(allow-quote 'noun-a 0)
    (noun-b => noun-a
            ;; sometimes want '新' as adj -> noun, e.g. '新 不如 舊'
            :stag ,(tn (if (= L1 1)
                           (not-prefer t1 'country-abbr-singapore *c-close-sep*)
                           0)))
    (noun-b -> noun-mod noun-a
            ;; noun-mods other than adj-x are also useful
            :tval ,*ctb-mod-noun*
            :stag ,*mod-noun-t1-t2*
            :tag ,(tn (mod-noun-tag t1 (up-ind t2)
                                    L1 L2)))

    (animate-or-abstract-suffix => animate-suffix)
    (animate-or-abstract-suffix => abstract-suffix)
    (noun-b -> noun-mod animate-or-abstract-suffix
            ;; NOTE: these animate-suffix is the tag (after up-ind),
            ;; but they are rarely used alone as noun. Similar for
            ;; abstract-suffix.
            :tval ,*ctb-mod-noun*
            :stag ,*mod-noun-t1-t2*
            :tag ,(tn (mod-noun-tag t1 (up-ind t2)
                                    L1 L2)))
    (noun-b -> noun-b animate-or-abstract-suffix
            ;; NOTE: there could be more than one animate-suffix on a noun.
            
            ;; NOTE: the animate-suffix (after up-ind) is the tag,
            ;; even if t1 is a noun, e.g. "園藝 師", "園 丁", "花 農"
            ;; "足球 迷", "生產 隊". In contrast, for noun-suffix, the
            ;; tag would often be the t1, e.g. "教授 們", "學生 們".
            :tval ,*ctb-mod-noun*
            :stag ,*mod-noun-t1-t2*
            ;; the trait 'with-noun-suffix is for 'pronoun (noun suffix)', e.g. '那些 (教授 們)'
            :tag ,(tn (more-trait (mod-noun-tag t1 (up-ind t2)
                                                L1 L2)
                                  'with-noun-suffix t)))
    (noun-b -> noun-b dir-suffix :com ,*c1*
            ;; NOTE: basically the same as 'place-b -> place-a dir-suffix' rule
            ;; e.g. '(试衣 间) 内'
            ;; also allow e.g. '纸上'
            :tag ,(make-instance 'place-with-dir)
            :tval ,*place-with-dir*
            :stag ,(tn (noun-dir-as-place t1 t2))
            )
    ;; to avoid ambiguity of noun-b -> adj-p and then quote the noun-b.
    ,@(allow-quote 'noun-b 2)
    (noun-b =1> time :com ,*c-time-as-noun*
            :tval ,#'x1)
    (noun-b =1> num :com ,*c-num-as-noun*
            ;; increased penalty for *c-num-as-noun*, to avoid ambiguity of rough-amount as noun-b and as noun-mod
            :tval ,#'x1)
    (noun-b =1> num-unit :com ,*c-num-unit-as-noun-p*
            :tval ,#'x1)
    (noun-b =1> pronoun-unit :com ,*c-num-unit-as-noun-p*
            :tval ,#'x1)
    (noun-b =1> serial-digits :com ,*c-num-as-noun*
            :tval ,#'x1)
    (noun-b =1> hex-digits :com ,*c-num-as-noun*
            :tval ,#'x1)
    (noun-b =1> score-num :com ,*c-num-as-noun*
            :tval ,#'x1)
    (noun-b =1> verb-x
            ;; ambiguity with 'noun-b -> noun-mod noun-b' when the
            ;; noun-mod is verb, and this verb-x is verb-n.
            ;; Often cause trouble, e.g. 免疫学, 全能赛
            :com ,*c-long-sep*
            :tval ,(tfn (ctb-IP-VP-JJ-to-NP x1))
            ;; (Vx N) slightly less penalty as (V N)
            :stag ,(tn (+
                        ;; 全能赛
                        (not-prefer-verb t1 'v-sai4-b *c-common*)
                        (if (typep t1 'verb-n)
                            (- *c2*
                               *c-verb-x-mod-noun*
                               *c-close-sep*)
                            *c-verb-as-noun*))))
    (noun-b =1> adj-p :com ,*c-adj-p-as-noun*
            :tval ,(tfn (ctb-IP-VP-JJ-to-NP x1)))
    ;;
    (noun-b => name)
    (noun-b => t-surname
            :com ,*c-close-suffix*
            ;; surname alone can be used as a noun to refer to a
            ;; person, but usually there would be at least two
            ;; characters
            :stag ,(tn (if (<= L1 1) *c-common* 0))
            )
    
    (noun-b => dir-suffix-place)
    ;;
    ,@(strs 'place-kou '("口" "口兒") '->)
    (noun-b -> noun-b place-kou
            ;; NOTE: changed to give noun-b instead of noun-t, so that
            ;; it can take noun-suffix, which may be useful.
            
            ;; copied and modified from the place-b special rule
            :com ,*c-short-close-suffix*
            :tval ,*pos-NN*
            :stag ,(tn (typecase t1
                         (place 0)
                         (inanimate 0)
                         (t nil)))
            :tag ,(tn (if (typep t1 'place)
                          (more-trait t1
                                      'with-noun-suffix t)
                          *tag-place-with-noun-suffix*))
            )
    (noun-b -> verb-x place-kou
            ;; NOTE: changed to give noun-b instead of noun-t, so that
            ;; it can take noun-suffix, which may be useful.
            
            ;; copied and modified from the place-b special rule
            :com ,*c-short-close-suffix*
            :tval ,*pos-NN*
            :stag ,(tn (typecase t1
                         (v-chu1 *c-rare*)
                         (v-into *c-rare*)
                         (v-ru4 *c-rare*)
                         (t 0)))
            :tag ,*tag-place-with-noun-suffix*
            )
    ;;
    (noun-b =1> noun-b noun-suffix
            ;; some suffix such as 子 and 兒 seem useful in forming
            ;; noun-b, for forming other words such as verb-a (* + 化),
            ;; adj-a (* + 形).
            :tag ,(let ((tag-inanimate (more-trait-to (make-instance 'inanimate) 'with-noun-suffix t))
                        (tag-abstract (more-trait-to (make-instance 'abstract) 'with-noun-suffix t))
                        (tag-noun-suffix (more-trait-to (make-instance 'noun-suffix) 'with-noun-suffix t)))
                       (tn (if (typep t1 'non-verb-noun)
                               ;; the trait 'with-noun-suffix is for 'pronoun (noun suffix)', e.g. '該 (男 子)'
                               (more-trait t1
                                           'with-noun-suffix t)
                               ;; adj or verb
                               (typecase t2
                                 (ns-jian4 tag-inanimate)
                                 (ns-cai2 tag-inanimate)
                                 (ns-shape-ind tag-inanimate)
                                 (ns-zuo4 tag-abstract)
                                 (t tag-noun-suffix)))))
            :tval ,*pos-NN*
            :stag ,*mod-noun-t1-t2*)
    ;;
    (noun-t -> noun-t num-unit
            :com ,*c-close-suffix*
            ;; NOTE: prefer t1 to be compound
            ;; e.g. '你們 三個', '兄弟 兩人', '蘋果 一個'
            :tag ,(tn (more-trait t1 'has-num-unit-suffix t))
            :tval ,(tfn `(NP ,(as-head x1 'NP) ,x2))
            :stag ,(tn (noun-unit t1 t2)))
    ;; NOTE: etc for noun-b, without conn-thing, for '我等'. But this
    ;; is easily confused with '等' being a verb, so give some penalty
    (noun-t -> noun-b etc
            :com ,*c-close-suffix*
            :tval ,(tfn (append (as-phrase-level x1 'NP)
                                (list x2)))
            ;; NOTE: this form does not allow 'noun-b etc noun-b'
            ;; which has to be composed from this noun-t and another
            ;; noun-t
            :tag ,(tn (more-trait t1 'has-etc t)))
    (noun-t -> noun-b etc noun-t
            :com ,*c-long-sep*
            :tval ,(tfn
                    `(NP
                      ,(append (as-phrase-level x1 'NP)
                               (list x2))
                      ,(as-phrase-level x3 'NP)))
            ;; NOTE: need to accomodate nouns such as '冯女士 等人'
            ;; which is quite confusing because of '等', but decided
            ;; to bias to 'etc' instead of '等' as verb or PP.
            
            ;; NOTE: changed from 'noun-b etc noun-b', to accomodate nouns such as '哈尔滨 等 东北重工业城市'
            ;; NOTE: want t1 to be subtype of t3
            :stag ,(tn (score-add
                        (+
                         (if (conn-subtype-of-p t1 (up-ind t3))
                             0
                             *c-common*)
                         (prefer-less-base-noun t3))
                        (not-prefer-etc t1)))
            :tag ,(tn (more-trait t1 'has-etc t)))
    ;;
    (noun-t => noun-b)
    (noun-t => place-p-a
            ;; to avoid ambiguity of 'place-p -> place -> noun-t ->
            ;; place-p-a' and 'place-p -> place-p-a'.
            :com ,*c-close-sep*)
    (noun-t =1> noun-zhi-mod :com ,*c-noun-mod-as-noun-p*
            :tval ,(tfn (ctb-IP-VP-JJ-to-NP x1))
            :stag ,(tn (noun-mod-as-noun t1)))
    ;;
    (noun-t -> noun-mod noun-t
            :com ,*c-close-sep*
            :tval ,*ctb-mod-noun*
            ;; NOTE: penalize the number of base nouns in t2, so that
            ;; left association is preferred for noun-mod
            :stag ,*count-mod-noun-t1-t2*
            ;; t1 does not add to the count of base nouns
            :tag ,(tn (mod-noun-tag t1 (up-ind t2)
                                    L1 L2)))
    (noun-t -> noun-t noun-t
            :com ,*c-close-sep*
            :tval ,*ctb-mod-noun*
            ;; NOTE: penalize the number of base nouns in t2, t1 not
            ;; prefer etc, t1 not prefer conn-thing
            :stag ,(tn (score-add
                        (mod-noun t1 t2 L1 L2)
                        (+ (prefer-less-base-noun t2)
                           (not-prefer t1 'conn-thing *c-close-sep*)
                           ;; reduced the penalty for etc, for '企业
                           ;; 产权 转让 等 投资 方式'.
                           (not-prefer-etc t1 *c-long-sep*))))
            ;; NOTE: add trait for total number of base nouns in t1
            ;; and t2
            :tag ,(tn (sum-count-base-nouns
                       t1 t2
                       (mod-noun-tag t1 (up-ind t2)
                                     L1 L2))))
    (noun-t -> noun-mod zhi noun-t
            :com ,*c-close-sep*
            :tval ,*ctb-mod-de-noun*
            ;; NOTE: penalize the number of base nouns in t3
            :stag ,*count-mod-de-noun-t1-t3*
            ;; t1 does not add to the count of base nouns
            :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
                                       L1 L3)))
    (noun-t -> noun-t zhi noun-t
            :com ,*c-close-sep*
            :tval ,*ctb-mod-de-noun*
            ;; NOTE: penalize the number of base nouns in t3
            :stag ,*count-mod-de-noun-t1-t3*
            ;; NOTE: add trait for total number of base nouns in t1
            ;; and t2
            :tag ,(tn (sum-count-base-nouns
                       t1 t3
                       (mod-de-noun-tag t1 (up-ind t3)
                                        L1 L3))))
    (noun-c -> noun-t noun-conn noun-t
            ;; to prefer noun-c as t1
            :com ,(* 2 *c-close-sep*)
            :tval ,(tfn (list :splice
                              (as-NP-or-NNs x1)
                              x2
                              (as-NP-or-NNs x3)))
            ;; NOTE: also sum the number of base nouns of t1 and t3
            :tag ,(tn (sum-count-base-nouns
                       t1 t3
                       (make-conn-thing
                        :first t1
                        :second t3
                        :sep t2)))
            ;; NOTE: t3 not prefer etc, t3 not prefer conn-thing
            ;; TODO: similar-noun should consider also the form
            :stag ,(tn (score-add
                        (+
                         (prefer-less-base-noun t3)
                         (not-prefer-etc t3)
                         (not-prefer t3 'conn-thing
                                     *c-close-suffix*))
                        (similar-noun t1 t3))))
    (noun-c -> noun-c noun-conn noun-t
            :com ,*c-close-sep*
            :tval ,(tfn (list :splice
                              x1 x2 (as-NP-or-NNs x3)))
            ;; NOTE: also sum the number of base nouns of t1 and t3
            :tag ,(tn (sum-count-base-nouns
                       t1 t3
                       (make-conn-thing
                        :first t1
                        :second t3
                        :sep t2)))
            ;; NOTE: t3 not prefer etc, t3 not prefer conn-thing
            ;; TODO: similar-noun should consider also the form
            :stag ,(tn (score-add
                        (+
                         (prefer-less-base-noun t3)
                         (not-prefer-etc t3)
                         (not-prefer t3 'conn-thing
                                     *c-close-suffix*))
                        (similar-noun t1 t3))))
    ,@(allow-quote 'noun-c 4)
    (noun-t => noun-c
            :tval ,*ctb-maybe-UCP*)
    (noun-t -> noun-c etc
            :tval ,(tfn (append (as-phrase-level x1 'NP)
                                (list x2)))
            ;; NOTE: t1 allows only conn-thing, not prefer etc
            :stag ,(tn (not-prefer-etc t1))
            ;; NOTE: this form syntactically only allow etc following
            ;; conn-thing, so there is no issue of the 'etc'
            ;; attaching to non-conn-thing
            :tag ,(tn (more-trait t1 'has-etc t)))
    (noun-t -> noun-c etc noun-t
            :tval ,(tfn
                    `(NP
                      ,(append (as-phrase-level x1 'NP)
                               (list x2))
                      ,(as-phrase-level x3 'NP)))
            ;; NOTE: this form syntactically only allows etc following conn-thing
            ;; NOTE: want t1 to be subtype of t3
            ;; NOTE: t1 allows only conn-thing, not prefer etc
            :stag ,(tn (score-add
                        (if (conn-subtype-of-p t1 (up-ind t3))
                            0
                            *c2*)
                        (not-prefer-etc t1)))
            :tag ,(tn (more-trait t1 'has-etc t)))
    ,@(allow-quote 'noun-t 3)
    ;; special case(s)
    (noun-t -> noun-t zhi v0-p
            :com ,*c-close-sep*
            ;; NOTE: originally from abstract rule, changed abstract
            ;; to noun-t, and noun-p to noun-t
              
            ;; an action
            :tval ,(tfn `(NP (DNP ,x1 ,x2)
                             ,(as-head (ctb-VP-to-NP x3) 'NP)))
            :stag ,(tn (score-add
                        (+
                         ;; mainly to bias against '下' as in '冯女士
                         ;; 的 (确认 下)', but seems other verb-supp
                         ;; are also not common in this context.
                         (min-over-conn
                          t3
                          #'(lambda (v)
                              (cond ((trait-value v 'verb-supp)
                                     *c3*)
                                    ((typep v 'verb0)
                                     (not-prefer-trait
                                      v 'verb-supp *c3*))
                                    (t 0))))
                         (if (= L3 1)
                             ;; want '道' as theory-word-dao4 in '人之道'
                             (+ (not-prefer-verb t3 'v-dao4-d *c-common*)
                                ;; not want '男的在' as noun in '男的在...'
                                (not-prefer-verb t3 'v-at *c-common*))
                             0))
                        (subj-verb t1 t3)))
            :tag ,(tn (more-trait-to
                       (make-instance 'verb-action :verb t3)
                       'noun-mod-has-de t)))
    ;; to distinguish the new noun-t with old noun-p
    (noun-p => noun-t
            :tval ,(tfn (as-phrase-level x1 'NP)))

    ;;;;;
    ;; NOTE: removed these old noun-p-a up to noun-p rules, as they
    ;; are covered by noun-t rules.
    
    ;;(noun -> unknown :com ,*c-unknown-as-noun*)
    ;;(noun-p-a => animate-p :com ,*c-animate-as-noun-p*)
    ;;(noun-p-a => inanimate-p :com ,*c-inanimate-as-noun-p*)
    ;;(noun-p-a => abstract-p :com ,*c-abstract-as-noun-p*)
    ;;(noun-p-a => place-p :com ,*c-place-as-noun-p*)
    ;;(noun-p-a => noun :com ,*c-noun-as-noun-p*)
    ;;(noun-p-a =1> noun-mod :com ,*c-noun-mod-as-noun-p*
	;;		  :tval ,(tfn (ctb-IP-VP-JJ-to-NP x1))
	;;		  :stag ,(tn (noun-mod-as-noun t1))) ;; x, copied and modified to noun-t rule

    ;;(p-pronoun-qi2-zhong1 => p-pronoun
    ;;                      :tval ,(tfn `(NP ,(tag-as 'NN x1)))
    ;;                      :stag ,(tn (prefer t1 'p-pronoun-qi2-zhong1))) ;; x, not needed anymore
    ;;(noun-p-a =2> p-pronoun-qi2-zhong1 num-unit :com ,*c-num-unit-as-noun-p*
    ;;          :tval ,*ctb-splice*) ;; x, composed in noun-t
    ;;(noun-p-a =3> p-pronoun-qi2-zhong1 zhi num-unit :com ,*c-num-unit-as-noun-p*
    ;;          :tval ,*ctb-mod-de-noun*) ;; x, composed in noun-t
    ;;(noun-p-a => clause-c :com ,*c-clause-as-noun-p*
    ;;          ;; NOTE: this is removed, seems not used often? May add
    ;;          ;; back to noun-t if needed.
	;;		  :tval ,*ctb-as-NP*) ;; x
    ;;(noun-p-a =2> noun-mod-b noun-p-a :com ,*c-noun-mod-noun-p*
	;;		  :tval ,*ctb-mod-noun-p*
    ;;          :stag ,*mod-noun-t1-t2*
    ;;          :tag ,*mod-noun-tag-t1-t2*)
	;;
	;;,@(allow-quote 'noun-p-a 2)
    ;;
    ;;(noun-p-b => noun-p-a)
    ;;(noun-p-b -> noun-p-a "的" :com ,*c-close-suffix*
	;;		  :tval ,(tfn `(NP (CP ,x1 ,(tag-as 'DEC x2))))
    ;;          :stag ,(tn (noun-de-as-noun t1))
    ;;          :tag ,(make-instance 'de-noun))
	;;
	;;,@(allow-quote 'noun-p-b 3)
    ;; to allow connective to connect two or more noun-p's
    ;; TODO: should allow the connection at different levels
    ;; i.e. noun CONN noun; noun-p CONN noun-p
    ;;(noun-p-c => noun-p-b)
    ;;(noun-p-c -> noun-p-b conn noun-p-c :com ,*c-close-suffix*
	;;		  :tval ,*ctb-splice*
	;;		  :tag ,*make-conn-t1-t3*
	;;		  :stag ,(tn (similar-noun t1 t3))
	;;		  )
	;;
	;;(noun-p => noun-p-c
	;;		:tval ,*ctb-maybe-UCP*)
    ;;(noun-p =1> noun-p etc :com ,*c-short-conn*
	;;		:tval ,*ctb-append-etc*)
	;;
	;;,@(allow-quote 'noun-p 4)
    ;;;
    (noun-suffix => :noun-suffix :com ,*c-noun-suffix*)
    ,@(strs 'etc-x '("等"
                     ;; "等等" "等等等" "等等等等" ;; these are now
                     ;; composed from '等'
                     "云云" "etc" "巴拉巴拉")
            '->
            :val #'val-cdr
            )
    (etc-a :> etc-x)
    (etc-a -> etc-a etc-x
           :com ,*c-close-sep*
           :val ,(fn (append x1 x2)))
    (etc -> etc-a
         :com ,*c-prep*
         :tval ,(t-str 'ETC))
    ;;;
    (animate-x => :animate
               :stag ,(tn (maybe-ind t1 *c-animate*)))
    (animate-a =1> animate-x
               :stag ,(tn (if (typep t1 'common-name)
                              ;; prefer common-name as human-name below
                              (+ *c-close-suffix*
                                 (if (= L1 1)
                                     *c-common*
                                     0))
                              0))
               :tval ,*may-be-NR*)
    ,@(strs 'animate-prefix '("老" "小") :>)
    ,@(strs 'animate-prefix '("阿" "啊" "呀") :>
            ;; these has more penalty, because they also appear in p-pronoun.
            ;; e.g. want "阿 (陳 經理)", instead of "(阿陳) (經理）"
            :com *c-human-prefix*)
    (animate-a -> animate-prefix integer :com ,*c-animate*
               ;; e.g. "老三", "小三", "阿三"

               ;; TODO: under the current pruning, "小一", ..., "小三
               ;; ", ..., "小六" could not be composed and still
               ;; survive in noun-a, because "小三" also means 'grade
               ;; three'. So "小三" is specifically made a
               ;; human-relationship noun, but could not help with the
               ;; others.
               :tval ,*pos-NN*
               :tag ,*tag-human*)
    (animate-a -> "老" place-a :com ,*c-animate*
               :stag ,(tn (+ (not-prefer t2 'building *c-rare*)
                             (not-prefer t2 'ind)))
               :tval ,*pos-NN*
               :tag ,*tag-human*)
    ;; someone who knows a place very well
    (animate-a -> place-a "通" :com ,*c-animate*
               :stag ,(tn (+ (prefer t1 'name *c-close-suffix*)
                             (typecase t1
                               (country 0)
                               (province 0)
                               (county 0)
                               (city 0)
                               (district 0)
                               (t *c-rare*))))
               :tval ,*pos-NN*
               :tag ,*tag-human*)
    ;; modifiers to kinship titles
    ,@(strs 'kinship-prefix
            '("曾" "太" "外" "內" "堂" "表" "祖"
              "姑" "姨" "舅" "叔" "乾" "繼" "後") :>)
    (kinship-prefix -> "過" "繼")
    (animate-a =2> kinship-prefix animate-a :com ,*c-close-suffix*
               :tval ,*pos-NN*
               :stag ,(tn (conn-must-be t2 'kinship)))
    ;; to refer to Mistresses, most common is 二奶,
    ;; but it is logically possible for higher numbers
    (animate-a -> integer "奶" :com ,*c-animate*
               :tval ,*pos-NN*
               :tag ,*tag-human*)
    ;;
    (animate-generation -> integer "代" :com ,*c-human-suffix*
                        :tag ,(make-instance 'human-suffix))
    ;; common phrase in China to refer to (mainly second) generation
    ;; of some privileged classes
    ,@(strs 'animate-generation-prefix
            '("官" "富" "星" "法") :>)
    (animate-a -> animate-generation-prefix animate-generation
               :com ,*c-animate*
               :tval ,*pos-NN*
               :tag ,*tag-human*)
    ;; common phrase for generation of people
    (animate-a -> integer "後" :com ,*c-animate* 
              :tval ,*pos-NN*
               :tag ,*tag-human*)
    ;;
    (surname =1> :surname
             ;; extra penalty for confusing surnames
             :stag ,(tn (if (typep t1 'confusing-surname)
                            ;; as if each character is a usual noun,
                            ;; connected with *c-close-suffix* penalty
                            (min *c-surname*
                                 (- (* L1 (+ *c-animate*
                                             *c-close-suffix*))
                                    *c-close-suffix*))
                            *c-surname*))
             :tval ,*pos-NR*)
    ;; double surname, used for married woman, where the first is the
    ;; surname of her husband, and the second is the surname of
    ;; herself.
    ;; TODO: mark it to be female for double surname?
    (t-surname => surname
               :stag ,(tn (not-prefer t1 'confusing-surname *c-close-suffix*)))
    (t-surname =1> surname surname
               ;; '西方' mistaken as t-surname
               ;; '路' is also a confusing surname, e.g. '中山 路'
               :stag ,(tn (+ (not-prefer t1 'confusing-surname *c-long-sep*)
                             (not-prefer t2 'confusing-surname *c2*)))
               ;; NOTE: sometimes a name would be mistaken as a
               ;; t-surname because both characters are surnames,
               ;; e.g. '孫文'. Note sure how to fix it without
               ;; context.
               :tval ,*pos-NR*
               ) ;; arbitrarily take the first one as tag
    ;; guess the unknown to be a name
    (name-x => :name)
    (name =1> name-x :com ,*c-unknown-as-noun*
          ;; since name-x is also used for unknown, so if the part is
          ;; not forbidden as name, it would have name as a tag,
          ;; otherwise it would have tag nil
          :stag ,(tn (must-be t1 'name))
          :tval ,*pos-NR*)
    (name => whole-name)
    (name => t-surname
          :stag ,(tn (prefer t1 'japanese-surname *c-fallback*)))
    ;; TODO: consider incurring less penalty for human-name here, so
    ;; that when it is combined with other thing below, the overall
    ;; penalty is lower.  But when using a human-name as animate-a,
    ;; incurring back the usual penalty.

    ;; TODO: may also consider the alternative of allowing any atomic
    ;; word (such as animate-a, abstract-a, etc) as name, but with a
    ;; little more penalty, so that the penalty for name is not too severe.
    (human-name => :human-name
                :tval ,*pos-NR*)
    (human-name => animate-x
                :stag ,(tn (must-be t1 'common-name)))
    (whole-name -> t-surname human-name :com ,*c-close-suffix*
                ;; :com reduced from original *c-surname-name*
                :tval ,*pos-NR*
                :tag ,*tag-human-name*)
    ;;
    
    ;; NOTE: for cases such as "陳 (總 經理)", we use the ad-hoc
    ;; method of letting "總", "副" be part of prefix of title.

    ;; TODO: An alternative is to let surname as special prefix in
    ;; noun-b, so that they are more freely combined. Currently only
    ;; allow t-surname as a noun with some penalty, so it is possible
    ;; to form more complicated expression after a surname.
    ,@(strs 'title-prefix '("總" "副") :>
            :com *c-close-suffix*)
    (title =1> animate-x
           :tval ,*pos-NN*
           :stag ,(tn (typecase t1
                        (occupation *c-prefer*)
                        (can-be-title *c-prefer*)
                        (t *c-fallback*))))
    (title =2> title-prefix title
           :tval ,*pos-NN*
           :stag ,(tn (prefer t2 'occupation *c-common*)))
    ,@(strs 'title-x '("氏" "某" "某某" "某人" "子")
            '-> :com *c-human-suffix*
            :tag (make-instance 'human-suffix))
    ,@(strs 'title-x '("總" "副") '->
            ;; e.g. "陳總", "張總". seems "陳副" is also possible, but
            ;; much less common.
            
             ;; NOTE: this has a bit higher penalty, for fear of
             ;; confusion with "總" as prefix, e.g. "陳 (總 經理)" as
             ;; opposed to "(陳總) (經理)"
             :com (+ *c-human-suffix*
                     *c-close-sep*
                     *c-close-sep*)
             :tag (make-instance 'human-suffix)
             )
    (animate-a -> animate-prefix t-surname :com ,*c-animate*
               ;; NOTE: seems this is not usually used on double
               ;; surname, but we would be more lenient.

               ;; often nicknames between friends, e.g. "老陳", "老李",
               ;; "小陳", "小李", "阿陳"
               :tval ,*pos-NR*
               :tag ,*tag-human-name*)
    (animate-a =2> t-surname title-x :com ,*c-name-close-title*
               :tval ,*pos-NR*)
    ;; double surname is used for woman only, and '生' is suffix for man
    (animate-a -> surname "生" :com ,*c-surname-mr*
               :tval ,*pos-NR*
               :tag ,*tag-human-name*)
    ;;
    (animate-a =2> t-surname title :com ,*c-name-close-title*
               :tval ,*ctb-NP*)
    (animate-a =2> name title :com ,*c-name-title*
               :tval ,*ctb-NP*)
    ;; Some titles are occupations, such as '通讯 员', which is
    ;; composed from '通讯' + '员', so is above animate-a.
    ;; TODO: seems currently cannot use this rule for '(通讯 员)
    ;; (XXX)', but it can be composed from noun-t rules.
    (animate-a =1> title name :com ,*c-title-name*
               ;; if there is something before the title, prefer the
               ;; title to be attached to the thing, so *c-title-name*
               ;; has slightly more penalty than *c-name-title*,
               ;; e.g. '(应急中心 主任) (张大伟)'.
               :tval ,*ctb-NP*)
    ;; names are really problematic!
    (animate-a =1> whole-name :com ,*c-whole-name-as-animate*
               :tval ,#'x1)
    ;;(animate-a =1> animate-a "類" :com ,*c-close-suffix*
    ;;           :stag ,(tn (prefer t1 'ind *c-fallback*)))
    (animate-a -> integer "隊" :com ,*c-animate*
               :tval ,*pos-NN*
               :tag ,*tag-human*)
	;;
	;;,@(allow-quote 'animate-a) ;; x, duplicate in noun-a
    ;;
    ;;; NOTE: conn is now handled in noun-t
    ;;(animate-a-conn => animate-a)
    ;;(animate-a-conn -> animate-a conn animate-a-conn
    ;;                :tval ,*ctb-splice*
    ;;                :tag ,*make-conn-t1-t3*
    ;;                :stag ,(tn (similar-noun t1 t3)))
    ;;
    ;;(animate => animate-a-conn
    ;;         :tval ,*ctb-N-CC-N*)
    ;;
    ;;; NOTE: no need to separate race, political-party, job, and
    ;;; other abstract, now uniformly composed in noun-t, and rely on
    ;;; the noun combination constraints
    ;;(race =1> abstract :stag ,(tn (must-be t1 'race)))
    ;;(political-party =1> abstract :stag ,(tn (must-be t1 'political-party)))
    ;;(job =1> abstract :stag ,(tn (prefer t1 'can-be-job *c-fallback*)))
    
    ;;(abstract-for-animate => abstract
    ;;                      :stag ,(tn (+ (not-prefer t1 'can-be-job *c-fallback*)
    ;;                                    (not-prefer t1 'race *c-fallback*)
    ;;                                    (not-prefer t1 'political-party *c-fallback*))))
    (animate-suffix => :animate-suffix :com ,*c-human-suffix*)
    ;;(animate-suffix => animate-a-conn
    ;;                :tval ,*ctb-N-CC-N*) ;; x, noun-b
    ;;;; NOTE: the animate rules are replaced by noun-b and noun-t rules
    ;;;; some animate-prefix has to be separated because they overlap,
    ;;;; and only the one with the best score will come out of
    ;;;; animate-prefix
    ;;,@(loop :for animate-prefix :in
    ;;     '(pronoun animate place race political-party
    ;;       job dynasty noun-mod animate-attribute
    ;;       abstract-for-animate inanimate dir verb-x
    ;;       num name adj-x)
    ;;     :collect
    ;;     `(animate -> ,animate-prefix animate-suffix
    ;;               ;; put into a splice list first, then promote the component into phrase if necessary
    ;;               :tval ,*ctb-mod-noun*
    ;;               :stag ,*mod-noun-t1-t2*
    ;;               :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                       L1 L2))))
    ;; special case
    ;;(animate =3> integer "路" animate
    ;;         :tval ,*pos-NN*
    ;;         :stag ,(tn (must-be t3 'army-like))) ;; x, integer "路" as num-unit, so composed instead
    ;;(animate =1> pronoun "方" :com ,*c-close-suffix*
    ;;         :tval ,*pos-NN*) ;; x, composed with animate-suffix
    ;;(animate =1> animate noun-suffix ;; :com ,*c-close-suffix*
    ;;         :tval ,*pos-NN*
    ;;         :stag ,*mod-noun-t1-t2*) ;; x, composed in noun-t by noun-b
    ;;(animate =2> "阿" animate :com ,*c-close-suffix*
    ;;         :tval ,*pos-NN*) ;; x, "阿" as p-pronoun
    ;;(animate =2> "啊" animate :com ,*c-close-suffix*
    ;;         :tval ,*pos-NN*) ;; x, "啊" as p-pronoun
    ;;(animate -> "副" animate :com ,*c-close-suffix*
    ;;         :tval ,*pos-NN*
    ;;         :tag ,(tn (more-trait t2 'secondary t))) ;; x, to be composed with adj "副"
    ;;(animate -> "總" animate :com ,*c-close-suffix*
    ;;         :tval ,*pos-NN*
    ;;         :tag ,(tn (more-trait t2 'chief t))) ;; x, to be composed with adj "總"
    ;;,@(strs 'former-JJ '("前") :>
    ;;        :tval *pos-JJ*)
    ;;(animate -> former-JJ animate :com ,*c-close-suffix*
    ;;         :tag ,(tn (more-trait t2 'former t)))
    ;;(animate =2> "神" animate :com ,*c-close-suffix*
    ;;	     :stag ,(tn (prefer t2 'occupation *c-close-suffix*))
    ;;         :tag ,(tn (more-trait t2 'amazing t)))

    ;; for CTB tags as NP as necessary, i.e. if it is chained from pieces instead of a single NN or NR
    ;; TODO: checking if other occurrences of 'animate' should be changed to 't-animate'
    ;;; NOTE: t-animate, animate-conn, animate-p are replaced by noun-t rules.
    ;;(t-animate => animate
    ;;           :tval ,*ctb-NN-chain-to-NP*)
	;;
	;;,@(allow-quote 't-animate 2)
    ;; generic modifiers are assumed to modify the nearest noun to the right
    ;;(animate-conn => t-animate)
    ;;(animate-conn -> t-animate conn animate-conn :com ,*c-close-sep*
    ;;              :tval ,*ctb-splice*
    ;;              :tag ,*make-conn-t1-t3*
    ;;              :stag ,(tn (similar-noun t1 t3)))
    ;; TODO: since animate and animate-p are intended to be used in
    ;; many other places, whether to replace animate with
    ;; animate-conn?

    ;; TODO: currently the conn at animate-p level is handled by the
    ;; generic noun-p conn, whether to change this?
    ;;(animate-p => animate-conn
    ;;           :tval ,*ctb-NP*)
    ;;(animate-p =2> animate animate :com ,*c-close-suffix*
    ;;           ;; TODO: check tval
    ;;           :tval ,*ctb-mod-noun-p*
    ;;           :stag ,*mod-noun-t1-t2*
    ;;           :tag ,*mod-noun-tag-t1-t2*)
    ;;(animate-p =2> noun-mod animate-conn :com ,*c-close-suffix*
    ;;           ;; TODO: check tval
    ;;           :tval ,*ctb-mod-noun-p*
    ;;           :stag ,*mod-noun-t1-t2*
    ;;           :tag ,*mod-noun-tag-t1-t2*)
    ;;(animate-p =1> animate-p etc :com ,*c-close-suffix*
    ;;           :tval ,*ctb-append-etc*)
    ;;

    ;;; TODO: place-p and dir as noun-t still need work, other than
    ;;; this, the following rules could be covered by noun-t rules.
    
    ;;,@(loop :for animate-prefix :in
    ;;     '(pronoun animate-p place-p race political-party
    ;;       job dynasty noun-mod
    ;;       abstract-p inanimate-p dir
    ;;       num name) ;; TODO: remove race, job, dynasty, and use generic noun
    ;;     :collect
    ;;     `(animate-p -> ,animate-prefix zhi animate-conn
    ;;                 :tval ,*ctb-mod-de-noun*
    ;;                 :stag ,*mod-de-noun-t1-t3*
    ;;                 :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                            L1 L3)))
    ;;     ;; prefer left association
    ;;     :collect
    ;;     `(animate-p -> ,animate-prefix zhi animate-p
    ;;                 :tval ,*ctb-mod-de-noun*
    ;;                 :com ,*c-noun-de-noun-p*
    ;;                 :stag ,*mod-de-noun-t1-t3*
    ;;                 :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                            L1 L3)))
    ;;     :collect
    ;;     `(animate-p -> ,animate-prefix animate-p
    ;;                 :tval ,*ctb-mod-noun-p*
    ;;                 :com ,*c-noun-de-noun-p*
    ;;                 :stag ,*mod-noun-t1-t2*
    ;;                 :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                         L1 L2)))
    ;;     )
	;;
	;;,@(allow-quote 'animate-p 3)
    ;;;
    (inanimate-a =1> :inanimate
                 :tval ,*may-be-NR*
                 :stag ,(tn (maybe-ind t1 *c-inanimate*)))
	;;
	;;,@(allow-quote 'inanimate-a) ;; x, duplicate in noun-a
    ;;
    ;;; NOTE: conn is now handled in noun-t
    ;;(inanimate-a-conn => inanimate-a)
    ;;(inanimate-a-conn -> inanimate-a conn inanimate-a-conn
    ;;                  :tval ,*ctb-splice*
    ;;                  :tag ,*make-conn-t1-t3*
    ;;                  :stag ,(tn (similar-noun t1 t3)))
    ;;
    ;;(inanimate => inanimate-a-conn
    ;;           :tval ,*ctb-N-CC-N*)
    ;; special case
    ;;(inanimate =1> inanimate noun-suffix
    ;;           :tval ,*pos-NN*
    ;;           :stag ,*mod-noun-t1-t2*) ;; x, composed in noun-t
    ;;
    
    ;;; NOTE: inanimate, t-inanimate, inanimate-conn, inanimate-p are
    ;;; replaced by noun-t rules, similar to animate-p above.
    
    ;;,@(loop :for inanimate-prefix :in
    ;;     '(inanimate animate place abstract noun-mod
    ;;       dir time num num-unit name verb-x adj-x)
    ;;     :collect
    ;;     `(inanimate -> ,inanimate-prefix inanimate-a-conn
    ;;                 :tval ,*ctb-mod-noun*
    ;;                 :stag ,*mod-noun-t1-t2*
    ;;                 :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                         L1 L2))))
    ;;
    ;;(t-inanimate => inanimate
    ;;             :tval ,*ctb-NN-chain-to-NP*)
	;;
	;;,@(allow-quote 't-inanimate 2)
    ;;
    ;;(inanimate-conn => t-inanimate)
    ;;(inanimate-conn -> t-inanimate conn inanimate-conn :com ,*c-close-sep*
    ;;                :tval ,*ctb-splice*
    ;;                :tag ,*make-conn-t1-t3*
    ;;                :stag ,(tn (similar-noun t1 t3)))
    ;;
    ;;(inanimate-p => inanimate-conn
    ;;             :tval ,*ctb-NP*)
    ;;(inanimate-p =2> adj-p inanimate-conn
    ;;             :tval ,*ctb-mod-noun-p*
    ;;             :com ,*c-short-close-suffix*
    ;;             :stag ,*mod-noun-t1-t2*
    ;;             :tag ,*mod-noun-tag-t1-t2*)
    ;;(inanimate-p =2> noun-mod inanimate-conn
    ;;             :tval ,*ctb-mod-noun-p*
    ;;             :com ,*c-close-suffix*
    ;;             :stag ,*mod-noun-t1-t2*
    ;;             :tag ,*mod-noun-tag-t1-t2*)
    ;;(inanimate-p =2> inanimate inanimate :com ,*c1*
    ;;             :tval ,*ctb-mod-noun-p*
    ;;             ;; TODO: more general list of things
    ;;             :stag ,*mod-noun-t1-t2*
    ;;             :tag ,*mod-noun-tag-t1-t2*)
    ;;(inanimate-p =2> pronoun inanimate-conn
    ;;             :tval ,*ctb-mod-noun-p*
    ;;             :com ,*c-pronoun-noun*
    ;;             :stag ,*mod-noun-t1-t2*
    ;;             :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                     L1 L2)))
    ;;(inanimate-p =2> animate-p inanimate-conn
    ;;             :tval ,*ctb-mod-noun-p*
    ;;             :com ,*c-noun-p-noun*
    ;;             :stag ,*mod-noun-t1-t2*
    ;;             :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                     L1 L2)))
    ;;(inanimate-p =1> inanimate-p etc :com ,*c-close-suffix*
    ;;             :tval ,*ctb-append-etc*)
    ;;
    ;; TODO: for CTB output, the zhi should be DEC for verb modifier,
    ;; but DEG for other noun or adjective modifiers, may use a tfn
    ;; function to check the tag of the modifier
    ;;,@(loop :for inanimate-prefix :in
    ;;     '(pronoun inanimate-p animate-p place-p abstract-p noun-mod
    ;;       dir time num num-unit name verb-x adj-p)
    ;;     :collect
    ;;     `(inanimate-p -> ,inanimate-prefix zhi inanimate-conn
    ;;                   :tval ,*ctb-mod-de-noun*
    ;;                   :stag ,*mod-de-noun-t1-t3*
    ;;                   :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                              L1 L3)))
    ;;     ;; prefer left association
    ;;     :collect
    ;;     `(inanimate-p -> ,inanimate-prefix zhi inanimate-p
    ;;                   :tval ,*ctb-mod-de-noun*
    ;;                   :com ,*c-noun-de-noun-p*
    ;;                   :stag ,*mod-de-noun-t1-t3*
    ;;                   :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                              L1 L3)))
    ;;     :collect
    ;;     `(inanimate-p -> ,inanimate-prefix inanimate-p
    ;;                   :tval ,*ctb-mod-noun-p*
    ;;                   :com ,*c-noun-de-noun-p*
    ;;                   :stag ,*mod-noun-t1-t2*
    ;;                   :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                           L1 L2)))
    ;;     )
	;;
	;;,@(allow-quote 'inanimate-p 3)
    ;;;
    (abstract-a =1> :abstract
                :tval ,*may-be-NR*
                :stag ,(tn (maybe-ind t1 *c-abstract*)))
    (abstract-a => sound :com ,*c-close-suffix*)
    (abstract-a -> unit "次" :com ,*c-close-suffix*
                ;; prefer '首' as ordinal in '首次'
                :stag ,(tn (not-prefer t1 'shou3 *c-common*))
                :tval ,*pos-M*
                :tag ,(make-instance 'number-of))
    (abstract-a -> noun-a noun-a "比" :com ,*c-close-suffix*
                :tval ,*pos-NN*
                :stag ,(tn (+ (prefer-mono L1 *c-rare*)
                              (prefer-mono L2 *c-rare*)))
                :tag ,(make-instance 'ratio-of))
    (abstract-a -> place-a "獨" :com ,*c-close-suffix*
                ;; e.g. "港獨", "藏獨"
                :tval ,*pos-NN*
                :stag ,(tn (prefer-mono L1 *c-rare*))
                :tag ,(make-instance 'event))
    (abstract-a -> integer "超" :com ,*c-close-suffix*
                ;; ?? e.g. "(美国 一超) 独大"
                :tval ,(tfn `(:splice ,(as-phrase-level x1)
                                      (NP ,(tag-as 'NN x2))))
                :tag ,(make-instance 'abstract-or-human))
    (abstract-a -> int "大"
                :com ,*c-abstract-suffix*
                ;; shorthand for '第 XX 次全國代表大會'
                :stag ,(tn (if (and (integerp t1) (> t1 1))
                               *c-prefer*
                               nil))
                :tval ,*pos-NN*
                :tag ,(make-instance 'discussion-meeting)
                )
    (qiang2-num => integer)
    (qiang2-num => ordinal)
    (abstract-a -> qiang2-num "強" :com ,*c-close-suffix*
                :tval ,(tfn `(:splice ,(as-phrase-level x1)
                                      (NP ,(tag-as 'NN x2))))
                :tag ,(make-instance 'abstract-or-human))
	;;
	;;,@(allow-quote 'abstract-a) ;; x, duplicate in noun-a
    ;;
    
    ;;; NOTE: abstract-a-conn, abstract, t-abstract, abstract-p are
    ;;; replaced by noun-t rules
    ;;(abstract-a-conn => abstract-a)
    ;;(abstract-a-conn -> abstract-a conn abstract-a-conn
    ;;                 :tval ,*ctb-splice*
    ;;                 :tag ,*make-conn-t1-t3*
    ;;                 :stag ,(tn (similar-noun t1 t3)))
    ;;
    ;;(abstract => abstract-a-conn
    ;;          :tval ,*ctb-N-CC-N*)
    ;;
    (abstract-suffix => :abstract-suffix :com ,*c-abstract-suffix*)
    ;;(abstract-suffix => abstract-a-conn
    ;;                 :tval ,*ctb-N-CC-N*)
    ;;,@(loop :for abstract-prefix :in
    ;;     '(pronoun abstract animate place noun-mod inanimate
    ;;       time unit num name dir verb-x adj-x)
    ;;     :collect
    ;;     `(abstract -> ,abstract-prefix abstract-suffix
    ;;                :tval ,*ctb-mod-noun*
    ;;                :stag ,*mod-noun-t1-t2*
    ;;                :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                        L1 L2))))
    ;; special case
    ;;(abstract -> noun-p zhi v0-p
    ;;          ;; NOTE: copied and modified to be noun-t special rule
    ;;          ;; an action
    ;;          :tval ,(tfn `(NP (DNP ,x1 ,x2)
    ;;                           ,(as-head (ctb-VP-to-NP x3) 'NP)))
    ;;          :stag ,(tn (subj-verb t1 t3))
    ;;          :tag ,(tn (make-instance 'verb-action :verb t3))) ;; x
    ;;(abstract =1> abstract noun-suffix
    ;;          :tval ,*pos-NN*
    ;;          :stag ,*mod-noun-t1-t2*) ;; x, covered by noun-t rules
    ;;(abstract =2> "總" abstract :com ,*c-close-suffix*
    ;;          :tval ,(tfn `(NP (ADJP (JJ ,x1)) (as-head x2 'NP)))
    ;;          :stag ,(tn (must-be t2 'amount-of))) ;; x, composed with adj-word-chief, with adj-noun constraints
    ;;
    ;;(t-abstract => abstract
    ;;            :tval ,*ctb-NN-chain-to-NP*)
	;;
	;;,@(allow-quote 't-abstract 2)
    ;;
    ;;(abstract-conn => t-abstract)
    ;;(abstract-conn -> t-abstract conn abstract-conn :com ,*c-close-sep*
    ;;               :tval ,*ctb-splice*
    ;;               :tag ,*make-conn-t1-t3*
    ;;               :stag ,(tn (similar-noun t1 t3)))
    ;;
    ;;(abstract-p => abstract-conn
    ;;            :tval ,*ctb-NP*)
    ;;(abstract-p =2> adj-p abstract-conn
    ;;            :tval ,*ctb-mod-noun-p*
    ;;            :com ,*c-short-close-suffix*
    ;;            :stag ,*mod-noun-t1-t2*
    ;;            :tag ,*mod-noun-tag-t1-t2*)
    ;;(abstract-p =2> abstract abstract :com ,*c-close-suffix*
    ;;            :tval ,*ctb-mod-noun-p*
    ;;            ;; TODO: how to tune this score
    ;;            :stag ,*mod-noun-t1-t2*
    ;;            :tag ,*mod-noun-tag-t1-t2*)
    ;;(abstract-p =1> abstract-p etc :com ,*c-close-suffix*
    ;;            :tval ,*ctb-append-etc*)
    ;;
    ;;,@(loop :for abstract-prefix :in
    ;;     '(pronoun abstract-p animate-p place-p noun-mod inanimate-p
    ;;       time unit num name)
    ;;     :collect
    ;;     `(abstract-p -> ,abstract-prefix zhi abstract-conn
    ;;                  :tval ,*ctb-mod-de-noun*
    ;;                  :stag ,*mod-de-noun-t1-t3*
    ;;                  :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                             L1 L3)))
    ;;     ;; prefer left association
    ;;     :collect
    ;;     `(abstract-p -> ,abstract-prefix zhi abstract-p
    ;;                  :tval ,*ctb-mod-de-noun*
    ;;                  :com ,*c-noun-de-noun-p*
    ;;                  :stag ,*mod-de-noun-t1-t3*
    ;;                  :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                             L1 L3)))
    ;;     :collect
    ;;     `(abstract-p -> ,abstract-prefix abstract
    ;;                  :tval ,*ctb-mod-noun-p*
    ;;                  :com ,*c-noun-p-noun*
    ;;                  :stag ,*mod-noun-t1-t2*
    ;;                  :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                          L1 L2)))
    ;;     :collect
    ;;     `(abstract-p -> ,abstract-prefix abstract-p
    ;;                  :tval ,*ctb-mod-noun-p*
    ;;                  :com ,(+ *c-noun-de-noun-p*
    ;;                           *c-close-suffix*)
    ;;                  :stag ,*mod-noun-t1-t2*
    ;;                  :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                          L1 L2)))
    ;;     )
	;;
	;;,@(allow-quote 'abstract-p 3)
    ;;;
    (x-place => :place)
    (country-abbr-x => x-place
                    :tval ,*pos-NR*
                    ;; TODO: whether to make a generic place-abbr?
                    :stag ,(tn (must-be t1 'country-abbr)))
    ;; also include province-abbr here, for e.g. 台港澳
    (country-abbr-x => x-place :com ,*c-close-sep*
                    :tval ,*pos-NR*
                    :stag ,(tn (must-be t1 'province-abbr)))

    ;; TODO: maybe add a union type for tags
    (country-abbr-xx => country-abbr-x :val ,(fn (list x1))
                     :tval ,(tfn (list x1)))
    (country-abbr-xx =2> country-abbr-x country-abbr-xx
                     :com ,*c-close-suffix*
                     :tval ,(tfn (cons x1 x2))
                     :tag ,(tn (make-conn-thing :first t1
                                                :second t2))
                     :val ,(fn (cons x1 x2)))
    (country-abbr =1> country-abbr-xx :com ,*c-place*
                  ;; penalize those confusing ones when used alone.
                  ;; note that when used in combination (a list), the
                  ;; tag would be a conn-thing, so will not be
                  ;; penalized.
                  :stag ,(tn (not-prefer t1 'confusing-country-abbr *c-close-suffix*))
                  :tval ,(tfn (cons :splice x1)))
    ;;
    (place-a => country-abbr)
    (place-a =1> x-place
             :tval ,*may-be-NR*
             :stag ,(tn (score-add
                         (not-prefer t1 'country-abbr nil)
                         (maybe-ind t1 *c-place*))))
    (place-a -> name :com ,*c-name-as-place*
             :stag ,(tn (not-prefer t1 'human-name))
             :tval ,*pos-NN*
             :tag ,(make-instance 'place-name))
    (place-a -> "方" "圓" num unit :com ,*c-place*
             :tval ,*pos-NN*
             :stag ,(tn (must-be t4 'unit-length))
             :tag ,(make-instance 'place))
    ,@(strs 'longitude-latitude '("東經" "西經" "南緯" "北緯"))
    (place-a -> longitude-latitude num :com ,*c-place*
             :tval ,*pos-NN*
             :tag ,(make-instance 'place))
    (place-a -> longitude-latitude num "度" :com ,*c-place*
             :tval ,*pos-NN*
             :tag ,(make-instance 'place))
	;;
	,@(allow-quote 'place-a)
    ;; NOTE: replaced by two special rules for noun-t
    ;;(noun-place-for-kou3 :> place) ;; ??
    ;;(noun-place-for-kou3 :> inanimate) ;; ??
    ;;(noun-place-for-kou3 :> verb-x :com ,*c-verb-as-place*
    ;;                     ;; prefer verb form of 进口, 出口, 入口
    ;;                     :stag ,(tn (+ (not-prefer t1 'v-chu1 *c-rare*)
    ;;                                   (not-prefer t1 'v-into *c-rare*)
    ;;                                   (not-prefer t1 'v-ru4 *c-rare*))))
    ;;(place-b =1> noun-place-for-kou3 "口" :com ,*c-short-close-suffix*
    ;;         :tval ,*pos-NN*
    ;;         :tag ,(tn (if (typep t1 'place)
    ;;                       t1
    ;;                       (make-instance 'place))))
    ;;
    (place-b => place-a)
    (place-b =1> place-a "子" :com ,*c-close-suffix*
             :tval ,*pos-NN*
             ;; prefer mono
             :stag ,(tn (+
                         (prefer-mono L1 *c-common*)
                         (not-prefer t1 'place-name *c-rare*))))
    (place-b =2> name place-a
             :tval ,*ctb-mod-noun*
             :stag ,*mod-noun-t1-t2*
             :tag ,*mod-noun-tag-t1-t2*)
    ;;(place-b -> place-a dir-suffix :com ,*c1*
    ;;         ;; moved to noun-b rule.
    ;;         :tag ,(make-instance 'place-with-dir)
    ;;         :tval ,*place-with-dir*
    ;;         :stag ,(tn (noun-dir-as-place t1 t2)))
    (place-b =2> dir place-a :com ,*c0*
             :tval ,*place-with-dir*
             :stag ,(tn (+ (prefer t1 'dir-NESW)
                           ;; e.g. 西岸
                           (if (>= L1 L2)
                               0
                               *c-rare*))))
    ;; NOTE: retain only up to place-b, and the rest (place-c to
    ;; place) covered with noun-t rules.
    
    ;;,@(loop :for place-prefix :in
    ;;     '(place-b pronoun animate inanimate abstract
    ;;       verb-x adj-x p-pronoun dir num time noun-mod)
    ;;     :collect
    ;;     `(place-b -> ,place-prefix place-a
    ;;               :tval ,*ctb-mod-noun*
    ;;               :stag ,*mod-noun-t1-t2*
    ;;               :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                       L1 L2))))
	;;
	;;,@(allow-quote 'place-b 2)
    ;; TODO: change the way these suffixes are handled, use ind and name instead
    ;; TODO: allow connective here for now, should generalize to other levels
    ;;(place-b-conn => place-b)
    ;;(place-b-conn -> place-b conn place-b-conn
	;;			  :tval ,*ctb-splice*
    ;;              :tag ,*make-conn-t1-t3*
    ;;              :stag ,(tn (similar-noun t1 t3)))
    ;;
    ;;(place-c => place-b-conn
	;;		 ;; x1 may be a single item, or (:splice ...) where each is a NN, NP or LCP
	;;		 :tval ,*ctb-place-conn*)
    ;;(place-c => dir-suffix-place :com ,*c-close-sep*) ;; x, replaced with noun-b rule
	;;
	;;,@(allow-quote 'place-c 3)
    ;;
    (x-dir-suffix => :dir-suffix :com ,*c-dir-suffix*)
    (pos-suffix => x-dir-suffix
                :tval ,*pos-LC*
                :stag ,(tn (must-be t1 'pos-suffix)))
    (dir-suffix => dir)
    (dir-suffix => pos-suffix)
    (dir-suffix => dir-suffix-place)
    (dir-suffix-place =1> dir :com ,*c-close-sep*
                      :tval ,#'x1)
    (dir-suffix-place =1> dir pos-suffix
                      :tval ,*pos-LC*)
    (dir-suffix-place =1> x-dir-suffix
                      :tval ,*pos-LC*
                      :stag ,(tn (must-be t1 'dir-suffix-place)))
    (dir-suffix-place =2> "最" dir-suffix-place
                      :tval ,*pos-LC*)
    ;;
    ;;(place-d => place-c
	;;		 :tval ,(tfn (as-phrase-level x1)))
    ;;(place-d =1> place-c noun-suffix
    ;;         ;; :com ,*c-close-suffix*
	;;		 ;; TODO: check tval
	;;		 :tval ,*ctb-NP*
    ;;         :stag ,*mod-noun-t1-t2*)
    ;;
    ;;,@(loop :for place-prefix :in
    ;;     '(place-c pronoun animate-p inanimate-p abstract-p
    ;;       verb-x adj-x p-pronoun dir num time noun-mod)
    ;;     :collect
    ;;     `(place-d -> ,place-prefix zhi place-c
	;;			   :tval ,*ctb-mod-de-noun*
    ;;               :stag ,*mod-de-noun-t1-t3*
    ;;               :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                          L1 L3)))
    ;;     ;;
    ;;     :collect
    ;;     `(place-d -> ,place-prefix place-d
	;;			   :tval ,*ctb-mod-noun-p*
    ;;               :com ,*c-noun-de-noun-p*
    ;;               :stag ,*mod-noun-t1-t2*
    ;;               :tag ,(tn (mod-noun-tag t1 (up-ind t2)
    ;;                                       L1 L2)))
    ;;     ;;
    ;;     :collect
    ;;     `(place-d -> ,place-prefix zhi place-d
	;;			   :tval ,*ctb-mod-de-noun*
    ;;               :com ,*c-noun-de-noun-p*
    ;;               :stag ,*mod-de-noun-t1-t3*
    ;;               :tag ,(tn (mod-de-noun-tag t1 (up-ind t3)
    ;;                                          L1 L3)))
    ;;     ;;
    ;;     :collect
    ;;     `(place -> ,(if (eq place-prefix 'place-c)
    ;;                     'place-d
    ;;                     place-prefix)
    ;;             dir-suffix
    ;;             :com ,*c0*
    ;;             :stag ,(tn (noun-dir-as-place t1 t2))
    ;;             :tval ,*place-with-dir*
    ;;             :tag ,(make-instance 'place-with-dir))
    ;;     ;;
    ;;     )
	;;
	;;,@(allow-quote 'place-d 4)
    ;;
    ;;(place-d-conn => place-d)
    ;;(place-d-conn -> place-d conn place-d-conn :com ,*c-close-sep*
	;;			  :tval ,*ctb-splice*
    ;;              :tag ,*make-conn-t1-t3*
    ;;              :stag ,(tn (similar-noun t1 t3)))
    ;; place has no more modifier at the front
    ;;(place-e => place-d-conn
	;;		 :tval ,*ctb-place-conn*)
    ;;(place-e -> place-d conn place-e :com ,(+ *c-close-sep* *c-close-sep*)
	;;		 :tval ,*ctb-splice*
	;;		 :tag ,*make-conn-t1-t3*
	;;		 :stag ,(tn (similar-noun t1 t3)))
    ;;(place-e =2> place-d place-d :com ,*c-close-suffix*
	;;		 :stag ,*mod-noun-t1-t2*
    ;;         :tag ,*mod-noun-tag-t1-t2*)
	;;
	;;(place => place-e
	;;	   :tval ,*ctb-place-conn*)
	;;
	;;,@(allow-quote 'place 5)
    ;;
    ;;
    (place => noun-t
           ;; NOTE: previously uses 'must-be', which ignores the
           ;; possibility of conn-thing
           ;; NOTE: Now also allow dir, e.g. dir-word-up, dir-word-down, dir-middle? They are for forming abstract places.
           :stag ,(tn (prefer-conn-any t1 '(place dir) nil)))
    ;; TODO: similar to time-from, time-at-to and time-starting?
    
    ;; NOTE: place-p is retained for use in a few grammar rules which
    ;; need specifically a 'place phrase', but place-p should not loop
    ;; back to noun-t.
    
    (place-p => place)
    (place-p => place-p-a) ;; covered with special noun-t rule to place-p-a
    ;; NOTE: place-p-a is retained, it is used in subj-pred, which
    ;; prefers a place with an ind.
    (place-p-a =2> place-ind place :com ,*c-place-ind-place*
               :tag ,(tn (more-trait t2 'place-with-ind t))
			   :tval ,(tfn `(PP ,x1 ,(as-head x2 'NP))))
    ,@(strs 'place-ind '("於" "從" "到" "向" "傍" "由") :>
            :tval *pos-P*
            :com *c-prep*)
    ,@(strs 'place-ind '("沿着" "延着") '->
            :tval *pos-P*
            :com *c-prep*)
    (place-ind :> "在" :com ,*c-prep*
               :tval ,*pos-P*
               :tag ,(make-instance 'v-at))
    (place-ind -> "a" "t" :com ,*c-prep*
               :tval ,*pos-P*
               :tag ,(make-instance 'v-at))
    ;;(place-p =1> place-p etc :com ,*c-close-suffix*
	;;		 :tval ,*ctb-append-etc*)
	;;
	;;,@(allow-quote 'place-p 6)
    ;;;
    ;;
    ,@(strs 'comma '("," "、") :>
            :tval *pos-PU*)
    ;;
    (word => noun-p :com ,*c-noun-p-alone*
          ;; prefer the sound literal as exclaim, if alone
          :stag ,(tn (not-prefer t1 'sound-literal)))
    ;;
    (noun-p-PU -> noun-p punctuations
               :stag ,(tn (if (member t2 '(:full-stop :exclaimation-mark :question-mark))
                              *c-common*
                              *c-close-suffix*))
               :tval ,*ctb-as-frag*)
    (word :> noun-p-PU :com ,*c-noun-p-alone*)
    ;; In CTB, there are FRAGs that are page numbers
    ,@(strs 'page-p. '("p" "p.") '->
            :tval *pos-NN*)
    (page-number -> page-p. num
                 :tval ,*ctb-as-frag*)
    (word :> page-number :com ,*c-noun-p-alone*)
    ;;
    (verb-mod-PU -> verb-mod punctuations
                 :tval ,(tfn (cond ((or (headed-by x1 'BA)
                                        (headed-by x1 'LB)
                                        (headed-by x1 :verb-mod-cross))
                                    ;; e.g. (BA (BA 把) noun-p)
                                    ;; e.g. (LB (LB 被) noun-p)
                                    ;; e.g. (LB (LB 被) noun-p (MSP 所)))
                                    
                                    ;; Not sure how to bracket these, seems not appeared in CTB
                                    `(FRAG (VP ,@(cdr x1))
                                           ,x2))
                                   (t `(FRAG ,x1 ,x2)))))
    (word :> verb-mod-PU :com ,*c-adv-alone*)
    ;;
    (x-dir => :dir :com ,*c-dir*)
    (dir =1> x-dir
         :tval ,*pos-LC*)
    (dir -> x-dir dir
         :com ,*c-close-sep*
         :tval ,*pos-LC*
         :tag ,(tn (make-conn-thing :first t1
                                    :second t2))
         :val ,(fn (cons 'dir (cons x1 (cdr x2)))))
    (word => dir :com ,*c-dir-alone*)
    ;;
    (unit-x => :unit :com ,(- *c-unit* *c-close-sep*)
            :stag ,(tn (cond ((typep t1 'compound) *c-prefer*)
                             (t *c-close-sep*))))
    (unit =1> unit-x :tval ,*pos-M*)
    ;;
    (len-sq :> "平")
    (len-sq -> "平" "方")
    (unit -> len-sq unit-x :com ,*c-close-sep*
          :tval ,*pos-M*
          :stag ,(tn (must-be t2 'unit-length))
          :tag ,(make-instance 'unit-area))
    (unit -> len-sq :com ,*c-close-sep*
          ;; shorthand
          :tval ,*pos-M*
          :tag ,(make-instance 'unit-area))
    ;;
    (unit -> "立" "方" unit-x :com ,*c-close-sep*
          :tval ,*pos-M*
          :stag ,(tn (must-be t3 'unit-length))
          :tag ,(make-instance 'unit-volume))
    ;;
    (unit-r-inanimate :> noun-b :com ,*c-close-sep*
                      ;; NOTE: replaced inanimate with noun-b
                      :stag ,(tn (cond ((typep t1 'container)
                                        *c-prefer*)
                                       ((and (typep t1 'tool)
                                             (typep t1 'ind))
                                        *c-close-sep*)
                                       ((typep t1 'body-part)
                                        *c-long-sep*)
                                       (t nil)))
                      :tag ,(let ((u-container
                                   (make-instance 'unit-container))
                                  (u-tool
                                   (make-instance 'unit-tool))
                                  (u-body-part
                                   (make-instance 'unit-body-part)))
                                 (tn (cond ((typep t1 'container) u-container)
                                           ((typep t1 'tool) u-tool)
                                           ((typep t1 'body-part) u-body-part)
                                           (t nil)))))
    (unit-r-place :> place-a :com ,*c-close-sep*
                  :stag ,(tn (must-be t1 'container))
                  :tag ,(make-instance 'unit-container))
    (unit-r-abstract -> abstract-a :com ,*c-close-sep*
                     :stag ,(tn (cond ((typep t1 'currency-unit)
                                       *c-prefer*)
                                      ((typep t1 'currency-maybe-unit)
                                       *c-close-sep*)
                                      (t *c-fallback*)))
                     :tag ,(let ((u-unit (make-instance 'unit))
                                 (u-money (make-instance 'unit-money)))
                                (tn
                                  (typecase t1
                                    (currency-unit u-money)
                                    (currency-maybe-unit u-money)
                                    (t u-unit)))))
    ;; the repeating pattern of derived units,
    ;; Note: does not enforce the two to be the same
    (unit =1> unit-r-inanimate
          :tval ,*pos-M*)
    (unit =1> unit-r-inanimate unit-r-inanimate
          :com ,*c-close-sep*
          :tval ,*pos-M*)
    ;;
    (unit =1> unit-r-place
          :tval ,*pos-M*)
    (unit =1> unit-r-place unit-r-place
          :com ,*c-close-sep*
          :tval ,*pos-M*)
    ;;
    (unit =1> unit-r-abstract
          :tval ,*pos-M*)
    (unit =1> unit-r-abstract unit-r-abstract
          :com ,*c-close-sep*
          :tval ,*pos-M*)
    ;;
    (unit =2> country-abbr unit
          :tval ,*pos-M*
          :stag ,(tn (cond ((typep t2 'yuan2) *c-prefer*)
                           ((typep t2 'unit-money) *c-close-sep*)
                           (t nil)))
          )
    ;;
    (unit =1> unit "兒" :com ,*c-close-suffix*
          :tval ,*pos-M*)
	;;
	,@(allow-quote 'unit)
    ;;
    (unit-p => unit
            :tval ,(tfn `(CLP ,x1)))
    (unit-p =2> adj-a unit
            :tval ,(tfn `(CLP ,(as-head x1 'ADJP) ,x2))
            :stag ,(tn (+
                        ;; prefer '一次 (重 污染 天气)' to '一 (次重) (污染 天气)'
                        (not-prefer t1 'adj-word-ci4 *c-common*)
                        (not-prefer t1 'adj-word-ma2 *c-common*)
                        (not-prefer t2 'unit-money *c-common*)))
            )
	;;
	,@(allow-quote 'unit-p 2)
	;;
    (word => unit-p :com ,*c-unit-alone*)
    
    ;; include approximate numbers
    ;; num: [pre-approx]* [pre-num-approx] num-a [num-suff-approx]*
    ;; num-unit: num unit-p [unit-suff-approx]*
    ;;   noun [noun-suff-approx]*
    ,@(strs 'pre-approx '("約" "約莫" "約摸" "大約" "將近" "每" "近" "不到")
            '->
            :tval *pos-AD*)
    ;; those that are more easily confused with other words are penalized
    (pre-num-approx -> "成" :com ,*c-pre-approx*)
    (pre-num-approx -> "上" :com ,*c-pre-approx*)
    ,@(strs 'pre-num-approx '("好幾" "不幾" "幾" "若干" "無數") '->
            :com *c-close-sep*)
    ;;
    ,@(strs 'num-suff-approx '("幾" "多" "數" "來" "把" "餘"))
    ,@(strs 'num-suff-approx-L '("左右" "開外" "上下") '->
            :tval *pos-LC*)
    ;;
    ,@(strs 'unit-suff-approx '("多") '->
            :tval *pos-AD*)
    ,@(strs 'unit-suff-approx '("來" "把" "左右" "上下" "開外"
                                "內" "以上" "以下" "以內" "以外")
            ;; TODO: monosyllabic?
            '->
            :tval *pos-LC*)
    ,@(strs 'noun-suff-approx '("左右" "上下" "開外"
                                "以上" "以下" "以內" "以外")
            '->
            :tval *pos-LC*)
    ,@(strs 'rough-amount '("許多" "許許多多" "好多" "很多"
                            "諸多" "眾" "眾多" "多" "多多少少"
                            "少許" "些許" "好少" "好些" "一些"
                            "一點" "不少" "一股子" "過半"
                            "大量" "少量" "天量" "海量" "大部"
                            "部分" "一部分" "大部分" "一大部分" "絕大部分"
                            "少部分" "絕少部分" "小部分" "一小部分" "絕小部分"
                            "大多" "多數" "極多數" "大多數" "絕多數" "絕大多數"
                            "少數" "極少數" "絕少數" "半數" "大半數"
                            "大批" "一大批" "大半" "一大半"
                            "一點" "一點點" "丁點" "一丁點" "丁點兒" "一丁點兒" "一星半點"
                            "七七八八" "八八九九" "九九十十"
                            "個把")
            ;; note: 很少 and 不多 are not tagged as CD in CTB
            '->
            :tval *pos-CD*)
    ,@(strs 'rough-amount '("每")
            '->
            :tval *pos-DT*)
    ;; the XXYY pattern as rough number
    (ten-power => hundred)
    (ten-power => thousand)
    (ten-power => ten-thousand)
    (ten-power => hundred-million)
    (ten-power => hundred-trillion)
    (rough-ten-power -> ten-power ten-power
                     :tval ,*pos-CD*
                     :com ,(+ *c-num* *c-close-sep*))
    (rough-ten-power -> ten-power ten-power ten-power ten-power
                     :com ,*c-num*
                     :tval ,*pos-CD*
                     :stag ,(tn (if (and (= t1 t2)
                                         (= t3 t4))
                                    *c-prefer*
                                    *c-rare*)))
    (rough-amount => rough-ten-power)
    ,@(strs 'shu4-yi3 '("以" "數") :>)
    ,@(strs 'shu4-yi3 '("數以") '->)
    (rough-amount -> shu4-yi3 num-a "計"
                  :com ,*c-num*
                  :tval ,*pos-CD*)
    ;;
    (x-rough-amount :> rough-amount :tag ,(make-instance 'rough-amount))
    ,@(strs 'ask-amount '("幾" "多少" "幾多" "幾許"
                          "凡幾" "幾何")
            '->
            :tval *pos-CD*)
    (x-ask-amount :> ask-amount :tag ,(make-instance 'ask-amount))

    ;;
    (num-a => number)
    (num-a -> integer integer :com ,*c-approx-int-int*
           ;; TODO: whether to include fraction, decimal, percentage?
           :tval ,*pos-CD*
           :stag ,(tn (same-order-of-magnitude t1 t2)))
    (num-a -> "成" int "上" int :com ,*c-num*
           :tval ,*pos-CD*)
    (num-a -> number "~" number :com ,*c-num-range*
           :tval ,*pos-CD*
           :stag ,(tn (same-order-of-magnitude t1 t3)))
    (num-a => ordinal)

    ;; CTB: collapse for XX幾, XX多, XX來.
    ;; num-b-x could be used in composing other numbers, e.g. XX幾万.
    ;; need to get number structure for tag, so that its order of magnitude could be compared.
    (num-b-x -> num-a num-suff-approx
             :com ,*c-close-sep*
             :tag ,#'tag-identity)
    (num-b-x -> num-b-x num-suff-approx
             :com ,*c-close-sep*
             :tag ,#'tag-identity)
    (num-b => num-a)
    (num-b => num-b-x
           :tval ,*pos-CD*)
    (num-b -> "$" num-b
           :tval ,*pos-CD*)
    (ratio-num => decimal)
    (ratio-num => int)
    (num-b -> ratio-num ":" ratio-num
           :stag ,(tn (if (or (integerp t1)
                              (integerp t3))
                          *c-prefer*
                          *c-common*))
           :com ,*c-num*
           :tval ,*pos-CD*)
    
    (num-c => num-b)
    (num-c -> pre-num-approx num-b
           :tval ,*pos-CD*)

    ,@(strs 'num-range-sep '("-" "到" "至") :>
            :tval *pos-CC*)
    ;; TODO: ... 不等 (as AD)
    ;; TODO: how to tag the num-range-sep?
    (short-pause-comma :> "、" :tval ,*pos-PU*)
    (num-c-range -> num-c num-range-sep num-c :com ,*c-num-range*)
    (num-c-range -> num-c short-pause-comma num-c)
    (num-c-range => num-c)

    ;; CTB: but not for XX/CD 左右/LC,  XX/CD 開外/LC, XX/CD 上下/LC
    (num-d => num-c-range)
    (num-d -> num-d num-suff-approx-L)

    (num-e => num-d)
    (num-e -> pre-approx num-e
           :com ,*c-close-sep* ;; to avoid ambiguity with pre-approx in num-unit
           :tval ,*ctb-splice*)
    ;;
    (num-f => num-e)
    (num-f -> num-e num-range-sep num-e :com ,*c-approx-num-range*)

    ;; TODO: propagate tags for numbers
    (num => num-f
         :tag ,(tn (as-num t1)))
    (num => x-rough-amount)
    (num =2> verb-x num-f
         ;; e.g. '逾 103万'
         :com ,*c1*
         :tag ,(tn (more-trait (as-num t2) 'num-has-verb-mod t))
         :stag ,(tn (score-max
                     (must-be t1 'vc-num-compare)
                     (must-be t1 'v-shao4-b))))
    ;; some approx words are tagged as CD in CTB, when followed by units
    ;;(num => pre-num-approx :tval ,*pos-CD*) ;; already in num10
    ;; ad-hoc words tagged as CD in CTB
    ,@(strs 'num '("單" "倆") '->
            :tag (make-instance 'num)
            :tval *pos-CD*)
    ;;(num => x-ask-amount) ;; ask-amount already in a digit
	,@(allow-quote 'num 1)
    ;;
    (num-unit-a =2> num unit-p
                :tval ,(tfn `(QP ,x1 ,x2)))
    (num-unit-a =1> num-unit-a unit-suff-approx
                :tval ,(tfn
                        (if (headed-by x2 'AD)
                            ;; 多
                            `(QP ,x1 (ADVP ,x2))
                            ;; 來, 左右, 上下, ...
                            `(LCP ,x1 ,x2))))

    (num-unit-b => num-unit-a)
    ;; sometimes the unit could be omitted
    (num-unit-b => x-rough-amount :com ,*c-close-sep*
				:tval ,(tfn `(QP ,x1)))
    (num-unit-b => x-ask-amount :com ,*c-close-sep*
				:tval ,(tfn `(QP ,x1)))
    (num-unit-b =2> pre-approx num-unit-b
                :tval ,*ctb-adv-num-unit*
                )
    (num-unit-b =2> adv num-unit-b
                :tval ,*ctb-adv-num-unit*
                :stag ,(tn (typecase t1
                             (adv-below-zero (must-be t2 'unit-temperature))
                             (adv-on-num-unit *c-close-sep*)
                             (adj-on-length (must-be t2 'unit-length))
                             (t nil)))
                )
	;;
	,@(allow-quote 'num-unit-b)
	;;
	(num-unit-conn => num-unit-b)
	(num-unit-conn -> num-unit-b noun-conn num-unit-conn
				   :tval ,*ctb-splice*
				   :tag ,*make-conn-t1-t3*)
	;;
	(num-unit => num-unit-conn
			  :tval ,(tfn (if (headed-by x1 :splice)
							  (as-head x1 'QP)
							  x1)))
	;;
	,@(allow-quote 'num-unit)
	;;
    (word => num-unit :com ,*c-num-unit-alone*)
    ;; time
    
    ;; For penalizing times with different scales (e.g. year vs day)
    ;; forming conn, we use traits to annotate the rough time
    ;; scale. Since time is roughly linear in scale, we use number for
    ;; convenience, and annotate the max and min scale.

    ;; The base time-scale-max and time-scale-min traits have been
    ;; prepared for the tags in *times-hash* and *dynasties-hash*, but
    ;; the newly created time-name have to be added below.
	(noun-conn => conn)
	,@(strs 'noun-conn '("至") :>
            ;; useful for place, time, or '4级中度污染 至 5级重度污染'
            :tag :conn-word
            :com (- *c-conn* *c-close-sep*)
			:tval *pos-CC*)
    ,@(strs 'noun-conn '("-" "--") '->
            ;; more often used for time
            :tag :conn-word
            :com (+ *c-conn* *c-long-sep*)
			:tval *pos-CC*)
    (noun-conn -> may-pause "甚" "至"
               ;; e.g. '台湾、日本和韩国，甚至美国'
               :com ,(+ *c-conn* *c-long-sep*)
               :tag :conn-word
               :tval ,(tfn `(:splice ,x1 ,(tag-as 'CC x2 x3)))
               :stag ,(tn (case t1
                            ((:colon) *c4*)
                            ((:semi-colon) *c2*)
                            ((:comma) *c-close-suffix*)
                            ((:short-comma) *c-long-sep*)
                            (t 0))))
	;;
    (time-num :> digits :com ,(+ *c-num* *c-digits-as-time-num*)
              ;; increased penalty *c-digits-as-time-num*, so that the '1981' in '1981年' is preferred as integer
              :tag ,(make-instance 'num)
			  :tval ,*pos-OD*)
    (time-num => num
              :stag ,(tn (if (numberp t1)
                             *c-close-sep*
                             *c-common*)))
	(time-num => ordinal) ;; prefer ordinal for "xx 世紀" etc
    
    ;; TODO: better organization, maybe use the same organization as
    ;; the new noun-t to reduce the number of rules?
    (x-time => :time
            :stag ,(tn (maybe-ind t1 *c-time*)))
    ;; those composed time names are treated as ad-hoc-time-pt,
    ;; although its tag may not be
    ,@(loop :for raw-time-prefix :in
         ;; NOTE: abstract, inanimate, animate, name are replaced by noun-b
         ;;'((abstract) (inanimate) (verb-x) (adj-x)
         ;;  (abstract "之") (inanimate "之") (verb-x "之")
         ;;  (x-time) (place) (animate) (name))
         '((noun-b) (noun-b "之") (verb-x) (verb-x "之") (adj-x)
           (x-time) (place))
         :collect
         `(ad-hoc-time-pt -> ,@raw-time-prefix x-time
                          ;; :com ,*c-short-close-suffix*
                          :com ,(if (eq (car raw-time-prefix) 'place)
                                    ;; ambiguity of 'place <- noun-t <- noun-b <- noun-a <- place-b' and use noun-b
                                    *c-close-sep*
                                    *c-prefer*)
                          :tval ,(tfn (cond ((and (> L1 1) (> L2 1)
                                                  (typep t1 'time-name))
                                             ;; need separate NT in some cases, e.g. (NP (NT "去年") (NT "十月"))
                                             `(:splice ,(tag-as 'NT x1)
                                                       ,(tag-as 'NT x2)))
                                            ;; 全年
                                            ((and (= L2 1) (= L1 1)
                                                  (typep t1 'adj-word-quan2)
                                                  (typep t2 'time-period-num-ind-year))
                                             `(DP ,(tag-as 'DT x1) (CLP ,(tag-as 'M x2))))
                                            ;;
                                            (t (tag-as 'NT (cdr args)))))
                          :stag ,(if (null (cdr raw-time-prefix))
                                     *mod-noun-t1-t2*
                                     *mod-de-noun-t1-t3*)
                          :tag ,(if (null (cdr raw-time-prefix))
                                    (tn (sum-count-combine-time-scale-tag
                                         t1
                                         (mod-noun-tag t1 (up-ind t2)
                                                       L1 L2)))
                                    (tn (sum-count-combine-time-scale-tag
                                         t1
                                         (mod-de-noun-tag t1 (up-ind t3)
                                                          L1 L3)))))
         )
    ;;
    ;; time period
    (time-period-a =2> num x-time
                   :tval ,(tfn `(:splice ,x1 (CLP ,(tag-as 'M x2))))
                   :stag ,(tn (prefer t2 'time-take-num *c-common*)))
    (ge :> "個" :tval ,*pos-M*)
    (ge :> ge-ba)
    (ge-ba -> "個" "把" :tval ,*pos-CD*)
    (time-period-a =3> ge-ba x-time
                   :tval ,(tfn `(QP ,x1 (NP ,(tag-as 'NN x2))))
                   :stag ,(tn (prefer t3 'time-take-ge *c-common*)))
    (time-period-a =3> num ge x-time
                   :tval ,(tfn `(:splice
                                 (QP ,x1
                                     (CLP ,x2))
								 (NP ,(tag-as 'NN x3))))
                   :stag ,(tn (prefer t3 'time-take-ge *c-common*)))
    (time-period-a =1> x-time
                   :tval ,(tfn (list
                                (cond ((typep t1 'time-period-unit)
                                       'M)
                                      ((or (typep t1 'time-period-num)
                                           (typep t1 'time-period-num-ge)
                                           (typep t1 'time-period-num-opt-ge))
                                       'NN)
                                      (t 'NT))
                                (collapse-str args)))
                   :stag ,(tn (cond ((typep t1 'time-take-num) *c-period-no-num*)
                                    ((typep t1 'time-take-ge) *c-period-ge-no-num*)
                                    ;; some ad-hoc-time-pt is also time-period, so check first
                                    ((typep t1 'ad-hoc-time-pt) *c-rare*)
                                    ((typep t1 'time-period) 0)
                                    (t nil))))
	;;
	,@(allow-quote 'time-period-a)
    ;;
    (time-period-b => time-period-a)
    (time-period-b =1> time-period-a "餘"
                   :tval ,*pos-NT*)
    (time-period-b =2> time-period-b time-period-a
                   :tag ,(tn (combine-time-scale-tag t1 t2))
				   :tval ,*ctb-splice*
                   :stag ,(tn (time-period-period t1 t2)))
    (time-period-b =3> time-period-b "零" time-period-a
                   :tag ,(tn (combine-time-scale-tag t1 t3))
                   :tval ,*pos-NT*
                   :stag ,(tn (time-period-period t1 t3)))
	;;
	,@(allow-quote 'time-period-b 2)
    ;;
    ;; more lenient to noun-conn for time
    (time-noun-conn => noun-conn)
    (time-noun-conn => pause
                    :com ,*c-close-suffix*)
    ;;
    (time-period-b-conn => time-period-b)
    (time-period-b-conn -> time-period-b-conn noun-conn time-period-b
						:tval ,*ctb-splice*
                        :tag ,*make-time-conn-t1-t3*
                        :stag ,(tn (similar-noun t1 t3)))
    ;;
    (time-period => time-period-b-conn
				 :tval ,*ctb-as-NP*)
    (time-period =2> noun-mod-a time-period :com ,*c-noun-mod-period*
                 ;; prefer to let ad-hoc-time-pt above take care of
                 ;; the composition, at least for now
				 :tval ,*ctb-mod-noun-p*
                 :tag ,*mod-time-tag-t1-t2*
                 :stag ,(tn (score-add
                             (mod-noun t1 t2 L1 L2)
                             (not-prefer t1 'ind *c-rare*))))
	;;
	,@(allow-quote 'time-period 3)
    ;;
    (time-period-conn-a => time-period)
    (time-period-conn-a -> time-period-conn-a time-noun-conn time-period
						:tval ,*ctb-splice*
						:com ,*c-close-sep*
						:tag ,*make-time-conn-t1-t3*
						:stag ,(tn (similar-noun t1 t3)))
	;;
	(time-period-conn => time-period-conn-a
					  :tval ,*ctb-splice-to-NP*)
	;;
	,@(allow-quote 'time-period-conn 4)
    ;; time point
    ,@(strs 'time-point-prefix '("西曆" "農曆") '->
            :tval *pos-NN*)
    ,@(strs 'time-point-prefix '("公元" "西元" "公元前" "公元後")
			'->
            :tval *pos-ADJP-JJ*)
    ;; 月 is also listed out in ad hoc time points
    ,@(str-tags
       'time-point-name
       `((,(time-pt-scale *time-scale-year*) "年" "歲")
         (,*tag-time-scale-month* "月")
         (,*tag-time-scale-day* "日" "號")
         (,(time-pt-scale *time-scale-second*) "秒")
         (,*tag-time-scale-hour* "更"))
       :>
       :com *c-noun-suffix*)
    ;; need to use -> for multi-characters
    ,@(strs 'time-point-name
			'("世紀") '->
            :tag (time-pt-scale *time-scale-century*)
			:com *c-noun-suffix*
			:tval *pos-NP-NN*)
    ,@(strs 'time-point-name
			'("世代") '->
            :tag (time-pt-scale *time-scale-generation*)
			:com *c-noun-suffix*
			:tval *pos-NN*)
    ,@(strs 'time-point-name
			'("更天") '->
            :tag *tag-time-scale-hour*
			:com *c-noun-suffix*
			:tval *pos-CLP-M*)
    ,@(str-tags
       'time-point-name
       `((,*tag-time-scale-decade* "年代")
         (,*tag-time-scale-month* "月份"))
       '->
       :com *c-noun-suffix*
       :tval *pos-NT*)
    ;; special treatment for minute
    (time-point-minute :> "分" :com ,*c-noun-suffix*
                       :tag ,(time-pt-scale *time-scale-minute*))
    (time-point-name => time-point-minute)
    ,@(strs 'time-point-suffix '("頭" "底" "初" "中" "間" "里" "末" "尾" "終" "前" "後") :>
            :com *c-noun-suffix*
			:tval *pos-LC*)
    ,@(strs 'time-point-suffix-name '("AM" "PM" "上旬" "中旬" "下旬" "中上旬" "中下旬") '->
            :com *c-noun-suffix*
			:tval *pos-NT*)
    ,@(strs 'time-point-suffix2 '("初" "早" "中" "後" "晚" "末" "尾" "前" "後" "終") :>
            :com *c-noun-suffix*)
    ,@(strs 'time-point-suffix3 '("期" "段" "葉") :>
            :com *c-noun-suffix*)
    (time-point-suffix-name -> time-point-suffix2 time-point-suffix3
							:tval ,*pos-NP-NN*)
    (time-point-suffix :> time-point-suffix-name)
    ;;
    (dynasty-a =1> :dynasty :com ,*c-time*
               :tval ,*may-be-NR*)
    (dynasty => dynasty-a)
    (dynasty =2> dynasty-a dynasty-a
			 ;; TODO: allow more in a chain?
			 :tval ,*ctb-NP*
             :stag ,(tn (maybe-name-ind t1 t2 *c-close-suffix*)))
    (time-pt-a => dynasty)
    (time-pt-a =2> time-num time-point-name
			   ;; more often separated: 世紀 (NP-NN), 世代 (NN), 更天 (CLP-M)
			   ;; more often one word: 年代 (NT), 月份 (NT)
               :tval ,(tfn
					   (cond ((or (headed-by x2 'NP)
								  (headed-by x2 'NN)
								  (headed-by x2 'CLP))
							  `(:splice (QP ,x1) ,x2))
							 (t (tag-as 'NT args))))
               :stag ,(tn (not-prefer t1 'time-date *c-rare*)))
    ,@(strs 'time-min-sep '(":") :>)
    ;; "," and "，" are strange, may be typo in the treebank
    ,@(strs 'date-sep '("." "-" "/" ",") :>)
    ,@(strs 'date-time-sep '(" ") :>)
    ;; TODO: distinguish the various date formats: Y/M/D, D/M/Y, M/D/Y
    ;; constrain the range of M (1~12) and D (1~31).
    ;; But Y could not be easily constrained, unless we prefer recent years.
    (time-pt-a => time-date)
    (time-date -> only-digits-as-int
               date-sep only-digits-as-int
               ;; this form could be confused with a number of things: decimal for '.'; range for '-'; fraction for '/';
               :com ,*c4*
               ;; could be two of Y, M, D.
               ;; so the constraint is either one is (M or D) => ((1~12) or (1~31)) => (1~31)
               :stag ,(tn (if (and (<= 1 t1 31)
                                   (<= 1 t3 31))
                              *c-prefer*
                              *c-rare*))
               :tag ,(more-trait-to
                      (make-instance 'time-date)
                      'time-scale-max *time-scale-month*
                      'time-scale-min *time-scale-day*
                      'confusing-time-date t)
               :tval ,*pos-NT*)
    (time-date -> only-digits-as-int
               date-sep only-digits-as-int
               date-sep only-digits-as-int
               ;; be loose, could be any order of Y, M, D.
               ;; Y/M/D,  D/M/Y,  M/D/Y
               ;; Y/D/M,  D/Y/M,  M/Y/D
               :stag ,(tn (cond ((<= 1 t3 12)
                                 ;; Y/M/D  or  D/M/Y
                                 (if (or (<= 1 t1 31)
                                         (<= 1 t5 31))
                                     *c-prefer*
                                     *c-rare*))
                                ;;
                                ((<= 1 t3 31)
                                 (cond ((<= 1 t1 12) ;; M/D/Y
                                        *c-prefer*)
                                       ((<= 1 t5 12) ;; Y/D/M
                                        *c-common*)
                                       (t *c-rare*)))
                                ;;
                                ((and (<= 1 t1 31)
                                      (<= 1 t5 12))
                                 ;; D/Y/M
                                 *c-common*)
                                ;;
                                ((and (<= 1 t5 31)
                                      (<= 1 t1 12))
                                 ;; M/Y/D
                                 *c-common*)
                                ;;
                                (t *c-rare*)))
               :tag ,(more-trait-to
                      (make-instance 'time-date)
                      'time-scale-max *time-scale-year*
                      'time-scale-min *time-scale-day*)
               :tval ,*pos-NT*)
    ;;
    (time-pt-a => time-hour-min) ;; possibly also second
    (time-hour-min -> only-digits time-min-sep only-digits time-point-minute
                   ;; special alternative format
                   :tag ,*tag-time-scale-hour-minute*
                   :tval ,*pos-NT*)
    (time-hour-min -> only-digits time-min-sep only-digits
                   ;; special alternative format
                   ;; assume it is minute:second, might also be hour:minute
                   :tag ,*tag-time-scale-minute-second*
                   :tval ,*pos-NT*)
    (time-hour-min -> only-digits time-min-sep only-digits time-min-sep only-digits
                   :tval ,*pos-NT*
                   :tag ,(more-trait-to
                          (make-instance 'time-pt)
                          'time-scale-max *time-scale-hour*
                          'time-scale-min *time-scale-second*))
    (time-hour-min -> hour-num hour-ind num time-point-minute
                   :tval ,(tfn `(:splice (NT ,(collapse-str x1 x2))
                                         (NT ,(collapse-str x3 x4))))
                   :tag ,*tag-time-scale-hour-minute*)
    ;;
    (time-pt-a -> time-date date-time-sep time-hour-min
               :tag ,(tn (combine-time-scale-tag t1 t3))
               :tval ,*pos-NT*)
    ;;
    ,@(strs 'week-day-ind '("星期" "禮拜" "周") '->
            :com *c-noun-suffix*)
    ,@(strs 'hour-ind '("時" "點") :>
            :com *c-noun-suffix*)
    ,@(strs 'month-ind '("月") :>
            :com *c-noun-suffix*)
    ,@(strs 'day-ind '("日" "號") :>
            :com *c-noun-suffix*)
    ;; we really only need digit 1 to 6, and rough-amount and ask-amount
    (time-pt-a -> week-day-ind num
               :tag ,(more-trait-to
                      (make-instance 'time-pt-num-opt-ge)
                      'time-scale-max *time-scale-day*
                      'time-scale-min *time-scale-day*)
               :stag ,(tn (prefer-if-num t2 wd (<= 1 wd 7) *c-common*))
               :tval ,*pos-NT*)
    (time-pt-a -> num month-ind num :com ,*c-close-suffix*
               :stag ,(tn (+ (prefer-if-num t1 m (<= 1 m 12) *c-common*)
                             (prefer-if-num t3 d (<= 1 d 31) *c-common*)))
               :tval ,*pos-NT*
               :tag ,*tag-time-scale-month-day*)
    (time-pt-a -> num month-ind num day-ind
               :stag ,(tn (+ (prefer-if-num t1 m (<= 1 m 12) *c-common*)
                             (prefer-if-num t3 d (<= 1 d 31) *c-common*)))
               :tag ,*tag-time-scale-month-day*
               :tval ,(tfn `(:splice (NT ,(collapse-str x1 x2))
                                     (NT ,(collapse-str x3 x4)))))
    (hour-suffix :> num
                 ;; '多' could also be rough-amount
                 :com ,*c-close-sep*)
    ,@(strs 'hour-suffix '("多" "半" "鐘") :>)
    ,@(strs 'hour-suffix '("多鐘" "半鐘"))
    (hour-num => num
              :stag ,(tn (prefer-if-num t1 h
                                        (and (integerp h)
                                             (<= 0 h 23))
                                        *c-common*)))
    (hour-num => consecutive-digit)
    (time-pt-a -> hour-num hour-ind
               ;; :com ,*c-short-close-suffix*
               :tval ,*pos-NT*
               :tag ,*tag-time-scale-hour*)
    (time-pt-a -> hour-num hour-ind hour-suffix
               ;; :com ,*c-close-suffix*
               :tval ,*pos-NT*
               :tag ,*tag-time-scale-hour-minute*)
    (time-pt-a -> "'" integer
               ;; 'xx is short for 19xx
               :tval ,*pos-NT*
               :tag ,*tag-time-scale-decade*)
    ,@(strs 'lunar-year-prefix '("年" "大年"))
    (time-pt-a -> lunar-year-prefix num
               :com ,*c2*
               ;; use num instead of integer to allow ranges
               :stag ,(tn (+
                           ;; not want e.g. '年 (逾百万)' for this
                           (not-prefer-trait t2 'num-has-verb-mod *c-rare*)
                           (prefer-if-num t2 v
                                          (and (integerp v)
                                               ;; '年3' not common, and
                                               ;; may be confused with
                                               ;; other parts of time,
                                               ;; e.g. not prefer '至今
                                               ;; (年3) 月'.
                                               (<= 14 v 30))
                                          *c-rare* *c2*)))
               :tval ,*pos-NT*
               :tag ,*tag-time-scale-day*)
    (time-pt-a =2> lunar-year-prefix x-time
               ;; x-time should already have time scale
               :tval ,*pos-NT*
               :stag ,(tn (must-be t2 'lunar-day)))
    ;;
    (time-pt-a -> "酒" "過" num "巡"
               :tval ,*pos-NT*
               :tag ,(make-instance 'time-name))
    ;; the stupid grammar of putting "兒" to pretty much everything!!!
    (time-pt-a =1> time-pt-a "兒"
               :tval ,*pos-NT*
               :com ,*c-close-suffix*)
    ;;
    (time-pt-a =1> x-time :stag ,(tn (must-be t1 'time-pt))
               :tval ,*pos-NT*)
    (time-pt-a => ad-hoc-time-pt
               :tval ,(tfn (cond ((headed-by x1 :splice)
                                  (as-head x1 'NP))
                                 ((and (consp x1)
                                       (symbolp (car x1)))
                                  ;; already has a head
                                  x1)
                                 (t (tag-as 'NT x1)))))
	;;
	,@(allow-quote 'time-pt-a)
    ;;
    (time-pt-b => time-pt-a)
    (time-pt-b =2> time-point-prefix time-pt-a :com ,*c-close-suffix*
			   :tval ,*ctb-splice*)
    ;;(time-pt-b => ad-hoc-time-pt)
    ;;(time-pt-b => time-pt-period-num-ge)
    ;;(time-pt-b => time-pt-period-num-opt-ge)
    ;; 
    ,@(strs 'time-mod '("前" "後" "上" "下" "本" "今" "有" "往" "翌"
                        "現" "近" "昔" "明" "當" "初") :>)
    ,@(strs 'time-mod '("早些" "晚些" "前些" "後些" "近些"
                        "早段" "晚段" "前段" "後段"))
    (time-pt-b =2> time-mod time-period :com ,*c-close-suffix*
               :stag ,(tn (if (and (= L1 1) (> L2 1))
                              *c-common*
                              0))
               :tval ,*pos-NT*)
    (time-pt-b =2> p-pronoun time-period :com ,*c-close-suffix*
			   :tval ,*ctb-mod-noun-p*)
    (time-pt-b =2> q-pronoun time-period :com ,*c-close-suffix*
			   :tval ,*ctb-mod-noun-p*)
    ;;(time-pt-b =2> time-mod period-num :com ,*c-close-suffix*)
    ;;(time-pt-b =3> time-mod "個" period-ge)
    (time-pt-b =2> time-mod x-time :com ,*c-close-suffix*
               :tval ,*pos-NT*
               :stag ,(tn (prefer t2 'time-take-num *c-common*)))
    (time-pt-b =3> time-mod ge x-time
			   :tval ,(tfn `(NP (DP ,(tag-as 'DT x1)
									(CLP ,x2))
								(NP ,(tag-as 'NN x3))))
               :stag ,(tn (prefer t3 'time-take-ge *c-common*)))
	;;
	(time-pt-b1 => time-pt-b
				:tval ,*ctb-splice-to-NP*)
	;;
	,@(allow-quote 'time-pt-b1 2)
    ;;
    (time-pt-b-conn-a => time-pt-b1)
    (time-pt-b-conn-a -> time-pt-b-conn-a noun-conn time-pt-b1
					  :tval ,*ctb-splice*
					  :tag ,*make-time-conn-t1-t3*
					  :stag ,(tn (similar-noun t1 t3)))
	;;
	(time-pt-b-conn => time-pt-b-conn-a
					:tval ,*ctb-N-CC-N*)
    ;;
    (time-pt-c :> time-point-suffix-name :com ,*c-close-suffix*
               :tag ,(make-instance 'time-name))
    (time-pt-c => time-pt-b-conn)
    ;; 
    (time-pt-c =1> time-point-name time-point-suffix
               :com ,*c-close-suffix*
               :tval ,(tfn (cond ((both-mono-p x1 x2)
								  (tag-as 'NT x1 x2))
								 ((headed-by x2 'LC)
								  `(LCP ,(as-head x1 'NP)
										,x2))
								 (t `(:splice ,x1 ,x2))))
               )
    (time-pt-c -> "季" time-point-suffix
               :com ,*c-close-suffix*
               :tval ,*pos-NT*
               :tag ,(time-pt-scale *time-scale-season*))
    (time-pt-c =1> time-pt-b time-point-suffix :com ,*c-close-suffix*
               :tval ,*ctb-splice*)
    (time-pt-c =2> noun-mod-a time-pt-c :com ,*c-noun-mod-close-pt*
               :tval ,(tfn (if (both-mono-p x1 x2)
                               (list 'NT (collapse-str args))
                               (ctb-mod-noun-p x1 x2)))
               :stag ,*count-mod-noun-t1-t2*
               :tag ,*mod-noun-tag-t1-t2*)
	;;
	,@(allow-quote 'time-pt-c 3)
    ;;
    (time-pt-c-conn => time-pt-c)
    (time-pt-c-conn -> time-pt-c-conn noun-conn time-pt-c
					:tval ,*ctb-splice*
                    :com ,*c-close-sep*
                    :tag ,*make-time-conn-t1-t3*
                    :stag ,(tn (similar-noun t1 t3)))
    ;;
    (time-pt-d => time-pt-c-conn
			   :tval ,*ctb-splice-to-NP*)
    (time-pt-d =2> time-pt-c time-pt-d
			   :tval ,*ctb-mod-noun-p*
               :com ,*c-long-sep*
               :stag ,*count-mod-noun-t1-t2*
               :tag ,*count-mod-time-tag-t1-t2*)
    (time-pt-d =3> time-pt-c zhi time-pt-d
			   :tval ,*ctb-mod-de-noun*
               :stag ,*count-mod-de-noun-t1-t3*
               :tag ,*count-mod-de-time-tag-t1-t3*)
	;;
	,@(allow-quote 'time-pt-d 4)
    ;;
    (time-pt-d-conn-a => time-pt-d)
    (time-pt-d-conn-a -> time-pt-d-conn-a noun-conn time-pt-d
					  :tval ,*ctb-splice*
					  :com ,(* 2 *c-close-sep*)
					  :tag ,*make-time-conn-t1-t3*
					  :stag ,(tn (similar-noun t1 t3)))
	;;
	(time-pt-d-conn => time-pt-d-conn-a
					:tval ,*ctb-splice-to-NP*)
	;;
	,@(allow-quote 'time-pt-d-conn 5)
    ;;
    ,@(strs 'time-F-B '("前" "後" "以前" "以後" "之前" "之後"
                        "此前" "此後" "之余")
            '-> :com *c-dir*
			:tval *pos-LC*)
    ,@(strs 'time-F-B-p '("内" "之内" "之中")
            ;; seems not used after verb-p, subj-pred, etc.
            '->
            :com *c-dir*
            :tval *pos-LC*)
    (time-F-B-p => time-F-B)
    ;;
    ,@(str-tags 'time-F-B-pt
                '((:time-xu3 "許"))
                ;; seems not used after verb-p, subj-pred, etc.
                '-> :com *c-dir*
                :tval *pos-LC*)
    (time-F-B-pt => time-F-B)
    ;;
    ,@(strs 'time-at-around '("前後" "左右" "時" "之時" "的時候" "時分" "之際")
            '-> :com *c-dir*
			:tval *pos-LC*)
    ;;
    (event-p => noun-t
             ;; an event could be used with time-at-around, time-F-B, etc to mark a time point.
             ;; e.g. '鴉片戰爭 後'
             :com ,*c-close-suffix*
             :stag ,(tn
                     (min-over-conn t1
                                    #'(lambda (x)
                                        (typecase x
                                          ;; But does not prefer '事件' only
                                          (event-ind *c-common*)
                                          (event 0)
                                          (t nil))))))
    ;;
    (time-pt-e => time-pt-d-conn)
    (time-pt-e =1> time-period-conn time-F-B-p :com ,*c-time-period-time-suffix*
               :tag ,*time-tag-with-FB-around-ind*
               :tval ,*time-with-F-B*)
    (time-pt-e =1> time-pt-d-conn time-F-B-pt :com ,*c-time-pt-time-suffix*
               :tag ,*time-tag-with-FB-around-ind*
               :stag ,(tn (if (and (eq t2 :time-xu3)
                                   (let ((tm (trait-value t1 'time-scale-min *time-scale-within-day*)))
                                     (and (numberp tm)
                                          (> tm *time-scale-within-day*))))
                              ;; seems '許' is more often used for
                              ;; time scale smaller than a day
                              *c-close-suffix*
                              0))
               :tval ,*time-with-F-B*)
    ;;
    (time-pt-e -> event-p time-F-B :com ,*c-event-time-suffix*
               :tval ,*time-with-F-B*
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    (time-pt-e -> event-p time-at-around :com ,*c-event-time-suffix*
			   :tval ,*time-with-F-B*
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    ;;
    ;; reduced *c-verb-p-time-suffix*, so that subj-pred is less preferred to verb-p
    ;; e.g. prefer '女性朋友 (在 遇到类似事件 时) ...' to '((女性朋友 在遇到类似事件) 时) ...'
    (time-pt-e -> verb-p time-F-B :com ,*c-verb-p-time-suffix*
               :stag ,*stag-not-prefer-pause-or-seps-nested-VP*
               :tval ,*time-with-F-B*
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    (time-pt-e -> verb-p time-at-around :com ,*c-verb-p-time-suffix*
               :stag ,*stag-not-prefer-pause-or-seps-nested-VP*
			   :tval ,*time-with-F-B*
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    ;;
    (time-pt-e -> subj-pred time-F-B :com ,*c-subj-pred-time-suffix*
               ;; to encourage putting time and place at outer scope
               :stag ,(tn (+ (not-prefer-trait t1 'has-time *c-close-suffix*)
                             (not-prefer-trait t1 'has-place *c-close-sep*)
                             (not-prefer-trait t1 'has-time-pause *c-common*)
                             (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t1 'has-subj-pause *c-common*)
                             ;; higher penalty for seps
                             ;; e.g. not want '商场保安 (人员赶到现场，初步了解情况 后) ...'
                             (not-prefer-subj-pred-body-seps t1
                                                             (+ *c-close-sep*
                                                                *c-long-sep*))))
			   :tval ,*time-with-F-B*
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    (time-pt-e -> subj-pred time-at-around :com ,*c-subj-pred-time-suffix*
               ;; to encourage putting time and place at outer scope
               :stag ,(tn (+ (not-prefer-trait t1 'has-time *c-close-suffix*)
                             (not-prefer-trait t1 'has-place *c-close-sep*)
                             (not-prefer-trait t1 'has-time-pause *c-common*)
                             (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t1 'has-subj-pause *c-common*)
                             ;; e.g. not want '商场保安 (人员赶到现场，初步了解情况 后) ...'
                             (not-prefer-subj-pred-body-seps t1
                                                             (+ *c-close-sep*
                                                                *c-long-sep*))))
			   :tval ,*time-with-F-B*
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    ;;
    (time-pt-e -> clause-c time-F-B :com ,*c-clause-time-suffix*
               ;; to encourage putting time and place at outer scope
               :stag ,(tn (+ (not-prefer-trait t1 'has-time *c-close-suffix*)
                             (not-prefer-trait t1 'has-place *c-close-sep*)
                             (not-prefer-trait t1 'has-time-pause *c-common*)
                             (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t1 'has-subj-pause *c-common*)
                             (conn-not-prefer-seps t1 *c-close-sep*
                                                   '(:comma :semi-colon))
                             (not-prefer-trait t1 'verb-mod-has-pause
                                               *c-close-suffix*)))
			   :tval ,*time-with-F-B*
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    (time-pt-e -> clause-c time-at-around :com ,*c-clause-time-suffix*
               ;; to encourage putting time and place at outer scope
               :stag ,(tn (+ (not-prefer-trait t1 'has-time *c-close-suffix*)
                             (not-prefer-trait t1 'has-place *c-close-sep*)
                             (not-prefer-trait t1 'has-time-pause *c-common*)
                             (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t1 'has-subj-pause *c-common*)
                             (conn-not-prefer-seps t1 *c-close-sep*
                                                   '(:comma :semi-colon))
                             (not-prefer-trait t1 'verb-mod-has-pause
                                               *c-close-suffix*)))
			   :tval ,*time-with-F-B*
               :stag ,(tn (not-prefer t1 'physical))
               :tag ,*time-name-tag-with-phrase-FB-around-ind*)
    ;;
    (time-pt-e =2> noun-mod time-pt-e :com ,*c-noun-mod-far-pt*
               ;; need to allow more noun-mod, e.g. '刚过去 的 冬天'
			   :tval ,*ctb-mod-noun-p*
               :stag ,*mod-noun-t1-t2*
               :tag ,*mod-time-tag-t1-t2*)
	;;
	,@(allow-quote 'time-pt-e 6)
    ;;
    (time-pt-e-conn => time-pt-e)
    (time-pt-e-conn -> time-pt-e-conn noun-conn time-pt-e
					:tval ,*ctb-splice*
                    :com ,(* 3 *c-close-sep*)
                    :tag ,*make-time-conn-t1-t3*
                    :stag ,(tn (similar-noun t1 t3)))
    ;;
    (time-pt-f => time-pt-e-conn
			   :tval ,*ctb-splice-to-NP*)
    (time-pt-f =2> time-pt-e time-pt-f :com ,*c-time-far-combine*

			   :tval ,*ctb-mod-noun-p*
               :stag ,*mod-noun-t1-t2*
               :tag ,*count-mod-time-tag-t1-t2*)
    (time-pt-f =3> time-pt-e zhi time-pt-f :com ,*c-time-far-combine*
			   :tval ,*ctb-mod-de-noun*
               :stag ,*mod-de-noun-t1-t3*
               :tag ,*count-mod-de-time-tag-t1-t3*)
	;;
	,@(allow-quote 'time-pt-f 7)
    ;;
    (time-pt-f-conn-a => time-pt-f)
    (time-pt-f-conn-a -> time-pt-f-conn-a noun-conn time-pt-f
					  :tval ,*ctb-splice*
					  :com ,(* 4 *c-close-sep*)
					  :tag ,*make-time-conn-t1-t3*
					  :stag ,(tn (similar-noun t1 t3)))
	;;
	(time-pt-f-conn => time-pt-f-conn-a
					:tval ,*ctb-splice-to-NP*)
	;;
	,@(allow-quote 'time-pt-f-conn 8)
    ;; TODO: add constraints to grammar rules for time
    ;; further prefix and suffix to time point
    ,@(str-tags 'time-from
                '((:from "自從" "從" "由")
                  "過了")
                '->
                :tval *pos-P*)
    ,@(str-tags 'time-from
                ;; this causes some confusion
                '((:from-zhi4 "自")
                  )
                '->
                :com *c-prep*
                :tval *pos-P*)
	,@(strs 'time-at-to '("在"
                          (:to "直到" "直至"))
            '->
            :com *c-noun-suffix*
            :tval *pos-P*)
    ,@(str-tags 'time-at-to
                '((:at-dang1 "當" "正當")
                  (:to "到" "至")
                  "於" "on" "截至" "臨")
                '->
                :com *c-noun-suffix*
                :tval *pos-P*)
    ,@(strs 'time-starting-upto '("開始" "以來" "起" "為止" "開始以來") '->
            :com *c-noun-suffix*
            :tval *pos-LC*)
    ,@(strs 'time-starting-upto '("來") '->
            ;; this seems quite rare, and confusing,
            ;; so increased penalty
            :tval *pos-LC*
            :com (+ *c-close-suffix* *c-time*))
	;;
    (time-pt-g => time-pt-f-conn)
	(time-pt-g =1> time-pt-f-conn time-starting-upto
               :tag ,(tn (more-trait t1
                                     'time-with-ind t
                                     'time-with-upto-ind t))
			   :tval ,*time-with-ind-suffix*)
    ;;
    (time-pt-g -> event-p time-starting-upto
               :com ,(+ *c-close-sep* *c-event-time-suffix*)
               :tag ,*time-name-tag-with-phrase-upto-ind*
			   :tval ,*time-with-ind-suffix*)
    (time-pt-g -> verb-p time-starting-upto
               :com ,(+ *c-close-sep* *c-verb-p-time-suffix*)
               :stag ,*stag-not-prefer-pause-or-seps*
               :tag ,*time-name-tag-with-phrase-upto-ind*
			   :tval ,*time-with-ind-suffix*)
    (time-pt-g -> subj-pred time-starting-upto
               :com ,(+ *c-close-sep* *c-subj-pred-time-suffix*)
               :stag ,(tn (+ (not-prefer-trait t1 'has-time *c-close-sep*)
                             (not-prefer-trait t1 'has-place *c-close-sep*)
                             (not-prefer-trait t1 'has-time-pause *c-common*)
                             (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t1 'has-subj-pause *c-common*)
                             (not-prefer-subj-pred-body-seps t1)))
               :tag ,*time-name-tag-with-phrase-upto-ind*
			   :tval ,*time-with-ind-suffix*)
    (time-pt-g -> clause-c time-starting-upto
               :com ,(+ *c-close-sep* *c-clause-time-suffix*)
               :stag ,(tn (+ (not-prefer-trait t1 'has-time *c-close-sep*)
                             (not-prefer-trait t1 'has-place *c-close-sep*)
                             (not-prefer-trait t1 'has-time-pause *c-common*)
                             (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t1 'has-subj-pause *c-common*)
                             (conn-not-prefer-seps t1 *c-close-sep*
                                                   '(:comma :semi-colon))
                             (not-prefer-trait t1 'verb-mod-has-pause
                                               *c-close-suffix*)
                             (not-prefer-trait t1 'has-clause-supp *c-common*)))
               :tag ,*time-name-tag-with-phrase-upto-ind*
			   :tval ,*time-with-ind-suffix*)
	;;
	,@(allow-quote 'time-pt-g 9)
	;;
	(time-pt-h => time-pt-g)
    (time-pt-h =2> time-at-to time-pt-g :com ,*c-at-to-time*
               :tag ,(tn (more-trait t2
                                     'time-with-ind t
                                     'time-with-at-ind t))
               :stag ,(tn (if (trait-value t2 'time-with-ind)
                              ;; prefer matching pairs such as '在 ... 时'
                              0
                              *c-close-suffix*))
			   :tval ,(tfn `(PP ,x1 ,(as-phrase-level x2))))

    ;; Seems time-at-to is more easily confused, so use an additional trait
    (time-pt-h -> time-at-to event-p
               :com ,(+ *c-short-close-suffix*
                        *c-event-time-suffix*)
               :tag ,*time-name-tag-with-phrase-at-ind*
			   :tval ,*time-with-ind-prefix*)
    (time-pt-h -> time-at-to verb-p
               :com ,(+ *c-short-close-suffix*
                        *c-verb-p-time-suffix*)
               :stag ,(tn (if (eq t1 :at-dang1)
                              (if (= L2 2)
                                  ;; 当选人
                                  (+ *c1*
                                     (not-prefer-verb t2 'v-xuan3-ren2 *c-common*))
                                  ;; '当' prefer subj-pred rather than verb-p
                                  *c2*)
                              0))
               :tag ,*time-name-tag-with-phrase-at-ind*
			   :tval ,*time-with-ind-prefix*)
    (time-pt-h -> time-at-to subj-pred
               :com ,*c-short-close-suffix*
               :stag ,(tn (if (eq t1 :at-dang1)
                              ;; reduce penalty for '當', because it is also v-SP
                              0
                              *c-subj-pred-time-suffix*))
               :tag ,*time-name-tag-with-phrase-at-ind*
			   :tval ,*time-with-ind-prefix*)
    (time-pt-h -> time-at-to clause-c
               :com ,(+ *c-short-close-suffix*
                        *c-clause-time-suffix*)
               :stag ,(tn (+ (if (trait-value t2 'has-clause-supp)
                                 *c-common*
                                 0)
                             (if (eq t1 :at-dang1)
                                 (if (= L2 2)
                                     ;; 当选人
                                     (+ *c1*
                                        (not-prefer-verb
                                         t2 'v-xuan3-ren2 *c-common*))
                                     *c2*)
                                 0))
                          )
               :tag ,*time-name-tag-with-phrase-at-ind*
			   :tval ,*time-with-ind-prefix*)
	;;
	,@(allow-quote 'time-pt-h 10)
    ;;
    (time-pt-h-conn-a => time-pt-h)
    (time-pt-h-conn-a -> time-pt-h-conn-a noun-conn time-pt-h
					  :tval ,*ctb-splice*
					  :com ,(* 5 *c-close-sep*)
					  :tag ,*make-time-conn-t1-t3*
					  :stag ,(tn (similar-noun t1 t3)))
	;;
	(time-pt-h-conn => time-pt-h-conn-a
					:tval ,*ctb-splice-to-NP*)
	;;
	,@(allow-quote 'time-pt-h-conn 11)
    ;;
    (time-pt-i => time-pt-h-conn)
    (time-pt-i =2> time-from time-pt-h-conn
               :tag ,(tn (more-trait t2
                                     'time-with-ind t
                                     'time-with-at-ind t))
               :tval ,*ctb-PP*)

    (time-pt-i -> time-from event-p
               :com ,(+ *c-close-sep*
                        *c-event-time-suffix*)
               :tag ,*time-name-tag-with-phrase-at-ind*
               :tval ,*time-with-ind-prefix*)
    (time-pt-i -> time-from verb-p
               :com ,(+ *c-long-sep*
                        *c-verb-p-time-suffix*)
               :stag ,(tn (if (eq t1 :from-zhi4)
                              ;; 自由, 自立, 自信, 自治
                              ;; 自动化
                              (cond ((= L2 2) *c-rare*)
                                    ((= L2 1) *c-very-rare*)
                                    (t 0))
                              0))
               :tag ,*time-name-tag-with-phrase-at-ind*
               :tval ,*time-with-ind-prefix*)
    (time-pt-i -> time-from subj-pred
               :com ,(+ *c-close-sep*
                        *c-subj-pred-time-suffix*)
               :tag ,*time-name-tag-with-phrase-at-ind*
               :tval ,*time-with-ind-prefix*)
    (time-pt-i -> time-from clause-c
               :com ,(+ *c-long-sep*
                        *c-clause-time-suffix*)
               :stag ,(tn (+ (if (trait-value t2 'has-clause-supp)
                                 *c-common*
                                 0)
                             (if (eq t1 :from-zhi4)
                                 ;; 自由, 自立, 自信, 自治
                                 ;; 自动化
                                 (cond ((= L2 2) *c-rare*)
                                       ((= L2 1) *c-very-rare*)
                                       (t 0))
                                 0)))
               :tag ,*time-name-tag-with-phrase-at-ind*
               :tval ,*time-with-ind-prefix*)
    ;;
    (time-pt => time-pt-i
             ;; for penalizing unpaired time-ind traits:
             ;; time-with-upto-ind, time-with-FB-around-ind, time-with-at-ind.
             :stag ,(tn (cond ((not (trait-value t1 'time-with-phrase))
                               ;; only penalize those composed with event-p, verb-p, subj-pred, clause-c.
                               ;; there are marked with 'time-with-phrase trait.
                               *c-prefer*)
                              ((trait-value t1 'time-with-upto-ind)
                               ;; check this first, because there
                               ;; could be multiple traits, and this
                               ;; better be paired.
                               (if (trait-value t1 'time-with-at-ind)
                                   *c-prefer*
                                   *c-close-suffix*))
                              ((trait-value t1 'time-with-FB-around-ind)
                               *c-prefer*)
                              (t (if (trait-value t1 'time-with-at-ind)
                                     ;; unpaired time-from or time-at-to
                                     ;; increased penalty from *c-close-suffix*
                                     *c3*
                                     *c-prefer*))))
             )
    ;;
    (time-pt =2> adv time-pt :com ,*c-adv-time-pt*
             ;; TODO: add more penalty?
			 :tval ,(tfn `(NP ,(as-phrase-level x1)
							  ,(as-phrase-level x2)))
             :stag ,*mod-noun-t1-t2*
             :tag ,*mod-noun-tag-t1-t2*)
	;;
	,@(allow-quote 'time-pt 12)
    ;;
    (time-pt-conn => time-pt)
    (time-pt-conn -> time-pt-conn time-noun-conn time-pt
				  :tval ,*ctb-splice*
                  :com ,(* 6 *c-close-sep*)
                  :tag ,*make-time-conn-t1-t3*
                  :stag ,(tn (let ((*sep* t2))
                               ;; similar-noun would be more lenient, for less tighter time-noun-conn
                               (similar-noun t1 t3))))
    ;;
    (time => time-pt-conn
		  :tval ,*ctb-splice-to-NP*)
    ;; arbitrarily prefer time-pt to time-period
    (time => time-period-conn :com ,*c-close-sep*
          :tval ,*ctb-splice-to-NP*)
	;;
	,@(allow-quote 'time 13)
    ;;
    (time-p => time
            ;; to be used similar to verb-mod, e.g. at the front of verb-p, subj-pred.
            ;; Seems a dynasty name is not usually used alone as time
            :stag ,(tn (if (trait-value t1 'time-with-ind)
                           *c-prefer*
                           (+ (not-prefer t1 'dynasty-name *c-common*)
                              ;; want '中国年度煤炭消耗量' as subject, not separate into time-p and subject
                              (not-prefer t1 'time-period-num-opt-ge-ind-year *c-close-suffix*)
                              ;; in case of ties, prefer time-pt to time-period
                              (not-prefer t1 'time-period (* 2 *c-close-sep*)))))
            :tval ,(tfn (as-phrase-level x1 'NP)))
    ;;
    (word => time :com ,*c-time-p-alone*)
    ;;
    (pronoun-suffix -> "自" "己")
    (pronoun =1> :pronoun :com ,*c-pronoun*
             :tval ,*pos-PN*)
    (pronoun =1> pronoun pronoun-suffix :com ,*c-close-suffix*
             :tval ,*pos-PN*)
    (word => pronoun :com ,*c-pronoun-alone*)
    ;;
    (q-pronoun =1> :q-pronoun :com ,*c-q-pronoun*
               :tval ,(tfn (list (cond ((typep t1 'q-dt-quantifier)
                                        'DT)
                                       (t 'PN))
                                 (collapse-str args))))
    (word => q-pronoun :com ,*c-q-pronoun-alone*)
    ;;
    (p-pronoun-a =1> :p-pronoun :com ,*c-p-pronoun*)
    (p-pronoun-a -> "其" integer :com ,*c-p-pronoun*
                 :tag ,(make-instance 'p-pronoun))
    (p-pronoun-a -> "其" "中" "之" integer :com ,*c-p-pronoun*
                 :tag ,(make-instance 'p-pronoun))
    (p-pronoun => p-pronoun-a
               :tval ,(tfn (list (cond ((typep t1 'p-pronoun-dt)
                                        'DT)
                                       (t 'PN))
                                 (collapse-str args))))
    (p-pronoun =2> p-pronoun-a p-pronoun
               :tval ,*pos-PN*)
    (word => p-pronoun :com ,*c-p-pronoun-alone*)
    ;;
    (verb-a =1> :verb :com ,*c-verb*
            ;; '并' is much more often used as CC or AD
            ;; TODO: may need a more general mechanism for confusing verbs
            :stag ,(tn (not-prefer t1 'v-bing4 *c3*))
            :tval ,(tfn (ctb-vnv t1
                                 (cond ((or (typep t1 'v-is)
                                            (typep t1 'v-vc))
                                        'VC)
                                       ((or (typep t1 'v-has)
                                            (typep t1 'v-ve))
                                        'VE)
                                       (t 'VV))
                                 x1)))
    ;;(verb-a -> unknown :com ,*c-unknown-as-verb*)
    (verb-a => adj-a
            ;; adj's are like verbs. Give some penalty for the
            ;; conversion, so that if there is a verb for the same
            ;; word, the verb form is preferred.
            :com ,*c-adj-as-verb*
            ;; some adj are not used like verb, not even as verb0
            :stag ,(tn (prefer t1 'adj-can-be-verb *c-rare*))
            )
    (verb-a =2> dir verb-a :com ,*c-verb-mod-verb-p*
            :tval ,(tfn (if (= 1 L1 L2)
                            ;; e.g. 西迁, 南移, 東輸.
                            (tag-as 'VV x1 x2)
                            `(:splice (NP-ADV ,(tag-as 'NN x1))
                                      (VP ,x2))))
            :stag ,(tn
                    ;; TODO: to prefer certain movement verbs?
                    (cond ((typep t2 'v-swim-in)
                           (not-prefer-conn t1 'dir-up-down *c-close-suffix*))
                          ((= 1 L1 L2)
                           *c-prefer*)
                          ;; currently only allow both mono case
                          (t nil)))
            )
    (verb-a =2> adv verb-a
            ;; e.g. '严控'
            :stag ,(tn (if (= 1 L1 L2)
                           (adv-verb t1 t2)
                           nil))
            :tval ,*pos-VV*
            )
    (verb-a => dir-verb :com ,*c-close-sep*)
    (verb-a => help-verb)
    ,@(loop :for hua-prefix :in
         ;; NOTE: replaced animate, inanimate, abstract, name with noun-b.

         ;; TODO: whether to change 'place' to 'place-b'?
         
         ;;'(adj-x animate inanimate abstract place
         ;;  verb-x time name)
         '(adj-x noun-b place verb-x time)
         :collect
         `(verb-a -> ,hua-prefix "化" :com ,*c-noun-suffix*
                  :tval ,*pos-VV*
                  :stag ,(tn (cond ((typep t1 'adj) *c1*)
                                   ((typep t1 'verb) *c2*)
                                   (t *c3-4*)))
                  :tag ,(make-instance 'hua-verb)))
    (verb-a -> place-ind :com ,(+ *c-verb*
                                  *c-place-ind-as-verb*)
            ;; TODO: check tval
            :tval ,*pos-VV*
            :tag ,(make-instance 'verb))
    ;; '改' short for 改革
    (verb-a -> noun-b "改" :com ,*c-short-close-suffix*
            ;; NOTE: replace abstract and inanimate with noun-b,
            ;; combined the two rules, prefer mono.
            :stag ,(tn (score-add
                        (typecase t1
                          (abstract *c-prefer*)
                          (inanimate *c-prefer*)
                          (t nil))
                        (prefer-mono L1 *c-close-suffix*)))
            :tval ,*pos-VV*
            :tag ,(make-instance 'verb))
    ;;(verb-a -> abstract "改" :com ,*c-short-close-suffix*
    ;;        ;; NOTE: replaced with the one rule above
    ;;        :tval ,*pos-VV*
    ;;        :tag ,(make-instance 'verb))
    ;;(verb-a -> inanimate "改" :com ,*c-short-close-suffix*
    ;;        ;; NOTE: replaced with the one rule above
    ;;        :tval ,*pos-VV*
    ;;        :tag ,(make-instance 'verb))
    (verb-a -> place-a "譯" :com ,*c-short-close-suffix*
            ;; NOTE: changed from previous place to place-a, also
            ;; added prefer-mono.

            ;; e.g. "中譯" "英譯"
            :tval ,*pos-VV*
            :stag ,(tn (+ (prefer-mono L1 *c-common*)
                          (prefer t1 'country-abbr *c-rare*)))
            :tag ,(make-instance 'verb))
    ;;
    (dir-verb =1> :dir-verb :com ,*c-dir-verb*
              :tval ,*pos-VV*)
    (help-verb =1> :help-verb :com ,*c-help-verb*
               :tval ,*pos-VV*)
	;;
	,@(allow-quote-splice 'verb-a)
    ;; close supplement
    (verb-b => verb-a)
    (verb-close-supp => adj-p :com ,*c-adj-as-verb-close-supp*)
    ;;(verb-close-supp :> verb-a :com ,*c-verb-as-verb-close-supp*)
    (verb-close-supp => dir-verb :com ,*c-dir-verb-as-verb-close-supp*
                     ;; 過 -> (AS "過")
                     :tval ,(tfn (if (typep t1 'v-pass)
                                     (t-str 'AS)
                                     x1)))
    (verb-close-supp => verb-close-suffix :com ,*c-close-suffix-as-verb-close-supp*)
    (verb-close-supp -> dir-verb verb-close-suffix
                     :tval ,*pos-VV*
                     :tag ,(tn (verb-supp-tag t1 t2)))
    ;; TODO: in CTB, 於 seems sometimes attached to verb, and sometimes as P, very confusing!!
    (verb-close-suffix => :verb-supp
                       :tval ,*pos-AS*
                       :com ,*c-verb-supp*)
    ,@(strs 'verb-close-suffix '("着" "著") :>
            :tval *pos-AS*
            :tag (make-instance 'verb-close-supp-zhe))
    ,@(strs 'verb-adj-close-suffix '("一下" "點" "些" "一下子" "不已"
                                     "多久" "沒多久" "不多久" "似的"
                                     "多時" "之極" "至極" "之至" "一空"
                                     "透頂" "絕倫" "得很")
            '->
            :tval *pos-AD*
            :tag (make-instance 'verb-close-supp))
    ,@(strs 'verb-adj-close-suffix '("下")
            '->
            ;; '下' ambiguities: dir (in forming abstract-place and place), unit (quantify action, as frequency), verb
            :tval *pos-AD*
            :com *c-prep*
            :tag (make-instance 'verb-close-supp))
    (verb-close-suffix => verb-adj-close-suffix)
    (verb-close-suffix :> "到" :tag ,(make-instance 'v-goto)
                       :tval ,*pos-VV*)
    (verb-close-suffix :> "給" :tag ,(make-instance 'v-give)
                       :tval ,*pos-VV*)
    ;; use the class for the verb v-for, but the semantic is 'as'
    ;; e.g. 判為
    (verb-close-suffix :> "為" :tag ,(make-instance 'v-for)
                       :com ,*c-prep*
                       :tval ,*pos-VV*)
    ;; e.g. '(写 在) (纸上)'
    (verb-close-suffix :> "在" :tag ,(make-instance 'v-at)
                       :com ,*c-prep*
                       :tval ,*pos-VV*)
    ;; specialized verb-close-suffix
    ;;(verb-b -> verb-a "在"
    ;;        ;; TODO: tval
    ;;        :com ,(+ *c-close-suffix-as-verb-close-supp*
    ;;                 *c-verb-close* *c-close-sep* *c-close-sep*)
    ;;        ;; in 'V S at N ...' where S could be V or supp
    ;;        ;; prefer (V S) at N ...
    ;;        ;; e.g. (說著) 在田野 ...
    ;;        ;; prefer 就在 as adv and v-at
    ;;        :stag ,(tn (not-prefer t1 'v-jiu4-b *c-rare*))
    ;;	    :tag ,(make-instance 'v-at))
    
    ;; add trait to help distinguish the small differences, for use in
    ;; similar-verb to penalize non-parallel structure
    (der :> "得" :tval ,(t-str 'DER)
         :tag ,(make-instance 'v-der))
    (verb-b -> verb-a der :com ,*c-verb-close*
            :tval ,*ctb-verb-verb*
            :stag ,(tn (verb-supp t1 t2))
            :tag ,(tn (verb-supp-tag-trait t1 t2 'verb-der)))
    (verb-b -> verb-a verb-close-supp :com ,*c-verb-close*
            :tval ,*ctb-verb-verb*
            :stag ,(tn (verb-supp t1 t2))
            :tag ,(tn (verb-supp-tag-trait t1 t2 'verb-supp)))
    (verb-b -> verb-a "個" verb-close-supp :com ,*c-verb-close*
            ;; e.g. (VP (VV 说) (DER 个) (VP (VA 明白)))
            :tval ,(tfn `(VP ,x1 ,(tag-as 'DER x2) ,(as-head x3 'VP)))
            :stag ,(tn (verb-supp t1 t2))
            :tag ,(tn (verb-supp-tag-trait t1 t3 'verb-ge-supp)))
    (verb-close-supp-or-liao => verb-close-supp)
    (verb-close-supp-or-liao :> "了"
                             :tval ,*pos-VV*)
    (verb-b -> verb-a der verb-close-supp-or-liao
            :com ,*c-verb-close*
            :tval ,(tfn `(VPT ,x1 ,x2 ,x3))
            :stag ,(tn (verb-supp t1 t2))
            :tag ,(tn (verb-supp-tag-trait t1 t3 'verb-der-supp)))
    (verb-b -> verb-a "不" verb-close-supp-or-liao
            :com ,*c-verb-close*
            :tval ,(tfn `(VPT ,x1 ,(tag-as 'AD x2) ,x3))
            :stag ,(tn (verb-supp t1 t2))
            :tag ,(tn (verb-supp-tag-trait t1 t3 'verb-bu-supp)))
    (verb-b -> verb-a verb-suffix-le verb-close-supp
            :com ,*c-verb-close*
            :tval ,*ctb-splice*
            :stag ,(tn (score-add
                        (not-prefer t3 'adj)
                        (verb-supp t1 t2)))
            :tag ,(tn (verb-supp-tag-trait t1 t3 'verb-le-supp)))
	;;
	,@(allow-quote-splice 'verb-b 2)
	;;
    (verb => verb-b)
    ,@(strs 'verb-suffix-le '("了") '->
            :tval *pos-AS*)
    (verb =1> verb-b verb-suffix-le :com ,*c-close-sep*
          :tag ,(tn (more-trait t1 'verb-le t))
          :tval ,*ctb-splice*)
    (verb =1> verb-b verb-suffix-le time-period :com ,*c-close-suffix*
          :tag ,(tn (more-trait t1 'verb-le-time t))
          :tval ,*ctb-splice*)
    ;; TODO: whether to keep this 'verb de shi' pattern?
    ,@(strs 'word-de-shi '("的是" "得是") '->)
    (verb -> verb word-de-shi
          :tag ,(tn (more-trait t1 'verb-de-shi t))
          :stag ,(tn (if (or (typep t1 'adj)
                             (typep t1 'v-has))
                         *c-prefer*
                         nil))
          :tval ,(tfn (cond ((typep t1 'adj)
                             (tag-as 'VA args))
                            ((typep t1 'v-has)
                             (tag-as 'VV args))
                            (t args))))
	;;
	,@(allow-quote-splice 'verb 3)
    ;;(word => verb) ;; covered by pred, as verb -> verb-p -> pred
    ;; further supplement
    (verb-c0 => verb
             ;; prefer '可能' on phrase, similar to adv, rather than a standalone verb.
             :stag ,(tn (+ (not-prefer t1 'v-ke4-neng2 *c-common*)
                           ;; '可' is also problematic
                           (not-prefer t1 'v-ke4-c *c-common*)))
             :tag ,(tn (make-instance 'verb0 :verb t1)))
    ;;
    (verb-c0-conn-a => verb-c0)
    (verb-c0-conn-a -> verb-c0-conn-a L-conn verb-c0
                    :stag ,(tn (similar-verb t1 t3))
                    :tval ,*ctb-splice*
                    :tag ,*make-conn-t1-t3*)
    (verb-c0-conn => verb-c0-conn-a
                  :tval ,*ctb-as-VP*)
	;;
	,@(allow-quote-splice 'verb-c0-conn 4)
    ;; for use in obj-pred
    ;; TODO: may consider add "v0-p => verb-x"
    ;; NOTE: currently verb will not be a conn-thing.
    (v0-p => verb) ;; retained for convenience, could be covered by verb-p
    (v0-p => verb-p :com ,*c-close-sep*
          ;; since verb-p now records when the verb has taken a noun
          ;; we use this information
          ;; NOTE: verb-p may be a conn-thing, want each to be verb0
          :stag ,(tn (min-over-conn
                      t1
                      #'(lambda (v)
                          (prefer-verb v 'verb0 nil)))))
    ;;
    (verb-c -> verb num-unit
            :tval ,*ctb-VP*
            :tag ,(tn (make-instance 'verb-n :verb t1 :obj t2))
            ;; TODO: add constraints on the unit that can be used?
            :stag ,(tn (score-add
                        (verb-noun t1 t2)
                        (verb-unit t1 t3))))
    (verb-c -> verb unit-p noun-p :com ,*c-verb-noun*
            :tval ,(tfn `(VP ,x1 ,(ctb-mod-noun-p x2 x3)))
            :tag ,(tn (make-instance 'verb-n :verb t1 :obj t3))
            :stag ,(tn (score-add
                        (verb-noun t1 t3)
                        (noun-unit t3 t2))))
    (verb-c -> verb noun-p :com ,*c-verb-noun*
            :tval ,*ctb-VP*
            :tag ,(tn (make-instance 'verb-n :verb t1 :obj t2))
            :stag ,(tn (score-add
                        (verb-noun t1 t2)
                        (not-prefer-trait t2 'has-num-unit-suffix *c-close-suffix*))))
    (verb-c -> verb noun-p "來" :com ,*c-verb-noun*
            ;; e.g. 探出 頭 來
            :tval ,(tfn `(VP ,x1 ,x2 (VP ,(tag-as 'VV x3))))
            :tag ,(tn (make-instance 'verb-n :verb t1 :obj t2))
            :stag ,(tn (score-add
                        (prefer-verb t1 'v-dir-O)
                        (verb-noun t1 t2))))
    (verb-c -> verb noun-p num-unit :com ,*c-verb-noun*
            ;; note: the num-unit may modify the verb instead of
            ;; noun-p, or the num-unit could be the second noun on its
            ;; own.
            :tval ,*ctb-VP*
            :tag ,(tn (make-instance 'verb-n :verb t1 :obj t2))
            ;; TODO: add constraints on the unit that can be used?
            :stag ,(tn (score-add
                        (verb-noun t1 t2)
                        (verb-unit t1 t3))))
    (verb-c -> verb noun-p adj-p :com ,(+ *c-close-suffix*
                                           *c-verb-noun*)
            :tag ,(tn (more-trait-to
                       (make-instance 'verb-n :verb t1 :obj t2)
                       'verb-n-adj t))
            :tval ,(tfn `(VP ,x1 ,x2 ,(as-head (ctb-JJ-to-VA x3) 'VP)))
            ;; the adj-p is to supplement the verb
            :stag ,(tn (score-add
                        (score-add
                         (verb-noun t1 t2)
                         ;; added constraint for the verb and adj-p
                         (verb-adj-supp t1 t3))
                        ;; e.g. prefer 在 ((草地)上), not 在 草地 上
                        (not-prefer t3 'adj-shang4))))
    (verb-c -> verb noun-p noun-p :com ,*c-verb-animate-noun-p*
            :tval ,*ctb-VP*
            :tag ,(tn (more-trait-to
                       (make-instance 'verb-n :verb t1 :obj t3)
                       'verb-n-n t))
            :stag ,(tn (score-add
                        (+ (prefer-verb t1 'v-beneficiary)
                           (prefer t2 'animate)
                           (not-prefer t2 'de-noun))
                        (verb-noun t1 t3))))
    ;;
	,@(allow-quote-splice 'verb-c 3)
    ;; CTB: verb-c should be in VP
    (verb-c-conn-a => verb-c)
    (verb-c-conn-a -> verb-c-conn-a L-conn verb-c
                   :stag ,(tn (similar-verb t1 t3))
                   :tval ,*ctb-splice*
                   :com ,*c-close-sep*
                   :tag ,*make-conn-t1-t3*)
    (verb-c-conn => verb-c-conn-a
                 :tval ,*ctb-as-VP*)
    ;;
	,@(allow-quote-splice 'verb-c-conn 4)
    ;; group together verb-c that may end with verb-p, so as to
    ;; penalize attaching supplements such as 了 to outer verb-p
    ;; rather than inner verb-p
    ,@(strs 'word-der-or-ge '("得" "個") :>
            :com *c-prep*)
    (verb-c2 -> verb word-der-or-ge verb-p :com ,*c-verb-de-verb-p*
             ;; e.g. (VP (VA "忙") (DER "个") (VP (ADVP (AD "不")) (VP (VV "停"))))
             :tag ,(tn (more-trait t1 'verb-der-vp t))
             :tval ,*ctb-VP*)
    ;;(verb-c2 =1> verb noun-p verb-p :com ,*c-verb-noun-verb-p*
    ;;         :stag ,(tn (+ (max (prefer-verb t1 'v-OP)
    ;;                            (prefer-verb t3 'v-at)) ;; TODO: may consider other ways to allow 'at place' as supplement
    ;;                       (verb-noun t1 t2)
    ;;			   (verb-verb t1 t3)))) ;; TODO

    (verb-may-pause => verb)
    (verb-may-pause =1> verb pause
                    ;; some verbs can be followed by a pause, then a
                    ;; verb-p or subj-pred.
                    ;; TODO: get a more complete list of those verbs.
                    :stag ,(tn (score-add
                                ;; the pause
                                (case t2
                                  ((:colon) *c-close-sep*)
                                  ((:comma :short-comma)
                                   *c-close-suffix*)
                                  (t nil))
                                ;; the verb
                                (if (verbs-with-pause-p t1)
                                    0
                                    *c-very-rare*))
                               )
                    ;; 'verb-with-pause for penalizing e.g. '(表示，女性朋友在遇到类似事件) 时' in forming time with ind
                    :tag ,(tn (more-trait t1 'verb-with-pause t))
                    :tval ,*ctb-splice*
                    )
    (verb-may-pause -> verb noun-p may-pause
                    ;; Specifically for verb such as '告诉', which could take a noun, possibly a pause, and then also a VP or SP.
                    ;; e.g. '告诉她，有一名...'.

                    ;; NOTE: seems verbs such as '告诉' are very
                    ;; problematic, in '告诉 NN pause VP', without
                    ;; much strong semantic constraints and context,
                    ;; it is difficult to determine whether to split
                    ;; the VP in as a parallel VP to '告诉 NN'. Or in
                    ;; '告诉 NN VP1 pause VP2', it is difficult to
                    ;; determine whether VP1 and VP2 should form a
                    ;; conn-thing, or VP2 and '告诉 NN VP1' should be
                    ;; parallel.

                    ;; TODO: get a more complete list of those verbs.
                    :stag ,(tn (score-add
                                ;; the pause
                                (if (verbs-with-pause-p t1)
                                    (case t3
                                      ((:colon) *c-close-sep*)
                                      ((:comma :short-comma)
                                       *c-long-sep*)
                                      (t 0))
                                    (if t3 *c-rare* 0))
                                ;;
                                (score-add
                                 ;; currently only the verb '告诉' and '警告'
                                 (prefer-verb t1 'v-BOP *c-fallback*)
                                 (prefer-animate-like t2)
                                 )))
                    :tag ,(tn
                           (let ((r (make-instance 'verb-n :verb t1 :obj t2)))
                             (if t3
                                 ;; 'verb-with-pause for penalizing e.g. '(表示，女性朋友在遇到类似事件) 时' in forming time with ind
                                 (more-trait-to r 'verb-with-pause t)
                                 r)))
                    :tval ,*ctb-splice*
                    )

    (verb-c2 -> verb noun-p verb-p :com ,*c-verb-OP*
             :tag ,(tn (more-trait t1 'verb-OP t))
             :tval ,(tfn `(VP ,x1 ,x2 ,(as-head x3 'IP)))
			 :stag ,(tn (score-add
                         (+ (prefer-verb t1 'v-OP)
                            (not-prefer t1 'v-at *c-common*)
                            (not-prefer-CC-verb-mod t3 *c-rare*)
                            ;; '叫 XXX 敢 VP' ?
                            (if (is-type-p t1 'v-ask-to)
                                (not-prefer-verb t3 'v-gan3-d *c-common*)
                                0)
                            (if (and (typep t1 'v-let)
                                     (trait-value t3 :verb-mod-gei3))
                                *c-rare*
                                0)
                            ;; conn verb-p attachment bias
                            (conn-not-prefer-seps t3 *c-long-sep*
                                                  '(:comma :semi-colon)))
                         (score-add
                          (verb-noun t1 t2)
                          (subj-verb t2 t3)))))
    (verb-c2 -> verb noun-p pred :com ,*c-verb-noun-pred*
             :tag ,(tn (more-trait t1 'verb-OP t))
             :tval ,(tfn `(VP ,x1 ,x2 ,(as-head x3 'IP)))
			 :stag ,(tn (score-add
                         (+ (prefer-verb t1 'v-OP)
                            (not-prefer t3 'a-subj-pred)
                            (not-prefer t1 'v-at *c-common*)
                            (not-prefer-trait t3 'has-time-pause *c-common*)
                            (not-prefer-trait t3 'has-place-pause *c-close-suffix*)
                            (not-prefer-trait t3 'has-subj-pause *c-common*)
                            (not-prefer-CC-verb-mod t3 *c-rare*)
                            ;; '叫 XXX 敢 VP' ?
                            (if (is-type-p t1 'v-ask-to)
                                (not-prefer-verb t3 'v-gan3-d *c-common*)
                                0)
                            (if (and (typep t1 'v-let)
                                     (trait-value t3 :verb-mod-gei3))
                                *c-rare*
                                0)
                            ;; conn verb-p attachment bias
                            (conn-not-prefer-seps t3 *c-long-sep*
                                                  '(:comma :semi-colon)))
                         (score-add
                          (verb-noun t1 t2)
                          (verb-verb t1 t3))))) ;; TODO
    (verb-c2 -> verb-may-pause verb-p
             ;; :com ,*c-verb-verb-p* ;; penalty in verb-on-phrase
             ;; NOTE: propagate 'verb-with-pause trait if wrap in other objects
             :tag ,(tn (more-trait t1 'verb-VP t))
             :tval ,*ctb-VP*
			 :stag ,(tn (score-add
                         (+
                          ;; if v-must-BOP, need the noun in verb-may-pause, so will be wrapped in verb-n, and not the bare verb.
                          (not-prefer t1 'v-must-BOP *c-common*)
                           ;; conn verb-p attachment bias
                          (conn-not-prefer-seps t2 *c-long-sep*
                                                '(:comma :semi-colon))
                           (prefer-verb t1 'v-VP))
                         (verb-on-phrase t1 t2))))
    (verb-c2 -> "得" verb-p :com ,*c-de-verb-p*
             :tag ,(tn (more-trait t2 'der-verb t))
             :tval ,(tfn `(VP ,(tag-as 'VV x1) ,x2)))
    ;;(verb-c2 =2> help-verb verb-p "的" :com ,*c-help-verb-verb-p-de*) ;; TODO: tval
    (verb-c2 -> verb-may-pause subj-pred
             ;; :com ,*c-verb-subj-pred* ;; penalty in verb-on-phrase
             ;; NOTE: propagate 'verb-with-pause trait if wrap in other objects
             :tag ,(tn (more-trait t1 'verb-SP t))
             :tval ,*ctb-VP*
			 :stag ,(tn (score-add
                         (+
                          ;; if v-must-BOP, need the noun in verb-may-pause, so will be wrapped in verb-n, and not the bare verb.
                          (not-prefer t1 'v-must-BOP *c-common*)

                          (not-prefer-trait t2 'has-time-pause *c-common*)
                          (not-prefer-trait t2 'has-place-pause *c-close-suffix*)
                          (not-prefer-trait t2 'has-subj-pause *c-common*)
                          ;; for ambiguity of attaching a verb-p to
                          ;; inner subj-pred as conn, or outer verb-p,
                          ;; give a little bias against long pause to
                          ;; arbitrarily prefer outer.
                          
                          ;; lenient if there is a pause in verb, e.g. '报导 指出，(2018年初的刺激支出可能使得工业活动大增，从而导致重度空气污染卷土重来)'
                          (if (trait-value t1 'verb-with-pause)
                              0
                              (let ((c (and (a-subj-pred-p t2)
                                            (a-subj-pred-pred t2))))
                                (conn-not-prefer-seps c (+ *c-close-sep*
                                                           *c-close-sep*)
                                                      '(:comma :semi-colon))))
                          (prefer-verb t1 'v-SP)
                          )
                         (verb-on-phrase t1 t2))))
    ;; general successive actions. Sometimes the first action is
    ;; preparation for the second, sometimes the second action is the
    ;; reason for the first action, but we cannot make this fine
    ;; distinction now, so lump them together.
    ;; For the tag, previously arbitrarily take the first, now
    ;; returning both as a conn-thing.
    (verb-c2 => verb-and)
    (verb-and -> verb-c verb-p :com ,*c-verb-and-verb-p*
              :stag ,(tn (score-add
                          (+ (not-prefer t1 'v-at *c-rare*)
                             (if (and (typep t1 'verb-n)
                                      (typep (verb-n-verb t1) 'v-at))
                                 *c-rare*
                                 0)
                             (conn-not-prefer-seps t1 *c-rare*)
                             (conn-not-prefer-seps t2 *c-rare*)
                             ;; prefer verb-p to attach to other parts, instead of paralle with v-is, unless it is also a v-is
                             (prefer-both-or-neither-conn t1 t2 'v-is))
                          (if (and (typep t1 'verb-n)
                                   (typep t2 'verb0)
                                   (is-type-p (verb-n-verb t1) 'dir-verbs))
                              ;; e.g. allow '(拿回 學校) 養', or '(回 學校) 學習'
                              ;; NOTE: '(把 花盆) ((拿回 學校) (種 花))' would need a different rule
                              *c-prefer*
                              (similar-verb t1 t2))))
              :tag ,*make-conn-t1-t2*
			  :tval ,*ctb-VP-VP*)
    (verb-and -> verb verb-p :com ,(+ *c-verb-and-verb-p*
                                      *c-verb-verb-p*)
              :stag ,(tn (score-add
                          (similar-verb t1 t2)
                          (+ (conn-not-prefer-seps t1 *c-rare*)
                             (conn-not-prefer-seps t2 *c-rare*)
                             ;; prefer verb-p to attach to other
                             ;; parts, instead of paralle with v-is,
                             ;; unless it is also a v-is
                             (prefer-both-or-neither-conn t1 t2 'v-is))))
              :tag ,*make-conn-t1-t2*
			  :tval ,*ctb-VP-VP*)
    (verb-and -> verb-c2 verb-p :com ,(+ *c-verb-and-verb-p*
                                         *c-verb-verb-p*)
              :stag ,(tn (score-add
                          (if (is-type-p t2 'passive-voice)
                              ;; since passive-voice receive penalty
                              ;; in similar-verb if t1 is not passive,
                              ;; accomodate this.
                              (score-max
                               (similar-verb t1 t2)
                               (score-add
                                *c-close-suffix*
                                (similar-verb t1 (passive-voice-verb t2))))
                              (similar-verb t1 t2))
                          (+ (conn-not-prefer-seps t1 *c-rare*)
                             (conn-not-prefer-seps t2 *c-rare*)
                             ;; e.g. '(下跪 (求 别 报警)) (遭拒)'
                             ;; where the first part is verb-c2. In
                             ;; the fear of introducing more
                             ;; ambiguity, currently bias towards
                             ;; passive-voice.
                             (prefer t2 'passive-voice *c-very-rare*)
                             ;; prefer verb-p to attach to other
                             ;; parts, instead of paralle with v-is,
                             ;; unless it is also a v-is
                             (prefer-both-or-neither-conn t1 t2 'v-is))))
              :tag ,*make-conn-t1-t2*
			  :tval ,*ctb-VP-VP*)
    ;;
	,@(allow-quote-splice 'verb-c2 3)
    ;;
    (verb-c2-conn-a => verb-c2)
    (verb-c2-conn-a -> verb-c2-conn-a L-conn verb-c2
                    :stag ,(tn (score-add
                                ;; to encourage the use of the above verb-c2 rule with verb-may-pause
                                (not-prefer-verb t1 'v-must-BOP *c-close-suffix*)
                                (similar-verb t1 t3)))
                    :tval ,*ctb-splice*
                    :com ,*c-close-sep*
                    :tag ,*make-conn-t1-t3*)
    (verb-c2-conn => verb-c2-conn-a
                  :tval ,*ctb-as-VP*)
    ;;
	,@(allow-quote-splice 'verb-c2-conn 4)
    ;; TODO: in CTB, vp-supp are bracketed differently from what we do
    ;; same level as VP: 了
    ;; same level as IP: exclaims, 的, 嗎, 吧, 麼, 也, 乎. ?否
    ;;    的話, 而已, 罷了, 也好, 也罷, 沒有, 來著.
    ;; AD in VP: 不曾. ?未曾
	,@(strs 'vp-supp '("了")
			:>
			:tval *pos-SP*)
    ,@(strs 'vp-supp '("一下" "一下子" "似的" "與否" "多時")
            '->
            :tval *pos-AD*)
    ;;
    (verb-d => verb-c0-conn)
    (verb-d =1> verb-c0-conn vp-supp :com ,*c-verb-inner-supp*
			:tval ,*ctb-splice*)
    (verb-d => verb-c-conn)
    (verb-d =1> verb-c-conn vp-supp :com ,*c-verb-inner-supp*
			:tval ,*ctb-splice*)
    (verb-d => verb-c2-conn)
    (verb-d =1> verb-c2-conn vp-supp :com ,*c-verb-outer-supp*
			:tval ,*ctb-splice*)
    ;;
	,@(allow-quote 'verb-d 5)
    ;;
    (verb-d-conn-a => verb-d)
    (verb-d-conn-a -> verb-d-conn-a L-conn verb-d
                   :stag ,(tn (similar-verb t1 t3))
                   :tval ,*ctb-splice*
                   :com ,(* 2 *c-close-sep*)
                   :tag ,*make-conn-t1-t3*)
    (verb-d-conn => verb-d-conn-a
                 :tval ,*ctb-as-VP*)
    ;;
	,@(allow-quote 'verb-d-conn 6)
    ;; try to work around preference of 'verb noun' over 'verb verb-p'
    ;; when there is indicators to help us.
    ;; this "就" is more like the 'then' in 'if-then', but sometimes
    ;; the 'if' is omitted.
    ;;(verb-e =1> verb "就" verb-p :com ,*c-verb-then-verb-p*)
    ;;
    (verb-p-a => verb-d)
    ;;(verb-p-a => verb-e)
    (verb-p-a -> verb-mod verb-p-a :com ,*c-verb-mod-verb-p*
			  ;; TODO: handle restructuring for some verb-mod
              :tval ,*ctb-verb-mod-verb-p*
              :tag ,(tn (adv-verb-tag t1 t2))
              :stag ,(tn (score-add
                          ;; '轉' is more often combined with the
                          ;; verb, not modifying the verb-p.
                          (+ (not-prefer t1 'adv-word-zhuan3 *c-common*)
                             ;; adv '年' should be attached to verb,
                             ;; e.g. '年產', rather than on verb-p
                             (not-prefer t1 'adv-time-year *c-common*)
                             ;; want '(XXX 時) (说 NNN)', not '(XXX) (時 (说 NNN))'
                             (not-prefer t1 'adv-word-shi2 *c-common*)
                             ;; encourage verb-mod not shared, if otherwise equal score
                             (conn-not-prefer-seps t2 *c-long-sep*
                                                   '(:comma :semi-colon))
                             (not-prefer-trait t2 'verb-mod-has-pause *c-close-sep*))
                          (let ((*L1* L1)
                                (*L2* L2))
                            (adv-verb t1 t2)))))
    ;;
	,@(allow-quote 'verb-p-a 7)
    ;;
    (verb-p-b => verb-p-a
              :tval ,*ctb-as-VP*)
    (verb-p-b => pred-v
              :tval ,*ctb-as-VP*)
    ;;;
    (verb-p-conn-a => verb-p-b)
    (verb-p-conn-a -> verb-p-conn-a L-conn verb-p-b
                   ;; NOTE: originally there is a similar-verb
                   ;; constraint here, but it seems it is too severe,
                   ;; as the verb-p's may be quite different (one may
                   ;; be verb-n, the other is 'V subj-pred'), even
                   ;; under the same subject. So relaxed the
                   ;; similar-verb.
                   
                   ;; But prefer '进行 (相关教育 和 惩戒)' to '(进行
                   ;; (相关教育)) 和 惩戒', so impose similar-verb
                   ;; constraints for some close L-conn.
                   :stag ,(tn (score-add
                               (if (member t2 '(:conn-word :short-comma))
                                   (score-add
                                    ;; :conn-word and :short-comma bind tighter than :comma and :semi-colon
                                    (conn-not-prefer-seps t1 *c-common*
                                                          '(:comma :semi-colon))
                                    (similar-verb t1 t3))
                                   0)
                               (+ (prefer-both-or-neither-conn t1 t3 'v-is)
                                  ;; in case of ties, prefer the
                                  ;; subj-pred to absorb the second
                                  ;; non-subj-pred, instead of being
                                  ;; parallel.
                                  (if (and (a-subj-pred-p t1)
                                           (not (a-subj-pred-p t3)))
                                      (- +sub-scale+)
                                      0))))
                   :tval ,*ctb-splice*
                   :com ,(* 3 *c-close-sep*)
                   :tag ,*make-conn-t1-t3*)
    (verb-p => verb-p-conn-a
            :tval ,*ctb-as-VP*)
    ;;
    ;; alternative simplified verb-p (only a subset of the rules), for
    ;; verbs which may be confused with PP, and easily shadowed,
    ;; currently only has '為' as in 'is'.
    (alt-verb => verb-a
              ;; give a slight penalty, in case the normal verb-p is not shadowed by PP
              :com ,(- +sub-scale+)
              :stag ,(tn (must-be t1 'v-for)))
    (alt-verb-c -> alt-verb noun-p :com ,*c-verb-noun*
                :tval ,*ctb-VP*
                :tag ,(tn (make-instance 'verb-n :verb t1 :obj t2))
                :stag ,(tn (score-add
                            (verb-noun t1 t2)
                            (not-prefer-trait t2 'has-num-unit-suffix *c-close-suffix*))))
    (alt-verb-c -> alt-verb noun-p num-unit :com ,*c-verb-noun*
                ;; note: the num-unit may modify the verb instead of
                ;; noun-p, or the num-unit could be the second noun on its
                ;; own.
                :tval ,*ctb-VP*
                :tag ,(tn (make-instance 'verb-n :verb t1 :obj t2))
                ;; TODO: add constraints on the unit that can be used?
                :stag ,(tn (score-add
                            (verb-noun t1 t2)
                            (prefer t3 'unit-event-frequency *c-common*))))
	,@(allow-quote-splice 'alt-verb-c 3)
    (alt-verb-c2 -> alt-verb verb-p
                 ;; NOTE: may need to duplicate for alt-verb-p, if alt-verb has other verbs
                 ;; :com ,*c-verb-verb-p* ;; penalty in verb-on-phrase
                 ;; NOTE: propagate 'verb-with-pause trait if wrap in other objects
                 :tag ,(tn (more-trait t1 'verb-VP t))
                 :tval ,*ctb-VP*
                 :stag ,(tn (score-add
                             (+
                              ;; if v-must-BOP, need the noun in verb-may-pause, so will be wrapped in verb-n, and not the bare verb.
                              (not-prefer t1 'v-must-BOP *c-common*)
                              ;; conn verb-p attachment bias
                              (conn-not-prefer-seps t2 *c-long-sep*
                                                    '(:comma :semi-colon))
                              (prefer-verb t1 'v-VP))
                             (verb-on-phrase t1 t2))))
    (alt-verb-c2 -> "得" alt-verb-p :com ,*c-de-verb-p*
                 :tag ,(tn (more-trait t2 'der-verb t))
                 :tval ,(tfn `(VP ,(tag-as 'VV x1) ,x2)))
	,@(allow-quote-splice 'alt-verb-c2 3)
    (alt-verb-d => alt-verb-c)
    (alt-verb-d =1> alt-verb-c vp-supp :com ,*c-verb-inner-supp*
                :tval ,*ctb-splice*)
    (alt-verb-d => alt-verb-c2)
    (alt-verb-d =1> alt-verb-c2 vp-supp :com ,*c-verb-outer-supp*
                :tval ,*ctb-splice*)
	,@(allow-quote 'alt-verb-d 5)
    (alt-verb-p-a => alt-verb-d)
    (alt-verb-p-a -> verb-mod alt-verb-p-a :com ,*c-verb-mod-verb-p*
                  ;; TODO: handle restructuring for some verb-mod
                  :tval ,*ctb-verb-mod-verb-p*
                  :tag ,(tn (adv-verb-tag t1 t2))
                  :stag ,(tn (score-add
                              ;; '轉' is more often combined with the
                              ;; verb, not modifying the verb-p.
                              ;; NOTE: Some verb-mods seems not suitable here, put some penalty in adv-verb for some PP and v-for.
                              (+ (not-prefer t1 'passive-voice *c-common*)
                                 (not-prefer t1 'adv-word-zhuan3 *c-common*)
                                 ;; adv '年' should be attached to verb,
                                 ;; e.g. '年產', rather than on verb-p
                                 (not-prefer t1 'adv-time-year *c-common*)
                                 ;; want '(XXX 時) (说 NNN)', not '(XXX) (時 (说 NNN))'
                                 (not-prefer t1 'adv-word-shi2 *c-common*)
                                 ;; encourage verb-mod not shared, if otherwise equal score
                                 (conn-not-prefer-seps t2 *c-long-sep*
                                                       '(:comma :semi-colon))
                                 (not-prefer-trait t2 'verb-mod-has-pause *c-close-sep*))
                              (let ((*L1* L1)
                                    (*L2* L2))
                                (adv-verb t1 t2)))))
    (alt-verb-p => alt-verb-p-a
                :tval ,*ctb-as-VP*)
	,@(allow-quote 'alt-verb-p 8)
    ;;;;
	,@(allow-quote 'verb-p 8)
    ;; short variant of verb-p, for use in forming nouns
    (verb-x => verb-a)
    (verb-x =1> verb-a verb-a
            :stag ,(tn (score-add
                        (not-prefer t2 'v-give)
                        (verb-verb t1 t2)))
            :tval ,*ctb-verb-verb*)
    (verb-x -> verb-a dir-verb :com ,*c-close-verb-verb*
            :stag ,(tn (verb-supp t1 t2))
            :tval ,*ctb-verb-verb*
            :tag ,(tn (verb-supp-tag t1 t2)))
    (verb-x =2> adv verb-x :com ,*c-verb-mod-verb-p*
            :stag ,(tn (score-add
                        (+ (not-prefer t1 'front-adv)
                           ;; prefer '有 ((逾103万人) (死于空气污染))', not '((有逾) 103万人) (死于空气污染)'
                           (not-prefer t1 'adv-yes-no *c-close-suffix*)
                           ;; not want '首善 之 ((都 执政) 权)'
                           (not-prefer t1 'adv-range-has-du1 *c-common*)
                           ;; to discourage multiple adv
                           (not-prefer-trait t2 'verb-x-has-adv
                                             *c-close-suffix*))
                        (let ((*L1* L1)
                              (*L2* L2))
                          (adv-verb t1 t2))))
            :tag ,(tn (more-trait t2 'verb-x-has-adv t))
            :tval ,(tfn (cond ((both-mono-p x1 x2)
                               (tag-as 'VV x1 x2))
                              ((headed-by x2 'VP)
                               ;; add more than one adv, should be uncommon
                               `(VP ,(as-phrase-level x1 'ADVP) ,@(cdr x2)))
                              (t
                               `(VP ,(as-phrase-level x1 'ADVP)
                                    ,(as-phrase-level x2 'VP))))))
    (verb-x =2> time verb-x
            :stag ,(tn (score-add
                        ;; to discourage multiple time modifier
                        (not-prefer-trait t2 'verb-x-has-time *c-close-suffix*)
                        (mod-noun t1 t2 L1 L2)))
            :tag ,(tn (more-trait
                       (mod-noun-tag t1 t2 L1 L2)
                       'verb-x-has-time t))
            :tval ,(tfn (if (both-mono-p x1 x2)
                            (tag-as 'VV x1 x2)
                            `(:splice ,x1 ,x2))))
    (verb-x =2> "被" verb-a :com ,*c-verb-mod-verb-p*
            :tval ,*pos-VV*)
    ;; TODO: whether to add "noun verb-a" rule here
    (verb-x -> verb-a noun-a
            :com ,*c-verb-x-mod-noun*
            :stag ,(tn (score-add
                        (+ (not-prefer t1 'v-is)
                           (not-prefer t1 'v-give)
                           (cond ((or (typep t1 'v-kong4-zhi4-b)
                                      (typep t1 'v-kong4))
                                  ;; for things like '(控制 污染) 措施'
                                  *c-prefer*)
                                 (t (+ (prefer-mono L1)
                                       (prefer-mono L2)))))
                        (verb-noun t1 t2)))
            :tval ,(tfn (if (both-mono-p x1 x2)
                            (tag-as 'VV args)
                            (cons 'VP (cdr args))))
            :tag ,(tn (make-instance 'verb-n :verb t1 :obj t2)))
    ;; TODO: more parallel?
    (verb-x -> verb-a noun-a verb-a noun-a
            :com ,*c-verb-x-mod-noun*
            :stag ,(tn (score-add
                        (+ (prefer-mono L1)
                           (prefer-mono L2)
                           (prefer-mono L3)
                           (prefer-mono L4))
                        (score-add
                         (verb-noun t1 t2)
                         (verb-noun t3 t4))))
            :tval ,(tfn (cond ((and (mono-p x1)
                                    (mono-p x2)
                                    (mono-p x3)
                                    (mono-p x4))
                               (tag-as 'VV x1 x2 x3 x4))
                              (t `(VP (VP ,x1 ,x2)
                                      (VP ,x3 ,x4)))))
            :tag ,(tn (make-conn-thing
                       :first (make-instance 'verb-n :verb t1 :obj t2)
                       :second (make-instance 'verb-n :verb t3 :obj t4))))
    ;;
	,@(allow-quote 'verb-x 3) ;; avoid ambiguity with quote in noun-b
    ;; TODO: whether to add connectives for verb-mod?

    ;; NOTE: some verb-mod could also be used as noun-zhi-mod (not
    ;; already covered by noun-mod and noun-zhi-mod). These are marked
    ;; n-verb-mod or n-verb-mod-front.
    (verb-mod => n-verb-mod)
    (verb-mod-front => n-verb-mod-front)
    
    ;; NOTE: seems some verb-mod could be used at the front of a
    ;; clause, they are labeled as verb-mod-front
    (verb-mod => verb-mod-front
              :stag ,(tn (if (and (pp-verb-mod-p t1)
                                  (pp-verb-mod-prefer-pause t1))
                             *c3*
                             0)))
    (verb-mod =1> verb-mod-front pause
              :tag ,(tn (more-trait t1 'verb-mod-has-pause t))
              :stag ,(tn (if (and (pp-verb-mod-p t1)
                                  (eq (pp-verb-mod-pp t1) :for-wei4))
                             ;; '为' causes a lot of trouble, with
                             ;; ambiguity with verb '为', so penalize
                             ;; this pp with pause, to hope to help a
                             ;; bit.
                             (+ *c1* *c-close-sep*)
                             0))
              :tval ,*ctb-splice*)
    (verb-mod-front =1> place-p :com ,*c-place-as-verb-mod*
                    ;; '一個個都' treated as place, because '都' could mean a city, but not preferred when used alone
                    :tval ,#'x1
                    :stag ,(tn (+ (not-prefer t1 'city-word-du1)
                                  (cond ((has-trait t1 'place-with-ind)
                                         0)
                                        ((typep t1 'place-with-dir)
                                         ;; '通常情况 下'
                                         *c-close-sep*)
                                        (t (+ (not-prefer t1 'word-building-jian4-zhu2)
                                              ;; not prefer '建筑' alone as place
                                              *c-place-no-ind-as-verb-mod*)))
                                  (if (= L1 1) *c-common* 0))))
    (verb-mod =1> q-pronoun :com ,*c-place-as-verb-mod*
			  :tval ,*ctb-NP*
              :stag ,(tn (must-be t1 'q-pronoun-place)))
    ;;
    (dev :> "地" :tval ,*pos-DEV*)
    (dev-de :> "的" :tval ,*pos-DEV*)
    ;;
    (verb-mod => adv-p)
    (verb-mod =1> adv-p dev
			  :tval ,(tfn `(DVP ,x1 ,x2)))
    (verb-mod =1> sound adv-word-dev
			  :tval ,(tfn `(DVP ,(as-head x1 'NP) ,x2)))
    (verb-mod -> adj dev
              :stag ,(tn (if (= L1 1)
                             (not-prefer t1 'adj-no-mono-de *c-rare*)
                             0))
			  :tval ,(tfn `(DVP ,(as-head (ctb-JJ-to-VA x1) 'VP) ,x2)))
    ;;(verb-mod =1> adj :com ,*c-adj-as-adv*) ;; seems adj can be turned into adv already, and any composition can be done at adv-p level
    (verb-mod :> compare :com ,*c-close-sep*)
    (verb-mod -> adj-p dev-de may-pause
              ;; seems mono adj is not usually used as adv and as verb-mod?
              :stag ,(tn (if (= L1 1)
                             (not-prefer t1 'adj-no-mono-de *c-rare*)
                             *c-close-suffix*))
			  :tval ,(tfn `(DVP ,(as-head (ctb-JJ-to-VA x1) 'VP) ,x2)))
    (verb-mod => num-unit :com ,*c-common*
			  ;; should be already QP
              :stag ,(tn (+ (not-prefer t1 'rough-amount *c-common*)
                            (prefer t1 'unit-event-frequency))))
    (verb-mod =1> num-unit adv-word-dev
			  :tval ,*ctb-DVP*)
    (verb-mod =1> time-p may-pause :com ,*c-time-as-verb-mod*
              :tag ,(tn (if t2
                            (more-trait t1 'verb-mod-has-pause t)
                            t1))
			  :tval ,*ctb-splice*)
    (verb-mod => help-verb)
    (verb-mod -> verb-a der adj-p adv-word-dev
			  :tval ,(tfn `(DVP (VP ,x1 ,x2 ,(as-head (ctb-JJ-to-VA x3) 'VP))
								,x4)))
    ;; NOTE: added agreement constraint on 把 and 將, which indicate (one of) the objects for a verb.
    ;; But consider: in 浪濤(把你)吞沒, '你' is the object of 吞沒
    ;; but in (把你)重新 推進 大海, '你' is also the object, but syntactically '大海' is the object of 推進, so need to address this.
    ;; TODO: allow a number of aspects for a verb: subj, obj, time, place?
	;; TODO: "有 所 VV" is (VP (VE 有) (NP (MSP 所) (VP (VV ..))))
    (msp-suo3 :> "所" :tval ,*pos-MSP*
              ;; '所' could also mean a place
              :com ,*c-prep*)
    (msp-er3 :> "而" :tval ,*pos-MSP*)
    ,@(strs 'BA '("把" "將") :>
            :tval (t-str 'BA)
            :tag :BA)
    ;;
    (verb-mod -> BA noun-p
			  ;; the structure of the tval will be modified at verb-p
			  :tval ,(tfn (cons 'BA (cdr args)))
              :tag ,*pp-verb-mod-t1-t2*)
	;; TODO: may need to use tag for msp-er3 to restrict its use, to avoid confusion with (CC 而), because in CTB, (MSP 而) is front of a VP if the VP is preceded by a (PP ..)

	;; (verb-mod :> msp-er3 :com ,*c-close-sep*) ;; has pred-v for "而"
	(verb-mod :> msp-suo3 :com ,*c-close-sep*
              :tag :verb-mod-suo3)
    (verb-mod -> "以" :com ,*c-prep*
              :tval ,*pos-MSP* :tag :yi3)
    (p-word-yi3 :> "以"
                :tval ,*pos-P*
                :tag :yi3)
    (verb-mod -> p-word-yi3 noun-p
			  :tval ,*ctb-PP*
              :tag ,*pp-verb-mod-t1-t2*)
    (verb-mod -> p-word-yi3 noun-p msp-er3
			  :tval ,(tfn `(:verb-mod-cross (PP ,x1 ,x2) ,x3))
              :tag ,*pp-verb-mod-t1-t2*)
    (verb-mod -> "給" :com ,*c-adv*
              ;; more often MSP than SB
              :tval ,*pos-MSP*
              ;; too add trait to the verb-p
              ;; if encounter LB or BA in outer verb-mod, will cancel its effect.
              ;; if got to subj-verb with its trait still in effect, will act like passive-voice
              :tag :verb-mod-gei3)
    (verb-mod -> verb-b noun-p
              ;; TODO: whether to keep this
              ;; '給 noun-p' or 'v+給 noun-p'
              :tval ,(tfn `(PP ,(tag-as 'P x1) ,x2))
              :stag ,(tn (must-be t1 'v-give)))
    ;; TODO: handle passive voice in a sane way for other applicable verb-mod
    ,@(str-tags 'passive-word '("被" "受" "遭" "讓") :>)
    ;; Seems 讓 is a bit different from 受 and 遭
    ;; 讓 could also be used as an active verb, but here only captures its passive sense.
    ;; The active sense of 讓 is left to the verb v-let
    ,@(strs 'LB-word '("被" "受" "遭" "讓") :>
            :tval *pos-LB*)
    ;;
    (verb-mod -> passive-word
              :tval ,*pos-SB*
              :tag ,(make-passive-voice))
    ;; TODO: prefer verb0 for "LB noun-p msp-suo3" cases
    (verb-mod -> LB-word noun-p
			  ;; structure of tval to be modified outside
              ;; TODO: seems some verbs are not used with '被', e.g. '被 NN 毫无察觉' ??
              ;; TODO: not sure if useful to record also the LB-word.
			  :tval ,*ctb-long-LB*
              :tag ,(tn (make-passive-voice :object t2))
              ;; t2 may be a conn, and is preferred to having '和 NN' as another verb-mod
              :stag ,(tn (+
                          ;; e.g. want '(被带到派出所) 时', not '被 (带到派出所 时)'
                          (not-prefer-trait t2 'time-with-ind *c-very-rare*)
                          (prefer-conn t2 'non-verb-noun *c-common*))))
    (verb-mod -> LB-word noun-p msp-suo3
			  ;; structure of tval to be modified outside
			  :tval ,*ctb-long-LB*
              :tag ,(tn (make-passive-voice :object t2))
              :stag ,(tn (prefer-conn t2 'non-verb-noun *c-common*)))
    ;;
    (p-word-wei4 :> "為"
                 :tval ,*pos-P*
                 ;; '為' could also be verb, similar to 'is'.
                 ;; so for PP, prefer animate as subject.
                 ;; if the subject is abstract or inanimate, prefer '為' as verb
                 ;; Need a special non-terminal for '為' as verb, otherwise, it may be easily shadowed by this PP
                 :tag :for-wei4)
    ,@(strs 'p-word-wei4 '("為了") '->
            :tval *pos-P*
            :tag :for)
	;; in CTB, 因為 and 為 are handled similarly, and with connection with 而
	(p-word-wei4 :> because
                 :tag :because)
    (wei4-suff-msp :> msp-er3)
    (wei4-suff-msp :> msp-suo3)
    (subj-pred-or-verb-p => subj-pred)
    (subj-pred-or-verb-p => verb-p
                         :tval ,(tfn (as-head x1 'IP)))
	(verb-mod-front -> p-word-wei4 noun-p
                    :tval ,*ctb-PP*
                    :tag ,*pp-verb-mod-t1-t2*
                    :com ,*c-for-noun-p-so*)
	(verb-mod-front -> p-word-wei4 subj-pred-or-verb-p
                    :stag ,(tn (not-prefer t2 'verb0 *c-close-sep*))
                    :tval ,*ctb-PP*
                    :tag ,*pp-verb-mod-t1-t2-prefer-pause*
                    :com ,*c-for-subj-pred-so*)
    (verb-mod -> p-word-wei4 noun-p wei4-suff-msp
			  :tval ,(tfn `(:verb-mod-cross (PP ,x1 ,x2) ,x3))
              :tag ,*pp-verb-mod-t1-t2*
              :com ,*c-for-noun-p-so*)
    (verb-mod -> p-word-wei4 subj-pred-or-verb-p wei4-suff-msp
			  :tval ,(tfn `(:verb-mod-cross (PP ,x1 ,x2) ,x3))
              :tag ,*pp-verb-mod-t1-t2*
              :com ,*c-for-subj-pred-so*)
    ;;
    ,@(strs 'besides '("除了" "除") '->
            :tag :besides
            :tval *pos-P*)
    ,@(strs 'besides-suffix '("外" "以外") '->
            :tval *pos-LC*)
    (verb-mod-front -> besides clause-c
                    :com ,*c-one-prep*
                    :tag ,*pp-verb-mod-t1-t2*
                    :stag ,*one-pred-not-prefer-conn-t2*
                    :tval ,(tfn `(PP ,x1 ,(ctb-VP-to-IP x2))))
    (verb-mod-front -> besides clause-c besides-suffix
                    ;; TODO: n-verb-mod?
                    :com ,*c-one-prep*
                    :tag ,*pp-verb-mod-t1-t2*
                    :stag ,*one-pred-not-prefer-conn-t2*
                    :tval ,(tfn `(PP ,x1
                                     ;; for this LCP, seems only want
                                     ;; VP to be IP, and it may accept
                                     ;; NP
                                     (LCP ,(ctb-VP-to-IP x2)
                                          ,x3))))
    ;;
    (verb-mod -> "教" noun-p
              :tag ,(tn (make-pp-verb-mod :pp :jiao4-teach
                                          :object t2))
			  :tval ,(tfn `(:splice ,(tag-as 'VV x1) ,x2)))
    (verb-mod -> "叫" noun-p :com ,*c-prep*
              :tag ,(tn (make-pp-verb-mod :pp :jiao4-ask
                                          :object t2))
			  :tval ,(tfn `(:splice ,(tag-as 'VV x1) ,x2))
              :stag ,(tn (prefer t2 'non-verb-noun *c-very-rare*)))
    ,@(strs 'p-towards '("往" "向") :>
            :tval *pos-P*
            :tag :towards-to)
    ,@(strs 'p-towards '("往着" "向着") '->
            :tval *pos-P*
            :tag :towards-to)
    (verb-mod -> p-towards noun-p
              ;; TODO: n-verb-mod? or use verb-p?
              :tag ,*pp-verb-mod-t1-t2*
			  :tval ,*ctb-PP*)
    (verb-mod -> p-towards dir
              ;; TODO: n-verb-mod? or use verb-p?
			  ;; in CTB, dir (e.g. 上, 東, 北) is tagged as NN
              :tag ,*pp-verb-mod-t1-t2*
			  :tval ,(tfn `(PP ,x1
							   (NP ,(tag-as 'NN x2)))))
    (p-towards-to => p-towards)
    (p-towards-to => time-at-to
                  ;; use only some of the words
                  :stag ,(tn (if (eq :to t1)
                                 0
                                 nil)))
    (p-from => time-from
            ;; NOTE: not for time, but use some of the words in time-from.
            :val ,(fn (cons x0 (cdr x1)))
            :stag ,(tn (if (member t1 '(:from :from-zhi4))
                           0
                           nil)))
    (verb-mod -> p-from noun-p p-towards-to noun-p
              ;; TODO: n-verb-mod? or use verb-p?
              ;; for phrases such as '自北向南'
              :stag ,(tn (and 
                          ;; for time, prefer the whole thing as time
                          (not-prefer-conn t2 'time-name nil) 
                          (not-prefer-conn t4 'time-name nil)
                          ;;
                          (similar-noun t2 t4)
                          ))
              :tag ,(tn (make-conn-thing
                         :first (make-pp-verb-mod :pp t1 :object :t2)
                         :second (make-pp-verb-mod :pp t3 :object :t4)
                         :sep nil))
              :tval ,(tfn `(:splice (PP ,x1 ,(as-phrase-level x2 'NP))
                                    (PP ,x3 ,(as-phrase-level x4 'NP))))
              )
    ;; 與 is CC when used between two things,
    ;; 與 is P when used at the front of VP it seems
    ;; 和 is predominately CC, similar to the case of 與
    ;; 管 XXX 叫 YYY
    ,@(strs 'p-noun-vp-sp-word '("與" "和" "跟" "同")
            ;; seems can say '(与 (1970年代末实施改革开放，大力发展经济)) 不无关系'
            :>
            ;; these could also be connectives, so discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
            ;; TODO: may create a struct for verb-mod, if there is need to get more information of the different verb-mods
            :tag :verb-mod-CC
            ;; need more penalty for this, to prevent '(让 冯女士) (和 朋友) 很难接受' where the two are separated into verb-mods, but prefer '(让 (冯女士 和 朋友)) 很难接受'
            :com *c-prep*
            :tval *pos-P*)
    ;; TODO: give all these meaning PP-tag for use in pp-verb-mod
    ,@(str-tags 'p-noun-word
                '((:use "用")
                  (:on-behalf-of "替")
                  (:spatial-along "沿")
                  (:spatial-separate "隔")
                  (:give "予")
                  "管")
                :>
                :tval *pos-P*)
    ,@(str-tags 'p-noun-word
                '((:towards-at "對着" "對")
                  (:spatial-separate "隔着")
                  (:together-with "會同" "連同" "随同")
                  ;; e.g. "跟着 他", "順着 他的想法",
                  ;; so :following is different from :together-with
                  (:following "跟着" "順着")
                  ;; '利用' is labeled as VV in majority of cases in
                  ;; CTB, but as PP, it is similar to :use above.
                  (:v-use "利用")
                  ;; also used in time and number as 'approx', prefer that use
                  (:near "近")
                  (:from "由" "從")
                  )
                '->
                :tval *pos-P*
                :com *c-prep*)
    ,@(strs 'p-noun-word '("除去"
                           "by" "of" "to" "with" "in"
                           "冒着" "帶着"
                           "冲着" "尋着" "跟在" "圍着")
            '->
            :tval *pos-P*
            :com *c-prep*)
    (verb-mod -> p-noun-word noun-p
              ;; give back the tag to distinguish the verb-mod
              ;; some p-noun-word are also CC, which give a lot of troubles, causing ambiguity with (N1 CC N2) verb-p,
              ;; so give it more penalty
              :stag ,(tn (case t1
                           ((:towards-at)
                            ;; '对 ((记者阐述澳对亚洲政策的框架) 时)'
                            (cond ((trait-value t2 'time-with-ind)
                                   *c-common*)
                                  ((typep t2 'adj)
                                   ;; want '对方 极 有可能', not '对 (方 极 有可能)'
                                   *c3*)
                                  ((typep t2 'verb)
                                   ;; not want '对 (偷拍者 (进行 (相关)))'
                                   *c-common*)
                                  (t 0)))
                           ((:spatial-along :spatial-separate)
                            (prefer t2 'place *c-rare*))
                           ((:near)
                            (prefer t2 'place *c-common*))
                           ((:from)
                            ;; for place and time, there are more specific PP
                            (score-add
                             (min-over-conn t2
                                            #'(lambda (x)
                                                (typecase x
                                                  ;; '建筑' could also be a verb, and a kind of '活动', not only used as place
                                                  (word-building-jian4-zhu2 *c-close-suffix*)
                                                  (place nil)
                                                  (t 0))))
                             (score-add
                              (not-prefer-conn t2 'dir nil)
                              (not-prefer-conn t2 'time-name nil))))
                           (t 0)))
              :tag ,*may-pp-verb-mod-t1-t2*
              :tval ,(tfn (if (eq t1 :v-use)
                              `(VP ,(tag-as 'VV x1)
                                   ,(as-phrase-level x2 'NP))
                              (as-lst-head (cdr args) 'PP))))
    ;; special case
    (verb-mod-front -> "對" "外"
                    :com ,*c-place*
                    :tval ,(tfn `(PP ,(tag-as 'P x1)
                                     (NP ,(tag-as 'NN x2)))))
    ;; 謹 here is followed only by 此
    (verb-mod -> "謹" "此"
              :tval ,(tfn `(PP ,(tag-as 'P x1)
                               (NP ,(tag-as 'PN x2)))))
    ;; 牟 暴利
    (verb-mod -> "牟" noun-p
              :tval ,(tfn `(PP ,(tag-as 'P x1) ,x2))
              :stag ,(tn (conn-must-be t2 'money-related)))
    ,@(str-tags 'p-noun-vp-sp-word
                '((:via
                   ;; some are moved down below
                   "隨" "隨着" "緊隨" "緊隨着"
                   "經由" "透過" "via"
                   "憑" "憑藉" "憑着"
                   "藉着" "藉由" "借由" "借着"
                   "依靠着" "靠着")
                  (:via-jing1 "經")
                  (:following
                   "據" "依據" "根據" "據着" "依據着" "根據着"
                   "按" "按照" "照" "按着" "按照着" "照着"
                   "依" "依照" "依着" "依照着"
                   "比照" "比照着")
                  (:towards-to "朝" "朝着")
                  (:relative-to
                   "相對" "對於" "相對於"
                   "針對" "針對着")
                  (:relative-related-to
                   "有關" "關於" "有關於")
                  (:because
                   "基於" "鑑於" "出於" "因着")
                  (:for "為着" "for")
                  (:surrounding "圍繞" "圍繞着")
                  (:while "趁" "趁着")
                  (:wait-for "等")
                  ;;
                  "to" "至於" "正如" "without"
                  "本着" "每當")
            '->
            :tval *pos-P*
            :com *c-close-suffix*)
    ,@(str-tags 'p-noun-vp-sp-word
                '((:taking "拿着")
                  (:relative-to-jiu4 "就")
                  (:via
                   "經過" "通過" "藉" "依靠" "靠")
                  "論"
                  )
                '->
                :tval *pos-P*
                :com *c-prep*)
    ,@(str-tags 'p-noun-vp-sp-word
                ;; this is confusing, so give more penalty
                '((:taking "拿")
                  (:away-from
                   "至" "離" "距離")
                  )
                '->
                :tval *pos-P*
                :com (+ *c-close-suffix* *c-prep*))
    (n-verb-mod-front -> p-noun-vp-sp-word noun-p
                      :stag ,(tn (case t1
                                   ((:verb-mod-CC) *c-common*)
                                   ((:while) *c-rare*)
                                   ((:taking)
                                    (not-prefer-verb t2 'dir-verbs))
                                   ((:away-from)
                                    ;; prefer '至' as time-ind
                                    (+ (not-prefer t2 'time-name *c-common*)
                                       ;; not want '至2012年的空气质素数据'
                                       (not-prefer t2 'abstract *c2*)))
                                   ((:via)
                                    (typecase t2
                                      (place 0)
                                      (event 0)
                                      (t *c-close-suffix*)))
                                   ((:via-jing1)
                                    (typecase t2
                                      (transaction-ind ;; 经 贸
                                       *c-very-rare*)
                                      (t *c-close-suffix*)))
                                   ((:wait-for)
                                    ;; seems '等' is more often used
                                    ;; on subj-pred if it is
                                    ;; followed by a noun. Also, could use the verb form of '等'.
                                    *c-very-rare*)
                                   ((:relative-to-jiu4)
                                    ;; ambiguity with '就' as adv
                                    ;; e.g. want '中共 把霾 (就 (变成了 雾))', not '中共 把霾 (就 变) (成了 雾)'

                                    ;; no strong semantic knowledge to
                                    ;; accurately classify this, so
                                    ;; penalize if the noun could also
                                    ;; be verb, to prefer the adv
                                    ;; form.
                                    (if (and (typep t2 'abstract)
                                             (also-verb-p t2))
                                        *c-close-suffix*
                                        0))
                                   ;;
                                   (t 0)))
                      :tag ,*may-pp-verb-mod-t1-t2*
                      :tval ,*ctb-PP*)
    (n-verb-mod-front -> p-noun-vp-sp-word verb-p
                      ;; may need to penalize some PP in letting this be verd-mod-front
                      :tag ,*may-pp-verb-mod-t1-t2*
                      ;; 至 may be confused with time ind
                      :stag ,(tn (cond ((and (eq :away-from t1)
                                             (has-trait t2 'verb-mod-time))
                                        *c-common*)
                                       ;; :following and :relative-to prefer noun than verb-p
                                       ((eq :following t1) *c-close-suffix*)
                                       ((eq :relative-related-to t1)
                                        ;; '有關' seems not even common as verb-mod?
                                        *c-rare*)
                                       ((eq :relative-to t1) *c-close-suffix*)
                                       ((eq :relative-to-jiu4 t1)
                                        ;; ambiguity with '就' as adv
                                        ;; offending sample: '闺蜜 (就 (告诉她，有一名身穿蓝 ...)) VV ...'
                                        (+
                                         (conn-not-prefer-seps t2 *c-close-suffix*
                                                               '(:comma :semi-colon))
                                         *c-close-sep*
                                         *c-common*))
                                       ((eq :taking t1)
                                        ;; :taking not prefer either verb-p or subj-pred
                                        (+ *c-close-suffix*
                                           (not-prefer-verb t2 'dir-verbs)))
                                       ((eq :wait-for t1)
                                        (+ (not-prefer-verb t2 'adj *c-common*)
                                           (conn-not-prefer-seps t2 *c-close-suffix*
                                                                 '(:comma :semi-colon))))
                                       ((eq :via-jing1 t1)
                                        ;; '經X' is often a verb or noun by itself, e.g. '經驗'
                                        (+
                                         (conn-not-prefer-seps t2 *c-close-suffix*
                                                               '(:comma :semi-colon))
                                         (if (= L2 1) *c2* *c-close-suffix*)))
                                       (t 0)))
                      :tval ,(tfn `(PP ,x1 ,(as-head x2 'IP))))
    (n-verb-mod-front -> p-noun-vp-sp-word subj-pred
                      ;; may need to penalize some PP in letting this be verd-mod-front
                      :stag ,(tn (score-add
                                  (not-prefer-subj-pred-pauses t2)
                                  (case t1
                                    ((:away-from)
                                     (not-prefer-trait t2 'has-time *c-common*))
                                    ((:verb-mod-CC)
                                     ;; '與 (NP VP)' easily confused with '(與 NP) VP'
                                     *c-rare*)
                                    ((:following :taking)
                                     ;; :taking not prefer either verb-p or subj-pred
                                     ;; :following prefer noun than verb-p and subj-pred
                                     *c-close-suffix*)
                                    ((:relative-to :relative-to-jiu4)
                                     ;; :relative-to prefer noun to verb-p and subj-pred
                                     ;; and it seems subj-pred is quite rare
                                     (+ *c-long-sep* *c-close-suffix*))
                                    ((:relative-related-to)
                                     ;; '有關' seems not even common as verb-mod?
                                     *c-rare*)
                                    (t 0))))
                      :tag ,*may-pp-verb-mod-t1-t2*
                      :tval ,(tfn `(PP ,x1 ,(as-head x2 'IP))))
    ;; sentence structure with LCP as suffix
    ,@(str-tags 'p-zuo4-wei2 '((:being-as "作為" "做為"))
                '->
                :tval *pos-P*)
    ,@(strs 'LC-lai2-shuo1 '("來說" "來講" "來看") '->
            :tval *pos-LC*)
    (verb-mod-front -> p-zuo4-wei2 noun-p
                    :tag ,*may-pp-verb-mod-t1-t2*
                    :tval ,(tfn `(PP ,x1 ,x2)))
    (p-LCP-prefix :> p-zuo4-wei2)
    ,@(strs 'p-LCP-prefix '("從") '->
            :tval *pos-P*)
    ,@(strs 'LC-kan4 '("看") '->
            :com *c-prep*
            :tval *pos-LC*)
    (verb-mod-front -> p-LCP-prefix noun-p LC-kan4
                    ;; e.g. '从 国际经验 看'
                    :stag ,(tn (prefer t2 'abstract *c3*))
                    :tag ,*may-pp-verb-mod-t1-t2*
                    :tval ,(tfn `(PP ,x1 (LCP ,x2 ,x3))))
    (verb-mod-front -> p-LCP-prefix verb-mod-LCP
                    :tag ,*may-pp-verb-mod-t1-t2*
                    :tval ,(tfn `(PP ,x1 ,x2)))
    (verb-mod-LCP =1> noun-p LC-lai2-shuo1
                  :tval ,(tfn `(LCP ,x1 ,x2)))
    (verb-mod-front :> verb-mod-LCP)
    ;;
    ;; 繼 is often followed by LCP, e.g. noun-p 之后
    ,@(strs 'p-word-ji4 '("繼") :>
            :tag :after
            :tval *pos-P*)
    ,@(strs 'p-word-after '("後" "之後") '->
            :tval *pos-LC*)
    (LCP-ji4 => noun-p)
    (LCP-ji4 => verb-p)
    (LCP-ji4 => subj-pred)
    (n-verb-mod-front -> p-word-ji4 LCP-ji4 p-word-after
                      :tag ,*may-pp-verb-mod-t1-t2*
                      :tval ,(tfn `(PP ,x1 (LCP ,x2 ,x3))))
    ;;
    ,@(strs 'p-word-from '("FROM")
            '->
            :tag :from
            :tval *pos-P*)
    (verb-mod -> p-word-from may-pause noun-p
              :tag ,(tn (let ((r (make-pp-verb-mod :pp t1 :object t3)))
                          (if t2 (more-trait r 'verb-mod-has-pause t) r)))
			  :tval ,*ctb-PP*)
    ;;
	;; TODO: whether to keep? In CTB, the structure seems to be (VP (VV "要") ..) (VP (ADVP (AD "才")) ..)
    ;;(verb-mod -> "要" time "才")
    ;; e.g. 當面, 當著他的面
    ;;(verb-mod -> verb-p :stag ,(tn (must-be t1 'v-as)))
    ,@(strs 'p-word-dang1-zhe '("當著") '->
            :tval *pos-P*)
    (verb-mod-front -> p-word-dang1-zhe noun-p
                    :stag ,(tn (prefer t2 'face *c-common*))
                    :tval ,(tfn `(PP ,x1 ,x2)))
    ;;
    (adj-a =1> :adj :com ,*c-adj*
           :tval ,(tfn (ctb-vnv t1 'VA x1)))
    ;;(adj-a -> unknown :com ,*c-unknown-as-adj*)
    (adj-a -> integer "流" :com ,*c-adj*
           :tval ,*pos-JJ*
           :tag ,(make-instance 'adj-on-noun))
    (adj-a -> noun-b "形"
           ;; NOTE: combined animate and inanimate rules, using noun-b
           :stag ,(tn (typecase t1
                        ;; TODO: whether to allow 'place'?
                        (animate *c-prefer*)
                        (inanimate *c-prefer*)
                        (t nil)))
           :tval ,*pos-JJ*
           :com ,*c-close-suffix*
           :tag ,(make-instance 'adj-shape))
    ;;(adj-a -> animate "形"
    ;;       ;; NOTE: combined
    ;;       :tval ,*pos-JJ*
    ;;       :com ,*c-close-suffix*
    ;;       :tag ,(make-instance 'adj-shape))
    ;;(adj-a -> inanimate "形"
    ;;       ;; NOTE: combined
    ;;       :tval ,*pos-JJ*
    ;;       :com ,*c-close-suffix*
    ;;       :tag ,(make-instance 'adj-shape))
    (adj-a -> verb-x "用"
           ;; e.g. 包装用, 殺敵用, 驅蚊用
           :tag ,(tn (make-yong4-adj :the-verb t1))
           :com ,*c-close-sep*
           :stag ,(tn (if (= L1 2)
                          ;; not want '(非 信) 用'
                          (not-prefer-verb t1 'v-fei1-b *c-common*)
                          *c-rare*))
           :tval ,*pos-JJ*)
    (adj-a -> "可" verb-a
           :tval ,*pos-VA*
           :com ,*c-close-suffix*
           :stag ,(tn (+ (not-prefer t2 'v-mo4-you2-a)
                         ;; prefer '可能' as adv
                         (not-prefer t2 'v-neng2 *c-common*)))
           :tag ,(make-instance 'adj))
    (adj-a =2> "粉" adj-a :com ,*c-close-suffix*
           :tval ,*pos-VA*
           :stag ,(tn (must-be t2 'adj-color)))
    (adj-a -> integer "次" "性" :com ,*c-long-sep*
           :tval ,*pos-JJ*
           :tag ,(make-instance 'adj))
    ,@(strs 'v-adj-place '("旅" "駐") :>)
    (adj-a -> v-adj-place country-abbr-x :com ,*c-long-sep*
           :tval ,*pos-JJ*
           :tag ,(make-instance 'adj))
    (adj-a -> time-point-name "均" :com ,*c-adj*
           :tag ,(make-instance 'adj)
           :tval ,*pos-JJ*)
    ;; may be used in other contexts
    (double-char :> :double-char)
    ;; e.g. 紅彤彤, 綠油油
    (adj-a =1> adj-a double-char :com ,(+ *c-long-sep*
                                          *c-double-char*)
           :tval ,*pos-VA*
           :stag ,(tn (must-be t1 'adj-color)))
    ;; e.g. 漆黑
    (adj-a =2> inanimate-a adj-a :com ,*c-close-suffix*
           :tval ,*pos-VA*
           :stag ,(tn (cond ((typep t2 'adj-gold-silver)
                             *c-common*)
                            ((typep t2 'adj-grey) *c-common*)
                            ((typep t2 'adj-color) 0)
                            (t nil))))
	;;
	,@(allow-quote 'adj-a)
	;;
    (adj =1> adj-a "色" :com ,*c-close-suffix*
         :tval ,*pos-VA*
         :stag ,(tn (must-be t1 'adj-color)))
    ;;
    (adj => adj-a)
    (adj =2> "非" adj :com ,*c-close-suffix*
         :tval ,*pos-JJ*)
    (adj =1> adj-a "觀" :com ,(+ *c-noun-suffix* *c2*)
         :tval ,*pos-JJ*)
    ;; TODO: do we need to have something between animate and animate-a ?
    ,@(loop :for noun-a :in
         '(animate-a inanimate-a place-b abstract-a
           verb-x)
         :collect
         `(adj -> "反" ,noun-a :com ,*c-short-close-suffix*
               :tval ,*pos-JJ*
               :stag ,(tn (not-prefer t2 'verb *c-close-sep*))
               :tag ,(make-instance 'adj-on-noun)))
    ,@(loop :for noun-a :in
         '(inanimate-a abstract-a)
         :collect
         `(adj -> "耗" ,noun-a :com ,*c-short-close-suffix*
               :tval ,*pos-JJ*
               :tag ,(make-instance 'adj-on-noun)))
	;;
	,@(allow-quote 'adj 2)
    ;;
    (adj-x => adj)
    (adj-x =2> adv adj :com ,*c-adj-mod-adj-p*
           ;; TODO: whether to combine into one word if both mono?
           :tval ,(tfn `(ADJP ,(as-head x1 'ADVP)
                              (ADJP ,x2)))
           :tag ,(tn (if (typep t1 'adv-with-shi4)
                         (more-trait t2 'adv-with-shi4 t)
                         t2))
           :stag ,(tn (mod-adj t1 t2)))
    (adj-x =2> adj adj :com ,*c-adj-adj*
           :tval ,(tfn (if (and (= L1 1) (= L2 1))
                           (tag-as 'JJ (cdr args))
                           `(ADJP ,x1 ,x2)))
           :stag ,(tn (mod-adj t1 t2)))
	;;
	,@(allow-quote 'adj-x 3)
	;;
	(adj-conn-a => adj-x)
	(adj-conn-a -> adj-x conn adj-conn-a
				:tval ,*ctb-splice*
				:tag ,*make-conn-t1-t3*)
	;;
	(adj-conn => adj-conn-a
			  :tval ,(tfn (if (headed-by x1 :splice)
							  (as-head x1 'ADJP)
							  x1)))
	;;
	,@(allow-quote 'adj-conn 4)
    ;;
    (adj-p-a => adj-conn)
    (adj-p-a =2> adj-p-a adj-conn :com ,(+ *c-adj-adj* *c-close-sep*)
             :tval ,*ctb-splice*
             :stag ,(tn (mod-adj t1 t2)))
    ;;
    (adj-p-b => adj-p-a :com ,*c-close-sep*
             :tval ,*ctb-ADJP*)
    (adj-p-b =2> adj-mod adj-p-b :com ,(+ *c-close-sep*
                                          *c-adj-mod-adj-p*)
             ;; to avoid ambiguity with adj-x
             :tval ,*ctb-splice*
             :stag ,(tn (mod-adj t1 t2)))
	;;
	,@(allow-quote 'adj-p-b 5)
    ;; In CTB, for compare, seems the adj becomes VA, and ADJP becomes VP
    ;;(adj-mod :> compare)
    (adj-p-c =2> compare adj-p-b :com ,*c-adj-mod-adj-p*
             :tval ,(tfn `(:splice ,x1
                                   ,(as-head (ctb-JJ-to-VA x2)
                                             'VP)))
             :stag ,(tn (mod-adj t1 t2)))
    (adj-p-c =2> adj-mod adj-p-c :com ,*c-adj-mod-adj-p*
             :tval ,*ctb-splice*
             :stag ,(tn (mod-adj t1 t2)))
	;;
	,@(allow-quote 'adj-p-c 6)
    ;;
    (adj-mod => num-unit
             ;; not prefer some units, or only prefer some units?
             :stag ,(tn (not-prefer t1 'unit-event-frequency *c-rare*)))
    ;;
    (adj-mod => adv
             :tval ,*ctb-ADVP*)
    (adv-word-dev :> dev)
    (adv-word-dev :> dev-de)
    (adj-mod =1> adv-p adv-word-dev
             :tval ,(tfn (cons 'ADVP (cdr args))))
    ;; adj-supp: to supplement adjective
    (adj-p-d => adj-p-b
			 :tval ,*ctb-ADJP*)
	;;
    (adj-p-e => adj-p-c
			 :tval ,*ctb-as-VP*)
    (adj-p-e =1> adj-p-a adj-supp
			 :tval ,(tfn `(VP ,(ctb-JJ-to-VA x1) ,x2))
			 :stag ,(tn (supp-adj t2 t1)))
    (adj-p-e =1> adj-p-c adj-supp
			 :tval ,(tfn `(VP ,x1 ,x2))
			 :stag ,(tn (supp-adj t2 t1)))
	;;
	(adj-p-d-conn => adj-p-d)
	(adj-p-d-conn -> adj-p-d conn adj-p-d-conn
				  :com ,*c-close-sep*
				  :tval ,*ctb-splice*
				  :tag ,*make-conn-t1-t3*)
	;;
	,@(allow-quote 'adj-p-d-conn 7)
	;;
	(adj-p-e-conn => adj-p-e)
	(adj-p-e-conn -> adj-p-e conn adj-p-e-conn
				  :com ,*c-close-sep*
				  :tval ,*ctb-splice*
				  :tag ,*make-conn-t1-t3*)
	;;
	,@(allow-quote 'adj-p-e-conn 7)
	;;
	(adj-p => adj-p-d-conn
		   :tval ,*ctb-ADJP*)
	(adj-p => adj-p-e-conn
		   :tval ,*ctb-as-VP*)
    ;; in CTB, seems adj becomes VA, and ADJP becomes VP when there is "得" supp, and adv as supp
    (adj-supp =2> "得" adv
              :tval ,(tfn `(:splice (DER ,x1) ,(as-head x2 'ADVP))))
    (adj-supp =1> adv
              :tval ,*ctb-ADVP*)
    (adj-supp =1> num-unit
              :tval ,(tfn (cond ((typep t1 'rough-amount)
                                 `(ADVP (tag-as 'AD x1)))
                                (t x1))))
    (adj-supp => verb-adj-close-suffix
              :tval ,*ctb-ADVP*)
    ;;
    ,@(strs 'eq-prefix '("像" "象" "好像" "跟" "如" "如同" "有如" "好似" "似"
                         "像是" "象是" "好像是" "如同是" "好似是" "似是")
            '->
            :com *c-prep*
            :tval *pos-P*)
    ,@(strs 'eq-suffix '("般" "一般" "一樣" "這樣" "那樣") '->
            ;; somehow in CTB, these are tagged as VA
            :tval *pos-VA*)
    (eq-suffix -> )
    (compare-to => noun-p)
    (compare-to => verb-p
                :stag ,*stag-not-prefer-pause-or-seps*)
    (compare-to => subj-pred
                :stag ,(tn (+
                            (not-prefer-trait t1 'has-time-pause *c-common*)
                            (not-prefer-trait t1 'verb-mod-has-pause *c-common*)
                            (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                            (not-prefer-trait t1 'has-subj-pause *c-common*)
                            (not-prefer-subj-pred-body-seps t1))))
    (compare-eq -> eq-prefix compare-to eq-suffix
                :stag ,(tn (typecase t2
                             (number ;; not want '如一' here
                              *c-common*)
                             (num *c-common*)
                             (t 0)))
                :tval ,(tfn
                        (cond ((empty-p x3)
                               `(PP ,x1 ,x2))
                              (t `(VP (PP ,x1 ,x2)
                                      (VP ,x3))))))
    (compare :> compare-eq)
    (compare -> compare-eq dev-de
             :tval ,(tfn `(DVP ,(as-head x1 'VP) ,x2)))
    ,@(strs 'p-word-bi3 '("比" "較") :>
            :tval *pos-P*)
    ,@(strs 'p-word-bi3 '("比起" "較之" "相比" "相比起") '->
            :tval *pos-P*)
    (compare -> p-word-bi3 compare-to
             :tval ,(tfn (cons 'PP (cdr args))))
    (compare -> place-p "最"
             :stag ,(tn (if (= L1 1)
                            ;; e.g. prefer '(全國 最) 大' to '全 ((國 最) 大)'
                            (not-prefer t1 'country-ind *c-close-suffix*)
                            0))
             :tval ,(tfn `(:splice ,(as-phrase-level x1 'NP)
                                   (ADVP ,(tag-as 'AD x2)))))
    ;; 
    (adv =1> :adv :com ,*c-adv*
         :tval ,*pos-AD*)
    (adv => num-unit
         ;; 首次
         :tag ,(make-instance 'adv-time-frequency)
         :stag ,(tn (must-be t1 'ci4)))
    ;; seems many adjective could also be adverb, use this lazy trick?
    (adv =1> adj-a :com ,(+ *c-close-sep* *c-close-sep*)
         :stag ,(tn (+ (not-prefer t1 'yong4-adj)
                       (not-prefer t1 'adj-unlikely-adv *c4*)))
         :tval ,*pos-AD*)
    ,@(strs 'word-every '("每" "逐" "整" "連" "終") :>)
    (adv-time-point-name :> time-point-name)
    (adv-time-point-name :> "天")
    (adv -> word-every adv-time-point-name :com ,*c-adv*
         :tag ,(make-instance 'adv-time-frequency)
         :tval ,*pos-AD*)
    ,@(strs 'adv-dir-ind '("往" "向") :>)
    (adv -> adv-dir-ind dir :com ,*c-long-sep*
         :tag ,(make-instance 'adv)
         :tval ,*pos-AD*)
    ;;(adv -> unknown :com ,*c-unknown-as-adv*)
    (front-adv =1> :front-adv :com ,*c-front-adv*
               :tval ,*pos-AD*)
    (front-adv -> integer "來" :com ,*c-front-adv*
               ;; This QP is in fact QP-ADV
               :tval ,(tfn `(QP ,t1 (CLP ,(tag-as 'M x2))))
               :stag ,(tn (if (integerp t1)
                              *c-prefer*
                              *c-rare*))
               :tag ,(make-instance 'front-adv))
    ;;(front-adv -> unknown :com ,*c-unknown-as-front-adv*)
	;; Not sure if front-adv also need connectives?
    (adv => front-adv
         ;; those adv with shi4 may be composed with verb form of
         ;; 'shi4', so give some penalty to encourage that form.
         :stag ,(tn (not-prefer t1 'adv-with-shi4 *c-long-sep*))
         )
	;;
	(adv-conn => adv)
	(adv-conn -> adv conn adv-conn
              ;; seems not very common
              :com ,*c-close-suffix*
              :stag ,(tn (case t2
                           ((:conn-word :short-comma) 0)
                           (t *c-common*)))
			  :tval ,*ctb-splice*
			  :tag ,*make-conn-t1-t3*)
	;;
	(adv-p => adv-conn
		   :tval ,(tfn (if (headed-by x1 :splice)
						   (as-phrase-level x1 'ADVP)
						   (as-phrase-level x1))))
	;;
    (word => adv-p :com ,*c-adv-alone*)
    ;;
    (conn -> :conn :com ,*c-conn*
          :tag :conn-word
          :tval ,*pos-CC*)
	(conn => short-pause :com ,*c-close-suffix*)
    ;; L-conn is more relaxed for longer pause
    (L-conn => conn)
    (L-conn => pause :com ,(+ *c-close-suffix*
                              *c-close-sep*))
    (L-conn =1> pause conn
            ;; e.g. '同时亦包括禁止相关企业停产限产，以及禁止燃放烟花爆竹和露天烧烤'
            :tval ,*ctb-splice*
            :com ,(+ *c-close-suffix* *c-close-sep*))
	;;
    (word :> conn :com ,*c-conn-alone*)
    ;;
    ,@(strs 'punctuation '("“" "”" "…" ".") :>
            :tval *pos-PU*)
    (punctuation => pause)
    (punctuation => end)
    (word :> punctuation :com ,*c-punctuation-alone*)
    ;;
    (x-punctuations =1> punctuation
                    :tval ,#'val-cdr
                    :val ,#'val-cdr)
    (x-punctuations =2> punctuation x-punctuations
                    :tval ,#'xList
                    :val ,#'xList)
    (punctuations =1> x-punctuations
                  :tval ,(tfn `(:splice ,@x1)))
    ;;
    (exclaim-a -> :exclaim :com ,*c-exclaim*)
    (exclaim :> exclaim-a :tval ,*pos-SP*)
    (exclaim -> exclaim-a exclaim
             :tval ,*pos-SP*)
    (word :> exclaim :com ,*c-exclaim-alone*
          :tval ,(tfn `(FRAG ,(ctb-SP-to-IJ x1))))
    ;;
    (exclaim-PU -> exclaim punctuations
                :tval ,(tfn `(FRAG ,(ctb-SP-to-IJ x1) ,x2)))
    (word :> exclaim-PU :com ,*c-exclaim-alone*)
    ;;
    (sound-x :> :sound)
    (sound-xx => sound-x :val ,(fn (list x1)))
    (sound-xx =2> sound-x sound-xx
              :com ,*c-close-sep*
              :val ,(fn (cons x1 x2)))
    (sound =1> sound-xx :com ,*c-sound*
           :tag ,(make-instance 'sound-literal)
           :tval ,*pos-ON*)
    (word => sound :com ,*c-sound-alone*)
    ;;
    (prep -> :prep :com ,*c-prep*)
    ;;,@(strs 'prep *preps*)
    (word :> prep :com ,*c-prep-alone*)
	;;
    ,@(allow-quote 'pred 9)
    ;; for tval of pred, we may leave it as (:splice ...) or the underlying thing
    (pred => verb-p)
    ;; TODO: CTB: NP as VP
    (pred => adj-p :com ,*c-adj-p-as-pred*
          :stag ,(tn (not-prefer t1 'adj-time-period *c-rare*)))
    (pred =1> adj-p sp-de :com ,*c-adj-p-as-pred*
		  :tval ,*ctb-CP*
          :stag ,(tn (not-prefer t1 'adj-time-period *c-rare*))
          :tag ,(tn (make-adj-has-de :the-adj t1)))
    (pred =3> time-p may-pause adj-p
          ;; e.g. not want '3.5年' as '(3.5) (年)'
          :com ,(+ *c-adj-p-as-pred* *c-close-suffix*)
          :stag ,(tn (not-prefer t3 'adj-time-period *c-rare*))
          :tag ,(tn (more-trait t3 'verb-mod-time t))
		  :tval ,*ctb-splice*)
    (pred -> time-p may-pause adj-p sp-de
          :com ,(+ *c-adj-p-as-pred* *c-close-suffix*)
		  :tval ,*ctb-CP*
          :stag ,(tn (not-prefer t3 'adj-time-period *c-rare*))
          :tag ,(tn (more-trait-to
                     (make-adj-has-de :the-adj t3)
                     'verb-mod-time t)))
    ;;(pred :> noun-p :com ,*c-noun-p-as-pred*)
    (pred => subj-pred :com ,*c-subj-pred-as-pred*)
    (pred => obj-pred :com ,*c-obj-pred-as-pred*)

    ;;
    (obj-pred =2> noun-p v0-p
			  :tval ,*ctb-IP*
              :stag ,(tn (score-add
                          (verb-noun t2 t1)
                          (not-prefer-CC-verb-mod t2 *c-very-rare*))))
    (obj-pred =3> noun-p may-pause subj-pred :com ,*c-close-suffix*
			  :tval ,(tfn (if (headed-by x3 'IP)
							  `(IP ,x1 ,x2 ,@(cdr x3))
							  (cons 'IP (cdr args))))
              :stag ,(tn (let ((v (if (a-subj-pred-p t3)
                                      (a-subj-pred-pred t3)
                                      nil)))
                           (score-add
                            (+ (not-prefer t1 'time-name *c-common*)
                               (if (typep t1 'time-name)
                                   (not-prefer-trait t3 'has-time *c-common*)
                                   0)
                               (not-prefer-subj-pred-body-seps t3 *c-close-suffix*))
                            (cond ((null v) *c-rare*)
                                  ;; TODO: can v be a conn-thing of verbs?
                                  ((typep v 'verb0)
                                   (verb-noun (verb0-verb v) t1))
                                  ((typep v 'de-noun-mod)
                                   (mod-noun (de-noun-mod-pred v) t1 L3 L1))
                                  (t (- *c-rare*
                                        *c-close-sep*))))))
              )
    ;; TODO: whether obj-pred need specialized rules for
    ;; subj-pred-or-verb-p as its subject noun-p?
	;;
	,@(allow-quote 'obj-pred)
    ;;
    (pred-s0 => noun-p :com ,*c-noun-p-as-pred*)
    (pred-s0 => num-unit
             ;; Only for noun-p as subj in subj-pred.
             ;; specifically because in CTB, num-unit could be (VP (QP ...)) as pred.
             :tval ,*ctb-VP*)
    (pred-s0 => pred)
    ;;
    (pred-s => pred-s0)
    (pred-s -> pred-s pause pred-s0
            ;; give a high penalty, so that is it used rarely
            ;; e.g. '三十多岁，赵县人，无业，还没有结婚'
            :tval ,*ctb-splice*
            :stag ,(tn (if (and (typep (conn-thing-last t1) 'verb)
                                (typep t3 'verb))
                           ;; consecutive verb-p type should already be merged in other rules
                           *c-common*
                           0))
            :tag ,*make-conn-t1-t3*
            :com ,(* 2 *c-close-suffix*))
    ;;
    ;; Since some PP would shadow some verb in verb-p, so created a
    ;; restricted version alt-verb-p for some verbs.  Therefore, need
    ;; to duplicate some subj-pred rules to use alt-verb-p as pred, so
    ;; that it has a chance.
    ;;
    ,@(loop :for s-pred :in '(pred-s alt-verb-p)
         :collect
         `(subj-pred -> noun-p not-prefer-pause ,s-pred
                     :tval ,*ctb-IP*
                     :tag ,(tn
                            (let ((n-vals nil))
                              (when t2 (setf n-vals (cons2 'has-subj-pause t
                                                           n-vals)))
                              (more-trait-with (subj-verb-tag t1 t3) n-vals)))
                     ;; discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
                     :stag ,(tn (score-add
                                 (subj-verb t1 t3)
                                 (+
                                  (if (and t2 (or (trait-value t1 'place-or-race-human)
                                                  (typep t1 'human)))
                                      ;; e.g. not prefer '赵县人' as subject in '... , 赵县人, ...'
                                      *c-common*
                                      0)
                                  (not-prefer-CC-verb-mod t3)))))
         :collect
         `(subj-pred -> time-p may-pause noun-p ,s-pred
                     ;; to balance the penalty of time as verb-mod, but
                     ;; lower penalty here
                     :com ,*c-time-place-subj-pred*
                     :tval ,*ctb-IP*
                     :tag ,(tn
                            (let ((n-vals '(has-time t)))
                              (when t2 (setf n-vals (cons2 'has-time-pause t
                                                           n-vals)))
                              (more-trait-with (subj-verb-tag t3 t4) n-vals)))
                     ;; discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
                     ;; penalizes for double 'time information'
                     :stag ,(tn (score-add
                                 (subj-verb t3 t4)
                                 (score-add
                                  (if (has-trait t4 'verb-mod-time)
                                      *c-common*
                                      0)
                                  (not-prefer-CC-verb-mod t4)))))
         ;; TODO: need to handle place properly, to avoid conflict of "到 place" with verb-suffix 到
         ;;(subj-pred -> place-p-a may-pause noun-p pred)
         :collect
         `(subj-pred -> time-p may-pause place-p-a may-pause noun-p ,s-pred
                     ;; to balance the penalty of time and place as
                     ;; verb-mod, but lower penalty here
                     :com ,(* 2 *c-time-place-subj-pred*)
                     :tval ,*ctb-IP*
                     :tag ,(tn
                            (let ((n-vals '(has-time t
                                            has-place t)))
                              (when t2 (setf n-vals (cons2 'has-time-pause t
                                                           n-vals)))
                              (when t4 (setf n-vals (cons2 'has-place-pause t
                                                           n-vals)))
                              (more-trait-with (subj-verb-tag t5 t6) n-vals)))
                     ;; discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
                     :stag ,(tn (score-add
                                 (subj-verb t5 t6)
                                 (score-add
                                  (if (has-trait t6 'verb-mod-time)
                                      *c-common*
                                      0)
                                  (not-prefer-CC-verb-mod t6)))))
         )
    ;;
    ;;(subj-pred -> place-p-a may-pause time may-pause noun-p pred)
    ;;; for using subj-pred and verb-p as subject in subj-pred, give
    ;;; them specialized rules, and avoid giving too little penalty
    ;;; for subj-pred and verb-p to be noun-p.
    (subj-pred -> subj-pred-or-verb-p may-pause pred
               :com ,*c-subj-pred-or-verb-p-as-subject*
			   :tval ,*ctb-IP*
               :tag ,(tn
                      (let ((n-vals nil))
                        (when t2 (setf n-vals (cons2 'has-subj-pause t
                                                     n-vals)))
                        (more-trait-with (subj-verb-tag t1 t3) n-vals)))
               ;; discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
               :stag ,(tn (score-add
                           (subj-verb t1 t3)
                           (+ (not-prefer-CC-verb-mod t3)
                              (prefer t1 'a-subj-pred *c-close-suffix*)))))
    (subj-pred -> time-p may-pause subj-pred-or-verb-p pred
               ;; to balance the penalty of time as verb-mod, but
               ;; lower penalty here.
               :com ,(+ *c-time-place-subj-pred*
                        *c-subj-pred-or-verb-p-as-subject*
                        ;; if there is time or place prefix, prefer
                        ;; them to be absorbed into the subject
                        *c-close-sep*)
			   :tval ,*ctb-IP*
               :tag ,(tn
                      (let ((n-vals '(has-time t)))
                        (when t2 (setf n-vals (cons2 'has-time-pause t
                                                     n-vals)))
                        (more-trait-with (subj-verb-tag t3 t4) n-vals)))
               ;; discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
               :stag ,(tn (score-add
                           (subj-verb t3 t4)
                           (+ (not-prefer-CC-verb-mod t4)
                              (if (has-trait t4 'verb-mod-time)
                                  *c-common*
                                  0)
                              (prefer t3 'a-subj-pred *c-close-suffix*)))))
    (subj-pred -> time-p may-pause place-p-a may-pause subj-pred-or-verb-p pred
               ;; to balance the penalty of time and place as
               ;; verb-mod, but lower penalty here
               :com ,(+ (* 2 *c-time-place-subj-pred*)
                        *c-subj-pred-or-verb-p-as-subject*
                        ;; if there is time or place prefix, prefer
                        ;; them to be absorbed into the subject
                        *c-close-sep*)
			   :tval ,*ctb-IP*
               :tag ,(tn
                      (let ((n-vals '(has-time t
                                      has-place t)))
                        (when t2 (setf n-vals (cons2 'has-time-pause t
                                                     n-vals)))
                        (when t4 (setf n-vals (cons2 'has-place-pause t
                                                     n-vals)))
                        (more-trait-with (subj-verb-tag t5 t6) n-vals)))
               ;; discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
               :stag ,(tn (score-add
                           (subj-verb t5 t6)
                           (+ (not-prefer-CC-verb-mod t6)
                              (if (has-trait t6 'verb-mod-time)
                                  *c-common*
                                  0)
                              (prefer t5 'a-subj-pred *c-close-suffix*)))))
	;;
	,@(allow-quote 'subj-pred)
    ;;
    (sentences => sentence)
    (sentences -> sentence sentences
			   :tval ,*ctb-splice*) ;; TODO: check tval
    ;;
    ,@(allow-quote-splice 'sentences 2)
    ;;
    (word => sentences :com ,*c-sentences-alone*)
    ;;
    (sentence -> clauses end
			  :tval ,(tfn (if (headed-by x1 'IP)
                              (append x1 (list x2))
                              (as-lst-head (cdr args) 'IP))))
    ;;
    ,@(allow-quote-splice 'sentence)
    ;;
    (word => sentence :com ,*c-sentence-alone*)
    ;;
    (clauses => clause-c)
    (clauses -> clause-a prefer-long-pause clauses
             :com ,*c-common*
			 :tval ,*ctb-splice*)
    (clauses -> clause-b may-pause clauses :com ,*c-common*
			 :tval ,*ctb-splice*)
    ;;
    ,@(allow-quote 'clauses 13)
    ;;
    (word => clauses :com ,*c-clauses-alone*
          :tval ,*ctb-as-IP*)
    ;;
	,@(strs 'clause-supp '("的" "嗎" "吧" "麼"
						   "否" "未" "乎")
            '->
            :tval *pos-SP*)
    ,@(strs 'clause-supp '("的話" "而已" "罷了" "也好" "也罷"
						   "沒有" "不曾" "未曾" "來著" "也")
            '->
            :com *c-prep*
            :tval *pos-SP*)
    (clause-supp :> exclaim)
	;;
    ,@(allow-quote 'clause-a0 10)
    ;;
    (clause-a0 -> subj-pred subj-pred
               ;; NOTE: special rule, no pause between the two
               ;; subj-pred, which could mean two separate actions, or
               ;; the first is the topic of the second.  e.g. '我叫一
               ;; 声『雾』 它敢答应么'
               :com ,*c-rare*
               :stag ,(tn (+ (not-prefer-trait t1 'has-time-pause *c-common*)
                             (not-prefer-trait t1 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t1 'has-subj-pause *c-common*)
                             (not-prefer-trait t2 'has-time-pause *c-common*)
                             (not-prefer-trait t2 'has-place-pause *c-close-suffix*)
                             (not-prefer-trait t2 'has-subj-pause *c-common*)
                             (not-prefer-subj-pred-body-seps t1 *c2*)
                             (not-prefer-subj-pred-body-seps t2 *c-common*)
                             ;; often some sentences are broken into two subj-pred with this, so need some constraints for unlikely cases
                             (if (a-subj-pred-p t1)
                                 ;; penalize non-v-V used in verb0
                                 ;; e.g. '(中国 年逾) (百万人 死于空气污染)'
                                 (min-over-conn
                                  (a-subj-pred-pred t1)
                                  #'(lambda (v)
                                      (if (and (typep v 'verb0)
                                               (not (typep (verb0-verb v) 'v-V)))
                                          *c2*
                                          0)))
                                 0)))
               :tag ,(tn (make-conn-thing :first t1
                                          :second t2
                                          :sep nil)))
    ;; originally (clause-a0 => pred), but replicate the pred rules
    ;; here, because the preference at clause level is different from
    ;; that at pred level. At clause level, prefer subj-pred to verb-p.
    (clause-a0 => adj-p :com ,*c-adj-p-as-pred*
               :stag ,*clause-a0-stag*)
    (clause-a0 =1> adj-p sp-de :com ,*c-adj-p-as-pred*
               :stag ,*clause-a0-stag*
               :tval ,*ctb-CP*
               :tag ,(tn (make-adj-has-de :the-adj t1)))
    (clause-a0 =3> time-p may-pause adj-p
               :com ,(+ *c-adj-p-as-pred* *c-close-suffix*)
               :stag ,*clause-a0-stag*
               :tag ,(tn (more-trait t3 'verb-mod-time t))
               :tval ,*ctb-splice*)
    (clause-a0 -> time-p may-pause adj-p sp-de
               :com ,(+ *c-adj-p-as-pred* *c-close-suffix*)
               :stag ,*clause-a0-stag*
               :tval ,*ctb-CP*
               :tag ,(tn (more-trait-to
                          (make-adj-has-de :the-adj t3)
                          'verb-mod-time t)))
    (clause-a0 => verb-p :com ,*c2*
               :stag ,*clause-a0-stag*)
    (clause-a0 => subj-pred
               :stag ,(tn (+ (clause-a0-stag-penalty t1)
                             ;; sometimes want the clauses to be nested inside
                             (if (a-subj-pred-p t1)
                                 (conn-not-prefer-seps (a-subj-pred-pred t1)
                                                       *c-close-sep*
                                                       '(:comma :semi-colon))
                                 0))))
    (clause-a0 => obj-pred :com ,*c-obj-pred-as-pred*
               :stag ,*clause-a0-stag*)
    ;; TODO: to disallow noun-p as clause?
    (clause-a0 => noun-p :com ,*c-noun-p-as-clause*
			   :stag ,(tn (not-prefer t1 'de-noun-mod *c-close-suffix*)))
	;; some clauses may be a list, with numbers.
	;; may consider checking the monotonicity of the numbers
    ;;
    ,@(allow-quote 'clause-a1 11)
    ;;
	(clause-a1 => clause-a0)
	(clause-a1 =2> list-number pause clause-a0
			   :com ,*c-close-sep*
			   :tval ,*ctb-IP*)
	;;
	(item-number => t-number
				 :com ,*c-close-sep*
				 :tval ,*pos-CD*)
	(item-number => abstract-a
				 :stag ,(tn (must-be t1 'counting-name))
				 ;; 甲, 乙, 丙, 丁, ...
				 :tval ,*ctb-NP*)
	(item-number :> A-Z
				 ;; e.g. for (LST (FW "B"))
				 :tval ,*pos-FW*)
	(item-number -> A-Z "-" t-number
				 ;; e.g. for (LST (FW "G") (PU "-") (CD "2"))
				 :com ,*c-close-sep*
				 :tval ,(tfn (list :splice
								   x1
								   (tag-as 'PU x2)
								   (tag-as 'CD x3))))
	(item-number =2> "其" int
				 :tval ,(tfn `(NP ,(tag-as 'NN args))))
	(item-number =1> int "則"
				 :tval ,*pos-CD*)
	(item-number =2> noun-b int
                 ;; NOTE: replaced noun with noun-b
				 ;; e.g.  in CTB, (LST (QP (NP (NN "焦点")) (QP (CD "三"))))
                 :stag ,(tn (prefer t2 'integer *c-fallback*))
				 :com ,*c-close-suffix*
				 :tval ,(tfn `(QP ,(as-head x1 'NP) (QP ,(tag-as 'CD x2)))))
	(item-number => num-unit-a
				 ;; e.g. in CTB, (LST (QP (OD "第二") (CLP (M "个"))))
				 ;; e.g. (LST (QP (CD "一") (CLP (M "个"))))
				 ;; e.g. (LST (QP (OD "第二") (CLP (M "点"))))
				 :stag ,(tn (max (prefer t1 'ge4 *c-rare*)
								 (prefer t1 'dian3 *c-rare*))))
	;; some strange symbols as list marker
	,@(strs 'list-PU '("——" "." "＞" "＊" "■" "●" "｛") '->
			:tval *pos-PU*)
	(list-number :> list-PU
				 :tval ,*ctb-LST*)
	(list-number => item-number
				 :tval ,*ctb-LST*)
	(list-number => ordinal
				 :tval ,*ctb-LST*)
	(list-number =2> L-paren item-number R-paren
				 :tval ,*ctb-LST*)
	(list-number =2> L-bracket item-number R-bracket
				 :tval ,*ctb-LST*)
	(list-number =2> list-PU item-number
				 ;; e.g. in CTB, (LST (PU "＞") (CD "２"))
				 :com ,*c-close-sep*
				 :tval ,*ctb-LST*)
	;;
	(clause-a => clause-a1)
	(clause-a =1> clause-a1 clause-supp :com ,*c-close-sep*
              :tag ,(tn (more-trait t1 'has-clause-supp t))
			  :tval ,(tfn `(:splice ,(as-head x1 'IP) ,x2)))
    ;;
    ,@(allow-quote 'clause-a 12)
    ;;
    (clause-front => front-adv
                  :tval ,*ctb-ADVP*)
    (clause-front => verb-mod-front
                  ;; especially lenient to place-p with ind as clause-front
                  :com ,*c-place-as-clause-front*
                  :stag ,(tn (prefer t1 'place
                                     (- *c-verb-mod-front-as-clause-front*
                                        *c-place-as-clause-front*))))
    (clause-front => exclaim
                  :tval ,*pos-IJ*)
    (clause-front -> "R" "e"
                  :tval ,*pos-P*)
    ;; subj-pred can be a clause through pred
    ;;(clause-a :> subj-pred :com ,*c-subj-pred-as-clause*)
    ;; allow multiple clause-front
    (clause-a00 =3> clause-front may-pause subj-pred :com ,*c-front-adv-subj-pred-as-clause*
                :tag ,*clause-a00-tag*
                :stag ,*clause-a00-stag*
                :tval ,*ctb-splice*)
    (clause-a00 =3> clause-front may-pause clause-a00 :com ,*c-front-adv-subj-pred-as-clause*
               :tag ,*clause-a00-tag*
               :stag ,*clause-a00-stag*
               :tval ,*ctb-splice*)
    (clause-a0 => clause-a00)
    ;; distinguish between clause that are bracketed by quotes, so may
    ;; not have further separator
    (quoted => sentences)
    ;; also, the things inside quotes may not be complete sentences
    (quoted => clauses
            :tval ,#'x1)
    (quoted =1> clauses pause
            :tval ,*ctb-splice*)
    ;; character level variations are handled in *cn-zh-char-pairs-list*
	;; simply use a symbol as the tag
	(L-paren :> "（" :tag L-paren :tval ,*pos-PU*)
	(R-paren :> "）" :tag R-paren :tval ,*pos-PU*)
    ,@(strs 'L-da-bracket '("《" "<" "<<") '->
            :tag 'L-da-bracket :tval *pos-PU*)
    ,@(strs 'R-da-bracket '("》" ">" ">>") '->
            :tag 'r-da-bracket :tval *pos-PU*)
	(L-bracket :> "［" :tag L-bracket :tval ,*pos-PU*)
	(R-bracket :> "］" :tag R-bracket :tval ,*pos-PU*)
	(L-quote :> "“" :tag L-quote :tval ,*pos-PU*)
	(R-quote :> "”" :tag R-quote :tval ,*pos-PU*)
	(double-quote :> "\"" :tag double-quote :tval ,*pos-PU*)
	(single-quote :> "'" :tag single-quote :tval ,*pos-PU*)
	(back-quote :> "`" :tag back-quote :tval ,*pos-PU*)
	(L-c-quote :> "「" :tag L-c-quote :tval ,*pos-PU*)
	(R-c-quote :> "」" :tag R-c-quote :tval ,*pos-PU*)
    (one-space :> " " :tag one-space :tval ,*pos-PU*)
    ;;
	(dash :> "—" :tag single-dash :tval ,*pos-PU*)
	(dash -> "——" :tag double-dash :tval ,*pos-PU*)
	;;
	(open-quote => L-paren)
	(open-quote => L-da-bracket)
	(open-quote => L-bracket)
	(open-quote => L-quote)
	(open-quote => double-quote)
	(open-quote => single-quote)
	(open-quote => back-quote)
	(open-quote => L-c-quote)
	(open-quote =1> one-space)
	;;
	(close-quote => R-paren)
	(close-quote => R-da-bracket)
	(close-quote => R-bracket)
	(close-quote => R-quote)
	(close-quote => double-quote)
	(close-quote => single-quote)
	(close-quote => R-c-quote)
	(close-quote =1> one-space)
	;; TODO: whether these quoted should be PRN or spliced?
    (clause-b =2> L-quote quoted R-quote
			  :tval ,*ctb-splice*)
    (clause-b =2> double-quote quoted double-quote
			  :tval ,*ctb-splice*)
    (clause-b =2> single-quote quoted single-quote
			  :tval ,*ctb-splice*)
    (clause-b =2> back-quote quoted single-quote
			  :tval ,*ctb-splice*)
    (clause-b =2> L-c-quote quoted R-c-quote
			  :tval ,*ctb-splice*)
    ;;
    (clause-c => clause-a)
    (clause-c => clause-b)
    ;;
    (clause => clause-c)
    (clause =1> clause-c sep
			:tval ,*ctb-splice*)
    ;;
    ,@(allow-quote 'clause 13)
    ;;
    (word => clause :com ,*c-clause-alone*
          :tval ,*ctb-as-IP*)
    ;;
    ;;; The following are originally pred, but for flexibility of
    ;;; conn, put them under verb-p, and in conjunction of the rule
    ;;; 'pred => verb-p', there is just some extra levels of
    ;;; indirection.
    ;; TODO: add *one-pred-not-prefer-conn* to stag as needed to encourage flatter conn-thing
    ,@(strs 'both-a '("一來" "二來")
            '->
            :tval (tfn `(LST ,(tag-as 'CD args))))
    ,@(strs 'both-a '("也" "又" "既")
            '->
            :tval *pos-CC*)
    ,@(strs 'both-a '("一邊" "一面" "一方面")
            '->
            :tval *pos-ADVP-AD*)
	;;
	(both => both-a)
	(both -> both-a pause :com ,*c-close-sep*
		  :tval ,*ctb-splice*)
	;;
    (pred-v -> both clause both clause-c :com ,*c-two-prep*
            :tval ,*ctb-splice*
            :tag ,*make-conn-t2-t4*)
    ;;
    ,@(strs 'first '("先" "首先") '->
            :tval *pos-ADVP-AD*)
    ,@(strs 'and-then '("接着" "然後" "於是" "於是乎") '->
            :tval *pos-ADVP-AD*)
    ;;(pred -> first may-pause clause-c) ;; first can be more flexible as adv
    (pred-v =3> and-then may-pause clause-c  :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t3*
            :tval ,*ctb-splice*)
    (pred-v -> first clause and-then clause-c :com ,*c-two-prep*
            :tval ,*ctb-splice*
            :tag ,*make-conn-t2-t4*)
    ;;
    ,@(strs 'not-only '("不但" "尚且" "不僅" "非但") '->
            :tval *pos-CC*)
    ,@(strs 'but-also '("而且" "并且" "並且" "且")
            '->
            ;; NOTE: '并' is also a verb, so may have ambiguity. '并' is put in AD.
            :tval *pos-CC*)
    ,@(strs 'but-also '("反而" "况且" "何况" "更不必說" "更何况"
                        "反" "還要")
            ;; removed "更是", compose it from '更' and '是'
            '->
            :tval *pos-ADVP-AD*)
    (pred-v =2> not-only clause-c :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t2*
            :tval ,*ctb-splice*)
    (pred-v =3> but-also may-pause clause-c :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t3*
            :tval ,*ctb-splice*)
    (pred-v -> not-only clause but-also clause-c
            :tval ,*ctb-splice*
            :com ,*c-two-prep*
            :tag ,*make-conn-t2-t4*)
    ;;
    ,@(strs 'either '("或" "或者" "是" "還是" "要麼")
            '->
            :tval *pos-CC*)
    (t-either :> either)
    (t-either -> either "說"
              :tval ,*pos-CC*)
    (pred-v -> t-either clause t-either clause-c
            :tval ,*ctb-splice*
            :com ,*c-two-prep*
            :tag ,*make-conn-t2-t4*)
    (either-this -> "不" "是" :tval ,*pos-CC*)
    (or-that -> "就" "是" :tval ,*pos-CC*)
    (pred-v -> either-this clause or-that clause-c
            :tval ,*ctb-splice*
            :com ,*c-two-prep*
            :tag ,*make-conn-t2-t4*)
    (if-not-this :> "不" :tval ,*pos-ADVP-AD*)
    (then-must-be :> "就" :tval ,*pos-ADVP-AD*)
    (pred-v -> if-not-this clause then-must-be clause-c :com ,*c-two-prep*
            ;; TODO: check tval
            :tval ,*ctb-splice*
            :tag ,*make-conn-t2-t4*)
    ;;
    ,@(strs 'because '("因為" "由於" "因")
            '->
            :tval *pos-P*
            :com *c-prep*
            :tag :because)
	;;
    ,@(strs 'therefore '("所以" "以致" "故" "以至於") '->
            :tval *pos-ADVP-AD*)
    (t-therefore => therefore)
    (t-therefore => therefore "說" :com ,*c-close-sep*
                 :tval ,*pos-ADVP-AD*)
    (pred-v =3> because may-pause clause-c
            :stag ,*one-pred-not-prefer-conn-t3*
            :tval ,*ctb-PP*
            :com ,*c-one-prep*)
    (pred-v =3> t-therefore may-pause clause-c
            :tval ,*ctb-IP*
            :com ,*c-one-prep*)
    (pred-v =4> because clause t-therefore clause-c
            :tval ,(tfn `(:splice (PP ,x1 ,x2) ,x3 ,x4))
            :com ,*c-two-prep*)
    ;;
    ,@(strs 'since '("既然") '->
            :tval *pos-ADVP-CS*)
    ,@(strs 'then '("就" "還" "那麼就") '->
            :tval *pos-ADVP-AD*)
    (pred-v =2> since clause-c :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t2*
            :tval ,*ctb-CP*)
    (pred-v =4> since clause then clause-c
            :tval ,(tfn `(:splice (CP ,x1 ,x2) ,x3 ,x4))
            :com ,*c-two-prep*)
    ;;
    ,@(strs 'if '("要是" "如果" "如" "若" "若果" "若是" "如若"
                  "假如" "假使" "只要" "萬一" "倘若"
                  "一旦" "若是")
            '->
            :tval *pos-ADVP-CS*)
    ,@(strs 'if '("要")
            '->
            :com *c-prep*
            :tval *pos-ADVP-CS*)
    (t-if => if)
    (t-if => if "說" :tval ,*pos-ADVP-CS*)
    ;;
    (pred-v =2> t-if clause-c :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t2*
            :tval ,*ctb-CP*)
    (pred-v =4> t-if clause then clause-c :com ,*c-two-prep*
            :tval ,(tfn `(:splice (CP ,x1 ,x2) ,x3 ,x4)))
    ;;
    (instead-of -> "與" "其" :tval ,*pos-CC*)
    (how-about -> "不" "如" :tval ,*pos-CC*)
    ;;
    (pred-v -> instead-of clause how-about clause-c
            :tval ,*ctb-VP*
            :com ,*c-two-prep*
            :tag ,*make-conn-t2-t4*)
    ;;
    (only-if -> "只" "有" :tval ,*pos-ADVP-CS*)
    (can-it :> "才" :tval ,*pos-ADVP-AD*)
    (pred-v =4> only-if clause can-it clause-c
            :tval ,(tfn `(:splice (IP (VP ,x1 ,x2)) (VP ,x3 ,x4)))
            :com ,*c-two-prep*)
    ;;
    ,@(strs 'so-as '("以便" "以免" "免得" "省得") '->
            :tval *pos-ADVP-AD*)
    (pred-v =2> so-as clause-c :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t2*
            :tval ,*ctb-splice*)
    ;;
    ,@(str-tags 'but
                '((:but-no-pause
                   "可" "而" "卻" "然")
                  "但" "但是" "可是" "然而" "不過" "只是"
                  "只不過" "再說")
                '->
                :com *c-prep*
                :tval *pos-ADVP-AD*)
    (pred-v =3> but may-pause clause-c :com ,*c-one-prep*
            :stag ,(tn (+
                        ;; want the 'but' to be at outer scope, e.g. '而 ((被偷拍 的 女士) (则 毫无察觉))' vs '((而 被偷拍) 的 女士) (则 毫无察觉)', but this workaround may not work in other cases.
                        (prefer t3 'a-subj-pred *c-close-sep*)
                        (if (and t2 (eq :but-no-pause t1))
                            *c-rare*
                            0)))
            :tag ,(tn (more-trait t3 'verb-mod-adv-but t))
            :tval ,*ctb-splice*)
    ;;
    (not-that -> "不" "是" :tval ,*pos-CC*)
    (but-this -> "而" "是" :tval ,*pos-CC*)
    (pred-v -> not-that clause but-this clause-c
            :tval ,*ctb-splice*
            :com ,*c-two-prep*
            :tag ,*make-conn-t2-t4*)
    ;; 'although' here also includes 'no matter', 'even though', 'even if'
    ,@(strs 'although '("無論" "不管" "即使" "寧可" "儘管"
                        "雖然" "雖" "就算" "即便" "不論"
                        "哪怕" "那怕" "雖說" "雖然說" "就是")
            '->
            :tval *pos-ADVP-CS*)
    (t-although => although)
    (t-although -> although "是" :tval ,*pos-ADVP-CS*)
    ,@(strs 'also '("都" "總" "也") '->
            :tval *pos-ADVP-AD*)
    (pred-v =4> t-although clause but clause-c
            :tval ,(tfn `(:splice (CP ,x1 ,(as-head x2 'IP)) ,x3 ,x4))
            :com ,*c-two-prep*)
    (pred-v =4> t-although clause also clause-c
            :tval ,(tfn `(:splice (CP ,x1 ,(as-head x2 'IP)) ,x3 ,x4))
            :com ,*c-two-prep*)
    (pred-v =3> t-although clause clause-c
            ;; this is mainly for t3 being a subj-pred, or have something before the 'but' or 'also'
            :stag ,(tn
                    ;; NOTE: strongly t3 having 'also or 'but traits
                    (min-over-conn
                     t3
                     #'(lambda (v)
                         (let ((pv (if (a-subj-pred-p v)
                                       (a-subj-pred-pred v)
                                       v)))
                           (if (or (trait-value pv 'verb-mod-adv-but)
                                   (trait-value pv 'verb-mod-adv-also)
                                   (trait-value pv 'verb-mod-adv-still))
                               *c-close-sep*
                               *c-rare*)))))
            :tval ,(tfn `(:splice (CP ,x1 ,(as-head x2 'IP)) ,x3))
            :com ,*c-two-prep*)
    ;;
    ,@(strs 'otherwise '("否則" "不然" "要不然") '->
            :tval *pos-ADVP-AD*)
    ,@(strs 'unless '("除非" "只有" "惟有") '->
            :tval *pos-ADVP-CS*)
    (pred-v =4> t-if clause otherwise clause-c
            :tval ,(tfn `(:splice (CP ,x1 ,x2) ,x3 ,x4))
            :com ,*c-two-prep*)
    (pred-v =4> unless clause otherwise clause-c
            :tval ,(tfn `(:splice (CP ,x1 ,x2) ,x3 ,x4))
            :com ,*c-two-prep*)
    (pred-v =2> unless clause-c :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t2*
            :tval ,*ctb-CP*)
    ;;
    ,@(strs 'until '("直到") '->
            :tval *pos-P*)
    ,@(strs 'upto '("為止") '->
            :tval *pos-LC*)
    (pred-v =2> until clause-c :com ,*c-until-clause*
            :stag ,*one-pred-not-prefer-conn-t2*
            :tval ,*ctb-PP*)
    (pred-v =1> clause-c upto :com ,*c-clause-upto*
            :stag ,(tn (not-prefer t1 'conn-thing *c-close-suffix*))
            :tval ,*ctb-LCP*)
    (pred-v =2> until clause-c upto :com ,*c-until-clause-upto*
            :tval ,(tfn `(PP ,x1 (LCP ,x2 ,x3))))
    ;;
    ,@(strs 'unexpectedly '("誰知" "誰不知" "不料") '->
            :tval *pos-ADVP-AD*)
    (pred-v =2> unexpectedly clause-c :com ,*c-one-prep*
            :stag ,*one-pred-not-prefer-conn-t2*
            :tval ,*ctb-splice*)
    ;;
    ,@(strs 'incidentally '("說來也巧" "也巧") '->
            :tval *pos-ADVP-AD*)
    (pred-v =3> incidentally may-pause clause-c
            :stag ,*one-pred-not-prefer-conn-t3*
            :tval ,*ctb-splice*
            :com ,*c-one-prep*)
    ;;
    ,@(strs 'even '("連" "就連") '->
            :tval *pos-ADVP-AD*)
    ;; 'also' is given above
    (pred-v =4> even clause also clause-c :com ,*c-two-prep*
            :tval ,*ctb-splice*)
    ;;;;
    ;; In CTB, there are quite a lot of FRAG, but the parsing guide
    ;; has little to say when to use FRAG, other than to 'Garbage
    ;; disposal'.
    ;; Here we capture a few common ones that are usually put at the end of a news article.
    (footer-dian4 :> "電"
                  :tval ,*pos-NN*)
    (footer-reporting -> noun-b place time footer-dian4
                      ;; NOTE: replaced abstract with noun-b
                      :tval ,*ctb-splice*
                      :stag ,(tn (prefer t1 'newspaper-like)))
    (footer-reporter -> L-paren noun-p R-paren
                     :tval ,*ctb-splice*
                     ;; the animate-p may have more than one
                     ;; reporters, linked by conn
                     :stag ,(tn (prefer-conn t2 'human-name *c-fallback*))
                     )

    (passage-footer -> footer-reporting
                    :tval ,*ctb-frag*)
    (passage-footer -> footer-reporting footer-reporter
                    :tval ,*ctb-frag*)
    (passage-footer -> L-paren "完" R-paren
                    :tval ,(tfn `(FRAG ,x1 ,(tag-as 'VV x2) ,x3)))
    
    (word :> passage-footer :com ,*c-passage-footer-alone*)
    ))
;; end sample grammar

;;;
(defun prepare-grammar (g)
  (let ((int-g (internalize-grammar g)))
    (convert-single-str int-g)
    int-g))
(defparameter *chinese-g* (prepare-grammar *chinese-grammar2*))

(defun random-pick (ls)
  (nth (random (length ls)) ls))
;;(defun random-sentence (s)
;;  (if (non-terminal-p s)
;;      (let ((r (random-pick (non-terminal-rules s))))
;;	(cons (non-terminal-name s)
;;	      (mapcar #'random-sentence (rule-body r))))
;;      s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
