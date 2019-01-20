(in-package :chinese-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add the time-scale traits to *times-hash* and *dynasties-hash* here
;; to reduce copying
(def-priority
  ;; for time scales
  (*time-scale-century* -1)
  (*time-scale-dynasty* -1)
  (*time-scale-generation* -1)
  (*time-scale-decade* -1)
  (*time-scale-year* -1)
  (*time-scale-season* -1)
  (*time-scale-month* -1)
  (*time-scale-week* -1)
  (*time-scale-day* -1)
  (*time-scale-within-day* -1)
  (*time-scale-hour* -1)
  (*time-scale-minute* -1)
  (*time-scale-second* -1)
  )
(defun time-scale-of (n)
  (typecase n
    (time-second *time-scale-second*)
    (time-minute *time-scale-minute*)
    (time-hour *time-scale-hour*)
    (time-within-day *time-scale-within-day*)
    (time-day *time-scale-day*)
    (time-week *time-scale-week*)
    (time-month *time-scale-month*)
    (time-season *time-scale-season*)
    (time-year *time-scale-year*)
    (time-decade *time-scale-decade*)
    (time-generation *time-scale-generation*)
    (dynasty *time-scale-dynasty*)
    (time-century *time-scale-century*)
    (t nil)
    ))
(defun add-time-scale-traits-to-tag (tag)
  (let ((time-scale (time-scale-of tag)))
    (when time-scale
      (more-trait-to tag
                     'time-scale-max time-scale
                     'time-scale-min time-scale))
    tag))
(defun add-time-scale-traits-to-hash (hash)
  (for-word-list-hash
   hash
   #'(lambda (pair)
       ;; pair is (word . class-instance)
       (add-time-scale-traits-to-tag (cdr pair)))))

(add-time-scale-traits-to-hash *times-hash*)
(add-time-scale-traits-to-hash *dynasties-hash*)

;;;
(defun time-scale-max-of (s1 s2)
  ;; ignore nil
  (cond ((null s1) s2)
        ((null s2) s1)
        (t (max s1 s2))))
(defun time-scale-min-of (s1 s2)
  ;; ignore nil
  (cond ((null s1) s2)
        ((null s2) s1)
        (t (min s1 s2))))

(defun combine-time-scale-tag (t1 t2)
  ;; take the union of the two ranges of time scales
  ;; works even if either one has no time scale
  (more-trait
   t2
   'time-scale-max (time-scale-max-of
                    (trait-value t1 'time-scale-max)
                    (trait-value t2 'time-scale-max))
   'time-scale-min (time-scale-min-of
                    (trait-value t1 'time-scale-min)
                    (trait-value t2 'time-scale-min))))

(defun sum-count-combine-time-scale-tag (t1 t2)
  ;; take the union of the two ranges of time scales
  ;; works even if either one has no time scale.
  ;; Also sum the base nouns.
  (more-trait
   t2
   'time-scale-max (time-scale-max-of
                    (trait-value t1 'time-scale-max)
                    (trait-value t2 'time-scale-max))
   'time-scale-min (time-scale-min-of
                    (trait-value t1 'time-scale-min)
                    (trait-value t2 'time-scale-min))
   'n-base-nouns (+ (trait-value t1 'n-base-nouns 1)
                    (trait-value t2 'n-base-nouns 1))
   ))

(defun make-time-conn (t1 t2 sep)
  ;; NOTE: also added n-base-nouns
  (more-trait-to
   (make-conn-thing :first t1 :second t2 :sep sep)
   'time-scale-max (time-scale-max-of
                    (trait-value t1 'time-scale-max)
                    (trait-value t2 'time-scale-max))
   'time-scale-min (time-scale-min-of
                    (trait-value t1 'time-scale-min)
                    (trait-value t2 'time-scale-min))
   'n-base-nouns (+ (trait-value t1 'n-base-nouns 1)
                    (trait-value t2 'n-base-nouns 1))
   ))

(defparameter *make-time-conn-t1-t3*
  (tn (make-time-conn t1 t3 t2)))

(defun time-pt-scale (scale)
  (more-trait-to
   (make-instance 'time-pt)
   'time-scale-max scale
   'time-scale-min scale))

(defparameter *tag-time-scale-decade*
  (time-pt-scale *time-scale-decade*))
(defparameter *tag-time-scale-month*
  (time-pt-scale *time-scale-month*))
(defparameter *tag-time-scale-day*
  (time-pt-scale *time-scale-day*))
(defparameter *tag-time-scale-hour*
  (time-pt-scale *time-scale-hour*))
(defparameter *tag-time-scale-minute-second*
  (more-trait-to
   (make-instance 'time-pt)
   'time-scale-max *time-scale-minute*
   'time-scale-min *time-scale-second*))
(defparameter *tag-time-scale-hour-minute*
  (more-trait-to
   (make-instance 'time-pt)
   'time-scale-max *time-scale-hour*
   'time-scale-min *time-scale-minute*))
(defparameter *tag-time-scale-month-day*
  (more-trait-to
   (make-instance 'time-pt)
   'time-scale-max *time-scale-month*
   'time-scale-min *time-scale-day*))

(defparameter *time-name-tag*
  (make-instance 'time-name))

;; need to distinguish more finely between (time-from, time-at-to), (time-F-B, time-at-around), (time-starting-upto)

(defparameter *time-tag-with-FB-around-ind*
  (tn (more-trait t1 'time-with-FB-around-ind t)))
(defparameter *time-name-tag-with-FB-around-ind*
  (more-trait-to (make-instance 'time-name)
                 'time-with-ind t
                 'time-with-FB-around-ind t))
(defparameter *time-name-tag-with-phrase-FB-around-ind*
  ;; with event-p, verb-p, subj-pred or clause-c
  (more-trait-to (make-instance 'time-name)
                 'time-with-ind t
                 'time-with-phrase t
                 'time-with-FB-around-ind t))

(defparameter *time-name-tag-with-upto-ind*
  (more-trait-to (make-instance 'time-name)
                 'time-with-ind t
                 'time-with-upto-ind t))
(defparameter *time-name-tag-with-phrase-upto-ind*
  ;; with event-p, verb-p, subj-pred or clause-c
  (more-trait-to (make-instance 'time-name)
                 'time-with-ind t
                 'time-with-phrase t
                 'time-with-upto-ind t))

(defparameter *time-name-tag-with-at-ind*
  ;; also used for time-from
  (more-trait-to (make-instance 'time-name)
                 'time-with-ind t
                 'time-with-at-ind t))
(defparameter *time-name-tag-with-phrase-at-ind*
  ;; also used for time-from
  ;; with event-p, verb-p, subj-pred or clause-c
  (more-trait-to (make-instance 'time-name)
                 'time-with-ind t
                 'time-with-phrase t
                 'time-with-at-ind t))

;;;

(defun similar-time-scale (t1 t2 &optional (penalty *c2*) (light-penalty *c2*))
  ;; sometimes t1 may have smaller time, e.g. '周日26日零时 至 周三28日', penalize it by light-penalty
  (score-add
   (if (not (eql (trait-value t1 'time-scale-max)
                 (trait-value t2 'time-scale-max)))
       penalty
       0)
   (let ((t1-time-min (trait-value t1 'time-scale-min))
         (t2-time-min (trait-value t2 'time-scale-min)))
     (cond ((eql t1-time-min t2-time-min) 0)
           ((and t1-time-min t2-time-min (< t1-time-min t2-time-min))
            light-penalty)
           (t penalty)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to score combinations of word with different tags.  Any
;; normal function could be used, but we mostly use generic functions,
;; to leverage the inheritance between different kinds of nouns.
(defmacro def-comb (rel-name &body comb-cost-lists)
  ;; one combination as ((arg1-type arg2-type ...) body ...)
  ;; where each arg-type could be a symbol as the type, or (name type) or (name (eql val))
  `(progn
     ,@(mapcar #'(lambda (x)
                   (let ((ns nil)
                         (arg-ls nil))
                     (dolist (y (car x))
                       (cond ((consp y)
                              (push y arg-ls))
                             (t (let ((n (gensym)))
                                  (push n ns)
                                  (push (list n y) arg-ls)))))
                     `(defmethod ,rel-name ,(nreverse arg-ls)
                        (declare (ignorable ,@ns))
                        ,@(cdr x))))
               comb-cost-lists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-type-p (val type)
  (cond ((typep val type) t)
        ((typep val 'verb0)
         (is-type-p (verb0-verb val) type))
        ((typep val 'verb-n)
         (is-type-p (verb-n-verb val) type))
        ((combine-verb-p val)
         (or (is-type-p (combine-verb-first val) type)
             (is-type-p (combine-verb-second val) type)))))

(defun any-type-p (val types)
  (some #'(lambda (x) (typep val x)) types))

(declaim (inline prefer-verb not-prefer-verb prefer-mono
                 prefer-less-base-noun not-prefer-etc
                 prefer not-prefer conn-not-prefer-seps
                 not-prefer-trait
                 must-be conn-must-be))
(defun prefer-verb (val type &optional (penalty *c-very-rare*))
  (if (is-type-p val type) *c-prefer* penalty))

(defun not-prefer-verb (val type &optional (penalty *c-very-rare*))
  (if (is-type-p val type) penalty *c-prefer*))

(defun prefer-mono (L &optional (penalty *c-very-rare*))
  (if (= L 1)
      0
      penalty))

(defun prefer-less-base-noun (n)
  (* (- (trait-value n 'n-base-nouns 1) 1)
     (- +sub-scale+)))
(defun not-prefer-etc (n &optional (penalty *c-close-suffix*))
  (if (trait-value n 'has-etc)
      penalty
      0))

(defun not-prefer-trait (n trait-name &optional (penalty *c-close-suffix*))
  (if (trait-value n trait-name)
      penalty
      0))

(defun prefer (val type &optional (penalty *c-very-rare*))
  (if (typep val type) *c-prefer* penalty))

(defun not-prefer (val type &optional (penalty *c-very-rare*))
  (if (typep val type) penalty *c-prefer*))

(defun prefer-conn (val type &optional (penalty *c-very-rare*))
  (cond ((typep val type) *c-prefer*)
        ((conn-thing-p val)
         (score-min
          (prefer-conn (conn-thing-first val) 
                       type
                       penalty)
          (prefer-conn (conn-thing-second val) 
                       type
                       penalty)))
        (t penalty)))

(defun prefer-conn-any (val types &optional (penalty *c-very-rare*))
  (cond ((any-type-p val types) *c-prefer*)
        ((conn-thing-p val)
         (score-min
          (prefer-conn-any (conn-thing-first val) 
                           types
                           penalty)
          (prefer-conn-any (conn-thing-second val) 
                           types
                           penalty)))
        (t penalty)))

(defun min-over-conn (val fun)
  ;; (fun x) should give the score for x
  (cond ((conn-thing-p val)
         (score-min
          (min-over-conn (conn-thing-first val) 
                         fun)
          (min-over-conn (conn-thing-second val) 
                         fun)))
        (t (funcall fun val))))

(defun min-over-also-conn (val fun)
  ;; (fun x) should give the score for x
  ;; convenient for penalizing pause in conn, and traits
  (cond ((conn-thing-p val)
         (score-min
          (funcall fun val)
          (score-min
           (min-over-conn (conn-thing-first val) 
                          fun)
           (min-over-conn (conn-thing-second val) 
                          fun))))
        (t (funcall fun val))))

(defun not-prefer-body-pause-seps (val)
  ;; recursively go into conn, body of subj-pred
  (min-over-also-conn
   val
   #'(lambda (v)
       (+ (not-prefer-trait v 'has-time-pause *c-common*)
          (not-prefer-trait v 'has-place-pause *c-common*)
          (not-prefer-trait v 'has-subj-pause *c-common*)
          (not-prefer-trait v 'verb-mod-has-pause *c-common*)
          (conn-not-prefer-seps
           v
           ;; increased penalty from *c-close-suffix*
           *c2*
           '(:comma :semi-colon))
          (if (a-subj-pred-p v)
              (not-prefer-body-pause-seps (a-subj-pred-pred v))
              0)))))

(defun conn-must-be (val type)
  (prefer-conn val type nil))

(defun not-prefer-conn (val type &optional (penalty *c-very-rare*))
  (cond ((typep val type) penalty)
        ((conn-thing-p val)
         (score-min
          (not-prefer-conn (conn-thing-first val) 
                           type
                           penalty)
          (not-prefer-conn (conn-thing-second val) 
                           type
                           penalty)))
        (t *c-prefer*)))

(defun prefer-both-or-neither-conn (v1 v2 type &optional (penalty *c-very-rare*))
  (cond ((typep v1 type)
         ;; v1 is, v2 should also be
         (prefer-conn v2 type penalty))
        ;;
        ((conn-thing-p v1)
         ;; check only one level into conn-thing
         ;; supposingly they have been previously checked
         (score-min
          (prefer-both-or-neither-conn
           (conn-thing-first v1) v2
           type penalty)
          (prefer-both-or-neither-conn
           (conn-thing-second v1) v2
           type penalty)))
        ;; v1 is not, v2 should also not be
        (t (not-prefer-conn v2 type penalty))))

(defgeneric subtype-of-word (w n))
(def-comb subtype-of-word
  ;; TODO: may change to return the penalty, and modify
  ;; conn-subtype-of-p to use defun-conn instead.

  ;; word-superclass noun-subclass
  (((w t) (n t))
   ;; fallback
   (typep n (type-of w)))
  ;; some words are quite general, so we would be more lenient here.

  ;; '地', '地區' could mean any place
  ((land place) t)
  ((district place) t)

  ;; '市' also means market, so make a special case for it, so that city-name would be consider a subtype of '市'
  ((market-or-city city) t)
  ((market-or-city market-like) t)

  ;; many predicates could be '條件'
  ((word-condition adj) t)
  ((word-condition verb) t)
  ((word-condition (n abstract))
   (also-verb-p n))
  ;; 優勢 ...
  ((tendency-or-momentum adj) t)
  ((tendency-or-momentum verb) t)

  ;; 活动
  ((word-huo2-dong4 (n abstract))
   (also-verb-p n))
  ((word-huo2-dong4 economy-can-be-job) t)
  ((word-huo2-dong4 word-building-jian4-zhu2) t)
  ((word-huo2-dong4 production) t)
  )
;; end subtype-of-word       

(defun conn-subtype-of-p (c s)
  ;; whether each of thing in conn-thing c is a subtype of s
  ;; s is also a tag object
  (labels ((is-subtype (a)
             (cond ((conn-thing-p a)
                    (and (is-subtype (conn-thing-first a))
                         (is-subtype (conn-thing-second a))))
                   ;; TODO: should we be lenient to verbs,
                   ;; adjectives, etc?
                   (t (subtype-of-word s a)))))
    (if (conn-thing-p s)
        (and (conn-subtype-of-p c (conn-thing-first s))
             (conn-subtype-of-p c (conn-thing-second s)))
        (is-subtype c))))

(defun conn-not-prefer-seps (c &optional
                                 (penalty *c-common*)
                                 (seps '(:comma :colon :semi-colon)))
  (if (and (conn-thing-p c)
           (member (conn-thing-sep c) seps))
      penalty
      0))

(defun not-prefer-subj-pred-body-seps (s &optional
                                           (penalty *c-close-sep*)
                                           (seps '(:comma :semi-colon)))
  (if (a-subj-pred-p s)
      (conn-not-prefer-seps (a-subj-pred-pred s) penalty seps)
      0))

(defun not-prefer-subj-pred-pauses (s)
  (+
   (not-prefer-trait s 'has-time-pause *c-common*)
   (not-prefer-trait s 'has-place-pause *c-close-suffix*)
   (not-prefer-trait s 'has-subj-pause *c-common*)
   (not-prefer-trait s 'verb-mod-has-pause *c-common*)
   (if (a-subj-pred-p s)
       (not-prefer-trait (a-subj-pred-pred s) 'verb-mod-has-pause *c-common*)
       0)
   (not-prefer-subj-pred-body-seps s)))

(defun not-prefer-CC-verb-mod (val &optional (penalty *c-close-suffix*))
  ;; discourage 'N1 ((CC N2) VP)' and prefer '(N1 CC N2) VP'
  ;; in subj-pred and obj-pred
  (if (has-trait val :verb-mod-CC)
      penalty
      (typecase val
        (conn-thing
         (score-min (not-prefer-CC-verb-mod (conn-thing-first val) penalty)
                    (not-prefer-CC-verb-mod (conn-thing-second val) penalty)))
        (verb0
         (not-prefer-CC-verb-mod (verb0-verb val) penalty))
        (t *c-prefer*))))

(defun must-be (val type)
  (if (typep val type) *c-prefer* nil))

(defun maybe-ind (val penalty)
  (cond ((typep val 'ind) *c-noun-ind*)
        ((typep val 'compound) (+ penalty penalty))
        (t penalty)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-conn (name (a b) fallback-fun)
  ;; to define binary constraint functions that dispatch conn-thing
  `(defun ,name (,a ,b)
    (cond ((conn-thing-p ,b)
           (score-min (,name ,a (conn-thing-first ,b))
                      (,name ,a (conn-thing-second ,b))))
          ((conn-thing-p ,a)
           (score-min (,name (conn-thing-first ,a) ,b)
                      (,name (conn-thing-second ,a) ,b)))
          (t (,fallback-fun ,a ,b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-conn noun-unit (n u) noun-unit-x)
(defgeneric noun-unit-x (n u))
(def-comb noun-unit-x
  ((t t) *c-ignore*)
  ;;
  ((noun unit) *c-fallback*)
  ((de-noun unit) *c-rare*)

  ;; to prevent 'num (num unit)'
  ((number unit) nil)
  ((num unit) nil)
  ;; TODO: whether these physical measurement should be treated as standalone nouns?
  ;; some simple physical measurements
  ((inanimate unit-weight) *c-prefer*)
  ((animate unit-weight) *c-common*)
  ((system unit-weight) *c-close-suffix*) ;; 二十五万吨 XXX 系统
  ;;
  ((inanimate unit-length) *c-prefer*)
  ((animate unit-length) *c-common*)
  ;;
  ((inanimate unit-area) *c-prefer*)
  ((animate unit-area) *c-common*)
  ;;
  ((inanimate unit-volume) *c-prefer*)
  ((animate unit-volume) *c-common*)
  ;;
  ((currency unit-money) *c-prefer*)
  ((money-related unit-money) *c-common*)
  ((amount-of unit-money) *c-common*)
  ;; container
  ((inanimate unit-container) *c-prefer*)
  ((animate unit-container) *c-common*)
  ;;
  ;;((physical unit-liquid-container) *c-rare*)
  ;;((liquid unit-liquid-container) *c-common*)
  ;;((drink unit-liquid-container) *c-prefer*)
  ;; individual unit words
  ;; 把 ;; general, loose
  ((inanimate ba2) *c-common*) ;; loose
  ((long-thin ba2) *c-prefer*)
  ((liquid-part ba2) *c-prefer*)
  ((age ba2) *c-prefer*)
  ;; 班
  ((scheduled ban1) *c-prefer*)
  ((lesson-class ban1) *c-prefer*)
  ((job-shift ban1) *c-prefer*)
  ;; 半 ;; basically half of anything
  ((noun ban4-half) *c-prefer*)
  ;; 瓣
  ((plant-flakes ban4) *c-prefer*)
  ;; 幫
  ((animate bang1) *c-prefer*)
  ;; 倍
  ((noun bei4) *c-prefer*)
  ;; 本
  ((plant ben3) *c-prefer*)
  ((book-like ben3) *c-prefer*)
  ((drama ben3) *c-prefer*)
  ;; 筆
  ((written-character bi3) *c-prefer*)
  ((information bi3) *c-prefer*)
  ((money-related bi3) *c-prefer*)
  ;; 部
  ((machine bu4) *c-prefer*)
  ((creative-work bu4) *c-prefer*)
  ;; 步
  ((steps bu4-step) *c-prefer*)
  ;; 餐
  ((meal can1) *c-prefer*)
  ((edible can1) *c-common*)
  ;; 冊
  ((book-like ce4) *c-prefer*)
  ;; 層 ;; very general
  ((building ceng2) *c-prefer*)
  ((stair ceng2) *c-prefer*)
  ((concern-understanding ceng2) *c-prefer*)
  ((noun ceng2) *c-common*)
  ;; 場
  ((drama chang2) *c-prefer*)
  ((event chang2) *c-prefer*)
  ((fever chang2) *c-prefer*)
  ((effort chang2) *c-prefer*)
  ((lengthy-spoken chang2) *c-common*)
  ((dream chang2) *c-prefer*)
  ((ball-like chang2) *c-prefer*)
  ;; 乘
  ((horse-powered-wheel sheng4) *c-prefer*)
  ((horse sheng4) *c-prefer*)
  ;; 重 ;; very general
  ((noun chong2) *c-common*)
  ;; 齣 出
  ((drama chu1) *c-prefer*)
  ;; 串 ;; very general
  ((fruit chuan4) *c-prefer*)
  ((human chuan4) *c-prefer*)
  ((noun chuan4) *c-common*)
  ;; 床
  ((bedding chuang2) *c-prefer*)
  ;; 次
  ((event ci4) *c-prefer*)
  ;; 叢 簇 ;; general
  ((plant cong2) *c-prefer*)
  ((physical cong2) *c-common*)
  ;; 撮 ;; general
  ((liquid cuo1) *c-prefer*)
  ((hair-like cuo1) *c-prefer*)
  ((inanimate cuo1) *c-common*)
  ;; 沓
  ((paper-like da2) *c-prefer*)
  ((currency da2) *c-close-sep*)
  ;; 代
  ((human dai4) *c-prefer*)
  ((product dai4) *c-prefer*)
  ;; 刀
  ((paper-like dao1) *c-prefer*)
  ;; 道
  ((beam-like dao4) *c-prefer*)
  ((wall dao4) *c-prefer*)
  ((door-like dao4) *c-prefer*)
  ((order dao4) *c-prefer*)
  ((posed-question dao4) *c-prefer*)
  ((edible dao4) *c-prefer*)
  ((process dao4) *c-prefer*)
  ;; 滴
  ((liquid di1) *c-prefer*)
  ;; 點 ;; very general
  ((inanimate dian3) *c-common*)
  ((abstract dian3) *c-common*)
  ;; 疊 ;; general
  ((paper-like die2) *c-prefer*)
  ((music die2) *c-prefer*)
  ((inanimate die2) *c-common*)
  ;; 頂
  ((has-roof-like ding3) *c-prefer*)
  ;; 錠
  ((metal ding4) *c-prefer*)
  ;; 棟
  ((building dong4) *c-prefer*)
  ;; 堵
  ((wall du3) *c-prefer*)
  ;; 段 ;; general
  ((inanimate duan4) *c-prefer*)
  ((road duan4) *c-prefer*)
  ((music duan4) *c-prefer*)
  ((abstract duan4) *c-common*)
  ;; 堆
  ((inanimate dui1) *c-prefer*)
  ((human dui1) *c-prefer*)
  ((animate dui1) *c-common*)
  ((abstract dui1) *c-common*)
  ;; 隊
  ((human dui4) *c-prefer*)
  ;; 對 ;; general
  ((human dui4-pair) *c-prefer*)
  ((inanimate dui4-pair) *c-prefer*)
  ((pronoun dui4-pair) *c-common*)
  ;; 頓
  ((meal dun4) *c-prefer*)
  ((event dun4) *c-prefer*)
  ;; 朵
  ((flower duo3) *c-prefer*)
  ((flower-like duo3) *c-common*)
  ;; 垛
  ((wall duo3-wall) *c-prefer*)
  ;; 度 ;; very scattered, just make it general
  ((event du4) *c-prefer*)
  ((noun du4) *c-common*)
  ;; 發
  ((bullet-like fa1) *c-prefer*)
  ((event fa1) *c-prefer*)
  ;; 番 ;; general
  ((abstract fan1) *c-common*)
  ;; 方 ;; general
  ((inanimate fang1) *c-common*)
  ;; 分
  ((inanimate fen1) *c-prefer*)
  ((abstract fen1) *c-common*)
  ((book-like fen1) *c-prefer*)
  ;; 份
  ((inanimate fen1) *c-prefer*)
  ((abstract fen1) *c-common*)
  ((book-like fen1) *c-prefer*)
  ;; 封
  ((letter feng1) *c-prefer*)
  ;; 峰
  ((camel feng1-camel) *c-prefer*)
  ;; 幅
  ((picture fu2) *c-prefer*)
  ((scenery fu2) *c-prefer*)
  ((land fu2) *c-prefer*)
  ;; 副 ;; general
  ((inanimate fu4) *c-common*)
  ((attribute fu4) *c-common*)
  ;; 服
  ((medicine-oral fu2-medicine) *c-prefer*)
  ;; 杆
  ((stick-like gan1) *c-prefer*)
  ;; 個 ;; very general
  ((place ge4) *c-prefer*) ;; 154个 城市
  ((noun ge4) *c-close-sep*)
  ((evidence ge4) *c-long-sep*) ;; not as prefer '一个 数据'
  ((metal ge4) *c-common*) ;; not prefer (一個金) 蘋果
  ((policy ge4) *c-close-sep*) ;; want '(一个 中国) 政策' more than '一个 (中国 政策)'
  ;; 根
  ((long-thin gen1) *c-prefer*)
  ;; 股 ;; general
  ((long-thin gu3) *c-prefer*)
  ((fluid-like gu3) *c-prefer*)
  ((smell gu3) *c-prefer*)
  ((sound gu3) *c-prefer*)
  ((human gu3) *c-prefer*)
  ((abstract gu3) *c-common*)
  ;; 掛 ;; general
  ((cart gua4) *c-prefer*)
  ((fruit gua4) *c-prefer*)
  ((inanimate gua4) *c-common*)
  ;; 管
  ((stick-like guan3) *c-prefer*)
  ;; 行 ;; general
  ((written-character hang2) *c-prefer*)
  ((job hang2) *c-prefer*)
  ((human hang2) *c-prefer*)
  ((inanimate hang2) *c-common*)
  ;; 號 ;; general, whatever that can be numbered
  ((human hao4) *c-prefer*)
  ((inanimate hao4) *c-common*)
  ((abstract hao4) *c-common*)
  ;; 戶
  ((human hu4) *c-prefer*)
  ;; 回
  ((event hui2) *c-prefer*)
  ;; 伙, 夥
  ((human ho3) *c-prefer*)
  ;; 級 ;; very general, whatever that has levels
  ((stair ji2) *c-prefer*)
  ((wind ji2) *c-prefer*)
  ((natural-phenomena ji2) *c-common*)
  ((noun ji2) *c-common*)
  ;; 集
  ((drama ji2-b) *c-prefer*)
  ;; 劑
  ((medicine ji4) *c-prefer*)
  ;; 家
  ((shop-like jia1) *c-prefer*)
  ((human jia1) *c-prefer*)
  ;; 架
  ((machine jia4) *c-prefer*)
  ;; 間
  ((mostly-indoor jian1) *c-prefer*)
  ;; 件 ;; general, loosen it
  ((clothing jian4) *c-prefer*)
  ((inanimate jian4) *c-common*)
  ((abstract jian4) *c-common*)
  ;; 截 ;; general
  ((long-thin jie2) *c-prefer*)
  ((inanimate jie2) *c-common*)
  ;; 節
  ((lesson-class jie2-period) *c-prefer*)
  ((cart jie2-period) *c-prefer*)
  ((train-like jie2-period) *c-prefer*)
  ;; 屆
  ((arranged-event jie4) *c-prefer*)
  ((human jie4) *c-prefer*) ;; loose
  ((organization jie4) *c-prefer*)
  ;; 介
  ((human jie4-b) *c-prefer*)
  ((organization jie4-b) *c-common*) ;; typo for 屆?
  ;; 局
  ((competition ju2) *c-prefer*)
  ((chess-like-game ju2) *c-prefer*)
  ;; 具 ;; general
  ((corpse ju4) *c-prefer*)
  ((coffin ju4) *c-prefer*)
  ((inanimate ju4) *c-common*)
  ;; 句
  ((sentence ju4-sentence) *c-prefer*)
  ;; 卷 ;; loose
  ((paper-like juan4) *c-prefer*)
  ((document juan4) *c-prefer*)
  ;; 棵
  ((plant ke1) *c-prefer*)
  ;; 顆 ;; any relatively small and ball-like shape
  ((ball-like ke1-ball) *c-prefer*)
  ((inanimate ke1-ball) *c-common*) ;; loose
  ;; 客
  ((edible ke4) *c-prefer*)
  ;; 課
  ((lesson-class ke4-lesson) *c-prefer*)
  ;; 口 ;; very general and varied
  ((human kou3) *c-prefer*)
  ((coffin kou3) *c-prefer*)
  ((livestock kou3) *c-prefer*)
  ((large-container kou3) *c-prefer*)
  ((edible kou3) *c-prefer*)
  ((drink kou3) *c-prefer*)
  ((teeth kou3) *c-prefer*)
  ((spoken kou3) *c-prefer*)
  ;; 塊 ;; general
  ((inanimate kuai4) *c-common*)
  ((land kuai4) *c-prefer*)
  ((landscape kuai4) *c-common*)
  ((territory kuai4) *c-prefer*)
  ((material kuai4) *c-prefer*)
  ((plate-like kuai4) *c-prefer*)
  ((edible-part kuai4) *c-prefer*)
  ((currency kuai4) *c-prefer*)
  ((specimen kuai4) *c-prefer*)
  ((field kuai4) *c-prefer*)
  ((transportation kuai4) *c-common*)
  ((business kuai4) *c-common*)
  ;; 款
  ((abstract kuan3) *c-common*)
  ;; 捆 ;; general, just about anything tied together
  ((inanimate kun3) *c-common*)
  ;; 類 ;; very general, just about anything
  ((noun lei4) *c-common*)
  ;; 粒 ;; generla, just about any small ball-like shape
  ((ball-like li4) *c-prefer*)
  ;; 輛
  ((wheeled liang4) *c-prefer*)
  ;; 列 ;; general, anything that can be placed at a row
  ((written-character lie4) *c-prefer*)
  ((wheeled lie4) *c-prefer*)
  ((inanimate lie4) *c-common*)
  ;; 領
  ((clothing ling3) *c-prefer*)
  ((bedding ling3) *c-prefer*)
  ;; 綹
  ((hair-like liu3) *c-prefer*)
  ;; 縷
  ((hair-like lu3) *c-prefer*)
  ((fluid-like lu3) *c-prefer*)
  ((light lu3) *c-prefer*)
  ((sun-word lu3) *c-prefer*)
  ((planet lu3) *c-close-suffix*)
  ;; 摞 ;; general, the way things are packed
  ((inanimate luo2) *c-common*)
  ;; 枚 ;; general, loose
  ((ball-like mei2) *c-prefer*)
  ((small-plate-like mei2) *c-prefer*)
  ((bullet-like mei2) *c-prefer*)
  ((star-like mei2) *c-prefer*)
  ((human mei2) *c-common*)
  ;; 門 ;; varied
  ((skill men2) *c-prefer*)
  ((knowledge-domain men2) *c-prefer*)
  ((job men2) *c-prefer*)
  ((transaction men2) *c-prefer*)
  ((event men2) *c-prefer*)
  ((canon men2) *c-prefer*)
  ;; 面 ;; general, loose
  ((inanimate mian4) *c-common*)
  ((wall mian4) *c-prefer*)
  ((mirror mian4) *c-prefer*)
  ((flag mian4) *c-prefer*)
  ((plate-like mian4) *c-prefer*)
  ;; 名
  ((human ming2) *c-prefer*)
  ;; 排 ;; general, the way things are arranged
  ((physical pai2) *c-common*)
  ((army-like pai2) *c-prefer*)
  ;; 盤 ;; varied, anything that can be placed on a plate
  ((competition pan2) *c-prefer*)
  ((gamble pan2) *c-prefer*)
  ((chess-like-game pan2) *c-prefer*)
  ((inanimate pan2) *c-common*)
  ;; 泡
  ((liquid pao4) *c-prefer*)
  ((feces pao4) *c-prefer*)
  ;; 批 ;; very general, a collection of things
  ((noun pi1) *c-common*)
  ;; 匹
  ((horse-like pi3) *c-prefer*)
  ((cloth pi3) *c-prefer*)
  ;; 篇
  ((article-like pian1) *c-prefer*)
  ;; 片 ;; very generla, varied, loose
  ((inanimate pian4) *c-common*)
  ((scenery pian4) *c-prefer*)
  ((landscape pian4) *c-prefer*)
  ((sound pian4) *c-prefer*)
  ((abstract pian4) *c-common*)
  ;; 鋪
  ((clay-bed pu1) *c-prefer*)
  ;; 撇
  ((hair-like pie3) *c-prefer*)
  ;; 期
  ((book-like qi2) *c-prefer*)
  ((lesson qi2) *c-prefer*)
  ((human qi2) *c-common*)
  ;; 腔 ;; loose
  ((abstract qiang1) *c-common*)
  ;; 圈 ;; general, loose
  ((inanimate quan1) *c-common*)
  ((abstract quan1) *c-common*)
  ((mahjong quan1) *c-prefer*)
  ;; 闋
  ((song que4) *c-prefer*)
  ((poem-like que4) *c-prefer*)
  ;; 群
  ((human qun2) *c-prefer*)
  ((animal qun2) *c-prefer*)
  ;; 扇 ;; loose, the shape
  ((inanimate shan4) *c-common*)
  ((door-like shan4) *c-prefer*)
  ;; 身
  ((animate-attribute shen1) *c-prefer*)
  ((wearable shen1) *c-prefer*)
  ;; 聲
  ((sound sheng1) *c-prefer*)
  ;; 首
  ((creative-work shou3) *c-prefer*)
  ((poem-like shou3) *c-prefer*)
  ((song shou3) *c-prefer*)
  ;; 束 ;; general, shape
  ((inanimate shu4) *c-common*)
  ((light shu4) *c-prefer*)
  ;; 雙 ;; general, anything in pair
  ((physical shuang1) *c-common*)
  ;; 絲
  ((long-thin si1) *c-prefer*)
  ((light si1) *c-prefer*)
  ((emotion si1) *c-prefer*)
  ((inanimate si1) *c-common*)
  ;; 艘
  ((ship sou1) *c-prefer*)
  ;; 所
  ((mostly-indoor suo3) *c-prefer*)
  ;; 台, 臺
  ((electric-appliance tai2) *c-prefer*)
  ((wheeled tai2) *c-prefer*)
  ((drama tai2) *c-prefer*)
  ;; 攤 ;; general, the shape
  ((liquid tan1) *c-prefer*)
  ((inanimate tan1) *c-common*)
  ;; 灘 ;; general, the shape
  ((liquid tan1) *c-prefer*)
  ((inanimate tan1) *c-common*)
  ;; 堂
  ((lesson tang2) *c-prefer*)
  ((furniture tang2) *c-prefer*)
  ;; 套 ;; general, a collection of things
  ((book-like tao4) *c-prefer*)
  ((clothing tao4) *c-prefer*)
  ((knowledge-domain tao4) *c-prefer*)
  ((noun tao4) *c-common*)
  ;; 挑 ;; general
  ((inanimate tiao1) *c-common*)
  ;; 條 ;; general, varied
  ((long-thin tiao2) *c-prefer*)
  ((beam-like tiao2) *c-prefer*)
  ((liquid tiao2) *c-prefer*)
  ((animal tiao2) *c-prefer*)
  ((human tiao2) *c-prefer*)
  ((clothing tiao2) *c-prefer*)
  ((ship tiao2) *c-prefer*)
  ((inanimate tiao2) *c-common*)
  ((abstract tiao2) *c-common*)
  ;; 貼
  ((medicine-oral tie1) *c-prefer*)
  ;; 挺 ;; general
  ((gun ting3) *c-common*)
  ((inanimate ting3) *c-common*)
  ;; 頭
  ((livestock tou2) *c-prefer*)
  ((horse-like tou2) *c-prefer*)
  ((hair-like tou2) *c-prefer*)
  ;; 團 ;; general, loose
  ((inanimate tuan2) *c-common*)
  ((abstract tuan2) *c-common*)
  ((human tuan2) *c-prefer*)
  ((army-like tuan2) *c-prefer*)
  ;; 趟
  ((event tang4) *c-prefer*)
  ((transportation tang4) *c-prefer*)
  ;; 通
  ((tele-communication tong1) *c-prefer*)
  ((event tong1) *c-common*)
  ;; 丸
  ((ball-like wan2) *c-prefer*)
  ;; 汪
  ((liquid wang1) *c-prefer*)
  ((eye wang1) *c-prefer*)
  ;; 尾
  ((fish-like wei3) *c-prefer*)
  ;; 味
  ((edible wei4) *c-prefer*)
  ;; 位
  ((human wei4-person) *c-prefer*)
  ((animate wei4-person) *c-common*)
  ;; 窩
  ((animal wo1) *c-prefer*)
  ((human wo1) *c-prefer*)
  ;; 席
  ((blanket-like xi2) *c-prefer*)
  ((seat xi2) *c-prefer*)
  ((social-status xi2) *c-prefer*)
  ((human xi2) *c-prefer*)
  ((meal xi2) *c-prefer*)
  ((lengthy-spoken xi2) *c-prefer*)
  ((clothing xi2) *c-prefer*)
  ;; 系列 ;; general
  ((product xi4-lie4) *c-prefer*)
  ((abstract xi4-lie4) *c-common*)
  ;; 線
  ((road xian4) *c-prefer*)
  ((light xian4) *c-prefer*)
  ((shadows xian4) *c-prefer*)
  ((abstract xian4) *c-common*)
  ;; 項 ;; general
  ((abstract xiang4) *c-common*)
  ((evidence xiang4) *c-close-sep*) ;; '一項 数据'
  ((event xiang4) *c-close-suffix*)
  ;; 些 ;; very general
  ((noun xie1) *c-prefer*)
  ;; 眼
  ((well-like yan3) *c-prefer*)
  ;; 樣 ;; very general, just about anything
  ((noun yang4) *c-common*)
  ((thing yang4) *c-prefer*)
  ;; 頁, 葉
  ((book-like ye4) *c-prefer*)
  ;; 元, 摳, 圓
  ((currency yuan2) *c-prefer*)
  ;; 則
  ((picture ze2) *c-prefer*)
  ((article-like ze2) *c-prefer*)
  ;; 盞
  ((liquid zhan3) *c-prefer*)
  ((bulb-like zhan3) *c-prefer*)
  ;; 章
  ((abstract zhang1) *c-common*) ;; loose
  ((lesson zhang1) *c-prefer*)
  ;; 張 ;; loose
  ((paper-like zhang1-piece) *c-prefer*)
  ((article-or-language zhang1-piece) *c-common*)
  ((furniture zhang1-piece) *c-prefer*)
  ((face zhang1-piece) *c-prefer*)
  ((blanket-like zhang1-piece) *c-prefer*)
  ((mouth zhang1-piece) *c-prefer*)
  ;; 幀
  ((picture zheng4) *c-prefer*)
  ;; 陣 ;; general
  ((natural-phenomena zhen4) *c-prefer*)
  ((abstract zhen4) *c-common*)
  ;; 隻, 只 ;; very general
  ((animal zhi1) *c-prefer*)
  ((inanimate zhi1) *c-prefer*)
  ((written-character zhi1) *c-prefer*)
  ((poem-like zhi1) *c-prefer*)
  ((music zhi1) *c-prefer*)
  ((fertilizer-or-fat-ind zhi1) *c-common*) ;; not prefer (一只肥) 鴨子
  ;; 支 ;; very general
  ((team zhi1-piece) *c-prefer*)
  ((religion-like zhi1-piece) *c-prefer*)
  ((long-thin zhi1-piece) *c-prefer*)
  ((song zhi1-piece) *c-prefer*)
  ((dance zhi1-piece) *c-prefer*)
  ((video zhi1-piece) *c-prefer*)
  ((baseball-strike zhi1-piece) *c-prefer*)
  ((light zhi1-piece) *c-prefer*)
  ((prize-or-penalty zhi1-piece) *c-prefer*)
  ((stock zhi1-piece) *c-prefer*)
  ;; 枝
  ((plant zhi1-long) *c-prefer*)
  ((long-thin zhi1-long) *c-prefer*)
  ;; 種 ;; very general
  ((noun zhong3) *c-common*)
  ;; 軸
  ((picture zhou2) *c-prefer*)
  ;; 株
  ((plant zhu1) *c-prefer*)
  ;; 桩, 樁
  ((event zhuang1) *c-prefer*)
  ;; 幢
  ((building zhuang4) *c-prefer*)
  ((light-shadow zhuang4) *c-prefer*)
  ;; 桌
  ((inanimate zhou1) *c-common*) ;; loose
  ((meal zhou1) *c-prefer*)
  ((human zhou1) *c-prefer*)
  ((chess-like-game zhou1) *c-prefer*)
  ;; 尊
  ((canon zun1) *c-prefer*)
  ((statue zun1) *c-prefer*)
  ;; 宗
  ((event zong1) *c-prefer*)
  ((cargo zong1) *c-prefer*)
  ;; 組 ;; general
  ((written-character zu3) *c-prefer*)
  ((abstract zu3) *c-common*)
  ((animate zu3) *c-common*)
  ((inanimate zu3) *c-common*)
  ((human zu3) *c-prefer*)
  ;; 座
  ((inanimate zuo4) *c-common*)
  ((machine zuo4) *c-prefer*)
  ((building zuo4) *c-prefer*)
  ((landscape zuo4) *c-prefer*)
  ;;;; from Treebank, those in lower case are hand edited
  ;;;; some units may already appear above, but with new constraints
  ;; MO3 "抹" 
  ((ADJ-COLOR MO3) *C-CLOSE-SEP*)
  ;; ZHUANG4 "幢" 
  ((GOODS ZHUANG4) *c-common*)
  ;; GAN1-HUMAN "干" 
  ((animate GAN1-HUMAN) *C-CLOSE-SEP*)
  ;; CONG2 "丛" 
  ((SHAPE-OR-TYPE-OF CONG2) *C-CLOSE-SEP*)
  ;; CAN1 "餐" 
  ((EDIBLE CAN1) *C-CLOSE-SEP*)
  ;; PI3 "匹" 
  ((HORSE-POWER PI3) *C-CLOSE-SEP*)
  ;; DING3 "顶" 
  ((FLOWER-IND DING3) *c-common*)
  ;; TUO2 "坨" 
  ((FECES TUO2) *C-CLOSE-SEP*)
  ;; DAN1 "单" 
  ((TRANSACTION DAN1) *C-CLOSE-SEP*)
  ((event dan1) *c-close-suffix*)
  ;; TAN1-LIQUID "滩" 
  ((FECES TAN1-LIQUID) *C-CLOSE-SEP*)
  ((liquid TAN1-LIQUID) *C-CLOSE-SEP*)
  ;; DA1 "打" 
  ((INANIMATE DA1) *C-CLOSE-SEP*)
  ;; DIE2 "叠" 
  ((DOCUMENT DIE2) *C-CLOSE-SEP*)
  ;; ZHA1 "扎" 
  ((DOCUMENT ZHA1) *C-CLOSE-SEP*)
  ((inanimate ZHA1) *c-common*)
  ;; TAN1 "摊" 
  ((SOIL TAN1) *C-CLOSE-SEP*)
  ;; QUAN1 "圈" 
  ((STRIP-OR-REGION-LIKE QUAN1) *C-CLOSE-SEP*)
  ;; ZHOU1 "桌" 
  ((EDIBLE ZHOU1) *C-CLOSE-SEP*)
  ;; KE1-SUBJECT "科" 
  ((lesson-class KE1-SUBJECT) *C-CLOSE-SEP*)
  ((knowledge-domain ke1-subject) *c-close-sep*)
  ((class-organization-ind ke1-subject) *c-common*) ;; 科班
  ;; GE2 "格格" 
  ((PHOTO GE2) *C-CLOSE-SEP*)
  ;; TIE1 "帖" 
  ((book-like-record TIE1) *c-close-suffix*)
  ;; XI2-CLOTH "袭" 
  ((clothing XI2-CLOTH) *C-CLOSE-SEP*)
  ((cloth XI2-CLOTH) *c-common*)
  ;; LUO4 "落" 
  ((VILLAGE LUO4) *C-CLOSE-SEP*)
  ;; GUAN3 "管" 
  ((INANIMATE GUAN3) *c-common*)
  ;; JIA4-CI4 "驾次" 
  ((transportation JIA4-CI4) *C-CLOSE-SEP*)
  ;; QIANG1 "腔" 
  ((LIQUID-PART QIANG1) *C-CLOSE-SEP*)
  ;; UNIT-VOLUME "立方米" "斗" "西西" "公升" "ml" "毫升" 
  ((VOLUME-OF UNIT-VOLUME) *C-CLOSE-SEP*)
  ;; JI2-ALBUM "辑" 
  ((CREATIVE-WORK JI2-ALBUM) *C-CLOSE-SEP*)
  ;; TAI2-TAO4 "台套" 
  ((SKILL TAI2-TAO4) *C-CLOSE-SEP*)
  ((machine TAI2-TAO4) *C-CLOSE-SEP*)
  ;; BIAN4 "遍" 
  ((event BIAN4) *C-CLOSE-SEP*)
  ((SONG-IND BIAN4) *C-CLOSE-SEP*)
  ;; SHU4 "束" 
  ((FLOWER SHU4) *C-CLOSE-SEP*)
  ((GOODS-IND SHU4) *c-common*)
  ;; HO3 "夥" "伙" 
  ((ORGANIZATION HO3) *C-CLOSE-SEP*)
  ;; JIE2-PERIOD "节" 
  ((NEWS JIE2-PERIOD) *C-CLOSE-SEP*)
  ((AUDIBLE-ARRANGED-EVENT JIE2-PERIOD) *C-CLOSE-SEP*)
  ;; MEN2-ZI3 "门子" 
  ((ADJ-ON-ABSTRACT-OR-ANIMATE MEN2-ZI3) *C-CLOSE-SEP*) ;; "公平"
  ((ABSTRACT MEN2-ZI3) *C-CLOSE-SEP*)
  ;; GAN1 "杆" 
  ((NUMBER-OF GAN1) *c-common*)
  ;; BAN3 "版" 
  ((creative-work BAN3) *C-CLOSE-SEP*)
  ((NEWS BAN3) *C-CLOSE-SEP*)
  ((non-verb-noun BAN3) *c-common*)
  ;; ZUN1 "尊尊" "尊" 
  ((GOD-LIKE ZUN1) *C-CLOSE-SEP*)
  ;; DI1 "滴" 
  ((RESULTING-EFFECT DI1) *C-CLOSE-SEP*)
  ;; LI4 "粒" 
  ((FECES LI4) *c-common*)
  ((VEGETABLE LI4) *c-common*)
  ;; YUAN2-HUMAN "员" 
  ((HUMAN YUAN2-HUMAN) *C-CLOSE-SEP*)
  ((OCCUPATION YUAN2-HUMAN) *C-CLOSE-SEP*)
  ;; ZHU1 "株" 
  ((plant ZHU1) *C-CLOSE-SEP*)
  ((GERMS-LIKE ZHU1) *C-CLOSE-SEP*)
  ;; ZHAO1 "招" 
  ((CHESS-LIKE-GAME ZHAO1) *C-CLOSE-SEP*)
  ((word-method ZHAO1) *C-CLOSE-SEP*)
  ;; JIE1 "阶" 
  ((STAIR JIE1) *C-CLOSE-SEP*)
  ;; CHENG2 "成" 
  ((ABSTRACT CHENG2) *C-CLOSE-SEP*)
  ((MONEY-RELATED CHENG2) *C-CLOSE-SEP*)
  ((power-rate cheng2) *c-common*) ;; 成功率
  ((s-zhang3 cheng2) *c-very-rare*) ;; prefer '成长' as one word
  ;; WA3 "瓦" 
  ((word-electricity WA3) *C-CLOSE-SEP*)
  ;; FU2-VOLT "伏" "伏特" 
  ((word-electricity FU2-VOLT) *C-CLOSE-SEP*)
  ;; BANG1 "帮子" "帮" 
  ((ADJ-ON-ANIMATE BANG1) *c-close-suffix*)
  ;; ZHI3 "纸" 
  ((ORDER ZHI3) *C-CLOSE-SEP*)
  ((LEGAL-DOCUMENT ZHI3) *C-CLOSE-SEP*)
  ((paper-like ZHI3) *c-close-suffix*)
  ;; CHU1 "出" 
  ((AUDIBLE-ARRANGED-EVENT CHU1) *C-CLOSE-SEP*)
  ((LIE CHU1) *C-CLOSE-SEP*)
  ;; SHOU3-HAND "手" 
  ((V-ZHUN3-BEI4 SHOU3-HAND) *c-close-suffix*)
  ((INFORMATION SHOU3-HAND) *c-close-suffix*)
  ((ABSTRACT SHOU3-HAND) *c-common*)
  ;; JU4 "具具" "具" 
  ((WORD-BUILDING-JIAN4-ZHU2 JU4) *C-CLOSE-SEP*)
  ;; BEI4-LIFE "辈" 
  ((HUMAN-IND BEI4-LIFE) *C-CLOSE-SEP*)
  ;; JUAN4 "卷" 
  ((STRIP-OR-REGION-LIKE JUAN4) *C-CLOSE-SEP*)
  ;; HANG2 "行" 
  ((ENCODING-OF HANG2) *C-CLOSE-SEP*)
  ((SEAT HANG2) *C-CLOSE-SEP*)
  ((PRODUCT HANG2) *c-common*)
  ;; CUO1 "撮" 
  ((HUMAN CUO1) *C-CLOSE-SEP*)
  ((ATTRIBUTE CUO1) *c-common*)
  ;; BAN1-TYPE "般" 
  ((abstract BAN1-TYPE) *c-common*)
  ;; ZHUANG1 "桩" 
  ((TRANSACTION ZHUANG1) *C-CLOSE-SEP*)
  ((V-SHOU1-GOU4 ZHUANG1) *C-CLOSE-SEP*)
  ((THINKING ZHUANG1) *c-close-suffix*)
  ;; DANG4 "档子" "档" 
  ((BUILDING-TYPE-IND-NS DANG4) *C-CLOSE-SEP*)
  ((ABSTRACT DANG4) *c-common*)
  ((drama DANG4) *c-close-suffix*)
  ((ABSTRACT-MATTER-IND DANG4) *C-CLOSE-SEP*)
  ;; BAN1 "班" 
  ((HUMAN BAN1) *C-CLOSE-SEP*)
  ((WHEELED BAN1) *c-close-suffix*)
  ;; ZHI1 "只只" "只" 
  ((V-QU1-C ZHI1) *C-CLOSE-SEP*)
  ((HUMAN ZHI1) *C-CLOSE-SEP*)
  ((FUND-IND ZHI1) *C-CLOSE-SEP*)
  ((stock ZHI1) *C-CLOSE-SEP*)
  ((EVIDENCE ZHI1) *c-common*)
  ;; PAN2 "盘" 
  ((SOIL PAN2) *C-CLOSE-SEP*)
  ((V-LU4-XIANG4 PAN2) *C-CLOSE-SEP*)
  ((VEGETABLE PAN2) *C-CLOSE-SEP*)
  ((V-JIAO4-LIANG5 PAN2) *C-CLOSE-SEP*)
  ;; DUN4 "顿" 
  ((V-DU2-DA2 DUN4) *C-CLOSE-SEP*)
  ((EDIBLE DUN4) *C-CLOSE-SEP*)
  ;; JIAN1 "间" 
  ((PLACE JIAN1) *C-CLOSE-SEP*)
  ((ORGANIZATION JIAN1) *C-CLOSE-SEP*)
  ((GOODS JIAN1) *c-common*)
  ;; ZONG1 "宗" 
  ((TELE-COMMUNICATION ZONG1) *C-CLOSE-SEP*)
  ((INFO-REPORT ZONG1) *C-CLOSE-SEP*)
  ((V-TIAO2-ZHA1 ZONG1) *c-common*)
  ((V-JIAN4-DING4-B ZONG1) *c-common*)
  ;; JI4-TIMES "记" 
  ((ABSTRACT JI4-TIMES) *c-common*)
  ((V-GUAN4-LAN2 JI4-TIMES) *C-CLOSE-SEP*)
  ((SKILL JI4-TIMES) *C-CLOSE-SEP*)
  ((BASEBALL-STRIKE JI4-TIMES) *C-CLOSE-SEP*)
  ;; XIA4 "下" 
  ((verb XIA4) *c-close-suffix*)
  ;; ZHANG1 "章" 
  ((V-JUE2-YI4-B ZHANG1) *C-CLOSE-SEP*)
  ((LEGAL-DOCUMENT ZHANG1) *C-CLOSE-SEP*)
  ((ARTICLE-LIKE ZHANG1) *C-CLOSE-SEP*)
  ;; MU4 "幕幕" "幕" 
  ((DRAMA MU4) *C-CLOSE-SEP*)
  ((SCENERY MU4) *C-CLOSE-SEP*)
  ((ABSTRACT MU4) *c-common*)
  ((STRIP-OR-REGION-LIKE MU4) *C-CLOSE-SEP*)
  ;; PIAO4 "票" 
  ((TICKET-OR-VOTE PIAO4) *C-CLOSE-SEP*)
  ((ORGANIZATION PIAO4) *C-CLOSE-SEP*)
  ((HUMAN PIAO4) *C-CLOSE-SEP*)
  ((paper-like piao4) *c-common*)
  ;; TOU2 "头" 
  ((ANIMATE TOU2) *c-close-suffix*)
  ((TOOL TOU2) *c-close-suffix*)
  ;; QUN2 "群群" "群" 
  ((GOD-LIKE QUN2) *C-CLOSE-SEP*)
  ((organization QUN2) *C-CLOSE-SEP*)
  ((LANDSCAPE QUN2) *c-close-suffix*)
  ;; KE1-BALL "颗颗" "颗" 
  ((OCCUPATION KE1-BALL) *c-close-suffix*)
  ((ATTITUDE KE1-BALL) *C-CLOSE-SEP*)
  ((ABSTRACT-HEART-ATTRIBUTE KE1-BALL) *C-CLOSE-SEP*)
  ((UNDER-WATER KE1-BALL) *C-CLOSE-SEP*)
  ;; SHEN1 "身" 
  ((EDIBLE-PART SHEN1) *C-CLOSE-SEP*)
  ((MONEY-RELATED SHEN1) *C-CLOSE-SEP*)
  ((LIGHT SHEN1) *C-CLOSE-SEP*)
  ;; SI1 "丝丝" "丝" 
  ((WAVE SI1) *C-CLOSE-SEP*)
  ((APPEARANCE SI1) *c-close-suffix*)
  ((ANIMATE-ATTRIBUTE SI1) *C-CLOSE-SEP*)
  ((ABSTRACT SI1) *c-common*)
  ;; SHUANG1 "双" 
  ((BODY-PART SHUANG1) *C-CLOSE-SEP*)
  ((WEARABLE SHUANG1) *C-CLOSE-SEP*)
  ((ANIMATE-ATTRIBUTE SHUANG1) *C-CLOSE-SEP*)
  ((KINSHIP SHUANG1) *C-CLOSE-SEP*)
  ((EATING-UTENSIL SHUANG1) *C-CLOSE-SEP*)
  ;; ZUO4 "座座" "座" 
  ((HUMAN-IND ZUO4) *c-common*) ;; 铜人
  ((PRIZE-OR-PENALTY ZUO4) *C-CLOSE-SEP*)
  ((PLACE ZUO4) *c-close-suffix*)
  ((ABSTRACT ZUO4) *c-common*)
  ;; SUO3 "所" 
  ((ABSTRACT SUO3) *c-common*)
  ((PLACE SUO3) *c-close-suffix*)
  ((ORGANIZATION SUO3) *C-CLOSE-SEP*)
  ;; KOU3 "口" 
  ((LIQUID KOU3) *C-CLOSE-SEP*)
  ((ABSTRACT KOU3) *c-common*)
  ((GAS KOU3) *C-CLOSE-SEP*)
  ((WORD-HOUSE-JIA1 KOU3) *C-CLOSE-SEP*)
  ((PRICE KOU3) *C-CLOSE-SEP*)
  ;; LU4 "路" 
  ((INANIMATE-ATTRIBUTE LU4) *C-CLOSE-SEP*)
  ((HUMAN LU4) *C-CLOSE-SEP*)
  ((INFORMATION LU4) *C-CLOSE-SEP*)
  ((GOD-LIKE LU4) *C-CLOSE-SEP*)
  ((OCCUPATION LU4) *C-CLOSE-SEP*)
  ((SCHEDULED-WHEELED LU4) *c-close-suffix*)
  ;; ZE2 "则" 
  ((EXAMPLE ZE2) *C-CLOSE-SEP*)
  ((ABSTRACT ZE2) *c-common*)
  ((V-BAO4-DAO4-C ZE2) *C-CLOSE-SEP*)
  ((INFORMATION ZE2) *C-CLOSE-SEP*)
  ((CAN-BE-JOB ZE2) *c-common*)
  ;; DUI4-PAIR "对" 
  ((BUILDING DUI4-PAIR) *C-CLOSE-SEP*)
  ((ABSTRACT DUI4-PAIR) *c-common*)
  ((V-DA1-DANG4 DUI4-PAIR) *C-CLOSE-SEP*)
  ((ANIMAL DUI4-PAIR) *C-CLOSE-SEP*)
  ((ORGANIZATION DUI4-PAIR) *c-close-suffix*)
  ;; TUAN2 "团团" "团" 
  ((GAS TUAN2) *C-CLOSE-SEP*)
  ((edible TUAN2) *c-close-suffix*)
  ;; TANG4 "趟" 
  ((WATER TANG4) *C-CLOSE-SEP*)
  ((PROCESS TANG4) *C-CLOSE-SEP*)
  ;; REN4 "任" 
  ((TIME-NAME REN4) *c-close-suffix*)
  ((KINSHIP REN4) *C-CLOSE-SEP*)
  ((OCCUPATION REN4) *C-CLOSE-SEP*)
  ((HUMAN REN4) *c-close-suffix*)
  ;; PAI4 "派" 
  ((BELIEF PAI4) *C-CLOSE-SEP*)
  ((IDEA-OPINION PAI4) *C-CLOSE-SEP*)
  ((OCCUPATION PAI4) *C-CLOSE-SEP*)
  ((INANIMATE-ATTRIBUTE PAI4) *C-CLOSE-SEP*)
  ((spoken-language pai4) *c-close-suffix*)
  ;; YE4 "叶" "页" 
  ((MATERIAL-OF YE4) *C-CLOSE-SEP*)
  ((WORD-HISTORY-OF YE4) *C-CLOSE-SEP*)
  ((CAN-BE-JOB YE4) *c-close-suffix*)
  ((OUTLINE YE4) *C-CLOSE-SEP*)
  ((SHIP-IND YE4) *C-CLOSE-SEP*)
  ((paper-like ye4) *c-close-suffix*)
  ((article-like ye4) *c-close-suffix*)
  ;; QU3 "曲曲" "曲" 
  ((CHAPTER-OR-LAW-OR-STAMP-OR-MEDAL QU3) *c-close-suffix*)
  ((AUDIBLE-ARTICLE-LIKE QU3) *C-CLOSE-SEP*)
  ((MUSIC QU3) *C-CLOSE-SEP*)
  ;; BA2 "把" 
  ((CURRENCY BA2) *C-CLOSE-SEP*)
  ((ANIMATE-ATTRIBUTE BA2) *c-close-suffix*)
  ((FISH-LIKE BA2) *c-close-suffix*)
  ((tool ba2) *c-close-sep*)
  ((paper-cash ba2) *c-close-sep*)
  ((MOTIVATION-OR-MOMENTUM-IND BA2) *C-CLOSE-SEP*)

  ;; SOU1 "艘" 
  ((GROUP-OF-PEOPLE SOU1) *c-common*)
  ((NAME-AS-NUMBER-OR-SIGN-IND SOU1) *c-common*)
  ((PLANE SOU1) *C-CLOSE-SEP*)

  ;; MEN2 "门" 
  ((V-YING4-YONG4 MEN2) *C-CLOSE-SEP*)
  ((LESSON MEN2) *C-CLOSE-SEP*)
  ((KNOWLEDGE-OR-UNDERSTANDING-OF MEN2) *C-CLOSE-SEP*)
  ((SPOKEN-LANGUAGE MEN2) *C-CLOSE-SEP*)
  ((MACHINE MEN2) *C-CLOSE-SEP*)

  ;; JI2-B "集" 
  ((FEELING-OR-SENSE-OR-SITUATION JI2-B) *c-common*)
  ((VIDEO-OR-PLATE JI2-B) *C-CLOSE-SEP*)
  ((V-BAO4-DAO4-C JI2-B) *C-CLOSE-SEP*)
  ((news ji2-b) *c-common*)
  ((info-report ji2-b) *c-common*)
  ((V-JIAO1-XUE2 JI2-B) *C-CLOSE-SEP*)

  ;; UNIT-CONTAINER
  ((AMOUNT-OF UNIT-CONTAINER) *C-CLOSE-SEP*)
  ((trend-or-wave unit-container) *c-common*) ;; 海潮, 湖潮

  ;; HUI2 "回" 
  ((verb HUI2) *c-common*)

  ;; BEN3 "本" 
  ((money-related BEN3) *c-common*)
  ((creative-work BEN3) *c-close-suffix*)
  ((ARTICLE-LIKE BEN3) *c-common*)
  ((INFORMATION BEN3) *c-close-suffix*)

  ;; LIANG4 "辆" 
  ((CART LIANG4) *C-CLOSE-SEP*)
  ((transportation liang4) *c-close-suffix*)

  ;; CHU4 "处" 
  ((ABSTRACT CHU4) *c-close-suffix*)
  ((ADDRESS-OF CHU4) *c-common*)
  ((PLACE CHU4) *c-close-suffix*)

  ;; SHI4 "式" 
  ((non-verb-noun shi4) *c-close-suffix*)

  ;; GEN1 "根根" "根" 
  ((inanimate gen1) *c-common*)
  ((EDIBLE GEN1) *c-close-suffix*)
  ((POINTY-TIP GEN1) *C-CLOSE-SEP*)
  ((WORD-ROAD GEN1) *c-close-suffix*)
  ((BODY-PART GEN1) *c-close-suffix*)
  ((MATERIAL GEN1) *C-CLOSE-SEP*)
  ((PLANT GEN1) *C-CLOSE-SEP*)

  ;; HU4 "户" 
  ((ABSTRACT HU4) *c-common*)
  ((SHOP-LIKE HU4) *C-CLOSE-SEP*)
  ((ORGANIZATION HU4) *C-CLOSE-SEP*)
  ((WORD-HOUSE-JIA1 HU4) *C-CLOSE-SEP*)

  ;; DAO4 "道" 
  ((abstract dao4) *c-common*)
  ((SPELL DAO4) *C-CLOSE-SEP*)
  ((SCENERY DAO4) *C-CLOSE-SEP*)
  ((PLACE DAO4) *c-common*)
  ((NATURAL-PHENOMENA DAO4) *C-CLOSE-SEP*)
  ((COLOR-OR-APPEARANCE DAO4) *C-CLOSE-SEP*)
  ((LINE DAO4) *C-CLOSE-SEP*)
  ((PAPER-LIKE-SIGN DAO4) *c-close-suffix*)
  ((GAP-LIKE DAO4) *C-CLOSE-SEP*)

  ;; FANG1 "方方" "方" 
  ((MONEY-RELATED FANG1) *c-common*)
  ((FIELD FANG1) *C-CLOSE-SEP*)
  ((CREATIVE-WORK FANG1) *c-common*)
  ((SHOP-LIKE FANG1) *c-close-suffix*)

  ;; KUAN3 "款" 
  ((physical kuan3) *c-common*)
  ((GOODS-PRODUCT KUAN3) *C-CLOSE-SEP*)
  ((LEGAL-DOCUMENT KUAN3) *C-CLOSE-SEP*)
  ((WHEELED KUAN3) *C-CLOSE-SEP*)
  ((SHAPE-OR-TYPE-OF KUAN3) *C-CLOSE-SEP*)
  ((tool KUAN3) *C-CLOSE-SEP*)
  ((WEARABLE KUAN3) *C-CLOSE-SEP*)

  ;; TAI2 "台" 
  ((ARRANGED-EVENT TAI2) *C-CLOSE-SEP*)
  ((DANCE TAI2) *C-CLOSE-SEP*)
  ((ABSTRACT TAI2) *c-common*)
  ((SHOP-LIKE TAI2) *c-common*)
  ((MACHINE TAI2) *C-CLOSE-SEP*)

  ;; PAI2 "排排" "排" 
  ((place PAI2) *c-common*)
  ((POSITION-OF PAI2) *c-common*)
  ((ABSTRACT PAI2) *c-common*)
  ((SEAT-OR-FURNITURE-OR-HOROSCOPE-IND PAI2) *C-CLOSE-SEP*)
  ((TREE PAI2) *C-CLOSE-SEP*)

  ;; ZU3 "组组" "组" 
  ((MACHINE ZU3) *C-CLOSE-SEP*)
  ((LINE ZU3) *C-CLOSE-SEP*)

  ;; MEI2 "枚" 
  ((CURRENCY MEI2) *c-close-suffix*)
  ((medal mei2) *c-close-sep*)
  ((plate-like MEI2) *C-CLOSE-SEP*)
  ((ABSTRACT MEI2) *c-common*)
  ((inanimate MEI2) *c-common*)
  ((SHELL-OR-CASING MEI2) *C-CLOSE-SEP*)
  ((WEAPON MEI2) *C-CLOSE-SEP*)
  ((FLYING-VEHICLE MEI2) *C-CLOSE-SEP*)
  ((TOOL-STAMP MEI2) *C-CLOSE-SEP*)
  ((HAIR-LIKE MEI2) *c-common*)
  ((STONE MEI2) *C-CLOSE-SEP*)

  ;; DUI1 "堆" 
  ((WRITTEN-CHARACTER DUI1) *C-CLOSE-SEP*)
  ((PLACE DUI1) *c-common*)
  ((REASON DUI1) *C-CLOSE-SEP*)

  ;; FU4 "副" 
  ((SCENERY FU4) *c-close-suffix*)
  ((BODY-PART FU4) *c-close-suffix*)
  ((SOUND FU4) *C-CLOSE-SEP*)
  ((LONG-THIN-FUEL FU4) *c-close-suffix*)
  ((POEM-LIKE FU4) *C-CLOSE-SEP*)
  ((WEARABLE FU4) *C-CLOSE-SEP*)
  ((PLATE-WORD FU4) *C-CLOSE-SEP*)
  ((ABSTRACT FU4) *c-common*)
  ((APPEARANCE FU4) *C-CLOSE-SEP*)

  ;; CHONG2 "重" 
  ((WAVE CHONG2) *C-CLOSE-SEP*)
  ((SKY-OR-GOD-LIKE-OR-PLACE-IND CHONG2) *C-CLOSE-SEP*)

  ;; SHENG1 "声声" "声" 
  ((verb SHENG1) *c-common*)
  ((CANON SHENG1) *C-CLOSE-SEP*)
  ((DISASTER SHENG1) *c-common*)
  ((ORDER SHENG1) *C-CLOSE-SEP*)
  ((INFO-REPORT SHENG1) *C-CLOSE-SEP*)
  (((n noun) sheng1)
   (if (trait-value n 'quoted)
       *c-close-sep*
       *c-fallback*))

  ;; JU4-SENTENCE "句" 
  ((creative-work ju4-sentence) *c-common*)
  ((POEM-LIKE JU4-SENTENCE) *C-CLOSE-SEP*)
  ((TOPIC JU4-SENTENCE) *C-CLOSE-SEP*)
  ((ANIMATE-ATTRIBUTE JU4-SENTENCE) *c-close-suffix*)
  ((BOOK-LIKE-RECORD JU4-SENTENCE) *c-close-suffix*)
  ((LAW-OR-TRAINING JU4-SENTENCE) *c-close-suffix*)
  (((n noun) ju4-sentence)
   (if (trait-value n 'quoted)
       *c-close-sep*
       *c-fallback*))

  ;; LI4-EXAMPLE "例" 
  ((noun li4-example) *c-common*)
  ((ABSTRACT-MATTER LI4-EXAMPLE) *C-CLOSE-SEP*)
  ((GERMS-LIKE LI4-EXAMPLE) *C-CLOSE-SEP*)
  ((EXAMPLE LI4-EXAMPLE) *C-CLOSE-SEP*)
  ((EVENT LI4-EXAMPLE) *C-CLOSE-SEP*)

  ;; FU2 "幅幅" "幅" 
  ((PAPER-LIKE FU2) *C-CLOSE-SEP*)
  ((CAN-BE-JOB FU2) *c-close-suffix*)
  ((ANIMATE-ATTRIBUTE FU2) *c-common*)
  ((SITUATION FU2) *c-common*)
  ((CREATIVE-WORK FU2) *C-CLOSE-SEP*)
  ((APPEARANCE FU2) *c-common*)
  ((ONLY-LIGHT FU2) *c-close-suffix*)
  ((WRITTEN-CHARACTER FU2) *C-CLOSE-SEP*)
  ((SPEECH-LIKE FU2) *c-close-suffix*)
  ((FENCE-OR-MONITOR FU2) *c-close-suffix*)
  ((PLATE-LIKE FU2) *C-CLOSE-SEP*)

  ;; CHUAN4 "串串" "连串" "串" 
  ((NAME-OF CHUAN4) *C-CLOSE-SEP*)
  ((EVENT CHUAN4) *C-CLOSE-SEP*)
  ((PLAN CHUAN4) *C-CLOSE-SEP*)
  ((PAPER-CASH CHUAN4) *C-CLOSE-SEP*)

  ;; ZHI1-PIECE "支" 
  ((abstract zhi1-piece) *c-common*)
  ((inanimate zhi1-piece) *c-common*)
  ((animate zhi1-piece) *c-common*)
  ((SHIP ZHI1-PIECE) *C-CLOSE-SEP*)
  ((BULLET-LIKE ZHI1-PIECE) *c-close-suffix*)
  ((TELE-COMMUNICATION ZHI1-PIECE) *C-CLOSE-SEP*)
  ((HUMAN ZHI1-PIECE) *C-CLOSE-SEP*)
  ((MUSIC ZHI1-PIECE) *C-CLOSE-SEP*)
  ((ORGANIZATION ZHI1-PIECE) *c-close-suffix*)
  ((tool ZHI1-PIECE) *C-CLOSE-SEP*)

  ;; BU4 "部" 
  ((KNOWLEDGE-DOMAIN BU4) *C-CLOSE-SEP*)
  ((LAW BU4) *C-CLOSE-SEP*)

  ;; JIAN4 "件件" "件" 
  ((JOB-WORD-GONG1-ZUO4 JIAN4) *C-CLOSE-SEP*)
  ((EXAMPLE JIAN4) *C-CLOSE-SEP*)
  ((EVENT JIAN4) *C-CLOSE-SEP*)
  ((CREATIVE-WORK JIAN4) *c-close-suffix*)
  ((SPECIMEN JIAN4) *C-CLOSE-SEP*)
  ((GOODS-IND JIAN4) *C-CLOSE-SEP*)

  ;; FEN1 "分" 
  ((BENEFIT FEN1) *C-CLOSE-SEP*)
  ((LAND FEN1) *c-close-suffix*)
  ((money-related FEN1) *c-close-suffix*)
  ((FIELD FEN1) *c-close-suffix*)

  ;; WEI4-PERSON "位" 
  ((POLITICAL-PARTY WEI4-PERSON) *C-CLOSE-SEP*)
  ((religion-name WEI4-PERSON) *C-CLOSE-SEP*)
  ((CORPSE WEI4-PERSON) *c-common*)
  ((GOD-LIKE WEI4-PERSON) *C-CLOSE-SEP*)
  ((number-of-ind WEI4-PERSON) *C-CLOSE-SEP*)

  ;; BI3 "笔" 
  ((BENEFIT BI3) *C-CLOSE-SEP*)
  ((AMOUNT-OF BI3) *C-CLOSE-SEP*)
  ((LEGAL-DOCUMENT BI3) *c-common*)
  ((BOOK-LIKE-RECORD BI3) *C-CLOSE-SEP*)
  ((LIST-OR-BROCHURE BI3) *c-common*)
  ((INSURANCE-INDUSTRY BI3) *C-CLOSE-SEP*)
  ((number-of BI3) *c-common*)

  ;; BU4-STEP "步" 
  ((FOOTPRINT-OR-STAMP BU4-STEP) *c-common*)
  ((PLAN BU4-STEP) *C-CLOSE-SEP*)
  ((CHESS-LIKE-GAME BU4-STEP) *C-CLOSE-SEP*)
  ((ROAD-IND BU4-STEP) *C-CLOSE-SEP*)
  ((event BU4-STEP) *c-close-suffix*)
  ((MUSIC BU4-STEP) *c-close-suffix*)
  ((song-ind bu4-step) *c-rare*) ;; 步调

  ;; MING2 "名" 
  ((corpse MING2) *C-CLOSE-SEP*)
  ((spirit ming2) *c-close-sep*)

  ;; QI3 "起" 
  ((EXAMPLE QI3) *C-CLOSE-SEP*)
  ((KNOWLEDGE-DOMAIN QI3) *c-common*)
  ((situation QI3) *C-CLOSE-SEP*)
  ((NEWS QI3) *C-CLOSE-SEP*)
  ((EVENT QI3) *C-CLOSE-SEP*)
  ((DRAMA QI3) *C-CLOSE-SEP*)
  ((ABSTRACT QI3) *c-common*)
  ((TRANSACTION QI3) *C-CLOSE-SEP*)

  ;; GU3 "股" 
  ((STREET GU3) *c-common*)
  ((STYLE-TREND GU3) *C-CLOSE-SEP*)
  ((MOTIVATION-OR-MOMENTUM GU3) *C-CLOSE-SEP*)
  ((GAS GU3) *C-CLOSE-SEP*)
  ((FLOW-OF GU3) *C-CLOSE-SEP*)
  ((FIRE GU3) *C-CLOSE-SEP*)
  ((STOCK GU3) *C-CLOSE-SEP*)

  ;; ZHEN4 "阵阵" "阵" 
  ((INTENTION-OR-FEEL ZHEN4) *C-CLOSE-SEP*)
  ((WAVE ZHEN4) *C-CLOSE-SEP*)
  ((EMOTION ZHEN4) *C-CLOSE-SEP*)
  ((SOUND ZHEN4) *C-CLOSE-SEP*)
  ((STYLE-TREND ZHEN4) *C-CLOSE-SEP*)
  ((EVENT ZHEN4) *C-CLOSE-SEP*)
  ((fluid-like ZHEN4) *C-CLOSE-SEP*)
  ((GAS ZHEN4) *C-CLOSE-SEP*)

  ;; ZHANG1-PIECE "张儿" "张张" "张" 
  ((CURRENCY ZHANG1-PIECE) *C-CLOSE-SEP*)
  ((INSURANCE-INDUSTRY ZHANG1-PIECE) *c-close-suffix*)
  ((OCCUPATION ZHANG1-PIECE) *c-common*)
  ((CREATIVE-WORK ZHANG1-PIECE) *C-CLOSE-SEP*)
  ((SPEECH-LIKE ZHANG1-PIECE) *c-common*)
  ((SMALL-PLATE-LIKE ZHANG1-PIECE) *C-CLOSE-SEP*)
  ((PLATE-LIKE ZHANG1-PIECE) *C-CLOSE-SEP*)

  ;; UNIT-AREA
  ((LAND UNIT-AREA) *C-CLOSE-SEP*)
  ((DISTRICT UNIT-AREA) *c-close-suffix*)
  ((LANDSCAPE UNIT-AREA) *c-close-suffix*)
  ((FIELD UNIT-AREA) *C-CLOSE-SEP*)
  ((PLACE UNIT-AREA) *c-common*)
  ((ABSTRACT UNIT-AREA) *c-common*)
  ((SITUATION UNIT-AREA) *c-common*)

  ;; KUAI4 "块块" "块" 
  ((WATCH KUAI4) *C-CLOSE-SEP*)
  ((MONEY-RELATED KUAI4) *c-close-suffix*)
  ((PLACE KUAI4) *c-close-suffix*)
  ((BODY-PART KUAI4) *C-CLOSE-SEP*)
  ((PICTURE KUAI4) *c-close-suffix*)

  ;; PIAN1 "篇篇" "篇" 
  ((WRITTEN-CHARACTER PIAN1) *C-CLOSE-SEP*)
  ((advertising PIAN1) *C-CLOSE-SEP*)
  ((BOOK-LIKE PIAN1) *C-CLOSE-SEP*)
  ((BELIEF PIAN1) *c-close-suffix*)
  ((AUDIBLE-ARRANGED-EVENT PIAN1) *C-CLOSE-SEP*)
  ((EVALUATION-OF PIAN1) *C-CLOSE-SEP*)
  ((info-report pian1) *c-close-suffix*)

  ;; DAI4 "代代" "代" 
  ((ATTRIBUTE DAI4) *c-close-suffix*)
  ((SHAPE-OR-TYPE-OF DAI4) *c-close-suffix*)
  ((INANIMATE DAI4) *c-common*)
  ((PLACE DAI4) *c-common*)
  ((STYLE-TREND DAI4) *C-CLOSE-SEP*)
  ((abstract dai4) *c-common*)
  ((NETWORK-LIKE DAI4) *C-CLOSE-SEP*)
  ((animate dai4) *c-close-suffix*)
  ((GERMS-LIKE DAI4) *C-CLOSE-SEP*)
  ((BIG-CHANGE-EVENT DAI4) *C-CLOSE-SEP*)

  ;; QI2 "期" 
  ((ABSTRACT QI2) *c-common*)
  ((PLAN QI2) *C-CLOSE-SEP*)
  ((MONEY-RELATED QI2) *c-close-suffix*)
  ((LEGAL-DOCUMENT QI2) *C-CLOSE-SEP*)
  ((JOB-WORD-GONG1-ZUO4 QI2) *C-CLOSE-SEP*)
  ((BUILDING QI2) *C-CLOSE-SEP*)
  ((lesson-class qi2) *c-close-suffix*)
  ((INFO-REPORT QI2) *C-CLOSE-SEP*)
  ((DAM QI2) *C-CLOSE-SEP*)
  ((INANIMATE QI2) *c-common*)

  ;; UNIT-WEIGHT "公吨" "公克" "克拉" "克" "昂司" "磅" "两" "斤" "吨" "公斤" 
  ((AMOUNT-OF UNIT-WEIGHT) *c-close-suffix*)
  ((ABSTRACT UNIT-WEIGHT) *c-common*)
  ((BUILDING UNIT-WEIGHT) *c-close-suffix*)
  ((animal UNIT-WEIGHT) *C-CLOSE-SEP*)

  ;; JIE4 "届" 
  ((abstract jie4) *c-common*)
  ((TIME-NAME JIE4) *c-close-suffix*)
  ((SHOP-LIKE JIE4) *c-close-suffix*)
  ((event JIE4) *c-common*)
  ((COMPETITION JIE4) *C-CLOSE-SEP*)

  ;; FAN1 "番" 
  ((SPOKEN-LANGUAGE FAN1) *C-CLOSE-SEP*)
  ((LIQUID-PART FAN1) *c-common*)
  ((SPEECH-LIKE FAN1) *C-CLOSE-SEP*)
  ((MARTIAL-ARTS FAN1) *c-close-suffix*)
  ((THINKING FAN1) *C-CLOSE-SEP*)
  ((TWIST-AND-FRUSTRATION FAN1) *C-CLOSE-SEP*)
  ((LENGTHY-SPOKEN FAN1) *C-CLOSE-SEP*)
  ((past-experience fan1) *c-close-suffix*)

  ;; PI1 "批批" "批" 
  ((animate PI1) *c-close-suffix*)
  ((FUND-IND PI1) *C-CLOSE-SEP*)
  ((goods PI1) *c-close-suffix*)
  ((HUMAN PI1) *C-CLOSE-SEP*)
  ((MONEY-RELATED PI1) *c-close-suffix*)

  ;; CENG2 "层层" "层" 
  ((WAVE CENG2) *C-CLOSE-SEP*)
  ((PLACE CENG2) *c-close-suffix*)
  ((liquid CENG2) *c-close-suffix*)
  ((PLATE-LIKE CENG2) *C-CLOSE-SEP*)
  ((MATERIAL CENG2) *C-CLOSE-SEP*)
  ((DOOR-LIKE CENG2) *c-close-suffix*)
  ((MEANING CENG2) *C-CLOSE-SEP*)
  ((CLOTHING CENG2) *C-CLOSE-SEP*)

  ;; DU4 "度", also a measure of degree
  ((TIME-PERIOD-NUM DU4) *c-close-suffix*)
  ((drink du4) *c-close-sep*)
  ((word-electricity DU4) *C-CLOSE-SEP*)
  ((FEVER DU4) *C-CLOSE-SEP*)
  ((TEMPERATURE-OF DU4) *C-CLOSE-SEP*)
  ((PROCESS DU4) *C-CLOSE-SEP*)
  ((SOUND-IND DU4) *c-close-suffix*)

  ;; DUAN4 "段段" "段" 
  ((FLOW-OF DUAN4) *C-CLOSE-SEP*)
  ((AMOUNT-OF DUAN4) *C-CLOSE-SEP*)
  ((SPOKEN-LANGUAGE DUAN4) *C-CLOSE-SEP*)
  ((AUDIBLE-ARTICLE-LIKE DUAN4) *C-CLOSE-SEP*)
  ((PAST-RECORD DUAN4) *C-CLOSE-SEP*)
  ((WRITTEN-CHARACTER DUAN4) *C-CLOSE-SEP*)
  ((SPEECH-LIKE DUAN4) *C-CLOSE-SEP*)
  ((TIME-NAME DUAN4) *C-CLOSE-SEP*)
  ((place DUAN4) *c-common*)
  ((FEELING-OR-SENSE DUAN4) *C-CLOSE-SEP*)
  ((creative-work DUAN4) *c-close-suffix*)

  ;; TAO4 "套" 
  ((PROCESS TAO4) *C-CLOSE-SEP*)
  ((ABSTRACT-TYPE-OF TAO4) *C-CLOSE-SEP*)
  ((HOUSING TAO4) *C-CLOSE-SEP*)
  ((MOSTLY-INDOOR TAO4) *C-CLOSE-SEP*)
  ((CAPABILITY-OF TAO4) *C-CLOSE-SEP*)
  ((SOUND TAO4) *C-CLOSE-SEP*)
  ((STYLE-TREND TAO4) *C-CLOSE-SEP*)
  ((PROPOSAL-LIKE TAO4) *C-CLOSE-SEP*)
  ((STAMP TAO4) *C-CLOSE-SEP*)
  ((MACHINE TAO4) *C-CLOSE-SEP*)
  ((TOOL TAO4) *C-CLOSE-SEP*)
  ((WORD-STANDARD TAO4) *C-CLOSE-SEP*)
  ((FURNITURE TAO4) *C-CLOSE-SEP*)
  ((ABSTRACT TAO4) *c-common*)
  ((musical-instrument TAO4) *C-CLOSE-SEP*)
  ((MATERIAL-OR-WORD-MATERIAL TAO4) *C-CLOSE-SEP*)
  ((SYSTEM-POLICY-MECHANISM-MADE TAO4) *C-CLOSE-SEP*)
  ((WORD-METHOD TAO4) *C-CLOSE-SEP*)

  ;; XI4-LIE4 "系列" 
  ((EVENT XI4-LIE4) *C-CLOSE-SEP*)
  ((PROCESS XI4-LIE4) *C-CLOSE-SEP*)
  ((SONG XI4-LIE4) *C-CLOSE-SEP*)
  ((STRATEGY XI4-LIE4) *C-CLOSE-SEP*)
  ((SYSTEM-POLICY-MECHANISM-MADE XI4-LIE4) *C-CLOSE-SEP*)
  ((PLACE XI4-LIE4) *c-common*)
  ((POSED-QUESTION XI4-LIE4) *C-CLOSE-SEP*)
  ((DOCUMENT XI4-LIE4) *C-CLOSE-SEP*)

  ;; JIA1 "家家" "家" 
  ((WEBSITE-IND JIA1) *C-CLOSE-SEP*)
  ((abstract jia1) *c-common*)
  ((BRAND JIA1) *C-CLOSE-SEP*)
  ((BOOK-LIKE JIA1) *c-close-suffix*)
  ((BATH JIA1) *c-common*)
  ((WORD-TAN1 JIA1) *C-CLOSE-SEP*)
  ((NEWSPAPER-LIKE JIA1) *C-CLOSE-SEP*)
  ((place JIA1) *c-common*)
  ((ORGANIZATION JIA1) *C-CLOSE-SEP*)
  ((BUILDING JIA1) *c-close-suffix*)

  ;; PIAN4 "片" 
  ((WORD-LIFE PIAN4) *C-CLOSE-SEP*)
  ((EDIBLE-PART PIAN4) *C-CLOSE-SEP*)
  ((MEDICINE-ORAL-IND PIAN4) *C-CLOSE-SEP*)
  ((PLACE PIAN4) *c-close-suffix*)
  ((field pian4) *c-close-sep*)
  ((TREND-OR-WAVE PIAN4) *C-CLOSE-SEP*)
  ((PLANT-PART PIAN4) *C-CLOSE-SEP*)
  ((ONLY-LIGHT PIAN4) *C-CLOSE-SEP*)
  ((SMALL-PLATE-LIKE PIAN4) *C-CLOSE-SEP*)
  ((PLATE-LIKE PIAN4) *C-CLOSE-SEP*)

  ;; LUN2 "轮" 
  ((EXAMINATION LUN2) *C-CLOSE-SEP*)
  ((ABSTRACT LUN2) *c-common*)
  ((EVENT LUN2) *C-CLOSE-SEP*)
  ((ARGUMENT LUN2) *C-CLOSE-SEP*)
  ((STEPS LUN2) *C-CLOSE-SEP*)
  ((BATTLE LUN2) *C-CLOSE-SEP*)
  ((COMPETITION LUN2) *C-CLOSE-SEP*)

  ;; BO1 "波波" "拨" "波" 
  ((BATTLE BO1) *C-CLOSE-SEP*)
  ((ARGUMENT BO1) *C-CLOSE-SEP*)
  ((EVENT BO1) *c-close-suffix*)
  ((GOODS BO1) *C-CLOSE-SEP*)
  ((TREND-OR-WAVE BO1) *C-CLOSE-SEP*)
  ((tendency-or-momentum BO1) *C-CLOSE-SEP*)
  ((style-trend BO1) *C-CLOSE-SEP*)
  ((SHOP-LIKE BO1) *c-common*)
  ((ABSTRACT BO1) *c-common*)
  ((animate BO1) *c-close-suffix*)
  ((WAVE BO1) *C-CLOSE-SEP*)

  ;; JI2 "级" 
  ((POSITION-OF JI2) *C-CLOSE-SEP*)

  ;; TIAO2 "条条" "条" 
  ((STRATEGY-OR-MEASURING-TOOL TIAO2) *C-CLOSE-SEP*)
  ((REASON TIAO2) *C-CLOSE-SEP*)
  ((SYSTEM-POLICY-MECHANISM-MADE TIAO2) *C-CLOSE-SEP*)
  ((SIN-OR-CRIME TIAO2) *C-CLOSE-SEP*)
  ((HEART TIAO2) *C-CLOSE-SEP*)
  ((SEAT-LIKE-FURNITURE TIAO2) *C-CLOSE-SEP*)
  ((LAW TIAO2) *C-CLOSE-SEP*)
  ((BELIEF TIAO2) *C-CLOSE-SEP*)
  ((WORD-STANDARD TIAO2) *C-CLOSE-SEP*)
  ((ORDER TIAO2) *C-CLOSE-SEP*)
  ((LETTER TIAO2) *C-CLOSE-SEP*)
  ((SPOKEN TIAO2) *C-CLOSE-SEP*)
  ((INFORMATION TIAO2) *C-CLOSE-SEP*)
  ((ABSTRACT-DAO4 TIAO2) *C-CLOSE-SEP*)
  ((STREET TIAO2) *C-CLOSE-SEP*)
  ((STICK-LIKE TIAO2) *C-CLOSE-SEP*)
  ((TOPIC TIAO2) *C-CLOSE-SEP*)
  ((SONG TIAO2) *C-CLOSE-SEP*)
  ((PLACE TIAO2) *c-common*)
  ((FLOW-OF TIAO2) *C-CLOSE-SEP*)
  ((BLANKET-LIKE TIAO2) *C-CLOSE-SEP*)
  ((ANIMATE-ATTRIBUTE TIAO2) *C-CLOSE-SEP*)
  ((ENCODING-OF TIAO2) *C-CLOSE-SEP*)
  ((PROPOSAL-LIKE TIAO2) *C-CLOSE-SEP*)
  ((ARTICLE-LIKE TIAO2) *C-CLOSE-SEP*)
  ((LINE TIAO2) *C-CLOSE-SEP*)
  ((LONG-THIN-BODY-PART TIAO2) *C-CLOSE-SEP*)
  ((ROAD TIAO2) *C-CLOSE-SEP*)

  ;; CHANG2 "场场" "场" 
  ((ANIMATE-ATTRIBUTE CHANG2) *c-close-suffix*)
  ((LESSON CHANG2) *C-CLOSE-SEP*)
  ((PHYSICAL-SKY-IND CHANG2) *C-CLOSE-SEP*)
  ((LIE CHANG2) *C-CLOSE-SEP*)
  ((DANCE CHANG2) *C-CLOSE-SEP*)
  ((PROCESS CHANG2) *C-CLOSE-SEP*)
  ((ABSTRACT CHANG2) *c-common*)
  ((GAS CHANG2) *C-CLOSE-SEP*)
  ((TRANSACTION CHANG2) *C-CLOSE-SEP*)
  ((WAVE CHANG2) *C-CLOSE-SEP*)

  ;; DIAN3 "点点" "点儿" "点"
  ((noun dian3) *c-common*)

  ;; FEN4 "份" , very general
  ((noun fen4) *c-common*)
  ((abstract fen4) *c-close-suffix*)

  ;; CI4 "次次" "次" 
  ((abstract ci4) *c-common*)
  ((TIME-NAME CI4) *C-CLOSE-SEP*)
  ((FIRE CI4) *C-CLOSE-SEP*)
  ((FATE-OR-CHANCE CI4) *C-CLOSE-SEP*)
  ((MARRIAGE CI4) *C-CLOSE-SEP*)
  ((VERB CI4) *C-CLOSE-SEP*)
  ((PROCESS CI4) *C-CLOSE-SEP*)
  ((WORD-RESEARCH CI4) *C-CLOSE-SEP*)
  ((past-experience ci4) *c-close-sep*)
  ((weather ci4) *c-close-sep*) ;; 一次 (重 污染 天气)
  ((word-pollution ci4) *c-close-sep*) ;; 一次 污染
  ((natural-phenomena ci4) *c-close-sep*)
  ((event ci4) *c-close-sep*)

  ;; XIANG4 "项" 
  ((inanimate xiang4) *c-common*)
  ((MUSICAL-INSTRUMENT XIANG4) *C-CLOSE-SEP*)
  ((GOODS XIANG4) *C-CLOSE-SEP*)

  ;;
  )
;; end noun-unit

(defun-conn verb-unit (v u) verb-unit-x)
(defgeneric verb-unit-x (v u))
;; e.g. '打 一下', '說 一聲'
(def-comb verb-unit-x
  ((t t) *c-unknown-pair*)
  ;;
  ((verb unit) *c-common*) ;; already used in the rules
  ;; frequency is basically applicable to all verb
  ((verb unit-event-frequency) *c-prefer*)
  ;;
  ((v-xie4-shi4-b sheng1) *c-close-sep*)
  ((v-xie4-shi4-b ju4-sentence) *c-close-sep*)

  ((v-di1-yi4-b sheng1) *c-close-sep*)
  ((v-di1-yi4-b ju4-sentence) *c-close-sep*)

  ((v-gu3-li4 sheng1) *c-close-sep*)
  ((v-gu3-li4 ju4-sentence) *c-close-sep*)

  ((v-hui2 sheng1) *c-close-suffix*)
  ((v-hui2 ju4-sentence) *c-close-suffix*)

  ((v-che3 sheng1) *c-close-suffix*)
  ((v-che3 ju4-sentence) *c-close-suffix*)

  ((v-xun4-d sheng1) *c-close-suffix*)
  ((v-xun4-d ju4-sentence) *c-close-suffix*)

  ((vc-speak sheng1) *c-close-sep*)
  ((vc-speak ju4-sentence) *c-close-sep*)
  ) ;; end verb-unit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-conn adj-noun (a n) adj-noun-x)
(defgeneric adj-noun-x (a n))
(def-comb adj-noun-x
  ((t t) *c-ignore*)
  ;;
  ((adj noun) *c-common*)
  ;; TODO: many adjectives can be applied to many types of nouns, should we keep the different categories of adj's
  ;; broad categories.
  ;; some adjectives belong to more than one type, and no special
  ;; combinations are given to them, if the rules of either one of its
  ;; parent class are applicable and no special rules are needed.

  ((adj degree-of-ind) *c-prefer*) ;; e.g. 长度

  ((adj-on-noun noun) *c-prefer*)
  ((adj-on-place place) *c-prefer*)
  ((adj-on-physical physical) *c-prefer*)
  ((adj-on-physical place) *c-prefer*)
  ;; in addition to adj-on-physical
  ;;((adj-on-physical-or-time noun-time) *c-prefer*)
  ((adj-on-physical-or-time abstract) *c-prefer*)
  ;;
  ((adj-on-food edible) *c-prefer*)
  ((adj-on-food smell) *c-prefer*)
  ((adj-on-food boobs) *c-prefer*)
  ((adj-on-food liquid) *c-prefer*)

  ((adj-on-animate animate) *c-prefer*)
  ((adj-on-animate abstract) *c-prefer*)
  ((adj-on-human human) *c-prefer*)
  ;; in addition to adj-on-human
  ((adj-on-animate-or-organization shop-like) *c-prefer*)
  ;;
  ((adj-on-occupation-or-title occupation) *c-prefer*)
  ((adj-on-animal animal) *c-prefer*)
  ((adj-on-inanimate inanimate) *c-prefer*)
  ((adj-on-inanimate place) *c-close-sep*)
  ((adj-on-weather weather) *c-prefer*)
  ((adj-on-weather natural-phenomena) *c-prefer*)
  ((adj-on-language spoken-language) *c-prefer*)
  ((adj-on-abstract abstract) *c-prefer*)
  ((adj-on-abstract time-name) *c-prefer*)
  ;; in addition to adj-on-abstract
  ((adj-on-abstract-or-movable animate) *c-prefer*)
  ((adj-on-abstract-or-movable can-fly) *c-prefer*)
  ((adj-on-abstract-or-movable transportation) *c-prefer*)
  ;; sometimes computers are describes as if they could move
  ((adj-on-abstract-or-movable computer-like) *c-prefer*)
  ;; penalize some combination because they are rare, or is more
  ;; usually other kinds of words.
  ((adj-word-good word-statue) *c-rare*) ;; 好 像

  ((adj-temperature abstract) *c-close-sep*) ;; e.g. 暖意
  ((adj-temperature time-name) *c-close-sep*) ;; e.g. 寒假

  ((adj-word-below-zero noun) *c-fallback*)
  ((adj-word-below-zero abstract) *c-rare*)
  ((adj-word-below-zero time-name) *c-rare*)
  ((adj-word-below-zero weather) *c-close-suffix*)
  ((adj-word-below-zero temperature-of) *c-close-sep*)
  
  ((adj-thin-or-thick money-related) *c-close-sep*) ;; e.g. 薄利, 厚利
  ((adj-thin-or-thick prize-or-penalty) *c-close-sep*)
  ((adj-thin-or-thick benefit) *c-close-sep*)

  ((adj-size abstract) *c-close-sep*)
  ((adj-size flow-of) *c-close-sep*) ;; 细流
  ((adj-color flow-of) *c-close-sep*) ;; 黑流

  ((adj-color color-or-appearance) *c-prefer*)
  ((adj-on-inanimate color-or-appearance) *c-close-sep*)

  ((adj-shape shape-of) *c-close-sep*)
  ((adj-shape personality-or-space-ind) *c-close-sep*) ;; 方格
  ((adj-thin-or-thick abstract) *c-close-sep*) ;; e.g. 厚礼

  ((adj-shang4 abstract-matter) *c-common*) ;; don't want 世界 (上的事情)
  ((adj-on-weight appearance-ind) *c-common*) ;; 重樣?
  ((adj-word-he2 noun) *c-common*) ;; prefer 和 as connective
  ((adj-word-wei4 aspect-word-mian4-ind) *c-common*) ;; prefer 外面 as place-with-dir
  ((adj-word-wei4 noodle-like-ind) *c-common*) ;; prefer 外面 as place-with-dir
  ((adj-word-wei4 face-ind) *c-common*) ;; prefer 外面 as place-with-dir

  ((adj-word-secondary occupation) *c0*)
  ((adj-word-chief occupation) *c0*)
  ((adj-word-master occupation) *c0*)

  ((adj-word-secondary building) *c0*)
  ((adj-word-chief building) *c0*)
  ((adj-word-master building) *c0*)

  ((adj-word-secondary inanimate) *c4*)
  ((adj-word-secondary organization) *c1*)
  ((adj-word-secondary human-suffix) *c-close-sep*)

  ((adj-word-chief noun) *c-rare*)
  ((adj-word-chief abstract) *c-rare*)
  ((adj-word-chief amount-of) *c0*) ;; e.g. "總 水量"
  ((adj-word-chief legal-document) *c-long-sep*) ;; 总 协定

  ((adj-time-period noun) *c-very-rare*)
  ((adj-time-period money-related) *c3*) ;; e.g. 月 工资
  ((adj-time-period amount-of) *c3*) ;; 月 总额
  ((adj-time-period price) *c4*) ;; 月 平均价

  ((adj-word-xian1 plant) *c-close-sep*) ;; 鲜花

  ((adj-price system) *c-close-sep*) ;; 低价 系统

  ((adj-word-zero-sum game-like) *c-close-sep*)

  ((adj-word-zhuan1 human-suffix) *c-common*) ;; not prefer 专长

  ((adj-word-unjust money-related-attribute) *c-close-sep*) ;; 不義之財
  
  ((adj-shape media) *c-prefer*) ;; 平面 媒体 
  
  ((adj-shape ADVERTISING) *c-close-sep*) ;; 平面 广告 

  ((adj-on-competition competition) *c-prefer*) ;; 男单 比賽
  ((adj-on-competition prize-ranks) *c-close-sep*) ;; 男单 冠军

  ((adj-on-competition v-chou1-qian1) *c-close-sep*) ;; 双打 抽签 

  ((adj-word-chemical material) *c-prefer*) ;; 化学 纤维

  ;; '种子' could mean candidate player in sports
  ((ADJ-ON-COMPETITION SEED-LIKE) *c-close-suffix*) ;; '男单 种子', '女单 种子'

  ((ADJ-SHAPE-UA NETWORK-LIKE) *c-close-suffix*) ;; 平面 网路

  ((adj-word-negative num) *c-close-sep*)
  ((adj-word-negative number) *c-close-sep*)

  ((adj-on-cloths clothing) *c-close-sep*)

  ((v-xiang1-guan1 abstract) *c2*) ;; e.g. prefer '相关 (偷拍 视频)' to '(相关 偷拍) 视频'
  ((v-xiang1-guan1 verbs) *c4*)

  ((adj-word-mei3 word-girl) *c-close-sep*) ;; prefer '美' as adj in '美女'

  ((adj-word-tall landscape-water-name) *c4*) ;; not prefer '高 淮河', resulted from subj-pred where pred is adj

  ((adj-word-lian2-xu4 time-period) *c-close-sep*) ;; 连续4天

  ((adj-color info-warning) *c-close-sep*) ;; '橙 預警'? more often '橙色 预警'

  ((adj-on-position noun) *c-long-sep*)
  ((adj-on-position situation) *c-close-suffix*) ;; not prefer '(省级 环境) (监测 中心)'
  ((adj-on-position word-place-center) *c-prefer*) ;; 省级 中心
  ((adj-on-position organization) *c-prefer*)

  ((adj-word-inside adj-color) *c-common*) ;; '内 红色'??

  ((adj-word-night-or-late time-pt) *c1*) ;; prefer '晚' as time, if preceded by other time
  ((adj-word-morning-or-early time-pt) *c1*) ;; prefer '早' as time, if preceded by other time

  ((adj-word-wei3 name-of-ind) *c-common*) ;; want '张大伟 称', not '张大 伟称'

  ((adj-word-dui4 abstract) *c-common*) ;; prefer PP '对', e.g. '对 环境'

  ((adj-word-xi4 pollutant-particles) *c-prefer*) ;; 细悬浮微粒
  )
;; end adj-noun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric noun-mod-as-noun (n))
(def-comb noun-mod-as-noun
  ((t) *c-ignore*)
  ((adj) *c-rare*)
  ((adj-has-de) *c-prefer*)
  ((de-noun-mod) *c-prefer*)
  ((verb) *c-rare*) ;; there is a more direct path for verb-x to be noun
  ((verb-n) *c-rare*)
  ((unit) *c-rare*)
  ((p-pronoun) *c-close-sep*)
  ;; prefer sound to be noun through abstract
  ((sound-literal) *c-fallback*)
  )

(defgeneric noun-de-as-noun (n))
(def-comb noun-de-as-noun
  ;; 'noun + 的' as noun
  ((t) *c-ignore*)
  ;;
  ((animate) *c-prefer*)
  ((inanimate) *c-common*)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun verb-mod-non-stack-trait (a)
  ;; these traits should be appear more than once in a verb-p
  (if (keywordp a)
      a ;; for some verb-mod
      (typecase a
        (place 'verb-mod-place)
        (time-name 'verb-mod-time)
        (pp-verb-mod
         ;; some pp-verb-mod should not be stacked
         ;; some would not normally repeat, but seems ok to do so.
         ;; currently only :verb-mod-CC, may add more
         (if (eq (pp-verb-mod-pp a) :verb-mod-CC)
             (pp-verb-mod-pp a)))
        (conn-thing
         ;; arbitrarily take the first one
         (verb-mod-non-stack-trait (conn-thing-first a)))
        (t nil))))

(defun second-obj-trait-penalty (v)
  (+ (if (trait-value v 'verb-n-n) *c-very-rare* 0)
     (if (trait-value v 'verb-vp) *c-rare* 0)
     (if (trait-value v 'verb-op) *c-rare* 0)
     (if (trait-value v 'verb-sp) *c-rare* 0)))
(defun pp-verb-mod-verb-basic-penalty (a v)
  (let ((pp-word (pp-verb-mod-pp a)))
    (cond ((member pp-word
                   '(:jiao4-teach :jiao4-ask :together-with))
           ;; some pp-verb-mods also introduce co-subject
           (subj-verb (pp-verb-mod-object a) v))
          ;; :verb-mod-CC such as "與", "和", "跟", "同" need special
          ;; handling, to favor it followed by verbs such as '有關', '
          ;; 没關', '無關', '相關', '息息相關'.
          ;; but cannot handle all possible cases, only the common
          ;; ones.
          ((eq pp-word :verb-mod-CC)
           (if (or (is-type-p v 'V-YOU2-GUAN1)
                   (is-type-p v 'V-MO2-GUAN1)
                   (is-type-p v 'V-XIANG1-GUAN1)
                   (is-type-p v 'V-XI1-XI1-XIANG1-GUAN1-A)
                   (is-type-p v 'V-GUAN1-XI5)
                   ;; '不无 关系', but can only check '不无' here
                   (is-type-p v 'V-BU2-MO2)
                   )
               ;; special favored cases
               0
               ;; no special handling
               (subj-verb (pp-verb-mod-object a) v)))
          ;;
          ((eq pp-word :BA)
           ;; additional object
           (typecase v
             (combine-verb
              ;; combine-verb, such as (推 進), only constrain
              ;; the first verb, e.g. (把你) 推進 大海 --> (推 你) (進 大海)
              (score-add
               (second-obj-trait-penalty v)
               (verb-noun (combine-verb-first v) (pp-verb-mod-object a))))
             (verb-n
              ;; single verb, e.g. 浪濤(把你)吞沒 --> 浪濤 (吞沒 你)
              ;; this may recursively give penalty to taking second object
              ;; for BA, not prefer some verbs
              (score-add
               (not-prefer (verb-n-verb v) 'v-for *c-common*)
               ;; TODO: relax for verbs such as '变'? e.g. '中共 把霾 (就 (变了雾))'
               (verb-noun v (pp-verb-mod-object a))))
             (verb0
              ;; more relax in case verb0 with verb-supp-adj, see the
              ;; note in verb-noun-xxx.
              ;; '(把 事) (講 得 簡單)' is ok.
              ;; '(講 得 簡單) 事' is not ok.
              (score-add
               (second-obj-trait-penalty v)
               (verb-noun (verb0-verb v) (pp-verb-mod-object a))))
             (t (score-add
                 (second-obj-trait-penalty v)
                 (verb-noun v (pp-verb-mod-object a))))))
          ;;
          ((eq pp-word :from)
           (typecase v
             (verb-n
              ;; '由 N 形成' not prefer verb-n of 形成
              (not-prefer-verb v 'v-xing2-cheng2 *c-close-suffix*))
             (t *c-ignore*)))
          ;;
          ((eq pp-word :towards-at)
           ;; prefer '(大连 对韩 出口) 已 达 一亿多美元' rather
           ;; than '对韩 出口' as verb-mod.

           ;; TODO: in general, need the preference of each verb on
           ;; each types of PP. This kind of information should be
           ;; available in a verb-frame? But do not have time yet.

           ;; It seems '达' and '达到' do not prefer '对 noun-p'?
           (+ (not-prefer-verb v 'v-da5 *c-rare*)
              (not-prefer-verb v 'v-da5-dao4 *c-rare*)
              ;; '对 XXX 删除'?
              (not-prefer-verb v 'v-shan1-chu2 *c-rare*)))
          ;;
          ((eq pp-word :relative-related-to)
           ;; '有关 XX' not commonly used as verb-mod?
           *c-very-rare*)
          ;;
          ((eq pp-word :wait-for)
           ;; prefer '(等...) (and-then ...)'
           (if (trait-value v 'verb-mod-adv-and-then)
               0
               *c3*))
          ;;
          (t *c-ignore*))))

(defun adv-verb (a v)
  (let ((mt (verb-mod-non-stack-trait a))
        (a-s (typecase v
               (conn-thing
                (score-min (adv-verb a (conn-thing-first v))
                           (adv-verb a (conn-thing-second v))))
               (combine-verb
                (score-max (adv-verb a (combine-verb-first v))
                           (adv-verb a (combine-verb-second v))))
               (verb0
                (adv-verb a (verb0-verb v)))
               (verb-n (adv-verb a (verb-n-verb v)))
               (verb-close-supp
                ;; ignore, but since verb-close-supp uses combine-verb,
                ;; give high penalty
                *c-fallback*)
               (t (if (conn-thing-p a)
                      (score-min (adv-verb (conn-thing-first a) v)
                                 (adv-verb (conn-thing-second a) v))
                      (adv-verb-x a v))))))
    ;;
    (when (and (typep a 'adv-word-lian2-xu4)
               (trait-value v 'verb-mod-time))
      ;; prefer '(连续4天) ...' rather than '连续(4天 ...)'
      (setf a-s (score-add a-s *c-close-suffix*)))
    (when (and (trait-value v 'verb-mod-has-pause)
               (not (trait-value a 'verb-mod-has-pause)))
      ;; inner one has pause, but outer one does not, not preferred
      (setf a-s (score-add a-s *c-close-suffix*)))
    (when (and (typep a 'adv-time-at)
               (trait-value v 'verb-mod-time))
      ;; e.g. prefer '(在 (其被带到派出所 时)) 已全部被删除' to '在 ((其被带到派出所 时) 已全部被删除)'
      (setf a-s (score-add a-s *c-close-suffix*)))
    ;;
    (if (and mt (has-trait v mt))
        ;; penalty for repeating some types of verb-mods in one verb-p
        (score-add a-s *c-common*)
        a-s)))

(defun pp-verb-mod-v-is-penalty (a)
  ;; a is pp-verb-mod
  (case (pp-verb-mod-pp a)
    ((:BA :towards-to :on-behalf-of :surrounding :while)
     *c-rare*)
    ((:jiao4-ask :jiao4-teach :yi3) *c-common*)
    ((:for :via) *c4*)
    ((:use) *c-fallback*)
    (t *c-ignore*)))

(defgeneric adv-verb-x (a v))
(def-comb adv-verb-x
  ((t t) *c-ignore*)

  ;;
  (((a (eql :verb-mod-suo3)) verb-n)
   ;; "所" should be followed by verb0 rather than verb-n
   (declare (ignore a))
   *c-very-rare*)
  ;;
  ((passive-voice passive-voice) *c-rare*) ;; should not have double passive voice
  (((a passive-voice) (v t))
   (score-add
    (+
     (not-prefer-verb v 'v-only-active *c-rare*)
     ;; seems '並 verb-p' is not usually used in passive voice, but
     ;; note that '並 passive-voice' is very natural.
     
     ;; NOTE: not quite sure for the case of conn-thing '被 (VP1 並
     ;; VP2)' seems quite natural, and '並' should be CC in this
     ;; case. But '被 ((並 VP1), VP2)' is not natural.
     (not-prefer-trait v 'verb-mod-bing4 *c3*))
    (if (passive-voice-object a)
        (subj-verb (passive-voice-object a) v)
        *c-ignore*)))

  ;;
  (((a pp-verb-mod) (v t))
   (pp-verb-mod-verb-basic-penalty a v))
  ;;
  (((a pp-verb-mod) (v verb-n))
   (typecase (verb-n-verb v)
     (v-is (score-add
            (pp-verb-mod-verb-basic-penalty a (verb-n-verb v))
            (pp-verb-mod-v-is-penalty a)))
     (t (pp-verb-mod-verb-basic-penalty a v))))
  ;;
  (((a pp-verb-mod) (v v-is))
   ;; preference of "是" for some pp-verb-mods
   ;; TODO: may use a table for many verbs
   (score-add
    (pp-verb-mod-verb-basic-penalty a v)
    (pp-verb-mod-v-is-penalty a)))
  ;;
  ((verb v-is) *c-rare*) ;; '是' does not prefer many kinds of verb-mod
  ((passive-voice v-is) *c-rare*) ;; '是' does not prefer many kinds of verb-mod
  ;;
  ((adv verb) *c-prefer*)
  ((adv-degree verb) *c-very-rare*)
  ((adv-degree verb-degree) *c-prefer*)
  ((adv-degree help-verb) *c-prefer*)

  ((adv-once v-hui2) *c-common*) ;; prefer num-unit 一回
  ((adv-once v-shi4-j) *c-rare*) ;; prefer num-noun 一事
  ((hui2 v-shi4-j) *c-rare*) ;; 一回事

  ((adv-yu4 v-bei4-an4) *c-rare*) ;; prefer 预备 案 to 预 备案
  ((adv-ji1-ben3 v-fei4-b) *c-common*) ;; prefer 基本 费 as abstract

  ((adv v-chu3) *c-common*) ;; prefer 处 as place
  ((adv v-zhu3-a) *c-common*) ;; prefer 主 as animate

  ((adv-jue2 v-suan4-shu3) *c-fallback*) ;; prefer 决算 数 to 决 算数

  ((adv-naturally v-jie4-a) *c-fallback*) ;; 自然界

  ((adv-duo1 v-shu3-a) *c-common*) ;; prefer 多数 as count

  ((adv-zheng4 verb) *c-close-sep*) ;; 正 can often modify a noun as a verb

  ((adv-yi4 v-dao4-d) *c-common*) ;; prefer noun 義道

  ((adj verb) *c-close-sep*)

  ;; e.g. want 好心 be adj noun in 好心自然會有好報
  ((adj-word-hao3-xin1 help-verb-hui4) *c-close-suffix*)
  ((adj-word-hao3-xin1 v-has) *c-close-suffix*)

  ((adj-word-wei4 verb) *c-rare*)

  ((adj v-mao4-yi4) *c-close-suffix*) ;; prefer adj noun, e.g. 双边 贸易

  ((adj-word-zhuan1 v-mai4-b) *c-close-sep*) ;; 专卖

  ((adv-word-zhi4 (v verb))
   ;; e.g. 自撰
   (if (= *L2* 1)
       *c-prefer*
       *c-close-suffix*))

  ((adj-word-inside verb)
   ;; not want '.. (位于 商场西门 的 一家店铺) (内 挑选 ...)'
   (prefer-mono *L2* *c-common*))
  ((adj-word-wei4 verb)
   ;; not want '.. (位于 商场西门 的 一家店铺) (内 挑选 ...)'
   (prefer-mono *L2* *c-common*))

  ((adv-time-at (v t))
   ;; often cause trouble with '在' as place-ind and time-ind
   (+ *c-close-suffix* *c-close-sep*))
  ((adv-time-at v-guo5-qu4)
   ;; '在' in '在 过去 ...' is often used as ind
   *c4*)

  ((adv-word-ben3 v-bao4-b) *c-common*) ;; prefer noun '本报'

  ((adj-word-kong1 v-qi4-e) *c4*) ;; prefer '空气' as noun

  ((adv-su4 verb) *c-common*) ;; TODO: whether to keep adv-su4?

  ((adj-word-healthy verb) *c3*) ;; prefer the adj-noun form in most cases
  ((adj-word-healthy v-sheng1-zhang3) *c-long-sep*)
  ((adj-word-healthy v-cheng2-zhang3) *c-close-suffix*)
  ((adj-word-healthy v-fa5-zhan3) *c-long-sep*)

  ((adv-yan2 (v verb))
   (if (= *L2* 1) *c-prefer* *c-close-suffix*))

  ((adv-word-xi4 v-xuan2-fu2) *c4*) ;; prefer adj for in '细 (悬浮 微粒)', rather than '(细 悬浮) 微粒'

  ((adv-word-ke3 v-neng2) *c4*) ;; prefer '可能' as one adv

  ((adj-word-da4-li4 v-bu2-mo2) *c-rare*) ;; '大力 不无'?

  ((adv-gai3 v-bian4-d) *c-rare*) ;; '改变' is itself a verb or noun

  ((adv-ji2 v-shi5-c) *c-common*) ;; prefer '即使' as a whole
  )
;; end adv-verb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adv-verb-tag (a v)
  (cond ((passive-voice-p a)
         ;; normally the verb slot should be empty in a
         (make-passive-voice
          :traits (traits-of a)
          ;; see if need to cancel the effect of :verb-mod-gei3
          :verb (if (has-trait v :verb-mod-gei3)
                    (more-trait v :verb-mod-gei3 nil)
                    v)
          :object (passive-voice-object a)))
        ((eq a :verb-mod-gei3)
         (more-trait v :verb-mod-gei3 t))
        (t (let ((n-vals '(has-non-passive-verb-mod t))
                 (mt (verb-mod-non-stack-trait a)))
             ;; to accumulate a list of name-vals as possible new
             ;; traits
             (when (trait-value a 'verb-mod-has-pause)
               (setf n-vals
                     (cons2 'verb-mod-has-pause t
                            n-vals)))
             (when mt
               ;; non-stackable traits
               (setf n-vals (cons2 mt t n-vals)))
             (when (pp-verb-mod-p a)
               (case (pp-verb-mod-pp a)
                 ((:jiao4-teach :jiao4-ask)
                  ;; for use in subj-verb, may change subject
                  (setf n-vals
                        (cons2 'verb-mod-change-subject
                               (pp-verb-mod-pp a)
                               n-vals)))
                 ((:for :for-wei4 :use)
                  ;; for use in subj-verb, to weakly prefer
                  ;; animate-like subject
                  (setf n-vals
                        (cons2 'verb-mod-prefer-animate-like t
                               n-vals)))
                 ((:BA)
                  ;; see if need to cancel the effect of :verb-mod-gei3
                  (when (has-trait v :verb-mod-gei3)
                    (setf n-vals
                          (cons2 :verb-mod-gei3 nil n-vals)))
                  ;; used two objects?
                  (if (typep v 'verb-n)
                      (setf n-vals
                            (cons2 'verb-n-n t n-vals))
                      ;; used one object only, make it an verb-n to mark it
                      (setf v (make-instance 'verb-n :verb v)))
                  )
                 ))
             (when (typep a 'adv-time-at)
               ;; mark for discouraging '在' as verb-mod in verb-p used for time in 'verb-p time-ind'
               (setf n-vals
                     (cons2 'verb-mod-zai4 t n-vals)))
             (when (typep a 'adv-word-bing4)
               ;; mark for '並', as it is confusing.
               ;; TODO: whether to mark similarly for other ind in pred-v?
               (setf n-vals
                     (cons2 'verb-mod-bing4 t n-vals)))
             ;; for matching with 'although'
             (when (typep a 'adv-but)
               (setf n-vals
                     (cons2 'verb-mod-adv-but t n-vals)))
             (when (typep a 'adv-also)
               (setf n-vals
                     (cons2 'verb-mod-adv-also t n-vals)))
             (when (typep a 'adv-still)
               (setf n-vals
                     (cons2 'verb-mod-adv-still t n-vals)))
             (when (typep a 'adv-and-then)
               (setf n-vals
                     (cons2 'verb-mod-adv-and-then t n-vals)))
             ;;
             (more-trait-with v n-vals)
             ))))

;; end adv-verb-tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe-name-ind-p (a b)
  (and (typep a 'name)
       (typep b 'ind)
       (typep a (category-of-ind b))))
(defun maybe-name-ind (a b penalty)
  ;; if a and b are name and ind respectively of the same parent type,
  ;; given no penalty, otherwise return the penalty
  (if (maybe-name-ind-p a b)
      0
      penalty))

;; NOTE: currently, tag equality is compared using equalp only, to
;; prevent making many objects unnecessarily and cause spurious
;; ambiguity, we memoize up-ind.

;; TODO: Alternatively, change the way tag equality if tested?
(defparameter *up-ind-hash* (make-hash-table :test 'eq))
(defun clear-up-ind () (clrhash *up-ind-hash*))
(defun up-ind-h (a)
  (let ((up-class (and (typep a 'ind)
                       (category-of-ind a))))
    (if up-class
        (replace-traits-to (make-instance up-class)
                           (traits-of a))
        a)))
(defun up-ind (a)
  (or (gethash a *up-ind-hash*)
      (setf (gethash a *up-ind-hash*)
            (up-ind-h a))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-conn supp-adj (s a) supp-adj-x)
(defgeneric supp-adj-x (s a))
(def-comb supp-adj-x
  ((t t) *c-ignore*)
  ;;
  ((adv adj) *c-rare*)
  ((adv-degree adj) *c-adj-adv-supp*)

  ((adj adj) *c-adj-adv-supp*)
  ;; e.g. num-unit, rough-amount
  (((s t) (a adj)) (mod-adj s a))
  )
;; end supp-adj

(defun-conn mod-adj (m n) mod-adj-x)
(defgeneric mod-adj-x (m n))
(def-comb mod-adj-x
  ((t t) *c-ignore*)
  ;;
  ((adv adj) *c-close-suffix*)

  ((unit adj) *c-very-rare*)

  ((adv-word-jiang1 adj) *c-common*) ;; e.g. not prefer 将 大力

  ((bei4 adj) *c4*)
  ((fen1 adj) *c4*)
  ;;
  ((unit-weight adj-on-weight) *c4*)
  ;;
  ((unit-length adj-thin-or-thick) *c4*)
  ((unit-length adj-on-length) *c4*)
  ((unit-length adj-word-tall) *c4*)
  ((unit-length adj-size) *c4*)
  ((unit-length adj-on-distance) *c4*)
  ((unit-length adj-on-wideness) *c4*)
  ((unit-length adj-on-dimension) *c4*)
  ;;
  ((unit-area adj-word-big) *c4*)
  ((unit-area adj-word-small) *c4*)
  ;;
  ((rough-amount adj) *c4*)
  ((ask-amount adj) *c4*)
  ;;
  ((adv-once adj) *c-common*)
  ;;
  ((adj adj-word-sharp) *c-common*) ;; prefer 利 as abstract
  ((adj adj-word-jin4) *c-common*) ;; prefer 勁 as abstract
  ((adj adj-word-curved) *c-common*) ;; prefer 曲 as abstract
  ((adj adj-word-prestige) *c-common*) ;; prefer 威 as abstract
  ((adj adj-word-long) *c-common*) ;; prefer 長 as human-suffix
  ((adj adj-word-sheng1) *c-common*) ;; prefer 生 as human-suffix 新生
  ((adj adj-word-fang1) *c-common*) ;; prefer 方 as human-suffix
  ((adj adj-gold-silver) *c-common*) ;; prefer 金, 銀 as metal
  ((adv adj-gold-silver) *c-common*) ;; prefer 金, 銀 as metal
  ((adj-word-sheng1 adj-word-chang2-qi2) *c-common*) ;; prefer 生长 期 to 生 长期
  
  ((adj adj-word-famous) *c-common*) ;; prefer 名 to mean name
  ((adv adj-word-famous) *c-close-suffix*)

  ((adj adj-word-se4) *c-close-suffix*)
  ((adj-color adj-word-se4) *c-rare*) ;; prefer 色 as color
  ((adj-word-se4 adj) *c-rare*) ;; not want '蓝色 短袖' be '蓝 (色 短袖)'

  ((adj-word-qing1 adj-word-mo4) *c-common*) ;; 清末
  ((adj adj-word-mo4) *c-common*)

  ((adj adj-word-fake) *c-common*) ;; prefer *+假 to mean holiday

  ((adj adj-word-original) *c-common*) ;; prefer 原 as plain

  ((adv-mistakenly adj-word-cha4) *c-common*) ;; prefer 误差 as abstract

  ((adj adj-word-handsome) *c-common*) ;; prefer 帥 as animate
  
  ((adj adj-word-bang4) *c-common*) ;; prefer 棒 as inanimate

  ((adj adj-word-mu3) *c-common*) ;; prefer 母 as animate

  ((adj adj-word-oily) *c-common*) ;; prefer 油 as oil

  ((adj adj-word-gu3) *c-common*) ;; prefer 古 as time

  ((adv-half adj-word-kong1) *c-close-sep*) ;; would prefer 半空 (adv adj) to have the same score as 半空 (fraction sky)

  ((adj adj-word-jing1) *c-common*) ;; prefer 精 as inanimate

  ((adj adj-word-huang1) *c-common*) ;; prefer 荒 as inanimate

  ((adj adj-word-valuable) *c-common*) ;; prefer 寶 as animate or inanimate
  ((unit adj-word-qiao1-qiao1) *c-common*) ;; want 悄悄 be adv in 一個個都悄悄

  ((adj-shang4 adj) *c-common*) ;; not prefer 街 (上 嘰嘰喳喳)

  ((adj-word-he2 adj) *c-rare*) ;; 和 is easily confused with CC

  ((adv adj-word-inside) *c-common*)  ;; not want '纷纷 内'

  ((adv-word-bu4-ru2 adj) *c-rare*) ;; prefer '不如' as verb, e.g. in '新 不如 舊'

  ((adj-word-fang1 adj-word-you2-ke2-neng2) *c-rare*) ;; in this case, '方 有可能', prefer '方' as adv

  ((adv-time-at adj) *c-rare*) ;; prefer '在' as verb or other ind

  ((adv-time-frequency adj) *c-common*) ;; not want '第19次 全'
  )
;; end mod-adj

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; penalty for putting a dir after a noun to give a place
(defun-conn noun-dir-as-place (n d) noun-dir-as-place-x)
(defgeneric noun-dir-as-place-x (n d))
(def-comb noun-dir-as-place-x
  ((t t) *c-fallback*)
  ;;
  ((num t) *c-common*)
  ((number t) *c-common*)
  ((place t) *c-common*)
  ((verb t) *c-common*) ;; 回游

  ((place dir-suffix) *c-prefer*)
  ((place dir) *c-prefer*)
  ((place pos-suffix-fang1) *c-common*) ;; prefer animate for most cases of 方, e.g. 馆方

  ((place-with-dir dir) *c-rare*) ;; want 境 內外, not 境內 外

  ((paper-like-ind t) *c-very-rare*) ;; 單邊 confusion
  ((brochure-ind t) *c-very-rare*)

  ((sun-word t) *c-very-rare*) ;; 日方
  ((country-abbr t) *c-very-rare*)

  ((stone-ind pos-suffix) *c-very-rare*) ;; 石头
  ((adj-color t) *c-very-rare*) ;; 白头
  ((v-bai2 pos-suffix) *c-very-rare*) ;; 白头
  ((animate pos-suffix) *c-rare*) ;; animate + 頭, prefer to mean the head

  ((region t) *c-prefer*)
  ((ring-like-tool t) *c-rare*) ;; prefer 圈 as region in 圈内
  ((region-ind t) *c-close-suffix*) ;; prefer '(试衣 间) 内' to '试衣 (间 内)'

  ((abstract pos-suffix-mian4) *c-rare*) ;; prefer aspect for abstract
  ((animate pos-suffix-mian4) *c-very-rare*) ;; prefer 面 as face
  ((adj pos-suffix-mian4) *c-rare*)
  ((inanimate pos-suffix-mian4) *c-prefer*)

  ((animate pos-suffix-mian4-qian2) *c-prefer*)

  ((abstract pos-suffix-fang1) *c-common*) ;; prefer 检方 as animate

  ((time-name dir) *c-rare*) ;; prefer it as time

  ((land dir-up-down) *c-close-sep*) ;; e.g. 地下

  ((country-abbr dir) *c-rare*)

  ((study-ind dir-before) *c-common*) ;; prefer 学前 as time

  ((organization-or-platform-ind dir-up-down) *c-close-suffix*) ;; prefer 台 as inanimate furniture

  ((inanimate dir-up-down) *c-close-suffix*) ;; e.g. 纸上

  ((basis-ind dir-up-down) *c-common*) ;; 底下

  ((body-part dir) *c-close-sep*) ;; 体内
  ((abstract dir) *c-close-suffix*)
  ((organization-or-event dir) *c-close-sep*)

  ((adj pos-suffix-duan1) *c-prefer*) ;; e.g. 弊端
  ((verb pos-suffix-duan1) *c-close-sep*) ;; e.g. 开端
  ((abstract pos-suffix-duan1) *c-close-suffix*)
  ((animate pos-suffix-duan1) *c-close-suffix*)

  ((animate dir-inside-outside) *c-rare*)
  ((word-wang2 dir-after) *c-rare*) ;; ambiguity with 王后

  ((street-ind dir) *c-common*) ;; not prefer 知 (道 裡面)
  ((theory-word-dao4 dir) *c-common*) ;; not prefer 知 (道 裡面)
  ((v-know dir) *c-very-rare*)

  ((de-noun-mod pos-suffix-ji4) *c-close-suffix*) ;; verb-p 之际 is more commonly time than place
  )
;; end noun-dir-as-place

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun noun-mod-trait (m L)
  (typecase m
    (unit 'noun-mod-has-unit)
    (adj-has-de 'noun-mod-has-de)
    (de-noun-mod 'noun-mod-has-de)
    (verb-n (if (> L 2)
                'noun-mod-has-long-verb-n
                nil))
    (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the common part of mod-de-noun and mod-noun
(defgeneric mod-noun-common (m n))
(def-comb mod-noun-common
  ;; put a default penalty, to save us trouble
  ((t t) *c-rare*)
  ;;
  (((m adj) (n adj))
   ;; normally n should be converted to noun form with some penalty
   (mod-adj m n))
  (((m adj) (n noun))
   (+ (adj-noun m n)
      *c0*))

  ;; adj-has-de, e.g. '高的', ' 漂亮的' could be used as noun, but we
  ;; know too little to give proper constraint, so give a default one
  ((noun adj-has-de) *c-common*)

  ;; treat verb as noun, but with a slight penalty to prefer noun
  ((adj verb) *c1-2*)
  ((verb verb) *c2-3*)
  ((noun verb) *c4*)

  ;;
  ((time-pt time-pt) *c0*)
  
  ;;
  (((m adj-has-de) (n noun))
   (adj-noun (adj-has-de-the-adj m) n))
  ((de-noun-mod noun) *c-common*)
  (((m unit) (n noun))
   (noun-unit n m))
  ((q-pronoun verb) *c-rare*)
  ;;
  ((rough-amount non-verb-noun) *c-close-suffix*)

  ((rough-amount human-suffix) *c-fallback*)
  ((ask-amount human-suffix) *c-fallback*)

  ((rough-amount occupation-suffix) *c-fallback*)
  ((ask-amount occupation-suffix) *c-fallback*)

  ((rough-amount animate-suffix) *c-fallback*)
  ((ask-amount animate-suffix) *c-fallback*)

  ;; treat verb and adj as noun
  ((verb-n noun) *c-verb-x-noun-mod-noun*)
  ((a-subj-pred non-verb-noun) *c2*)
  ((verb noun) *c2*)
  ;; (subj-verb animate verb) default to *c-prefer*, so not adding here.
  (((v verb) (n animate)) *c2*)
  (((v verb) (n inanimate)) (subj-verb n v))
  (((v verb-n) (n inanimate)) (subj-verb n v))
  (((m noun) noun)
   (+ *c3*
      (not-prefer m 'ind *c-close-sep*)))
  ((num noun) *c3*)
  ((number noun) *c3*)
  ((p-pronoun (n noun))
   (+ *c0*
      ;; prefer 'pronoun (noun suffix)' to '(pronoun noun) suffix', e.g. '該 (男 子)'
      (if (trait-value n 'with-noun-suffix)
          0
          (* *c-close-sep* 2))))
  ((q-pronoun noun) *c1*)

  ;; default
  ((verb-n great-difficulty) *c1*)
  ((verb great-difficulty) *c-common*) ;; (年净增二十亿元)大关, 難關
  
  ((v-ping2-yi4 place) *c-rare*) ;; prefer verb-noun
  
  ((v-give noun) *c-common*) ;; prefer verb-noun for 給
  ((v-want-to noun) *c-common*) ;; prefer verb-noun for 想

  ((dir-inside-outside aspect-word-mian4-ind) *c-common*) ;; prefer 外面 as place-with-dir
  ((dir-inside-outside noodle-like-ind) *c-common*) ;; prefer 外面 as place-with-dir
  ((dir-inside-outside face-ind) *c-common*) ;; prefer 外面 as place-with-dir

  ((word-sum-he2 noun) *c-common*) ;; prefer 和 as connective

  ((v-discover animate) *c-common*)
  ((v-know noun) *c-rare*) ;; prefer 知道 as verb-noun
  ((v-use animate) *c-very-rare*) ;; prefer 用 as verb-noun or verb-mod
  ((verb penalty-word-guo4) *c-common*) ;; prefer 過 as verb-supp

  ((v-into noun) *c-common*) ;; prefer verb noun for 進
  ((v-is noun) *c-rare*) ;; prefer verb noun for 是
  ((v-tao1 noun) *c-rare*) ;; prefer verb noun for 掏
  ((v-ji3-ji3-a noun) *c-rare*) ;; prefer pronoun 自己 when combined with noun
  ((v-to idea-opinion-ind) *c-rare*) ;; prefer 去 + 見 as two verbs
  ((v-see noun) *c-very-rare*) ;; prefer verb noun for 見
  ((v-zhu4-b noun) *c4*) ;; prefer verb-x noun for 注
  ((v-yun2-b noun) *c-common*) ;; prefer 云 as inanimate
  ((v-hua4-c noun) *c4*) ;; prefer 画 as abstract, e.g. 画质, 画風
  ((v-ming4 noun) *c4*) ;; prefer 命 as abstract
  ((v-zu2 noun) *c4*) ;; prefer 足 as inanimate
  ((v-shi1-b noun) *c4*) ;; prefer 师 as animate
  ((v-zi1 noun) *c4*) ;; prefer 资 as abstract
  ((v-xing2 noun) *c4*) ;; prefer 形 as abstract
  ((v-bu4-a noun) *c4*) ;; prefer 步 as abstract
  ((v-xi3-a noun) *c4*) ;; prefer 喜 as abstract
  ((v-bu4-b noun) *c4*) ;; prefer 布 as inanimate
  ((v-suo3-a noun) *c4*) ;; prefer 索 as inanimate, or verb-x noun
  ((v-guang1 noun) *c4*) ;; prefer 光 as light
  ((v-ma2 noun) *c4*) ;; prefer 麻 as inanimate
  ((v-zhi5 noun) *c4*) ;; prefer 指 as inanimate
  ((v-huan2 noun) *c4*) ;; prefer 环 as inanimate
  ((v-niao4-b noun) *c4*) ;; prefer 尿 as inanimate
  ((v-wang3-a noun) *c4*) ;; prefer 网 as net
  ((v-du2-a noun) *c4*) ;; prefer 毒 as inanimate poison
  ((v-wait noun) *c-common*) ;; prefer 等 as position-of-ind or as verb-x noun
  ((v-has noun) *c-common*) ;; prefer verb-x noun
  ((v-ka3 noun) *c4*) ;; prefer 卡 as inanimate
  ((v-xia4-c noun) *c4*) ;; prefer 下 as dir

  ((v-shu4-a noun) *c-common*) ;; prefer 树 as plant, or verb-x noun form

  ((v-qi4-e noun) *c-common*) ;; prefer 气 as gas, inanimate

  ((v-zhan4-c inanimate) *c4*) ;; prefer 站 as station
  
  ((v-wa3 noun) *c-common*) ;; prefer inanimate 瓦

  ;;((v-jiang1-a noun) *c-common*) ;; prefer 将+* as verb-x, or 将 as animate

  ((place dir) *c-common*) ;; encourage the combination using more restrictive rules using noun-dir-as-place
  ((place dir-suffix) *c-common*)

  ((v-zhen4-a noun) *c-common*) ;; prefer place 镇
  
  ((v-guang3-gao4 noun) *c-rare*) ;; prefer either abstract 广告, or verb-x noun
  
  ((verb checking-of-ind) *c-common*) ;; prefer verb form of 檢
  
  ((v-bie4 noun) *c-rare*) ;; prefer 别 as p-pronoun

  ((v-ban3 noun) *c-common*) ;; prefer 板 as inanimate
  ((v-tu2-b noun) *c-common*) ;; prefer 图 as inanimate

  ((v-hua1 noun) *c-common*) ;; prefer 花 as inanimate

  ((v-guan1-xi5 noun) *c-common*) ;; prefer 关系 as abstract

  ((adj-word-god-like s-lun4) *c-common*) ;; prefer 神 as god

  ((v-tong2 noun) *c-common*) ;; prefer verb-x 同
  ((v-yue4-d noun) *c-common*) ;; prefer 乐 as abstract
  ((v-xian2-c noun) *c-common*) ;; prefer 闲 as adj
  ((v-huo2 noun) *c-common*) ;; prefer 活 as adj
  ((v-hao4 noun) *c-common*) ;; prefer 好 as adj
  
  ((v-cang2 noun) *c-common*) ;; prefer 藏 as place

  ;;((name (n noun))
  ;; (+ *c-common* (prefer n 'ind *c-common*)))

  ((v-zhan3-a noun) *c-common*) ;; prefer 展 as abstract
  ((v-sheng1-huo2 noun) *c-common*) ;; prefer 生活 as animate-attribute

  ;;((light noun) *c-close-suffix*) ;; prefer 光 as light than as adj
  ((adj-word-light noun) *c-common*)
  
  ((physical-sky-ind noun) *c-common*) ;; prefer 空 as adj
  ((word-burnt-coke noun) *c-rare*) ;; prefer 焦 as adj
  
  ;;((verb bullet-like) *c-close-suffix*)
  ((dir bullet-like) *c4*) ;; prefer 中子 弹 to 中 子弹
  ((adj-word-original bullet-like) *c-common*) ;; prefer 原子 弹 to 原 子弹
  
  ((explanation-ind noun) *c-common*) ;; prefer 解 as verb
  ;;((v-xie4-d noun) *c-close-suffix*)
  
  ;;((valuable-treasure-ind noun) *c-close-suffix*) ;; prefer 寶 as inanimate to adj
  ((adj-word-valuable noun) *c-common*)
  
  ((pointy-tip-ind inanimate) *c-common*)
  ;;((adj-word-pointy inanimate) *c-close-suffix*) ;; prefer 尖 as adj to inanimate
  
  ;;((adj-gold-silver inanimate) *c-close-suffix*) ;; prefer 金, 銀 as color
  ((gold-or-cost-ind inanimate) *c-common*) ;; 金 less likely to be gold
  ((word-standard noun) *c-rare*) ;; prefer 標準 as adj
  ((help-verb-hui4 noun) *c-rare*) ;; prefer 會 as a noun, as meeting
  ((adj dynasty-word-liang) *c-very-rare*) ;; 梁 and 樑 are unfortunately the same character in simplified chinese, but 梁is also a surname and name of dynasty. prefer 樑 most of the time.
  ((inanimate dynasty-word-liang) *c-very-rare*)
  ((verb dynasty-word-liang) *c-rare*)
  ((v-zhang3-b time-name) *c-rare*) ;; prefer 长 as adj for time
  ((word-landscape-plain-ind noun) *c-rare*) ;; prefer 原 as adj
  ;;((p-pronoun-you3 noun) *c-rare*) ;; prefer 有 as verb
  ((holiday-ind noun) *c-rare*) ;; prefer 假 as adj
  ;;((holiday-ind time-name) *c-close-suffix*)
  ((adj-word-ma2 inanimate) *c-rare*) ;; prefer 麻 as material
  ((v-di1-b noun) *c-rare*) ;; prefer verb-x for 提
  ((noun word-ding1) *c-rare*) ;; prefer 丁 as human suffix
  ;;((adj-word-shi2 inanimate) *c-close-suffix*) ;; 實
  ;;((adj-word-shi2 abstract) *c-close-suffix*) ;; 實
  ((truth-ind inanimate) *c-rare*)
  ((name animate-attribute) *c-rare*)
  ((name inanimate-attribute) *c-rare*)
  ;;((num noun) *c-common*)
  ;;((time-name verb) *c-close-suffix*)
  ((dynasty verb) nil) ;; 汉学, 代售, etc
  ((time-name v-zhuang1-a) *c-rare*) ;; 时装
  ((sun-word noun) *c-rare*) ;; decrease the default
  ((sun-word animate) *c-rare*)
  ((moon-word abstract) *c-common*) ;; 月 收入
  ((furniture-tai2 inanimate) *c-common*)
  ;;((adj-word-te4 noun) *c-common*)
  ((cost-or-book-ind noun) *c-rare*) ;; prefer p-pronoun 本
  ((v-chu2-a noun) *c-rare*) ;; prefer verb-x 蓄
  ((v-zhong4-b noun) *c-very-rare*) ;; prefer verb-x 中, or country-abbr
  ((v-mo2-b noun) *c-fallback*) ;; prefer verb-x 无
  ((job-word-gong1-zuo4 noun) *c-rare*) ;; prefer verb 工作
  ((v-ke1-a noun) *c-rare*) ;; prefer abstract 科
  ((word-research noun) *c-rare*) ;; prefer verb 研究
  ((kindness-favour noun) *c-rare*) ;; 德 ambiguity
  ((video-producer noun) *c-rare*) ;; prefer the verb form of 製片
  ((marks noun) *c-rare*) ;; prefer verb form of 分
  ((style-trend-ind noun) *c-rare*) ;; prefer 風 as wind
  ((queen noun) *c-fallback*) ;; prefer 后 as 後
  ((v-bing4-b noun) *c-rare*) ;; prefer 病 as inanimate form
  ((v-shu1-a noun) *c-rare*) ;; prefer 书 as inanimate
  ;;((adj-word-famous noun) *c-close-suffix*) ;; prefer 名 as adj
  ((name-or-reputation-ind noun) *c-rare*)
  ((v-shi4-j noun) *c-rare*) ;; prefer 事 as abstract thing
  ((word-tuo1-er noun) *c-rare*) ;; prefer verb 托儿
  ((v-ping2-c noun) *c-rare*) ;; prefer adj 平
  ((v-hun1-a noun) *c-rare*) ;; prefer abstract 婚
  ((flow-of-ind noun) *c-rare*) ;; prefer verb 流
  ((v-sheng3 noun) *c-rare*) ;; prefer either 省 as province or verb-x form
  ((adj-word-oily noun) *c-very-rare*) ;; prefer 油 as inanimate
  ;;((oil noun) *c-close-suffix*)
  ((v-lu4 noun) *c-rare*) ;; prefer verb-x 录
  ((v-jia1-a noun) *c-rare*) ;; prefer verb-x 加
  ((verb sun-word) *c-rare*) ;; prefer 日 as time
  ((abstract sun-word) *c-rare*)
  ((physical sun-word) *c-rare*)
  ((time-name sun-word) *c-rare*)
  ((adj sun-word) *c-rare*)
  ;;((holiday (n time-period)) (prefer n 'ind *c-rare*))

  ((place moon-word) *c0*) ;; want moon in 水中 月
  ((seasons moon-word) *c0*) ;; want moon in 秋月
  
  ((noun word-forehead) *c-rare*) ;; prefer 額 as amount
  ((verb word-forehead) *c-rare*)
  ((abstract word-forehead) *c-rare*)
  ((p-pronoun word-forehead) *c-rare*)
  ((place word-forehead) *c-rare*)
  ((adj-gold-silver word-forehead) *c-common*)
  
  ;; animate
  ;;((abstract animate) *c-fallback*)
  ;;((inanimate animate) *c-fallback*)
  (((m animate) (n animate))
   (+ *c3*
      (if (maybe-name-ind-p m n) (- *c-close-sep*) 0)
      (if (has-trait m 'secondary)
          *c-close-suffix*
          0)))
  ;;((place (n animate))
  ;; (+ *c-close-suffix* (prefer n 'ind *c-very-rare*)))
  ;;((province animate) *c-close-suffix*) ;; 省议长

  ((pronoun pronoun-word-self) *c0*)
  ((num word-ge4-ren2) *c-rare*) ;; prefer (num 個) 人
  ((number word-ge4-ren2) *c-rare*) ;; prefer (num 個) 人

  ((v-jiao1-c word-human-huang2) *c4*) ;; prefer abstract 教 in 教皇
  
  ((v-xian2-b word-human-fan4) *c4*) ;; prefer abstract 嫌 in 嫌犯
  
  ((v-du2-a word-human-xiao1) *c4*) ;; prefer inanimate 毒 in 毒枭
  
  ((word-mao2 occupation) *c-rare*) ;; prefer 毛 as surname, e.g. 毛主席

  ((country-abbr word-guest) *c0*) ;; e.g. 台客
  ((country-abbr committee-member) *c0*) ;; e.g. 台委员

  ((hole-ind word-zi3) *c-rare*) ;; 孔子

  ((idea-opinion-ind human) *c-common*) ;; prefer 见证 人 to 见 证人

  ((place animate) *c2-3*)

  ((v-quan1 word-nei4-ren2) *c-fallback*) ;; prefer 圈内 人 to 圈 内人
  ((ring-like-tool word-nei4-ren2) *c-fallback*)
  ((region-ind word-nei4-ren2) *c-fallback*)
  ((place word-nei4-ren2) *c-fallback*)

  ((country-abbr-america human) *c-common*) ;; 美人
  ((country-abbr-mongolia human) *c-common*) ;; prefer 蒙古 人 to 蒙 古人
  ((v-meng2 human) *c-common*)
  ((v-man3 human) *c-common*) ;; prefer 满 as race
  
  ((dynasty-han4 human) *c-common*) ;; prefer 漢 as race for human
  ((human-han4 noun) *c-common*) ;; prefer 漢 as either race, or dynasty
  
  ;;((country-abbr animate) *c-close-suffix*) ;; e.g. 日寇
  ;;((country-abbr human) *c-close-suffix*)

  ((dir human) *c-common*) ;; prefer 中华 人 to 中 华人

  ((v-tong2 human) *c-rare*) ;; e.g. prefer 同路 人 to 同 路人
  
  ;;((adj-size animate) *c-close-suffix*)
  ;;((adj-word-big animate) *c-close-suffix*)
  
  ((word-mu3 kinship) *c3*)
  ((adj-word-mu3 kinship) *c-common*) ;; prefer animate 母
  ((kinship kinship) *c-common*) ;; discourage it for cases like 大姑 娘 instead of 大 姑娘; 小姑 娘 instead of 小 姑娘
  ((kinship human-word-po2) *c-common*) ;; prefer 老 婆婆 to 老婆 婆
  ((human human-word-po2) *c-common*) ;; prefer 大 老婆 to 大老 婆
  ;;((adj kinship) *c-close-suffix*)
  ((v-jiao1-c kinship) *c4*) ;; prefer abstract 教 in 教父
  
  ;;((political-party word-representative) *c-close-suffix*) ;; 党 代表 instead of (党 代) 表
  
  ((country-abbr word-media) *c0*)
  
  ((adj human-ind) *c1*)
  ((verb human-ind) *c2*) ;; prefer verb to noun for modifying human
  ((noun human-ind) *c3*)
  ((adj-word-flat human-ind) *c-rare*) ;; prefer 平民 as one term
  ((human-name human-ind) *c-rare*)
  ((dynasty-word-tang2 human-ind) *c-common*) ;; prefer 唐 as race-name
  ((adj-word-full human-ind) *c-common*) ;; prefer 满 as race
  ;;((jia1 human-ind) *c-close-suffix*) ;; 一家人
  ((surname human-ind) *c4*) ;; e.g. want 東方 to be dir in 東方人

  ((num family-member) *c-fallback*) ;; prefer 一家 人 to 一 家人
  ((number family-member) *c-fallback*) ;; prefer 一家 人 to 一 家人

  ((adj-word-full race-or-group) *c-common*) ;; prefer 满 as race
  
  ((animate ns-men2) *c0*)
  ;;((political-party ns-men2) *c-common*)
  ;;((race ns-men2) *c-common*)
  ;;((can-be-job ns-men2) *c-common*)

  ;; NOTE: s-men2 removed, only ns-men2 is retained
  ;;((animate s-men2) *c-common*) ;; prefer the ns-men2
  ;;((inanimate s-men2) *c-common*) ;; prefer the ns-men2
  ((place ns-men2) *c-rare*)
  ;;((verb s-men2) ;; prefer animate form if possible
  ;; *c3-4*)

  ((fertilizer-or-fat-ind animate) *c-rare*) ;; prefer 肥 as adj

  ((animate (n animate))
   (if (typep n 'relationship)
       *c0*
       *c2*))

  ((pronoun (n animate))
   (if (typep n 'relationship)
       *c0*
       *c-pronoun-animate*))
  ((pronoun (n occupation))
   ;; e.g. "我 老師"
   (if (typep n 'relationship)
       *c0*
       *c3*))
  ((pronoun-zhi noun) *c-rare*) ;; '之' is used as object, and often alone
  ;;((p-pronoun animate) *c-short-close-suffix*)
  ;;((organization-ind animate) *c-short-close-suffix*)
  ;;((animate-attribute (n animate))
  ;; (+ *c-close-suffix* (prefer n 'ind *c-common*)))
  ;((verb (n animate)) (prefer n 'ind *c-fallback*))

  ;;((adj-word-big word-boss) *c-close-suffix*) ;; 大 老板 rather than 大老 板

  ((v-jiao1-c friend-ind) *c4*) ;; prefer 教 as abstract
  ((adj-word-difficult friend-ind) *c-rare*) ;; 難友
  ((v-yue4-d friend-ind) *c-rare*) ;; 乐友
  ;;((music-ind friend-ind) *c-close-suffix*)

  ((v-jiao1-c word-human-tu2) *c4*) ;; prefer 教 as abstract
  
  ((v-jian4-zhu4 group-of-people-ind) *c-rare*) ;; prefer 建筑 as inanimate
  ((animate group-of-people-ind) *c-common*) ;; prefer ns-qun2
  ((inanimate group-of-people-ind) *c-common*) ;; prefer ns-qun2
  ((building group-of-people-ind) *c-common*)  ;; prefer ns-qun2
  ((place group-of-people-ind) *c-common*)
  ((num group-of-people-ind) *c-common*) ;; prefer num-unit 群
  ((number group-of-people-ind) *c-common*) ;; prefer num-unit 群

  ((animate word-cluster) *c-common*) ;; prefer ns-qun2
  ((inanimate word-cluster) *c-common*) ;; prefer ns-qun2
  ((building word-cluster) *c-common*)  ;; prefer ns-qun2
  
  ((religion-ind bird-ind) *c-rare*) ;; prefer verb-x 教鸟
  
  ;;((organization occupation) *c-close-suffix*)
  ((country-abbr-america occupation) *c2-3*)
  ((adj-word-mei3 occupation) *c-common*) ;; prefer 美 as country-abbr
  ((country-abbr-america human) *c2-3*)
  ((adj-word-mei3 human) *c4*)
  ((word-beauty human) *c4*) ;; prefer 美 as either country-abbr or adj

  ;; 軍
  ((adj-word-mei3 army-like) *c-rare*) ;; 美军 
  ((adj-word-qing1 army-like) *c-rare*) ;; 清军
  ((kindness-favour army-like) *c-rare*) ;; 德军
  ((country-abbr army-like) *c0*)
  ((dynasty army-like) *c0*)
  ;;((dir army-like) *c-close-suffix*)
  ;;(((m place) army-like)
  ;; (+ *c-close-suffix* (prefer m 'ind *c-close-suffix*)))
  ;;((verb army-like) *c-close-suffix*)
  
  ;; animate-suffix
  ;; 方
  ((country-abbr s-fang1) *c0*)
  ((p-pronoun s-fang1) *c0*)
  ((q-pronoun s-fang1) *c0*)
  ((animate s-fang1) *c4*)
  ((adj s-fang1) *c1*)
  ((dir s-fang1) *c-common*) ;; prefer e.g. 東方 as dir-suffix-place
  ((adj-word-liang2 s-fang1) *c-common*) ;; 良方
  ((v-xia4-c s-fang1) *c-rare*) ;; 下方
  ((adj-shang4 s-fang1) *c-common*) ;; 上方
  ((queen s-fang1) *c-rare*) ;; 后方 confused with 後方
  ((sun-word s-fang1) *c-rare*) ;; 日方
  ((land-ind s-fang1) *c-very-rare*) ;; 地方
  
  ;; noun-suffix
  ((noun noun-suffix) *c2*)

  ((place ns-zuo4) *c-rare*)

  ((noun ns-tuan2) *c-common*) ;; prefer the abstract organization 團
  ((inanimate ns-tuan2) *c2*) ;; except for inanimate, which could mean a ball-like stuff of the thing
  
  ((vegetable ns-ban3) *c-common*) ;; for 菜板, prefer 板 as inanimate
  ((place ns-ban3) *c-common*)
  ((container ns-ban3) *c-common*)
  ((machine ns-ban3) *c-common*)
  ((furniture ns-ban3) *c-common*)
  
  ;;((animate general-suffix) *c-prefer*)
  ((animate ns-fang1) *c0*)
  ((queen ns-fang1) *c-rare*) ;; 后方 confused with "後方"
  ((money-related ns-fang1) *c4*) ;; prefer ns-fang1, e.g. 资方
  
  ((place ns-long2) *c-very-rare*) ;; prefer 龍 as animal

  ;;((inanimate general-suffix) *c-inanimate-suffix*)
  ;;((inanimate ns-men2) *c-inanimate-suffix*)
  
  ((plant ns-hua1) *c-rare*) ;; prefer 花 as flower instead of shapes for plants
  ((tree ns-hua1) *c-rare*)
  ((plant-or-drink ns-hua1) *c-rare*)
  
  ((inanimate ns-mu4) *c-rare*)
  ((abstract ns-mu4) *c0*)
  
  ((sports ns-dui4) *c-rare*) ;; prefer 隊 as animate for sports
  ((place ns-dui4) *c-rare*) ;; prefer 隊 as animate for place
  ((inanimate ns-dui4) nil)
  ((abstract ns-dui4) *c-rare*)
  ((transportation ns-dui4) *c3*)
  
  ((inanimate ns-bu4) *c-very-rare*)
  ((human-name ns-bu4) *c-common*)
  ((animate ns-bu4) *c-rare*) ;; prefer 部 as organization
  ((abstract ns-bu4) *c-rare*) ;; prefer 部 as organization
  ((body-part ns-bu4) *c0*)
  ((face-ind ns-bu4) *c0*)
  ((noodle-like-ind ns-bu4) *c-rare*) ;; prefer face 面部
  
  ((aspect-ind organization-ind) *c-common*) ;; conflict with 面部
  ((aspect-word-mian4-ind organization-ind) *c-common*) ;; conflict with 面部
  ((noodle-like-ind organization-ind) *c4*) ;; prefer face 面部
  ((human-name organization-ind) *c-rare*)
  ((word-representative organization-ind) *c0*) ;; prefer animate 代表  in 代表 部

  ((noun ns-fen4) *c-fallback*)
  ((inanimate ns-fen4) *c3*)
  ((word-body ns-fen4) *c-rare*) ;; 身分
  ((cost-or-book-ind ns-fen4) *c-rare*) ;; 本分
  
  ((noun ns-material-ind) *c-common*) ;; prefer inanimate 器
  ((material ns-material-ind) *c0*)
  ((inanimate ns-material-ind) *c2*)
  ((abstract ns-material-ind) *c-common*)
  ((animal ns-gan1) *c0*)
  ((edible ns-gan1) *c0*)

  ((inanimate ns-ping3) *c0*)
  ((noun ns-ping3) *c-common*)
  ((adj ns-ping3) *c-common*) ;; prefer inanimate 品
  
  ((clothing ns-fen3) *c-very-rare*)

  ((hair-like ns-shape-ind) *c-very-rare*)
  ((inanimate ns-shape-ind) *c1*)
  ((noun ns-shape-ind) *c-common*)
  ((abstract ns-shape-ind) *c-common*)
  ;; prefer inanimate 片
  ((place ns-pian4) *c-common*)
  ((book-like-record ns-pian4) *c-common*)
  
  ((place ns-kuai4) *c2*)

  ;; prefer 球 as inanimate
  ((ball-like ns-qiu2) *c-rare*) 
  ((gas ns-qiu2) *c-rare*)
  ((body-part ns-qiu2) *c-rare*)
  ((word-body ns-qiu2) *c-rare*) ;; 健身球

  ((noun ns-di1) *c-rare*)
  ((liquid ns-di1) *c-close-suffix*)

  ((inanimate word-human-er2) *c4*) ;; prefer ns-er2 for inanimate
  ((place word-human-er2) *c4*) ;; prefer ns-er2 for place
  
  ;;((animate-attribute general-suffix) *c-close-suffix*)
  ((talent ns-zi3) *c-rare*) ;; 才子
  ((position-or-count-ind ns-zi3) *c-rare*) ;; 次子
  ((hole-ind ns-zi3) *c-rare*) ;; 孔子
  ((furniture ns-zi3) *c0*)
  ((lid ns-zi3) *c0*)
  ((tool ns-zi3) *c0*)
  (((m noun) ns-zi3)
   (+ *c1*
      ;; e.g. prefer '陌生 (男 子)' to '(陌生 男) 子'
      (not-prefer-trait m 'has-adj-noun-mod (* 2 *c-close-sep*))))
  ((adj ns-zi3) *c2*)
  ((verb ns-zi3) *c3*)

  ((inanimate ns-lei4) *c0*) ;; e.g. prefer the inanimate form for 品类, 雕塑类
  ((abstract ns-lei4) *c0*) ;; e.g. 小说类
  ((animate ns-lei4) *c0*)
  
  ;;((inanimate-attribute general-suffix) *c-prefer*) ;; *c-close-suffix* in the rule
  
  ;;((abstract general-suffix) *c-prefer*) ;; *c-close-suffix* in the rule
  ((abstract ns-qun2) *c-rare*)
  ((animate ns-qun2) *c0*)
  ((position-of ns-qun2) *c0*) ;; *c-close-suffix* in the rule
  
  ;; human-suffix
  ((adj human-suffix) *c1*)
  ((verb human-suffix) *c2*)
  ((noun human-suffix) *c3*)
  ((furniture-tai2 human-suffix) *c-rare*) ;; 台长

  ((place human-suffix) *c2-3*)
  ((animate human-suffix) *c2-3*)
  ((inanimate human-suffix) *c2-3*)
  ((political-party human-suffix) *c2-3*)
  ((race human-suffix) *c2-3*)
  ((can-be-job human-suffix) *c2-3*)

  ((dynasty-shang1 noun) *c-common*) ;; conflict with 商 as business
  ((dynasty-shang1 dynasty-ind) *c0*)

  ((country-abbr s-bao1) *c0*)
  
  ;; 户
  ((dynasty-shang1 s-hu4) *c-very-rare*) ;; 商户
  ;;((verb-n s-hu4) *c-close-suffix*)
  
  ;; 们
  ((organization ns-men2) *c1*) ;; prefer pulral of the organizations
  
  ;; 商
  ((time-name s-shang1) *c-rare*) ;; 美日商
  ((adj-word-mei3 s-shang1) *c-rare*) ;; 美商
  ((country-abbr s-shang1) *c0*)
  ((country-abbr-america s-shang1) *c0*)
  ((organization-ind s-shang1) *c-common*)
  
  ;; 農
  ;;((plant s-nong2) *c-close-suffix*)
  ((v-hua1 s-nong2) *c-rare*) ;; 花农
  ((work-ind s-nong2) *c-common*) ;; 工农
  
  ;; 者
  ;;((verb s-zhe3) *c-close-suffix*)
  ;;((adj s-zhe3) (+ *c-adj-as-noun-mod* *c-close-suffix*))
  ((place s-zhe3) *c-rare*)
  ((can-be-job s-zhe3) *c-job-animate*)
  ((animate-attribute s-zhe3) *c-attribute-zhe3*)
  ;((doctrine s-zhe3) *c-close-suffix*)
  ;;((belief s-zhe3) *c-close-suffix*)
  ((v-yue4-d s-zhe3) *c0*) ;; 乐者

  ;; verb-p 員
  ;;((verb s-yuan2) *c-close-suffix*)

  ;; 長
  ((country-abbr s-zhang3) *c-rare*)
  ((v-cheng2-f s-zhang3) *c-rare*) ;; confused with 成长 as verb
  ;; easily confused with 处长
  ((v-chu3 s-zhang3) *c-rare*)
  (((m animate) s-zhang3)
   (if (has-trait m 'secondary)
       *c4*
       *c1*))
  
  ((noun s-zhang3) *c1*)
  ((organization s-zhang3) *c1*)
  ((adj s-zhang3) *c2*)
  ((verb s-zhang3) *c3*)
  ((abstract s-zhang3) *c4*)
  ((adj-word-xing2 s-zhang3) *c4*)

  ;; 家
  ;;((study s-jia1) *c-close-suffix*)
  ((adj s-jia1) *c-common*)
  ;;((abstract s-jia1) *c-close-suffix*)
  ((num s-jia1) *c-fallback*)
  ((number s-jia1) *c-fallback*)
  ((word-guest s-jia1) *c-rare*) ;; 客家
  ((v-sang1 s-jia1) *c-rare*) ;; prefer verb-x 丧家
  ((human s-jia1) *c-rare*) ;; prefer 家 as home for human

  ;; adj 手
  ;;((adj s-shou3) (+ *c-adj-as-noun-mod* *c-close-suffix*))
  ((adj-word-slim s-shou3) *c-rare*) ;; 纤手 for 手 as hand
  ((long-thin-fiber s-shou3) *c-common*)
  ;;((verb s-shou3) *c-close-suffix*)
  ((v-xi3-b s-shou3) *c-very-rare*) ;; prefer verb-x 洗手
  ;;((inanimate s-shou3) *c-short-close-suffix*)
  ;;((abstract s-shou3) *c-short-close-suffix*)
  ((organization-ind s-shou3) *c-rare*) ;; prefer verb for 帮 in 帮手
  
  ;; 迷
  ((adj s-mi2) *c-rare*)
  ;; 隊
  ((transportation s-dui4) *c-rare*)
  ((dynasty s-dui4) *c-fallback*)
  ((animate s-dui4) *c0*)
  ((abstract s-dui4) *c3*)
  ;; 主
  ;; easily confused with 民主
  ((human-ind s-zhu3) *c-rare*)
  ;;((inanimate s-zhu3) *c-close-suffix*)
  ((hair-like s-zhu3) *c-rare*) ;; 毛主席
  ((verb s-zhu3) *c-common*)
  ;;((v-ling2 s-zhu3) *c-close-suffix*) ;; 领主
  ((word-ling3 s-zhu3) *c-rare*) ;; prefer 领主 or verb 领
  ((adj-word-big s-zhu3) *c-common*) ;; prefer 大 主教 to 大主 教
  ;; 生
  ;;((verb s-sheng1) *c-close-suffix*)
  ((v-qin1-b s-sheng1) *c-fallback*) ;; confused with 亲生
  ((v-wei4-f s-sheng1) *c-rare*) ;; confused with 卫生
  ((adj-word-qin1 s-sheng1) *c-fallback*) ;; confused with 亲生
  ((adj-word-big word-student) *c-rare*) ;; confused with 大學 生
  ((country-abbr-china word-student) *c-rare*) ;; confused with 中學生
  ((adj-word-small word-student) *c-rare*) ;; confused with 小學 生
  ((place s-sheng1) *c-common*)
  ;;((abstract s-sheng1) *c-close-suffix*)
  ((adj-word-si1 s-sheng1) *c-rare*) ;; prefer verb 私生
  ((num s-sheng1) *c-rare*) ;; prefer abstract 一生
  ((number s-sheng1) *c-rare*) ;; prefer abstract 一生
  ((v-jiu4-a s-sheng1) *c-rare*) ;; prefer 救生
  ;; 工
  ((v-jia1-a s-gong1) *c-rare*) ;; 加工
  ((v-da2-b s-gong1) *c-fallback*) ;; 打工

  ;; prefer 工 as human-suffix, which is also work, because it is very difficult to distinguish between the two
  ;; so 工 as work-ind is discouraged
  ((adj work-ind) *c2*)
  ((verb work-ind) *c3*)
  ((verb-n work-ind) *c2*)
  ((noun work-ind) *c4*)
  ((abstract work-ind) *c4*)
  
  ;;((verb s-dai4) *c-rare*) ;; discourage use of 代 as shorthand of 代表
  ;;((adj s-dai4) *c-rare*)
  ;;((political-party s-dai4) *c-common*)
  ;;((place s-dai4) *c-common*)
  ((dynasty s-dai4) *c-common*) ;; prefer the dynasty
  ((race-name s-dai4) *c-common*)
  ((human-han4 s-dai4) *c-common*) ;; prefer 漢代 as dynasty
  ;;;;
  
  ;; inanimate
  ;;((inanimate (n inanimate))
  ;; (+ *c-close-suffix* (prefer n 'ind *c-common*)))

  ((verb paper-like) *c4*)

  ((dynasty letter-ind) *c-common*) ;; want 證明 信, not 證 明信

  ((street edible) *c-common*) ;; prefer 道 as unit in 一道菜

  ((animate word-door-like-hu4) *c-common*) ;; prefer 客户 as animate with s-hu4
  
  ((v-jia4-zhi2 word-chain) *c4*) ;; prefer abstract 价值 in 价值链

  ((queen wearable) *c2*) ;; e.g. 王后的戒指
  ((body-part armor) *c0*) ;; 指甲, 拇指甲
  
  ((v-zheng4-a photo) *c4*) ;; prefer inanimate 证 in 证照
  
  ((word-smell-xiang1 cooking-utensil) *c0*) ;; 香炉
  
  ((v-zhi5 long-thin-body-part) *c-common*) ;; prefer 指 as shorthand for finger in 指脚

  ((organization flag) *c0*)
  
  ((v-bao4-b word-tan1) *c-common*) ;; prefer 报 as newspaper in 报摊
  
  ((dynasty tomb) *c0*)

  ((adj-word-mu3 boobs) *c-common*) ;; 母乳

  ((country-abbr-japan light) *c-common*) ;; prefer 日 as the sun
  ((sun-word light) *c3*)

  ((inanimate organization-or-platform-ind) *c4*) ;; 球台, 炉台, etc
  ((tv organization-or-platform-ind) *c0*) ;; 電視台 as platform or organization
  ((building organization-or-platform-ind) *c-common*)
  ((adj-on-physical organization-or-platform-ind) *c4*)
  
  
  ((v-san3 word-cargo-ship) *c-very-rare*) ;; prefer 散貨 輪 to 散 貨輪

  ((landscape word-grain) *c-rare*) ;; prefer 谷 as valley, but 谷 is same as 穀 in simplified chinese
  ((word-plant-rice word-grain) *c0*) ;; to prefer 稻谷 as 稻穀, instead of 稻 valley

  ((dynasty-word-liang stick-like) *c-rare*) ;; 樑柱 instead of 梁柱

  ;;((inanimate head) *c-prefer*)
  ;;((animate head) *c-prefer*)
  ((place head) *c-common*) ;; 桥头, 山头
  ;;((animate (n inanimate)) (prefer n 'ind *c-common*))
  ;;((verb-n inanimate) *c-close-suffix*)
  ;;((verb inanimate) *c-common*)
  ;;((animate-attribute inanimate) *c-common*)
  ;;((abstract inanimate) *c-common*)

  ((explanation-ind poison) *c-rare*) ;; prefer verb-x 解毒
  ((v-xie4-d poison) *c-rare*) ;; prefer verb-x 解毒

  ((v-cui1 tear) *c-rare*) ;; prefer 催泪 as verb-x
  
  ((abstract ring-like-tool) *c-rare*) ;; prefer 圈 as abstract region for abstract, animate
  ((animate ring-like-tool) *c-rare*)
  ((animate-attribute ring-like-tool) *c-rare*)
  ((inanimate ring-like-tool) *c3*)
  ;;((adj-shape ring-like-tool) *c-close-suffix*)
  
  ((abstract bottle-or-furniture-ind) *c-rare*) ;; prefer 坛 as region for abstract, e.g. 乐坛
  
  ;;((adj-word-sharp inanimate) *c-close-suffix*)
  ;;((adj-word-sharp blade-like) *c-close-suffix*)
  
  ((sun-word cargo) *c-rare*) ;; prefer 日 as Japan
  ((country-abbr cargo) *c0*)
  
  ;;((verb bag) *c-close-suffix*)
  
  ((knowledge-domain-or-organization-or-type
    rope) *c-rare*) ;; 系绳

  ;;((verb nerves) *c-close-suffix*) ;; 视神经
  
  ;;((metal ore-ind) *c-close-suffix*)
  ((adj-gold-silver ore-ind) *c-rare*) ;; prefer 金 as metal
  
  ((adj-grey soil) *c-common*) ;; prefer 灰 as inanimate
  
  ((verb rain-ind) *c-rare*) ;; prefer verb-noun
  ((adj-word-qing2 rain-ind) *c-common*) ;; prefer 晴 as abstract in 晴雨

  ;;((adj-gold-silver furniture) *c-close-suffix*) ;; 金 is more likely to refer to color for furniture
  ((gold-or-cost-ind furniture) *c-common*)
  ((inanimate furniture-tai2) *c3*)

  ((verb furniture-ind) *c3*) ;; prefer noun form if possible, e.g. 框架, 画架
  ((noun furniture-ind) *c2*)
  
  ((milk-or-boobs milk-or-boobs) *c-rare*)

  ((adj-word-jian4 word-body) *c-rare*) ;; prefer verb 健身

  ((v-tong2-qing2 word-you1-lu4) *c4*) ;; prefer verb-verb 同情 忧虑

  ;;((verb disease-ind) *c-close-suffix*)
  ((v-mian3 disease-ind) *c-rare*) ;; 免疫
  ((v-fang2 disease-ind) *c-rare*) ;; 防疫

  ;;((verb ball-ind) *c-close-suffix*)
  
  ;;((adj-color sky-or-god-like-or-place-ind) *c-close-suffix*)
  ((num sky-or-god-like-or-place-ind) *c-rare*) ;; prefer 天 as time period
  ((number sky-or-god-like-or-place-ind) *c-rare*) ;; prefer 天 as time period
  ((verb sky-or-god-like-or-place-ind) *c4*) ;; prefer 天 as time

  ((country-abbr stock-ind) *c0*) ;; 美股
  ((adj-word-mei3 stock-ind) *c-rare*) ;; 美股
  ((furniture-tai2 stock-ind) *c-rare*)

  ((country-abbr book-like) *c0*) ;; 中 译本
  ((sun-word cost-or-book-ind) *c-rare*) ;; 日本
  
  ((picture book-ind) *c1*) ;; prefer animate 画 in 画册

  ;;((verb-n medicine) *c-close-suffix*)
  ((country-abbr medicine) *c0*)
  ;;((dir medicine) *c-rare*)
  
  ;;((num inanimate) *c-common*)
  ;;((number inanimate) *c-common*)

  ;;((country-abbr ticket-or-vote-ind) *c-close-suffix*) ;; 台票, 日票
  ;;((time-period-num-ind ticket-or-vote-ind) *c-close-suffix*) ;; 日票
  ((sun-word ticket-or-vote-ind) *c-rare*)
  ((furniture-tai2 ticket-or-vote-ind) *c-rare*) ;; 台票
  
  ((num strip-or-region-like-ind) *c-rare*)
  ((number strip-or-region-like-ind) *c-rare*)
  ;;((num word-region) *c-close-suffix*)
  ((land strip-or-region-like-ind) *c-very-rare*) ;; 地带
  
  ((examination-ind gold-or-cost-ind) *c-rare*) ;; 试金

  ((adj-gold-silver stone-ind) *c-rare*) ;; 金石
  ;;((verb-n stone-ind) *c-close-suffix*)

  ;;((verb paper-like-ind) *c-short-close-suffix*) ;; 旅行卡

  ;;((v-liu2-a liquid) *c-close-suffix*) ;; 流水, etc
  ((liquid liquid) *c0*) ;; 尿液

  ((adj-grey word-liquid-jiang1) *c4*) ;; prefer inanimate 灰 in 灰浆
  
  ((market-or-city heart-ind) *c-rare*) ;; 市 中心, not 市中 心
  ((v-shu4-a heart-ind) *c-common*) ;; prefer 树 as tree in 树心
  ((ring-ind heart-ind) *c4*) ;; prefer 戒 as verb in 戒心
  ((inanimate heart-ind) *c0*)

  ((tree tree) *c0*) ;; e.g. 松柏, 松树
  ((plant plant) *c0*) ;; 松花

  ((country-abbr-france eye) *c-common*) ;; prefer abstract 法 in 法眼
  
  ;; 髮
  ;;((adj-color hair-like) *c-close-suffix*) ;; prefer color for hair
  ((metal hair-like) *c-common*)
  ((hair-like hair-like) *c0*) ;; 鬚眉

  ((long-thin-fiber word-mao2) *c0*) ;; 纤毛
  
  ;; 證 as in paper like document, or evidence
  ;;((verb paper-like-evidence-ind) *c-close-suffix*)
  
  ;; 風 as in wind
  ;;((time-name wind-ind) *c-close-suffix*)
  ((abstract wind-ind) *c-common*)
  ((animate wind-ind) *c-common*)
  ((adj wind-ind) *c4*)
  ((adj-temperature wind-ind) *c1*)
  ((adj-word-big wind-ind) *c1*) ;; 大
  ((adj-word-strong wind-ind) *c1*) ;; 強
  ((inanimate wind-ind) *c-common*)
  ((verb wind-ind) *c-rare*)
  
  ;; 口
  ((place mouth) *c-rare*) ;; prefer 口 as place
  ((verb mouth) *c-rare*) ;; prefer 口 as place composed from verbs

  ;; 手, prefer the human-suffix form
  ((adj word-hand) *c-common*)
  ((adj-on-inanimate word-hand) *c1*)
  ;;((adj-word-slim word-hand) *c-close-suffix*)
  ((verb word-hand) *c-common*)
  ((v-xi3-b word-hand) *c-very-rare*) ;; prefer verb-x 洗手
  ((inanimate word-hand) *c-common*)
  ((abstract word-hand) *c-common*)

  ;; 書
  ;;((verb book-character-style-ind) *c-close-suffix*)
  
  ((v-lu4 word-statue) *c-rare*) ;; prefer verb form of 录像

  ((brochure-ind noun) *c-rare*) ;; ambiguity of 單
  ((brochure-ind rail) *c-rare*) ;; 单轨

  ((organization-ind noodle-like-ind) *c-rare*) ;; 台面
  ((place noodle-like-ind) *c-rare*) ;; e.g. 路面
  ((abstract noodle-like-ind) *c-rare*) ;; prefer aspect
  
  ;;((adj line-ind) *c-close-suffix*)
  ((inanimate line-ind) *c0*)
  ((paper-like-ind line-ind) *c-common*) ;; 單線
  ((adj-word-ma2 line-ind) *c-common*) ;; 麻线
  ((v-ma2 line-ind) *c-common*) ;; 麻线

  ((adj-color plate-word) *c-rare*) ;; 金牌, 銀牌, ...
  ;;((name-of plate-word) *c-close-suffix*) ;; 姓名 牌
  ;;((verb-n plate-like) *c-close-suffix*)
  ((verb plate-like) *c2*)
  ((v-hao4-a plate-like) *c4*) ;; prefer 号 as abstract

  ((verb oil) *c-very-rare*) ;; 炼油, 加油
  ((v-jia1-a oil) *c-rare*) ;; prefer 加油 as verb or verb-x
  ((v-use oil) *c2*)
  ((word-smell-xiang1 oil) *c0*) ;; 香油

  ;; 水
  ((sperm-or-liquid-agent water-ind) *c0*) ;; 精水
  ;;((verb water-ind) *c-short-close-suffix*)
  ;;((adj water-ind) *c-short-close-suffix*)
  
  ;; 道
  ((ore-pit-word street-ind) *c-common*) ;; 矿坑道
  ((human-ind street-ind) *c-rare*) ;; 人道
  ((adj-shang4 street-ind) *c-rare*) ;; 上道
  ((adj-word-qing1 street-ind) *c-rare*) ;; 清道
  ((bullet-like street-ind) *c0*) ;; 弹道

  ;; 品, 物
  ;;((adj goods-ind) *c-close-suffix*)
  ;;((abstract goods-ind) *c-short-close-suffix*)
  ((inanimate goods-ind) *c-common*) ;; special case fo inanimate
  ((v-fu4-a goods-ind) *c-rare*) ;; 复制 品 confused with 复 制品
  ;;((verb goods-ind) *c-short-close-suffix*)
  ((place goods-word-ind) *c-rare*)
  ((v-zao4 goods-ind) *c-rare*) ;; prefer verb-x 造物
  ((v-du2-a goods-ind) *c-common*) ;; prefer 毒品 as poison + ns-ping3
  ((position-or-count goods-ind) *c-common*) ;; prefer *次品 as *次 品, but with 次 as adj
  ((cheng2 goods-ind) *c-common*) ;; prefer 半 成品 to 半成 品
  ((word-building-jian4-zhu2 goods-ind) *c0*)
  
  ((inanimate machine-ind) *c-common*)
  ;;((verb machine-ind) *c-prefer*)
  ;;((abstract machine-ind) *c-common*)
  ;;((verb wheeled) *c-prefer*)
  ((v-sui2 wheeled) *c-rare*)
  ((v-xi3-b wheeled) *c-rare*) ;; 洗车
  ((adj-word-xing2 wheeled-ind) *c-rare*) ;; 行车
  ((queen head)
   ;; 后头, easily confused with 後頭
   *c-rare*)
  ;;((place inanimate) *c-prefer*)
  ((doctrine inanimate) *c-rare*)
  ;;((abstract (n inanimate))
  ;; (prefer n 'ind *c-short-close-suffix*))
  ;; 面 as in face easily confused with 面 as in aspect,
  ;; and 面 as relative to place
  ((abstract face-ind) *c-common*)
  ((place face-ind) *c-rare*)
  ((furniture-tai2 face-ind) *c-very-rare*) ;; 台面
  ((country-abbr-taiwan face-ind) *c-very-rare*)
  ((country-abbr-taiwan aspect-word-mian4-ind) *c-very-rare*)
  ((organization-or-platform-ind aspect-word-mian4-ind) *c-very-rare*)

  ((place aspect-word-mian4-ind) *c-common*)
  ((inanimate aspect-word-mian4-ind) *c-common*) ;; prefer as place, e.g. 台面
  ((num aspect-word-mian4-ind) *c0*) ;; 两面
  ((number aspect-word-mian4-ind) *c0*) ;; 两面

  ((p-pronoun face-ind) *c-rare*)
  ;;((p-pronoun aspect-ind) *c-close-suffix*)
  ((adj-color face-ind) *c1*) ;; e.g. 红脸
  ((adj face-ind) *c-common*)
  ;;((adj aspect-ind) *c-close-suffix*)
  ((adj-shang4 aspect-word-mian4-ind) *c-rare*) ;; confused with 上面 as up
  ;; animate-attribute, inanimate-attribute
  ((pronoun animate-attribute) *c0*)
  ;;((abstract (n attribute))
  ;; (+ *c-close-suffix* (prefer n 'ind *c-common*)))
  ;;
  ;;((animate (n animate-attribute))
  ;; (+ *c-close-suffix* (prefer n 'ind *c-close-suffix*)))
  ((human-name animate-attribute) *c-fallback*)
  ;;((verb animate-attribute) *c-close-suffix*)
  
  ;; 行 as conduct
  ((verb conduct-ind) *c-rare*) ;; prefer the shop-like-ind
  ((p-pronoun conduct-ind) *c-rare*)
  ((adj-color conduct-ind) *c-rare*) ;; prefer bank for 银行
  ((army-like-lu3 conduct-ind) *c-rare*) ;; prefer verb 旅行
  ((inanimate conduct-ind) *c-common*) ;; prefer 行 as place

  ;;
  ((human-name rights-ind) nil)
  ;;((place animate-attribute) *c-fallback*)
  ;;((inanimate animate-attribute) *c-fallback*)
  ;;((verb kindness-favour) *c-close-suffix*)
  ((v-min2-zhu3 kindness-favour) *c-rare*) ;; 民主 德国 confused with 民主德 国
  (((m place) rights-ind)
   (prefer m 'ind *c-rare*))
  ;;((inanimate rights-ind) *c-close-suffix*)
  ;;((verb rights-ind) *c-close-suffix*)
  ((animate rights-ind) *c-rare*)
  ;; abstract

  ((animate word-method) *c0*)
  ((verb word-method) *c0*)
  ((abstract word-method) *c-common*)

  ((currency word-hui2-long2) *c0*) ;; 货币回笼
  
  ((unit adj-word-qiao1-qiao1) *c-common*) ;; want 悄悄 be adv in 一個個都悄悄
  ((pronoun aspect-word-mian4-ind) *c4*) ;; 他的面, prefer 面 as face

  ((v-pai2 position-of-ind) *c0*) ;; want verb instead of unit for 排 in 排位

  ((v-shi4-k posed-question) *c4*) ;; prefer abstract 试 in 试题
  
  ((physical-sky-ind natural-phenomena-ind) *c0*) ;; 空难

  ((inanimate creative-work-ind) *c4*) ;; prefer ns-zuo4

  ((wind-ind appearance-ind) *c4*) ;; prefer 风 as style or trend in 风貌
  ((style-trend-ind appearance-ind) *c0*)

  ((adj-word-xing2 law-ind) *c4*) ;; 行规, prefer 行 as place
  ((v-hang2 law-ind) *c4*) ;; 行规
  ((conduct-ind law-ind) *c4*) ;; 行规

  ((v-zi1 job) *c4*) ;; prefer abstract 资 in 资职

  ((num word-steps-bu4) *c-common*) ;; prefer num-unit
  ((number word-steps-bu4) *c-common*) ;; prefer num-unit
  
  ((germs-like element-of) *c0*) ;; 霉素
  
  ((bullet-like trace-of) *c0*) ;; prefer bullet 弹 in 弹痕
  
  ((animate word-jia3) *c4*) ;; prefer inanimate 甲, e.g. 马甲
  ((inanimate word-jia3) *c4*) ;; prefer inanimate 甲, e.g. 马甲
  
  ((abstract temperature-of) *c4*) ;; prefer inanimate 体  in 体温

  ((character-style-ind situation-ind) *c4*) ;; prefer body-part of 体 in 体态
  
  ((country-abbr seat) *c-common*) ;; 台座

  ((v-ti2 shape-of) *c-common*) ;; prefer 题 as abstract in 题型

  ((moon-word money-related) *c-rare*) ;; prefer 月 as month, time

  ((surname surname-of-word-shi4) *c4*) ;; prefer surname + title
  ((dynasty surname-of-word-shi4) *c-rare*) ;; dynasty is often confused with surname, and in which case prefer surname + title

  ((adj written-character) *c1*)

  ((country-abbr source-ind) *c4*) ;; prefer abstract 法 in 法源
  
  ((country-abbr network-or-website-ind) *c4*) ;; prefer abstract 法 in 法网
  
  ((inanimate character-style-ind) *c-common*) ;; prefer 體 as body-part, e.g. for 车体, 船体
  ((planet character-style-ind) *c-common*)
  ((landscape character-style-ind) *c-common*)
  
  ((v-yu4-d word-suan4-fa3) *c-common*) ;; prefer 预算 法 to 预 算法

  ((v-bao3-a cost-or-budget-ind) *c-common*) ;; prefer 保 as abstract in 保 费

  ((adj degree-of) *c0*)

  ((dir word-culture) *c-rare*) ;; conflict with 中文 化
  ((country-abbr-china word-culture) *c-rare*) ;; conflict with 中文 化

  (((m abstract) (n abstract))
   (+ *c3*
      (not-prefer m 'ind *c-close-sep*)
      (if (maybe-name-ind-p m n) (- *c-close-sep*) 0)))

  ((adj abstract-suffix) *c1*)
  ((verb abstract-suffix) *c2*)
  ((abstract abstract-suffix) *c3*)
  ((unit abstract-suffix) *c3*)
  ((animate abstract-suffix) *c3-4*)

  ;;((adj abstract) *c-common*)
  ;;((p-pronoun abstract) *c-common*)
  ((num abstract) *c-common*)
  ((number abstract) *c-common*)

  ((num position-or-count-ind) *c-fallback*) ;; prefer num-unit 次
  ((number position-or-count-ind) *c-fallback*) ;; prefer num-unit 次
  ((adj position-or-count-ind) *c-common*)

  ((country-abbr registered-status-or-book-like-record)
   *c0*)
  ((country registered-status-or-book-like-record)
   *c0*)
  ((adj word-nationality) *c-common*)
  ((country-abbr word-nationality) *c-rare*) ;; want 中国 籍 rather than 中 国籍

  ((adj race-han4) *c-rare*) ;; prefer 漢 as a man
  ((verb race-han4) *c-common*) ;; prefer 漢 as a man
  ((animate race-han4) *c-rare*)

  ((v-yu4-d fallback-or-preparation-ind) *c-rare*) ;; prefer 预备 as verb
  
  ;;((abstract basis-ind) *c-close-suffix*)
  ((place basis-ind) *c-rare*) ;; prefer 底 as dir
  ((inanimate basis-ind) *c-rare*) ;; prefer 底 as dir

  ;;((time-name word-history-of) *c-close-suffix*)
  ;;((dynasty word-history-of) *c-close-suffix*)
  ;;((verb word-history-of) *c-close-suffix*)
  
  ((num currency-unit) *c-common*)
  ((number currency-unit) *c-common*)
  ((num currency-ind) *c-rare*) ;; prefer num-unit for num+元
  ((number currency-ind) *c-rare*)
  ((adj-gold-silver currency-ind) *c-rare*) ;; prefer 金, 銀 as material for currency
  ((adj-gold-silver metal-currency) *c-rare*) ;; prefer 金, 銀 as material for currency
  ((country-abbr currency-ind) *c0*)
  ((country-abbr currency-or-marks-ind) *c0*)
  ((adj-word-mei3 currency-or-marks-ind) *c-rare*)
  ((word-da4-bu4 currency-or-marks-ind) *c-rare*) ;; prefer 大 部分 to 大部 分
  ((edible currency-or-marks-ind) *c-common*) ;; 鹽分
  ((inanimate currency-or-marks-ind) *c-common*) ;; prefer 分 as ns-fen4

  ((surname-of brand) *c-very-rare*) ;; prefer 姓名 牌 to 姓 名牌
  ;;((word-zi3 brand) *c-close-suffix*) ;; 子品牌
  ;;((word-mu3 brand) *c-close-suffix*) ;; 母品牌
  
  ((noun abstract-type-of-ind) *c-rare*) ;; animate has its own
  ;;((verb abstract-type-of-ind) *c-close-suffix*)
  ;;((adj abstract-type-of-ind) *c-close-suffix*)
  ((unit abstract-type-of-ind) *c-common*) ;; e.g. prefer inanimate 片 in 片种
  ((video-or-plate-ind abstract-type-of-ind) *c3*)
  ((v-pin3 abstract-type-of) *c4*) ;; prefer inanimate 品 in 品系
  
  ((v-ji4-d abstract-matter-ind) *c-rare*) ;; prefer verb-x or verb 记事
  
  ((country-abbr drama-ind) *c0*)
  ((time-name drama-ind) *c-common*)
  ((v-jia1-a drama-ind) *c-rare*) ;; 加剧
  
  ;; 税
  ;;((verb tax-ind) *c-close-suffix*)
  ((v-guan1-b tax-ind) *c-very-rare*) ;; 关税
  ((place tax-ind) *c-common*)
  ((benefit tax-ind) *c0*) ;; prefer 利 as benefit rather than verb in 利税

  ;; 令 as in order
  ;;((verb order) *c-close-suffix*)
  ((country-abbr-france order-ind) *c-common*) ;; prefer 法 as abstract in 法令
  
  ;; 款
  ;;((verb money-related-can-be-fang4) *c-close-suffix*)
  ((adj-word-mei3 money-related-can-be-fang4) *c-rare*)
  ((country-abbr money-related-can-be-fang4) *c0*)
  
  ((country-abbr money-related) *c0*)
  ((sun-word money-related) *c-rare*)

  ((human-suffix church-like-organization) *c-rare*) ;; prefer 天主教 会 to 天主 教会
  
  ;;((music-ind political-party-ind) *c-close-suffix*) ;; prefer 乐派 as in music
  ((v-yue4-d political-party-ind) *c-rare*)
  ((v-zhi2-a political-party-ind) *c-common*) ;; 执政 党 rather than 执 政党
  ;;((v-zhi2-zheng4 political-party-ind) *c-close-suffix*) ;; 执政 党 rather than 执 政党
  
  ;; 理
  ((street-place reason-or-ethics-ind) *c-rare*) ;; prefer abstract 道理
  ((organization reason-or-ethics-ind) *c-rare*) ;; prefer verb 处理
  
  ;; 台 as organization
  
  ;; 名
  ;;((inanimate name-or-reputation-ind) *c-close-suffix*)
  ((v-zhan4-c name-or-reputation-ind) *c4*) ;; prefer 站 as station in 站名
  
  ;; 情
  ((v-tong2 feeling-or-sense-or-situation-ind) *c-rare*) ;; prefer verb 同情
  ((v-yan2-c feeling-or-sense-or-situation-ind) *c-rare*) ;; prefer verb 言情
  ((v-chen2-a feeling-or-sense-or-situation-ind) *c-rare*) ;; prefer verb 陈情
  
  ;; 法 as in method or law
  ((p-pronoun-fen1 method-or-law-ind) *c-rare*) ;; 分法
  ;;((v-fen4 method-or-law-ind) *c-close-suffix*)
  ((money-related method-or-law-ind) *c1*) ;; prefer noun form, e.g. 预算法
  ((place method-or-law-ind) *c1*) ;; e.g. prefer 公司 法 to 公 司法
  ((noun method-or-law-ind) *c1*)
  ((verb method-or-law-ind) *c2*)
  ((adj method-or-law-ind) *c3*)
  
  ((country-abbr style-or-process-ind) *c0*)
  ;;((place style-or-process-ind) *c-close-suffix*)
  ((transaction style-or-process) *c2*) ;; prefer '转让 等 (投资 方式)' to '(转让 等 投资) 方式'
  ((transaction import-export) *c4*)
  
  ;; 風 as in style or trend
  ;;((verb style-trend) *c-short-close-suffix*)
  ;;((abstract style-trend) *c0*)
  ((time-name style-trend) *c-common*)
  ((dynasty style-trend) *c0*)
  ((country-abbr style-trend) *c0*)
  ;;((animate style-trend) *c-close-suffix*)
  ((chess-like-game style-trend) *c0*)
  ((picture style-trend) *c0*)
  ((book-like style-trend) *c0*)
  ((adj-word-curved style-trend) *c4*)
  ((adj-temperature style-trend) *c4*)
  ((adj-word-big style-trend) *c4*)
  ((adj-word-strong style-trend) *c4*)
  
  ;; 數
  ((verb number-of-ind) *c2*)
  ;;((abstract number-of-ind) *c-close-suffix*)
  ((unit number-of-ind) *c0*)
  ((v-ji2-e number-of-ind) *c-rare*) ;; 集数
  ((sun-word number-of-ind) *c-fallback*) ;; 日数
  ((country-abbr number-of-ind) *c-rare*) ;; 日数
  
  ((musical-instrument sound) *c0*) ;; 鼓声
  ((v-zhang3-a sound) *c4*) ;; prefer 掌 as palm
  
  ;; 界 as abstract domain or circles
  ((p-pronoun domain-circles-ind) *c0*)
  ((verb domain-circles-ind) *c2*)
  ((place domain-circles-ind) *c-rare*)
  ((v-bao4-b domain-circles-ind) *c-rare*) ;; 报界
  ((bank domain-circles-ind) *c0*) ;; 银行界
  ((abstract domain-circles-ind) *c1*) ;; better score, because 'place place-boundary-ind' may get an advantage
  ((animate domain-circles-ind) *c1*)
  
  ;; 價
  ;;((inanimate price-ind) *c-close-suffix*)
  ((goods-ind price-ind) *c-close-sep*) ;; 物價
  ((place price) *c-close-suffix*)
  
  ;; 量, 額
  ;;((verb amount-of-ind) *c-close-suffix*)
  ;;((inanimate amount-of-ind) *c-short-close-suffix*)
  ((video-producer amount-of-ind) *c-fallback*) ;; prefer verb form of 製片
  ((place amount-of-ind) *c-rare*) ;; 进口量, 出口量
  ((adj-gold-silver amount-of-ind) *c-common*) ;; prefer gold, silver as inanimate

  ;; 制
  ((system-policy-mechanism-made-ind noun) *c-rare*) ;; prefer the verb form
  ((system-policy-mechanism-made-ind place) *c-rare*) ;; prefer the verb form
  ((country-abbr system-policy-mechanism-made) *c0*)
  ((v-suo3-you2 system-policy-mechanism-made-ind) *c0*)
  ((v-kong4 system-policy-mechanism-made-ind) *c-rare*) ;; 控制
  ((p-pronoun-all system-policy-mechanism-made-ind) *c-common*) ;; prefer verb-noun 所有 制

  ;; 文, 話, 語
  ((country-abbr article-or-language-ind) *c0*)
  ((country-abbr spoken-language-ind) *c0*)
  ((dynasty spoken-language-ind) *c4*) ;; e.g. prefer race in 汉语
  ((holiday-ind spoken-language-ind) *c-rare*) ;; 假话
  ((adj-word-mei3 spoken-language-ind) *c-rare*) ;; 美语 prefer 美 as USA

  ((v-wan2-b jokes) *c-common*) ;; prefer 玩笑 话 to 玩 笑话

  ;; 片
  ((country-abbr video-or-plate-ind) *c0*)
  ;;((verb video-or-plate-ind) *c-short-close-suffix*)
  ((hair-like video-or-plate-ind) *c-very-rare*) ;; 发片
  ((inanimate video-or-plate-ind) *c4*) ;; prefer 片 as ns-shape-ind

  ;; 号
  ((verb name-as-number-or-sign-ind) *c-common*)
  ;;((inanimate name-as-number-or-sign-ind) *c-close-suffix*)
  
  ;; 商 as in industry
  ((abstract industry-word-shang1) *c-common*)
  ((industry industry-word-shang1) *c-rare*)
  ((time-name industry-word-shang1) *c-rare*)
  ((adj-word-mei3 industry-word-shang1) *c-rare*) ;; 美商
  
  ;; 感
  ;;((verb feeling-or-sense-ind) *c-short-close-suffix*)
  ;;((adj feeling-or-sense-ind) *c-close-suffix*)
  ;;((abstract feeling-or-sense-ind) *c-short-close-suffix*)
  
  ;; * + 政局 easily confused with (*+政) 局
  ((verb politics-state) *c-fallback*) ;; 邮政局
  ((abstract politics-state) *c-fallback*) ;; 财政局
  ((place politics-state) *c-fallback*) ;; 市政局
  (((m country) politics-state)
   (+ *c0* (not-prefer m 'ind *c-rare*)))
  (((m province) politics-state)
   (+ *c0* (not-prefer m 'ind *c-rare*)))
  (((m town) politics-state)
   (+ *c0* (not-prefer m 'ind *c-rare*)))

  (((m place) politics-ind) ;; 市政
   (if (typep m 'ind) *c0* *c3*))

  ((politics word-organization-ju2) *c-close-sep*)

  ;; easily confused with 旅行 业
  ((army-like-lu3 industry-ind) *c-rare*)
  ((v-lv3 industry-ind) *c-common*)
  ((army-like-ying2 industry-ind) *c-rare*) ;; 营业
  
  ((verb arranged-event) *c1-2*)
  ((v-dai4-biao3 arranged-event) *c-common*) ;; prefer animate 代表
  ((v-hua4-c arranged-event) *c-common*) ;; prefer inanimate 画

  ((v-zhu4-c study-ind) *c-rare*) ;; 助学
  ((human-han4 study-ind) *c-rare*) ;; prefer 漢 as race or dynasty here

  ;; 
  ;; verb-p 論
  ;;((verb s-lun4) *c-close-suffix*)
  ((animate s-lun4) *c-rare*)

  ;; 力
  ((animate strength-or-ability-ind) *c-rare*)
  ;;((adj strength-or-ability-ind) *c-common*)
  ;;((wind-ind strength-or-ability-ind) *c-close-suffix*)

  ;; 性
  ((verb s-xing4) *c1-2*) ;; prefer verb form
  ((v-tiao2 s-xing4) *c-rare*) ;; prefer abstract 调

  ((animate word-xing4) *c-common*)
  ((adj word-xing4) *c-rare*) ;; conflict with adj + s-xing4
  ((verb word-xing4) *c-rare*)

  ;; abstract 道
  ;;((place theory-word-dao4) *c-rare*)

  ;; 義
  ((animate meaning-or-ethics-ind) *c-rare*)
  ((human-suffix meaning-or-ethics-ind) *c-rare*)

  ;; time-name
  ((t time-name) *c-fallback*)
  ((time-date t) *c-rare*)
  ((adv time-name) *c-close-suffix*)
  ((word-current time-pt) *c-rare*) ;; prefer 今 as time-mod
  ((holiday-name holiday-ind) *c-prefer*)
  ((time-name holiday-ind) *c0*)
  ((adv dynasty) *c-rare*) ;; want animate in 好汉
  ((time-period-num dynasty) *c-rare*) ;; 美日商
  ((adj dynasty-han4) *c-rare*) ;; 漢 also means a man, 壯漢, etc
  ((verb dynasty-han4) *c-rare*) ;; 漢 also means a man, 壯漢, etc

  ((country-abbr time-period-num-opt-ge-ind) *c-common*) ;; e.g. don't want country-abbr in 中世紀, 中期, 日期

  ((abstract (n time-name))
   (+ *c3* (prefer n 'ind *c-fallback*)))
  ((adj-word-small time-name) *c-common*) ;; prefer 小时 as time period rather than 小 时
  ;;((adj time-name) *c-short-close-suffix*)
  ((adj-color time-name) *c-common*) ;; prefer 天 as inanimate for color
  ((adj-word-fake time-name) *c-rare*)
  ((adj-shape time-name) *c-common*)
  ;;((v-fang1 time-name) *c-common*) ;; 方 志学 conflict with 方志 学
  ((adj-on-length time-name) *c1*)

  ((v-guo5-duo2 time-period) *c0*) ;; prefer 过度 as verb (meaning transitioning) for time-period, instead of adj (more commonly meaning excessive)

  ((v-ha1 time-period-num-ind) *c-rare*) ;; 哈日
  ((material time-period-num-ind) *c-common*) ;; prefer inanimate 木刻

  ((abstract time-hour-ind) *c-common*) ;; prefer 鐘 as clock, rather than hour
  ((adj time-hour-ind) *c-common*)
  ((verb time-hour-ind) *c-common*)
  ((animate time-hour-ind) *c-common*)
  ((inanimate time-hour-ind) *c-common*)

  ((v-tong2 time-name) *c2*)
  ((v-zhai1-b time-name) *c0*) ;; want month for 斋月
  ((abstract word-time-shi2) *c3*) ;; e.g. 课时

  ;; place
  ;;((noun place) *c-fallback*)
  ;;((dir place) *c-common*)
  ;;((time-name place) *c-common*)
  ;;((animate (n place)) (prefer n 'ind *c-fallback*))
  ;;((inanimate (n place))
  ;; (+ *c-common* (prefer n 'ind *c-fallback*)))
  ;;((abstract (n place))
  ;; (+ *c-short-close-suffix* (prefer n 'ind *c-fallback*)))

  ((num word-house-jia1) *c-rare*)
  ((number word-house-jia1) *c-rare*)
  ((rough-amount word-house-jia1) *c-rare*)
  ((ask-amount word-house-jia1) *c-rare*)

  ((place word-jin4-tou2) *c0*)

  ((penalty-word-guo4 landscape-water-ind) *c-rare*) ;; 过河
  ((country-abbr landscape-water-ind) *c0*) ;; 台海
  ((furniture landscape-water-ind) *c-rare*) ;; 台海
  ;;(((m place) (n place))
  ;; (maybe-name-ind m n *c-close-suffix*))
  ;;((place district-ind) *c-prefer*)

  ((province-abbr tibet-or-treasure) *c0*) ;; prefer 藏 to mean tibet when used with other shorthands for provinces
  ((tibet-or-treasure province-abbr) *c0*)
  ((race-name tibet-or-treasure) *c0*) ;; prefer 藏 to mean tibet when used with other shorthands for other races
  ((tibet-or-treasure race-name) *c0*)
  
  
  ((word-mu3 school-ind) *c-rare*) ;; prefer 母 as adj in 母校

  ((dynasty country-ind) *c-rare*) ;; prefer country-abbr
  ((country-abbr country-ind) *c0*)
  
  ((place place-name) *c-fallback*)
  ((place-name place) *c-rare*) ;; prefer the direct name
  
  ;; some names looks like human name, but end with a place ind such
  ;; as 岛, 山, 林 which are also commonly seen in human names.
  ;; make them have similar penalty
  ((human-name (n place))
   (+ (prefer n 'ind *c-common*)
      *c-likely-name* *c-likely-name*
      (- *c-surname-name* *c-place* *c-unlikely-name*)))
  
  ;;((verb region-ind) *c-close-suffix*)
  ;;((animate region-ind) *c-close-suffix*) ;; e.g. 华人圈
  ;;((abstract region-ind) *c-close-suffix*) ;; 商圈
  ;;((creative-work region-ind) *c-close-suffix*) ;; 乐坛
  ;;((animate-attribute region-ind) *c-close-suffix*) ;; 生活圈
  ((inanimate region-ind) *c-common*) ;; prefer 圈 as inanimate
  ((adj-shape region-ind) *c-common*) ;; prefer 圆圈 as inanimate

  ((creative-work-ind noun) *c-rare*)

  ((liquid bottle-or-furniture-ind) *c3*)

  ((dir collar-embassy) *c-rare*) ;; 中领馆
  ((country-abbr collar-embassy) *c0*)
  ((adj-word-big collar-embassy) *c-common*) ;; prefer 大使 馆 to 大 使馆

  ((dir word-jing1-cheng2) *c-rare*) ;; 南京 城, 北京 城 rather tan 南 京城, 北 京城

  ((verb port-or-hong-kong-abbr-ind) *c-common*)
  ;;((adj port-or-hong-kong-abbr-ind) *c-close-suffix*)
  ;;((animate port-or-hong-kong-abbr-ind) *c-short-close-suffix*)
  ((v-ju1-c port-or-hong-kong-abbr-ind) *c-rare*) ;; prefer verb-x 居港

  ((v-hang2-b port-kong1-gang3) *c-common*) ;; prefer 航空 港 to 航 空港
  
  ;;((verb place) *c-fallback*)
  ;;((verb-n place) *c-common*)
  
  ;; 路
  ((v-tong2 road-ind) *c-rare*) ;; prefer verb-x of 同路

  ;;((verb building-type-ind) *c-short-close-suffix*)
  ((v-kai1-feng1 place) *c-rare*) ;; prefer 开封 as place if followed by place
  ((v-di2-a place) *c-common*) ;; prefer 敌 as animate

  ;;((verb district-ind) *c-short-close-suffix*)
  ((v-jian4-zhu4 district-ind) *c-common*) ;; prefer noun form of 建筑

  ;;((verb land-ind) *c-short-close-suffix*)
  ((word-grain land-ind) *c-very-rare*) ;; 穀(谷)+地 confused with 谷地
  ((word-eye-mu4 land-ind) *c-rare*) ;; prefer 目的 地 to 目 的 地
  ;;((landscape-ind land-ind) *c-close-suffix*)
  
  ((marked-prize-or-penalty landscape-ind) *c-rare*) ;; 過山
  ((plant landscape-ind) *c0*) ;; prefer plant 树 in 树林
  ((word-plant-rice landscape-ind) *c3*) ;; 稻谷 the normal score, so that 穀 is prefered. for other landscape, just normal.

  ;;((place place-boundary-ind) *c-close-suffix*) ;; 界 as place
  ((bank place-boundary-ind) *c-rare*) ;; prefer abstract 银行界
  ((animate place-boundary-ind) *c4*)
  ((abstract place-boundary-ind) *c4*)
  ((inanimate place-boundary-ind) *c4*)
  ((verb place-boundary-ind) *c-common*)
  ((p-pronoun place-boundary-ind) *c4*)
  ((adj place-boundary-ind) *c3*)
  
  ;; 廳, 房, 廂
  ;;((verb room-like-ind) *c-close-suffix*)
  ((v-jian4-c room-like-ind) *c-rare*) ;; prefer verb-x form
  ((inanimate room-like-ind) *c-common*)

  ((shop-like shop-like-ind) *c0*) ;; 宅院
  ((v-wang3-a shop-like-ind) *c4*) ;; prefer 网 as network in 网店
  
  ;; 樓
  ;;((verb-n shop-like-ind) *c-close-suffix*)
  
  ;; 院
  ;;((verb shop-like-ind) *c-common*)
  ;;((inanimate shop-like-ind) *c-close-suffix*)
  
  ;; 行 as in bank
  ;;((verb shop-like-ind-ns) *c-close-suffix*)
  ;;((p-pronoun shop-like-ind-ns) *c-close-suffix*)
  ((v-ke4-c shop-like-ind-ns) *c-rare*) ;; prefer verb form 可行
  ((v-neng2 shop-like-ind-ns) *c-rare*) ;; prefer verb form 能行
  ((religion-ind shop-like-ind-ns) *c-rare*) ;; prefer verb 教 in 教室 
  ((army-like-lu3 shop-like-ind-ns) *c-rare*) ;; prefer verb 旅行
  
  ;;((verb suo3-ind) *c-close-suffix*)
  ((abstract suo3-ind) *c-common*)

  ;;((place factory-ind) *c-close-suffix*)
  ((inanimate factory-ind) *c-common*)
  ;;((verb factory-ind) *c-close-suffix*)
  ((animate factory-ind) *c-common*)

  ;;((adj place) *c-close-suffix*)

  ((place country-abbr) *c-fallback*)
  ((pronoun country-abbr) *c-fallback*)
  ((animate country-abbr) *c-fallback*)
  ((inanimate country-abbr) *c-fallback*)
  ((abstract country-abbr) *c-fallback*)
  ((verb country-abbr) *c-fallback*)
  ((adj country-abbr) *c-fallback*)
  ((adj-word-big country-abbr) *c-close-suffix*)
  ((place-name country-abbr) *c-fallback*)
  ((name country-abbr) *c-fallback*)
  ((time-name country-abbr) *c-rare*)
  ;;
  ((inanimate import-export) *c0*)

  ((p-pronoun-dt neck-or-aspect-ind) *c-rare*) ;; prefer 項 as unit
  ((num neck-or-aspect-ind) *c-rare*) ;; prefer 項 as unit
  ((number neck-or-aspect-ind) *c-rare*) ;; prefer 項 as unit

  ((non-verb-noun army-like-word-general-ind) *c-rare*) ;; "將" alone is usually adverb or verb-mod

  ;; '投' is rarely used as noun, in CTB, only in 'int 投', meaning
  ;; the throwing of baseball.
  ((non-verb-noun v-tou2) *c-rare*)
  ((verb v-tou2) *c-rare*)
  ((adj v-tou2) *c-rare*)
  ((number v-tou2) *c-rare*)
  ((integer v-tou2) *c4*)

  ;; (港 澳 台) 地区
  ((port-or-hong-kong-abbr-ind district-ind) *c1*)
  ((country-abbr district-ind) *c1*)
  ((country-name district-ind) *c1*)
  ((continent-name district-ind) *c1*)
  ((province-name district-ind) *c1*)
  ((county-name district-ind) *c1*)
  ((city-name district-ind) *c1*)
  ((town-name district-ind) *c1*)
  ((district-name district-ind) *c1*)
  ((village-name district-ind) *c1*)

  ;; (经贸 往来) 和 (市场 联系)
  ((market-word-ind contact-and-communication) *c1*)
  ((transaction contact-and-communication) *c1*)
  ((transaction-ind contact-and-communication) *c-common*)

  ((v-hua4-b non-verb-noun) *c-rare*) ;; prefer "(工业 化) 状态" to "工业 (化 状态)"
  ((dir-nesw human-ind) *c1*) ;; e.g. 北方 人

  ((COLOR-OR-APPEARANCE ARMY-LIKE) *c3*) ;; e.g. 红色 娘子军

  ((ABSTRACT WORD-TIME-SHI2-JIAN1) *c4*) ;; e.g. 高峰 时间
  
  ((verb-n time-period-num-ind-day) *c4*) ;; "出头 之 日", "出头 的 一日"

  ((competition-ind ordinal) *c-rare*) ;; want '赛季' as season

  ((human-suffix body-part) *c1*)

  ((word-nei4-ren2 s-shi4) *c-common*) ;; '业内 人士', not '业 (内人 士)'
  ((country-abbr-america human-word-ren2-min2) *c1*)
  ((adj-word-mei3 human-word-ren2-min2) *c4*)

  ((v-ji3-ji3-a non-verb-noun) *c-very-rare*) ;; prefer '自己' as pronoun

  ((time-name (n ad-hoc-time-period-word-during))
   ;; 节日 期间
   (+ (if (> (trait-value n 'n-base-nouns 1) 1)
          ;; prefer '((周日26日零时) 至 (周三28日)) 期间' to '周日 ((26日零时 至 周三28日) 期间)'
          *c-close-suffix*
          0)
      *c0*))
  ((event ad-hoc-time-period-word-during) *c1*)
  ((time-name ad-hoc-time-pt-word-tong2-shi2) *c0*)

  ((num moon-word) *c-rare*) ;; prefer 几月 as time
  
  ((event details) *c1*) ;; 事 的 細節

  ((edible-part noodle-like-ind) *c0*) ;; 鸭肉 面
  ((edible-part aspect-word-mian4-ind) *c-common*) ;; 鸭肉 面

  ((abstract word-time-shi2) *c-common*) ;; want '(VV (XXX 框架)) 时', not 'VV (XXX (框架 时))'

  ((animate abstract-heart-attribute) *c0*) ;; (美国 商人) 的 ((对 华) (投资 信心))

  ((transaction word-xin4-yong4) *c1*) ;; '出口 信用 保险 制度'
  ((import-export word-xin4-yong4) *c1*) ;; '出口 信用 保险 制度'

  ((word-xin4-yong4 insurance) *c1*) ;; '出口 信用 保险 制度'

  ((abstract-matter process) *c2*) ;; '事情 经过'

  ;; e.g. '管治 下', '確認 下'.
  ;; Note that the verb has to be turned into noun with a penalty, so this penalty need not be too severe.
  ;; Also, some verbs may not be applicable, add exceptions as needed.
  ((verbs dir-word-down) *c-close-suffix*)
  ((v-at dir-word-down) *c-common*) ;; prefer '在' as place ind for this case, e.g.  '在 XXX 管治下'
  ((verb-action dir-word-down) *c-close-suffix*)

  ((order dir-word-down) *c1*) ;; '命令 之 下', '命令 下'

  ;; '賽事 中', '賽事 之 中'
  ((event dir-middle) *c1*)

  ;; '過程 中', '過程 之 中'
  ((process dir-middle) *c1*)

  ;; '事情 (之) 上', '事件 (之) 上'
  ((abstract-matter dir-word-up) *c1*)
  ((event-ind dir-word-up) *c1*)
  ((event dir-word-up) *c2*)

  ((strategy dir-word-down) *c1*) ;; '政策 (之) 下', '戰略 (之) 下'

  ((animate animate-attribute) *c0*)

  ;;;;;;;;
  ;; semi-automatically extracted from CTB, manually scanned
  ;; 阿 国家 
  ;; OECD 国家 
  ;; 亚力山大 健康 
  ;; 奥尔布赖特 样子 
  ;; 布什 立场 
  ;; 麦康莫 人脉 
  ;; 乔治·ｗ·布什 前途 
  ;; 亚太 国家 
  ;; 阿加该拉 国家 
  ;; 里根 国家 
  ;; 基督 一生 
  ;; NBA 梦想 
  ;; 央视 权威 
  ;; 阿Q 精神 
  ;; android 怀抱 
  ((NAME ANIMATE-ATTRIBUTE) *c3*);

  ;; 亚 联盟 
  ;; A 组 
  ((NUM ORGANIZATION-IND) *c3*);

  ;; 他们 本人 
  ;; 我 本人 
  ;; 你 本人 
  ;; 她 本人 
  ;; 他 本人 
  ((PRONOUN WORD-BEN3-REN2) *c3*);

  ;; 我们 亲朋好友 
  ;; 她 男友 
  ;; 我 男朋友 
  ;; 他们 好友 
  ;; 你们 朋友 
  ;; 我们 朋友们 
  ;; 她 朋友 
  ;; 我 女友 
  ;; 我 朋友们 
  ;; 他 猎友 
  ;; 他 朋友们 
  ;; 她 男朋友 
  ;; 她 好友 
  ;; 他 朋友 
  ;; 我 朋友 
  ;; 你 朋友 
  ;; 我们 朋友 
  ((PRONOUN FRIEND) *c3*);

  ;; 台湾 形象 
  ;; 中国 形象 
  ((COUNTRY-NAME ANIMATE-APPEARANCE) *c3*);

  ;; 经济 办法 
  ;; 电影 手法 
  ;; 经济 手段 
  ;; 科技 手段 
  ((CAN-BE-JOB WORD-METHOD) *c3*);

  ;; 市场 方面 
  ((MARKET-WORD-IND ASPECT-IND) *c3*);

  ;; 健康 大计 
  ((ANIMATE-ATTRIBUTE STRATEGY-OR-MEASURING-TOOL) *c3*);

  ;; 工作 难点 
  ;; 工作 安排 
  ;; 工作 项目 
  ;; 工作 价值 
  ;; 工作 位置 
  ;; 工作 序幕 
  ;; 工作 对象 
  ;; 工作 小组 
  ;; 工作 方向 
  ;; 工作 效率 
  ;; 工作 环节 
  ;; 工作 节奏 
  ;; 工作 重点 
  ;; 工作 单位 
  ((JOB-WORD-GONG1-ZUO4 ABSTRACT) *c3*);

  ;; 技术 手段 
  ((SKILL WORD-METHOD) *c3*);

  ;; 南海 争议 
  ((LANDSCAPE-WATER-NAME OPINION-OR-DISCUSSION) *c3*);

  ;; 大棒 政策 
  ((STICK-LIKE-TOOL STRATEGY) *c3*);

  ;; 供应 办法 
  ;; 辅助 手段 
  ;; 和平 途径 
  ;; 服务 手段 
  ;; 社会 手法 
  ;; 情节 手法 
  ;; 改革 办法 
  ;; 磁 手段 
  ;; 资源 手段 
  ;; 外交 途径 
  ;; 行政 手段 
  ;; 打击 手段 
  ;; 军事 途径 
  ;; 外交 手段 
  ;; 和平 手段 
  ;; 军事 手段 
  ((ABSTRACT WORD-METHOD) *c3*);

  ;; 巴西 方面 
  ;; 苏联 方面 
  ;; 朝鲜 方面 
  ;; 南非 方面 
  ;; 瑞士 方面 
  ;; 法国 方面 
  ;; 希腊 方面 
  ;; 所国 方面 
  ;; 墨西哥 方面 
  ;; 俄罗斯 方面 
  ;; 北韩 方面 
  ;; 新西兰 方面 
  ;; 巴基斯坦 方面 
  ;; 印度 方面 
  ;; 尼泊尔 方面 
  ;; 巴勒斯坦 方面 
  ;; 以色列 方面 
  ;; 英国 方面 
  ;; 韩国 方面 
  ;; 台湾 方面 
  ;; 美国 方面 
  ;; 日本 方面 
  ;; 菲律宾 方面 
  ;; 叙利亚 方面 
  ;; 蒙古 方面 
  ;; 中国 方面 
  ((COUNTRY-NAME ASPECT-IND) *c3*);

  ;; 台湾 力量 
  ;; 日本 力量 
  ;; 多哥 力量 
  ;; 印度 力量 
  ;; 巴勒斯坦 力量 
  ;; 美国 力量 
  ;; 叙利亚 力量 
  ;; 伊朗 力量 
  ;; 中国 力量 
  ;; 越南 力量 
  ((COUNTRY-NAME POWER) *c3*);

  ;; 年轻人 报 
  ((HUMAN NEWSPAPER-LIKE-IND) *c3*);

  ;; 中国 需要 
  ;; 越南 需求 
  ((COUNTRY-NAME NEEDS) *c3*);

  ;; 组织 政策 
  ;; 联盟 战略 
  ((ORGANIZATION-IND STRATEGY) *c3*);

  ;; 越 主权 
  ((COUNTRY-ABBR RIGHTS) *c3*);

  ;; 南海 自由权 
  ;; 南海 主权 
  ((LANDSCAPE-WATER-NAME RIGHTS) *c3*);

  ;; 南海 国家 
  ((LANDSCAPE-WATER-NAME ANIMATE-ATTRIBUTE) *c3*);

  ;; 俺 娘 
  ;; 俺 爹 
  ;; 他们 夫人 
  ;; 他 父母亲 
  ;; 他 曾祖父 
  ;; 我 老爷 
  ;; 我 外婆 
  ;; 他 岳父母 
  ;; 她 弟弟 
  ;; 我 媳妇 
  ;; 我 外孙 
  ;; 我 太太 
  ;; 她 双亲 
  ;; 她 父母亲 
  ;; 我 大哥 
  ;; 她 太太 
  ;; 她 媳妇 
  ;; 他们 孙子 
  ;; 他 妹妹 
  ;; 她 孩子 
  ;; 我 姥爷 
  ;; 他们 丈夫 
  ;; 他们 大哥 
  ;; 他 夫人 
  ;; 她 母亲 
  ;; 他们 父亲 
  ;; 他 太太 
  ;; 我们 儿子 
  ;; 他们 儿子 
  ;; 他们 孩子们 
  ;; 你 儿子 
  ;; 它们 母亲 
  ;; 你 丈夫们 
  ;; 我 前夫 
  ;; 你 前夫 
  ;; 我 孩子们 
  ;; 你 孩子们 
  ;; 我们 孩子们 
  ;; 她们 丈夫 
  ;; 你 丈夫 
  ;; 她 前妻 
  ;; 他 前妻 
  ;; 我 祖父 
  ;; 我 父母亲 
  ;; 他 祖父 
  ;; 你 伯父 
  ;; 您 叔父 
  ;; 您 父亲 
  ;; 我 伯父 
  ;; 我 丈夫 
  ;; 他 奶奶 
  ;; 你 妈妈 
  ;; 她 舅舅 
  ;; 我 妈妈 
  ;; 我 老妈 
  ;; 我 表姐 
  ;; 俺 堂妹 
  ;; 她 妈 
  ;; 他 岳父 
  ;; 他 老婆 
  ;; TITI 姐姐 
  ;; 你们 孩子 
  ;; 吾 父 
  ;; 她 父母 
  ;; 她 儿子 
  ;; 他们 母女 
  ;; 我们 母亲 
  ;; 他 弟弟 
  ;; 我 妻子 
  ;; 我 父亲 
  ;; 他 女儿们 
  ;; 她 哥哥 
  ;; 他 母亲 
  ;; 他 孩子 
  ;; 他们 女儿 
  ;; 他 妻子 
  ;; 他 孩子们 
  ;; 他 女儿 
  ;; 他 父亲 
  ;; 你 父亲 
  ;; 她 丈夫 
  ;; 他们 父母 
  ;; 你 爸爸 
  ;; 我们 祖母 
  ;; 我 孙子 
  ;; 他们 老大 
  ;; 我 表妹 
  ;; 您 女儿 
  ;; 你 亲戚 
  ;; 你们 父母 
  ;; 双方 父母 
  ;; 人家 孩子 
  ;; 你 奶奶 
  ;; 你 爹 
  ;; 人家 妈妈 
  ;; 你 老父 
  ;; 我 老婆 
  ;; 我 叔 
  ;; 他 爸爸 
  ;; 我 亲属 
  ;; 俺 老公 
  ;; 我们 孩子 
  ;; 她 爸 
  ;; 他 丈夫 
  ;; 她 妈妈 
  ;; 你 父母 
  ;; 我 爸 
  ;; 俺 爸 
  ;; 我 妈 
  ;; 我 父母 
  ;; 他 姥姥 
  ;; 他 妈妈 
  ;; 他 父母 
  ;; 我 儿子 
  ;; 你 老婆 
  ;; 我 妹妹 
  ;; 她 爸爸 
  ;; 穷人家 孩子 
  ;; 她 奶奶 
  ;; 我 哥们 
  ;; 她 女儿 
  ;; 我 老公 
  ;; 我 女儿 
  ;; 我 婆婆 
  ;; 你 孩子 
  ;; 他们 孩子 
  ;; 你 爸 
  ;; 我 爸爸 
  ;; 你 母亲 
  ;; 他 老爸 
  ;; 你 老爸 
  ;; 她 父亲 
  ;; 我 爷爷 
  ;; 你 妈 
  ;; 他 儿子 
  ;; 我 孩子 
  ;; 他 娘 
  ;; 我 母亲 
  ((PRONOUN KINSHIP-RELATIONSHIP) *c3*);

  ;; 人口 政策 
  ;; 供求 战略 
  ;; 关系 战略 
  ;; 供应 政策 
  ;; 整体 政策 
  ;; 能源 政策 
  ;; 资源 政策 
  ;; 水利 政策 
  ;; 关系 政策 
  ;; 国际 政策 
  ;; 生产 政策 
  ;; 社会 政策 
  ;; 交通 政策 
  ;; 国防 政策 
  ;; 和平 方针 
  ;; 寡头 战略 
  ;; 总体 战略 
  ;; 军事 部署 
  ;; 外交 战略 
  ;; 能源 战略 
  ;; 实体 战略 
  ;; 军事 策略 
  ;; 外交 方针 
  ;; 改革 政策 
  ;; 霸权 战略 
  ;; 国际 战略 
  ;; 军事 战略 
  ;; 外交 政策 
  ((ABSTRACT STRATEGY) *c3*);

  ;; 缅甸 国家 
  ;; 中国 视野 
  ;; 捷克 国家 
  ;; 韩国 实力 
  ;; 阿尔巴尼亚 国家 
  ;; 巴西 国家 
  ;; 伊拉克 目标 
  ;; 葡萄牙 血统 
  ;; 中国 血统 
  ;; 台湾 立场 
  ;; 台湾 作为 
  ;; 台湾 用意 
  ;; 帛琉 国家 
  ;; 中国 内涵 
  ;; 台湾 经验 
  ;; 澳洲 国家 
  ;; 台湾 生命力 
  ;; 台湾 精神 
  ;; 法国 国家 
  ;; 台湾 能力 
  ;; 哥国 国家 
  ;; 北韩 国家 
  ;; 伊拉克 健康 
  ;; 巴勒斯坦 目标 
  ;; 科索沃 前途 
  ;; 索马里 国家 
  ;; 中非 国家 
  ;; 美国 观点 
  ;; 日本 立场 
  ;; 西班牙 精神 
  ;; 波兰 国家 
  ;; 新西兰 国家 
  ;; 中国 实力 
  ;; 美国 立场 
  ;; 中国 尊严 
  ;; 中国 能力 
  ;; 日本 举 
  ;; 美国 目标 
  ;; 苏联 实力 
  ;; 俄罗斯 雄风 
  ;; 俄罗斯 实力 
  ;; 苏联 阴影 
  ;; 苏联 遗产 
  ;; 加拿大 国家 
  ;; 中国 劳动力 
  ;; 中国 立场 
  ;; 阿拉伯 国家 
  ;; 日本 国家 
  ;; 蒙古 国家 
  ;; 印度 能力 
  ;; 中国 目标 
  ;; 伊朗 国家 
  ;; 利比亚 立场 
  ;; 叙利亚 立场 
  ;; 叙利亚 决策 
  ;; 以色列 国家 
  ;; 中国 精神 
  ;; 美国 举 
  ;; 美国 追求 
  ;; 美国 国家 
  ;; 中国 国家 
  ;; 美国 实力 
  ;; 中国 前途 
  ;; 中国 智慧 
  ;; 越南 举 
  ((COUNTRY-NAME ANIMATE-ATTRIBUTE) *c3*);

  ;; 中东欧 国家 
  ;; 中南美洲 国家 
  ;; 东亚 前景 
  ;; 东南亚 前景 
  ;; 南亚 国家 
  ;; 中亚 国家 
  ;; 西非 国家 
  ;; 亚洲 观点 
  ;; 欧洲 观点 
  ;; 中欧 国家 
  ;; 中非 国家 
  ;; 东亚 国家 
  ;; 东欧 国家 
  ;; 美洲 国家 
  ;; 西欧 国家 
  ;; 东南亚 国家 
  ;; 欧洲 国家 
  ;; 亚洲 国家 
  ;; 非洲 国家 
  ((CONTINENT-NAME ANIMATE-ATTRIBUTE) *c3*);

  ;; 法国 电力 
  ;; 中国 翻译 
  ;; 印度 滋味 
  ;; 中国 味道 
  ;; 中国 幅员 
  ;; 台湾 社会力 
  ;; 台湾 规模 
  ;; 越南 翻译 
  ;; 中国 滋味 
  ;; 中国 火力 
  ;; 日本 军力 
  ;; 俄罗斯 目的 
  ;; 中国 国力 
  ;; 阿根廷 延伸 
  ;; 美国 军力 
  ;; 中国 军力 
  ;; 中国 特色 
  ((COUNTRY-NAME INANIMATE-ATTRIBUTE) *c3*);

  ;; 自己 配偶 
  ;; 自己 祖先 
  ;; 自己 小孩 
  ;; 自己 队伍 
  ;; 自己 近邻 
  ;; 自己 先生 
  ;; 自己 亲人 
  ;; 自己 同胞们 
  ;; 自己 领袖 
  ;; 自己 同胞 
  ;; 自己 手下 
  ;; 自己 男人 
  ;; 自己 女人 
  ;; 自家 主子 
  ;; 自己 对手 
  ((PRONOUN-WORD-SELF HUMAN-RELATIONSHIP) *c3*);

  ;; 大师 称 
  ((OCCUPATION-SUFFIX NAME-OF-IND) *c3*);

  ;; 葡萄牙 文化 
  ;; 台湾 文化 
  ;; 西班牙 文化 
  ;; 伊拉克 文化 
  ;; 美国 文化 
  ;; 日本 文化 
  ;; 波斯 文化 
  ;; 中国 文化 
  ((COUNTRY-NAME WORD-CULTURE) *c3*);

  ;; 投资 途径 
  ((TRANSACTION WORD-METHOD) *c3*);

  ;; 楼兰 装扮 
  ;; 萨科奇 形象 
  ((NAME ANIMATE-APPEARANCE) *c3*);

  ;; 新贵 专利 
  ;; 同性恋 权利 
  ;; 妇女 权利 
  ;; 私人 权利 
  ;; 人类 权利 
  ;; 公民 权利 
  ;; 穷人 权利 
  ;; 富人 专利 
  ;; 犯罪人 权利 
  ;; 百姓 权利 
  ((HUMAN RIGHTS-IND) *c3*);

  ;; 安第斯 国家 
  ;; 张学良 举动 
  ;; 苏哈托 名义 
  ;; 李登辉 命 
  ;; 李登辉 指示 
  ;; 邓相扬 人脉 
  ;; 程建人 回忆 
  ;; 王壮为 个性 
  ;; 林华庆 经验 
  ;; 伍兹 风采 
  ;; 利马窦 经验 
  ;; 龙发塘 患 
  ;; 巴尔干 国家 
  ;; 利文思通 决定 
  ;; 欧盟 国家 
  ;; 欧盟 决定 
  ;; 陈水扁 大位 
  ;; 彭德怀 警惕 
  ;; 小泉 倾向 
  ;; 荣毅仁 一生 
  ;; 山河 智能 
  ;; 雷锋 内涵 
  ;; 纪思道 观点 
  ;; 倪萍 行为 
  ;; 雷锋 精神 
  ;; 韩寒 素质 
  ;; 骆家辉 举 
  ;; 丘振良 品味 
  ;; 东盟 国家 
  ;; 毛泽东 精神 
  ;; 菅直人 生涯 
  ((HUMAN-NAME ANIMATE-ATTRIBUTE) *c3*);

  ;; 加拿大 这里 
  ;; 朝鲜 这里 
  ;; 美国 那里 
  ;; 俄罗斯 那里 
  ((COUNTRY-NAME DIR) *c3*);

  ;; 远洋 舰队 
  ;; 太平洋 舰队 
  ((LANDSCAPE-WATER SHIP-IND) *c3*);

  ;; 外高加索 国家 
  ;; 大隘社 能力 
  ;; 布什 立场 
  ;; 内塔尼亚胡 隐患 
  ;; 北约 目标 
  ;; 朱迪·米勒 行为 
  ;; 比约克 举 
  ;; 拉姆斯菲尔德 遗产 
  ;; 北约 国家 
  ((PLACE-NAME ANIMATE-ATTRIBUTE) *c3*);

  ;; 德 才 
  ((KINDNESS-FAVOUR TALENT) *c3*);

  ;; 西方 男士 
  ;; 西 音乐家 
  ;; 北头 分局长 
  ;; 西方 传教士 
  ;; 西方 分析家 
  ;; 西方 资本家 
  ;; 西方 预报员 
  ;; 西方 人士 
  ;; 西方 学者 
  ;; 西方 政治家 
  ;; 西方 殖民者 
  ((DIR-NESW HUMAN-SUFFIX) *c3*);

  ;; 过去 形式 
  ;; 目前 模式 
  ;; 以后 模式 
  ;; 当前 形式 
  ((AD-HOC-TIME-PT STYLE-TREND) *c3*);

  ;; 九冬会 期间 
  ;; 书展 期间 
  ;; 集会 同时 
  ;; 集会 自由 
  ;; 谈判 期间 
  ;; 大选 期间 
  ;; 竞选 期间 
  ;; 十运会 期间 
  ;; 选举 期间 
  ;; 演习 期间 
  ((ARRANGED-EVENT TIME-NAME) *c3*);

  ;; 红色 娘子军 
  ;; 蓝色 海军 
  ((COLOR-OR-APPEARANCE ARMY-LIKE) *c3*);

  ;; 战略 部署 
  ;; 政策 战略 
  ;; 战略 步署 
  ;; 战略 方针 
  ;; 方针 政策 
  ;; 战略 策略 
  ;; 下策 下策 
  ((STRATEGY STRATEGY) *c3*);

  ;; 中国 理念 
  ;; 中国 思想 
  ;; 印度 意图 
  ;; 美国 意图 
  ((COUNTRY-NAME THINKING) *c3*);

  ;; 棋子 以色列 
  ((CHESS-LIKE-GAME-IND COUNTRY-NAME) *c3*);

  ;; 朝鲜 主权 
  ;; 伊拉克 主权 
  ;; 意大利 主权 
  ;; 阿富汗 主权 
  ;; 台湾 主权 
  ;; 中国 主权 
  ((COUNTRY-NAME RIGHTS) *c3*);

  ;; 项目 时间 
  ;; 交通 时间 
  ;; 作业 时间 
  ;; 转机 时间 
  ;; 环节 时间 
  ;; 系统 时间 
  ;; 安排 时间 
  ;; 高峰 时间 
  ;; 和平 时间 
  ((ABSTRACT WORD-TIME-SHI2-JIAN1) *c3*);

  ;; 抗战 期间 
  ;; 二战 期间 
  ;; 战争 期间 
  ;; 大战 期间 
  ((BATTLE TIME-NAME) *c3*);

  ;; 战争 手段 
  ;; 斗争 手段 
  ((BATTLE WORD-METHOD) *c3*);

  ;; 武功 技法 
  ((MARTIAL-ARTS WORD-METHOD) *c3*);

  ;; 研究 基金会 
  ((WORD-RESEARCH ORGANIZATION-OR-EVENT) *c3*);

  ;; 她 个人 
  ;; 你 个人 
  ;; 你们 个人 
  ;; 他 个人 
  ;; 我们 个人 
  ;; 我 个人 
  ((PRONOUN WORD-GE4-REN2) *c3*);

  ;; 领域 方面 
  ((REGION ASPECT-IND) *c3*);

  ;; 交易 时间 
  ;; 转让 时间 
  ((TRANSACTION WORD-TIME-SHI2-JIAN1) *c3*);

  ;; 深圳 面积 
  ((CITY-NAME AREA-OF) *c3*);

  ;; 基金 定投 
  ((FUND-IND V-TOU2) *c3*);

  ;; 工作 精神 
  ;; 工作 压力 
  ;; 工作 生涯 
  ;; 工作 纪律 
  ;; 工作 能力 
  ;; 工作 经验 
  ;; 工作 目标 
  ;; 工作 境界 
  ;; 工作 岗位 
  ((JOB-WORD-GONG1-ZUO4 ANIMATE-ATTRIBUTE) *c3*);

  ;; 工作 初期 
  ((JOB-WORD-GONG1-ZUO4 TIME-PERIOD-NUM-OPT-GE-IND) *c3*);

  ;; 经济 严冬 
  ((CAN-BE-JOB SEASONS) *c3*);

  ;; 他 党 
  ;; 我 政党 
  ;; 他 政党 
  ;; 咱们 党 
  ;; 我们 党 
  ;; 我 党 
  ((PRONOUN POLITICAL-PARTY-IND) *c3*);

  ;; 集装箱 房 
  ((CONTAINER ROOM-LIKE-IND) *c3*);

  ;; 舞台 中间 
  ;; 阳台 对面 
  ((PLATFORM DIR-SUFFIX-PLACE) *c3*);

  ;; 电梯 房 
  ((ELECTRIC-APPLIANCE ROOM-LIKE-IND) *c3*);

  ;; 工作 会议 
  ((JOB-WORD-GONG1-ZUO4 DISCUSSION-MEETING) *c3*);

  ;; 市场 底 
  ((MARKET-WORD-IND DIR) *c3*);

  ;; 货币 政策 
  ((CURRENCY-IND STRATEGY) *c3*);

  ;; 校园 内部 
  ;; 浴室 旁边 
  ;; 宫 周围 
  ((SHOP-LIKE DIR-SUFFIX-PLACE) *c3*);

  ;; 工作 指挥 
  ;; 工作 人员们 
  ;; 工作 人员 
  ((JOB-WORD-GONG1-ZUO4 OCCUPATION) *c3*);

  ;; 本 鉴定 
  ((COST-OR-BOOK-IND V-JIAN4-DING4-B) *c3*);

  ;; 新文 版 
  ((ARTICLE-OR-LANGUAGE VERSION-OF-IND) *c3*);

  ;; 世博会 前夕 
  ((ARRANGED-EVENT-NAME TIME-PT-NUM-OPT-GE-WITHIN-DAY) *c3*);

  ;; 研究 报告 
  ((WORD-RESEARCH INFO-REPORT) *c3*);

  ;; 日 意图 
  ((COUNTRY-ABBR-JAPAN THINKING) *c3*);

  ;; 日 争议 
  ((COUNTRY-ABBR-JAPAN OPINION-OR-DISCUSSION) *c3*);

;;;;

  ;; 副局长 心 
  ((HUMAN-SUFFIX HEART-IND) *c3*);

  ;; 集团 手 
  ((COMPANY WORD-HAND) *c3*);

  ;; 零和 博弈 
  ((WORD-SUM-HE2 V-BO2-YI4) *c3*);

  ;; 自己 角色 
  ;; 自己 班底 
  ;; 自己 领导人 
  ;; 自己 接班人 
  ;; 自己 用户 
  ;; 自己 老人 
  ;; 自己 子子孙孙 
  ;; 自己 小家 
  ;; 自己 成员 
  ;; 自己 后代 
  ;; 自己 渔民 
  ;; 自己 姑娘 
  ;; 自己 班子 
  ;; 自己 公民 
  ;; 自己 国民 
  ;; 自已 国民 
  ;; 自己 国人 
  ((PRONOUN-WORD-SELF HUMAN) *c3*);

  ;; 年轻人 报 
  ((HUMAN NEWSPAPER-LIKE-IND) *c3*);

  ;; 大师 称 
  ((OCCUPATION-SUFFIX NAME-OF-IND) *c3*);

  ;; 荷叶形 版图 
  ((ADJ-SHAPE TERRITORY) *c3*);

  ;; 市场 底 
  ((MARKET-WORD-IND BASIS-IND) *c3*);

  ;; 新文 版 
  ((ARTICLE-OR-LANGUAGE VERSION-OF-IND) *c3*);

  ;; 远洋 舰队 
  ((LANDSCAPE-WATER SHIP-IND) *c3*);

  ;; 人道主义 灾难 
  ((DOCTRINE NATURAL-PHENOMENA-IND) *c3*);

  ;; 公约 组织 
  ((LEGAL-DOCUMENT ORGANIZATION-IND) *c3*);

  ;; 力量 休战期 
  ((POWER TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 行动 未来 
  ((PROCESS AD-HOC-TIME-PT) *c3*);

  ;; 陈词烂调 作 
  ((SONG CREATIVE-WORK-IND) *c3*);

  ;; 研究 经验 
  ;; 研究 能力 
  ((WORD-RESEARCH ANIMATE-ATTRIBUTE) *c3*);

  ;; 高价 药 
  ;; 高价 药品 
  ((PRICE MEDICINE-ORAL-IND) *c3*);

  ;; 国家 周年 
  ((WORD-COUNTRY TIME-PT-NUM-OPT-GE-YEAR) *c3*);

  ;; 胜利 周年 
  ((EVENT TIME-PT-NUM-OPT-GE-YEAR) *c3*);

  ;; 6.23 事件 
  ((TIME-DATE EVENT-IND) *c3*);

  ;; 事变 周年 
  ((BIG-CHANGE-EVENT TIME-PT-NUM-OPT-GE-YEAR) *c3*);

  ;; 我们 王鲁湘 
  ;; 他 缪斯 
  ;; 我 老邓 
  ;; 你 老姜 
  ;; 他 老任 
  ;; 人家 金隅 
  ;; 他 吕玉良 
  ((PRONOUN HUMAN-NAME) *c3*);

  ;; 2007－01－14 14：00：25 
  ;; 07－01－31 18：14：17 
  ;; ２００８－０５－２２ １７：０６：３９ 
  ;; 2012-01-24 12:04:57 
  ;; 2011-10-26 11:38 
  ((TIME-DATE TIME-PT) *c3*);

  ;; 社会主义 道路 
  ;; 资本主义 道路 
  ((DOCTRINE STREET) *c3*);

  ;; 马 力 
  ((HORSE STRENGTH-OR-ABILITY-IND) *c3*);

  ;; 异国 小时 
  ((COUNTRY TIME-PERIOD-NUM-OPT-GE-IND-HOUR) *c3*);

  ;; 強国 心 
  ((COUNTRY HEART-IND) *c3*);

  ;; 辛普森 家 
  ;; 李玫瑾 家 
  ;; 吴强敏 家 
  ((HUMAN-NAME WORD-HOUSE-JIA1) *c3*);

  ;; 盛世 时代 
  ((WORD-LIFE DYNASTY-OR-PERIOD-IND) *c3*);

  ;; 选票 层面 
  ((TICKET-OR-VOTE ASPECT-IND) *c3*);

  ;; １１·２５ 事故 
  ;; １２·２５ 事故 
  ;; 7.23 事故 
  ((TIME-DATE ABSTRACT-MATTER) *c3*);

  ;; 金钱 方面 
  ((PAPER-CASH ASPECT-IND) *c3*);

  ;; 工作 时间 
  ((JOB-WORD-GONG1-ZUO4 WORD-TIME-SHI2-JIAN1) *c3*);

  ;; 雅培 奶粉 
  ((NAME MILK-OR-BOOBS) *c3*);

  ;; 官德 锤炼 
  ((KINDNESS-FAVOUR V-CHUI2-LIAN4) *c3*);

  ;; 官德 塑造 
  ((KINDNESS-FAVOUR V-SU4-ZAO4) *c3*);

  ;; 早晚 事 
  ((TIME-PERIOD-NUM-IND-WITHIN-DAY ABSTRACT-MATTER-IND) *c3*);

  ;; 钱 份 
  ((PAPER-CASH CURRENCY-OR-MARKS-IND) *c3*);

  ;; 政治 幼年期 
  ((POLITICS AD-HOC-TIME-PERIOD) *c3*);

  ;; 大米 市场 
  ((BALL-LIKE-FOOD-PLANT MARKET-WORD-IND) *c3*);

  ;; 大米 协会 
  ((BALL-LIKE-FOOD-PLANT ORGANIZATION-IND) *c3*);

  ;; 12306.cn 网站 
  ((ORDINAL-A-Z WEBSITE-IND) *c3*);

  ;; 社会主义 明灯 
  ((DOCTRINE BULB-LIKE) *c3*);

  ;; 制 硕士 
  ((SYSTEM-POLICY-MECHANISM-MADE-IND OCCUPATION) *c3*);

  ;; 年货 价格 
  ((CARGO PRICE-IND) *c3*);

  ;; 名包 奢侈品 
  ((BAG-OR-BREAD GOODS-IND) *c3*);

  ;; 界 代表 
  ((DOMAIN-CIRCLES WORD-REPRESENTATIVE) *c3*);

  ;; 安玻 公司 
  ;; 富兰克林·邓普顿 公司 
  ;; 邓普顿 公司 
  ;; 格鲁曼 公司 
  ;; 诺思罗普 公司 
  ;; 摩托罗拉 公司 
  ;; 杜邦 公司 
  ;; 穆迪 公司 
  ;; 夏普 公司 
  ;; 浦益斯 公司 
  ;; 罗弗 公司 
  ;; 康塞 公司 
  ;; 莫里斯 公司 
  ;; 亚马逊 公司 
  ;; 王鼎 公司 
  ;; 柯达 公司 
  ((HUMAN-NAME WORD-COMPANY) *c3*);

  ;; 自己 人民 
  ;; 自己 人 
  ((PRONOUN-WORD-SELF HUMAN-IND) *c3*);

  ;; 金条 方面 
  ((GOLD-OR-COST-IND ASPECT-IND) *c3*);

  ;; 德 考 
  ((KINDNESS-FAVOUR EXAMINATION-IND) *c3*);

  ;; 德 测评 
  ;; 德 评价 
  ((KINDNESS-FAVOUR EVALUATION-OF) *c3*);

  ;; 德 考核 
  ((KINDNESS-FAVOUR EXAMINATION) *c3*);

  ;; 金属 制品 
  ;; 金属 织品 
  ;; 五金 制品 
  ;; 不锈钢 制品 
  ((METAL GOODS-IND) *c3*);

  ;; 副主任 委员 
  ((OCCUPATION-RELATIONSHIP COMMITTEE-MEMBER) *c3*);

  ;; 准备 阶段 
  ((FALLBACK-OR-PREPARATION TIME-PT-NUM-OPT-GE-WITHIN-DAY) *c3*);

  ;; 研究 馆员 
  ;; 研究 人士 
  ;; 研究 学者 
  ((WORD-RESEARCH HUMAN-SUFFIX) *c3*);

  ;; 农作物 方面 
  ((PLANT ASPECT-IND) *c3*);

  ;; 番木瓜 证书 
  ((BALL-LIKE-FRUIT LEGAL-DOC-IND) *c3*);

  ;; 甜椒 证书 
  ((LONG-THIN-FOOD-PLANT LEGAL-DOC-IND) *c3*);

  ;; 大事 记 
  ((ABSTRACT-MATTER BOOK-LIKE-RECORD-IND) *c3*);

  ;; 大豆 油 
  ((BALL-LIKE-FOOD-PLANT OIL-IND) *c3*);

  ;; 大豆 制品 
  ((BALL-LIKE-FOOD-PLANT GOODS-IND) *c3*);

  ;; 青铜 时代 
  ;; 青铜器 时代 
  ((METAL-ELEMENT-NAME DYNASTY-OR-PERIOD-IND) *c3*);

  ;; 媒体 手 
  ((CAN-BE-JOB WORD-HAND) *c3*);

  ;; 共产党 手 
  ((POLITICAL-PARTY-NAME WORD-HAND) *c3*);

  ;; 国家 手 
  ((WORD-COUNTRY WORD-HAND) *c3*);

  ;; 阶层 手 
  ((POSITION-OR-LAYER WORD-HAND) *c3*);

;;;;

  ;; 我 神 
  ((PRONOUN GOD-IND) *c3*);

  ;; google 中国 
  ;; 微软 中国 
  ;; 谷歌 中国 
  ((COMPANY-NAME COUNTRY-NAME) *c3*);

  ;; 他 人民 
  ;; 它 人民 
  ;; 我们 人民 
  ((PRONOUN HUMAN-WORD-REN2-MIN2) *c3*);

  ;; 博物馆 方面 
  ;; 裁决所 方面 
  ;; 总统府 方面 
  ;; 医院 方面 
  ;; 教学楼 面 
  ;; 酒店 方面 
  ;; 幼儿园 方面 
  ((SHOP-LIKE ASPECT-IND) *c3*);

  ;; 首 位置 
  ((ORDINAL POSITION-OF) *c3*);

  ;; 春运 期间 
  ((TRANSPORTATION-OF AD-HOC-TIME-PERIOD-WORD-DURING) *c3*);

  ;; 罗罗 公司 
  ;; 菲利浦 公司 
  ;; 威康 公司 
  ;; 飞利浦 公司 
  ;; 万国 公司 
  ;; 花王 公司 
  ;; 明田 公司 
  ;; 伊利 公司 
  ((SURNAME WORD-COMPANY) *c3*);

  ;; 和 少 
  ((WORD-SUM-HE2 ADJ-MORE-LESS) *c3*);

  ;; 界 代表 
  ((DOMAIN-CIRCLES WORD-REPRESENTATIVE) *c3*);

  ;; 人生 岁月 
  ;; 人生 瞬间 
  ((LIFE AD-HOC-TIME-PERIOD) *c3*);

  ;; 放牛班 春天 
  ((CLASS-ORGANIZATION SEASONS) *c3*);

  ;; 01-11 现代 
  ((TIME-DATE DYNASTY) *c3*);

  ;; ９·２１ 灾 
  ;; 5·12 灾 
  ((TIME-DATE NATURAL-PHENOMENA-IND) *c3*);

  ;; 华梵 大学 
  ;; 中央 大学 
  ;; 牛津 大学 
  ;; 吴兴 国小 
  ;; 桑枣 中学 
  ;; 李冰 中学 
  ((HUMAN-NAME SCHOOL) *c3*);

  ;; 图书 方面 
  ((BOOK-LIKE ASPECT-IND) *c3*);

  ;; 界 支持 
  ((DOMAIN-CIRCLES SUPPORT) *c3*);

  ;; 医德 问题 
  ((KINDNESS-FAVOUR POSED-QUESTION) *c3*);

  ;; 社会主义 阶段 
  ((DOCTRINE time-word-stage) *c3*);

  ;; 体制 阶段 
  ((SYSTEM-POLICY-MECHANISM-MADE time-word-stage) *c3*);

  ;; 深圳 方面 
  ;; 汉城 方面 
  ;; 澳门 方面 
  ;; 温哥华 方面 
  ;; 北京 方面 
  ;; 重庆 方面 
  ((CITY-NAME ASPECT-IND) *c3*);

  ;; 厚德 方面 
  ((KINDNESS-FAVOUR ASPECT-IND) *c3*);

  ;; 民族 节日 
  ((RACE-IND HOLIDAY-DAY) *c3*);

  ;; 国家 节日 
  ((WORD-COUNTRY HOLIDAY-DAY) *c3*);

  ;; 我 老天 
  ((PRONOUN SKY-OR-GOD-LIKE) *c3*);

  ;; 违法 因素 
  ((V-WEI2-FA3 REASON) *c3*);

  ;; 工作 市场 
  ((JOB-WORD-GONG1-ZUO4 MARKET-WORD-IND) *c3*);

  ;; 商品 房子 
  ((GOODS ROOM-LIKE-IND) *c3*);

  ;; 钢材 方面 
  ((METAL-ELEMENT-NAME ASPECT-IND) *c3*);

  ;; 工作 疏忽 
  ((JOB-WORD-GONG1-ZUO4 V-SHU1-HU1) *c3*);

  ;; 制 和 
  ((SYSTEM-POLICY-MECHANISM-MADE-IND WORD-SUM-HE2) *c3*);

  ;; 理事国 权利 
  ((COUNTRY RIGHTS-IND) *c3*);

  ;; 主义 路线 
  ((DOCTRINE LINE) *c3*);

  ;; 蓝绿 争斗 
  ((ADJ-COLOR BATTLE) *c3*);

  ;; 制 支持率 
  ((SYSTEM-POLICY-MECHANISM-MADE-IND RATE) *c3*);

  ;; 制 模式 
  ((SYSTEM-POLICY-MECHANISM-MADE-IND STYLE-TREND) *c3*);

  ;; 内地 方面 
  ;; 大陆 面 
  ;; 大陆 方面 
  ((LANDSCAPE-COUNTRY ASPECT-IND) *c3*);

  ;; 蓝绿 看法 
  ((ADJ-COLOR IDEA-OPINION) *c3*);

  ;; 国家 政局 
  ((WORD-COUNTRY POLITICS-STATE) *c3*);

  ;; 零和 游戏 
  ((WORD-SUM-HE2 GAME-LIKE) *c3*);

  ;; 研究 重点 
  ;; 研究 设计 
  ;; 研究 项目 
  ;; 研究 单位 
  ;; 研究 对象 
  ;; 研究 方向 
  ;; 研究 小组 
  ;; 研究 服务 
  ((WORD-RESEARCH ABSTRACT) *c3*);

  ;; 界 言论 
  ((DOMAIN-CIRCLES SPEECH-LIKE) *c3*);

  ;; 界 言行 
  ((DOMAIN-CIRCLES CONDUCT) *c3*);

  ;; 药物 方面 
  ;; 物资 方面 
  ((GOODS ASPECT-IND) *c3*);

  ;; 板桥 捷运站 
  ;; 松山 机场 
  ;; 松山 火车站 
  ;; 玉田 加油站 
  ;; 浦东 机场 
  ;; 北川 县城 
  ;; 福岛 核电站 
  ;; 福岛 电站 
  ((JAPANESE-SURNAME BUILDING) *c3*);

  ;; 空 军 
  ((PHYSICAL-SKY-IND ARMY-LIKE-WORD-IND) *c3*);

  ;; 研究 基地 
  ;; 研究 中心 
  ((WORD-RESEARCH PLACE) *c3*);

  ;; 我 族类 
  ((PRONOUN RACE-OR-GROUP-IND) *c3*);
  ;; 我们 部落 
  ;; 你 部落 
  ;; 我 部落 
  ;; 我 氏族 
  ((PRONOUN RACE-OR-GROUP) *c3*);

  ;; 稀土 制品 
  ((LAND-OR-SOIL GOODS-IND) *c3*);

  ;; 年 展 
  ((ADJ-TIME-PERIOD ARRANGED-EVENT-IND) *c3*);

  ;; 研究 专家 
  ;; 研究 主管 
  ;; 研究 人员 
  ((WORD-RESEARCH OCCUPATION) *c3*);

  ;; 领土 方面 
  ((LAND ASPECT-IND) *c3*);

  ;; 界 主宰 
  ((DOMAIN-CIRCLES ABSTRACT-OR-HUMAN) *c3*);

  ;; 界 努力 
  ((DOMAIN-CIRCLES EFFORT) *c3*)

  ;; 生态 周期 
  ;; 历史 一瞬 
  ((KNOWLEDGE-DOMAIN AD-HOC-TIME-PERIOD) *c3*);

  ;; 人生 阶段 
  ((LIFE time-word-stage) *c3*);

;;;;
  ;; 无知 表现 
  ((ADJ-ON-HUMAN BEHAVIOR) *c3*);

  ;; 阶段 作风 
  ((TIME-WORD-STAGE STYLE-TREND) *c3*);

  ;; 贫穷 帽子 
  ((ADJ-ON-ANIMATE-OR-ORGANIZATION HAT-IND) *c3*);

  ;; 教育 阶段 
  ((TEACHING TIME-WORD-STAGE) *c3*);

  ;; 他 共和党 
  ;; 我们 共产党 
  ((PRONOUN POLITICAL-PARTY-NAME) *c3*);

  ;; 民族主义 旗 
  ;; 民族主义 旗帜 
  ((DOCTRINE FLAG) *c3*);

  ;; 半殖民 国家 
  ((V-ZHI2-MIN2 WORD-COUNTRY) *c3*);

  ;; 上海 电力 
  ;; 上海 特色 
  ;; 深圳 质量 
  ;; 澳门 治安 
  ;; 澳门 特点 
  ;; 常宁 治安 
  ((CITY-NAME INANIMATE-ATTRIBUTE) *c3*);

  ;; 电子 制品 
  ;; 电子 物 
  ((PARTICLES GOODS-IND) *c3*);

  ;; 历史 阶段 
  ((KNOWLEDGE-DOMAIN TIME-WORD-STAGE) *c3*);

  ;; Ｒ 计划 
  ((ORDINAL-A-Z PLAN) *c3*);

  ;; 官僚主义 棍棒 
  ((DOCTRINE STICK-LIKE-TOOL) *c3*);

  ;; 文具 用品 
  ((STATIONERY GOODS-IND) *c3*);

  ;; 陆家嘴 楼宇 
  ;; 牛津 校园 
  ;; 吾尔沃斯 大厦 
  ;; 平壤 体育馆 
  ;; 安祯 医院 
  ;; 舒城县 图书馆 
  ;; 巴斯克 首府 
  ;; 海淀区 饭店 
  ;; 徐汇区 法院 
  ;; 薄斯顿 走廊 
  ;; 罗伯逊 办公大楼 
  ((HUMAN-NAME SHOP-LIKE) *c3*);

  ;; 试验 期间 
  ((EXPERIMENT AD-HOC-TIME-PERIOD-WORD-DURING) *c3*);

  ;; 西门町 影城 
  ;; 苗丽 车站 
  ;; 中顿 机场 
  ;; 奥林匹克 公园 
  ;; 埃雷兹 检查站 
  ;; 山下町 公园 
  ;; 沃特金斯 码头 
  ;; 杜勒斯 机场 
  ((HUMAN-NAME BUILDING) *c3*);

  ;; 工作 地方 
  ;; 工作 中心 
  ((JOB-WORD-GONG1-ZUO4 PLACE) *c3*);

  ;; 音像 制品 
  ((WORD-STATUE-OR-APPEARANCE GOODS-IND) *c3*);

  ;; 机械 方面 
  ;; 设备 方面 
  ((MACHINE ASPECT-IND) *c3*);

  ;; 供水 方面 
  ((WATER ASPECT-IND) *c3*);

  ;; 人生 节日 
  ((LIFE HOLIDAY-DAY) *c3*);

  ;; 水泥 房子 
  ((MATERIAL ROOM-LIKE-IND) *c3*);

  ;; 需求 周期 
  ((NEEDS AD-HOC-TIME-PERIOD) *c3*);

  ;; 工作 简报 
  ((JOB-WORD-GONG1-ZUO4 DOCUMENT) *c3*);

  ;; 我们 厂 
  ((PRONOUN FACTORY-IND) *c3*);

  ;; 工作 班底 
  ;; 工作 角色 
  ((JOB-WORD-GONG1-ZUO4 HUMAN) *c3*);

  ;; 工作 范围 
  ;; 工作 领域 
  ((JOB-WORD-GONG1-ZUO4 REGION) *c3*);

  ;; 啤酒 方面 
  ;; 酒精 方面 
  ((DRINK ASPECT-IND) *c3*);

  ;; 双极紊乱症 方面 
  ((DISEASE ASPECT-IND) *c3*);

  ;; Ｂ 字 
  ((ORDINAL-A-Z WRITTEN-CHARACTER) *c3*);

  ;; 城市 方面 
  ((BUILDING-TYPE-IND ASPECT-IND) *c3*);

  ;; 十一五 规划 
  ((NUM PLAN) *c3*);

  ;; 史前陶 仿制品 
  ;; 塑料 制品 
  ((MATERIAL GOODS-IND) *c3*);

  ;; 工作 进展 
  ((JOB-WORD-GONG1-ZUO4 V-JIN4-ZHAN3) *c3*);

  ;; 原则 阶段 
  ((STANDARD-OF TIME-WORD-STAGE) *c3*);

  ;; 危机 阶段 
  ((FATE-OR-CHANCE TIME-WORD-STAGE) *c3*);

  ;; 电器 用品 
  ((ELECTRIC-APPLIANCE GOODS-IND) *c3*);

  ;; 核反应堆 方面 
  ;; 岸 方面 
  ((PLACE ASPECT-IND) *c3*);

  ;; 电话 厂 
  ((TELE-COMMUNICATION FACTORY-IND) *c3*);

  ;; 外壳 层面 
  ((SHELL-OR-CASING ASPECT-IND) *c3*);

  ;; 危机 期间 
  ((FATE-OR-CHANCE AD-HOC-TIME-PERIOD-WORD-DURING) *c3*);

  ;; 工作 场所 
  ((JOB-WORD-GONG1-ZUO4 SHOP-LIKE) *c3*);

  ;; 年 吞吐量 
  ((ADJ-TIME-PERIOD AMOUNT-OF) *c3*);

  ;; 戈尔 阵营 
  ((HUMAN-NAME CAMP) *c3*);

  ;; 措施 过度期 
  ((STEPS TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 三级跳远 比赛 
  ((SPORTS COMPETITION) *c3*);

  ;; 年 吉祥物 
  ((ADJ-TIME-PERIOD GOODS) *c3*);

  ;; ９·２１ 捐款 
  ((TIME-DATE MONEY-RELATED) *c3*);

  ;; ９·２１ 地震 
  ((TIME-DATE NATURAL-PHENOMENA) *c3*);

  ;; 你 那方 
  ((PRONOUN ANIMATE-SUFFIX) *c3*);

  ;; 机场 方面 
  ;; 加油站 方面 
  ((BUILDING ASPECT-IND) *c3*);

  ;; ｏ 型 
  ((ORDINAL-A-Z SHAPE-OR-TYPE-OF-IND) *c3*);

  ;; ９·２１ 重建 
  ((TIME-DATE V-CHONG2-JIAN4) *c3*);

  ;; １１·２５ 爆炸 
  ((TIME-DATE DISASTER) *c3*);

  ;; ５·４ 运动 
  ;; １２·９ 运动 
  ((TIME-DATE ARRANGED-EVENT-OR-SPORTS) *c3*);

  ;; ３·１５ 活动 
  ((TIME-DATE ARRANGED-EVENT) *c3*);

  ;; 愤怒日 游行 
  ((ADJ-TIME-PERIOD EVENT) *c3*);

  ;; 木板 房 
  ((MATERIAL-WOOD ROOM-LIKE-IND) *c3*);

  ;; ９．１８ 事变 
  ((TIME-DATE BIG-CHANGE-EVENT) *c3*);

  ;; 基督教 节日 
  ((RELIGION-LIKE HOLIDAY-DAY) *c3*);

  ;; 医药 用品 
  ((MEDICINE-ORAL GOODS-IND) *c3*);

  ;; 晶片 厂 
  ;; 主机板 厂 
  ((PLATE-LIKE FACTORY-IND) *c3*);

  ;; 核武 方面 
  ((WEAPON ASPECT-IND) *c3*);

  ;; 国 方面 
  ;; 国 层面 
  ((COUNTRY-IND ASPECT-IND) *c3*);

  ;; M 族 
  ;; Ｍ 族 
  ((ORDINAL-A-Z RACE-OR-GROUP-IND) *c3*);

  ;; 工作 伙伴们 
  ;; 工作 奴隶 
  ;; 工作 夥伴 
  ((JOB-WORD-GONG1-ZUO4 HUMAN-RELATIONSHIP) *c3*);

  ;; 年 栏位 
  ((ADJ-TIME-PERIOD FIELD-OR-COLUMN) *c3*);

  ;; 日光 灯 
  ((ADJ-WORD-LIGHT BULB-LIKE-IND) *c3*);

  ;; 典藏 方面 
  ((VALUABLE-TREASURE ASPECT-IND) *c3*);

  ;; 服饰 成品 
  ((WEARABLE GOODS-IND) *c3*);

  ;; 制片 基础 
  ((VIDEO-PRODUCER FOUNDATION) *c3*);

  ;; 制片 公司 
  ((VIDEO-PRODUCER WORD-COMPANY) *c3*);

  ;; 制片 资金 
  ((VIDEO-PRODUCER MONEY-RELATED) *c3*);

  ;; 制片 成本 
  ((VIDEO-PRODUCER COST-OR-BUDGET) *c3*);

  ;; 民俗 节日 
  ((CUSTOM HOLIDAY-DAY) *c3*);

  ;; Ｒ 原则 
  ((ORDINAL-A-Z STANDARD-OF) *c3*);

  ;; 长期 摇钱树 
  ((ADJ-WORD-CHANG2-QI2 TREE) *c3*);

  ;; 光碟 厂 
  ((SMALL-PLATE-LIKE FACTORY-IND) *c3*);

  ;; 兵马俑 真品 
  ((STATUE GOODS-IND) *c3*);

  ;; 非洲 方面 
  ((CONTINENT-NAME ASPECT-IND) *c3*);

  ;; 研究 耗材 
  ((WORD-RESEARCH WORD-MATERIAL) *c3*);

  ;; 研究 工具 
  ((WORD-RESEARCH TOOL) *c3*);

  ;; 工作 站 
  ((JOB-WORD-GONG1-ZUO4 BUILDING-TYPE-IND) *c3*);
  ((JOB-WORD-GONG1-ZUO4 station-BUILDING-TYPE-IND) *c3*);

  ;; 学分 政策 
  ((CURRENCY-OR-MARKS STRATEGY) *c3*);

  ;; 学分 收费 
  ((CURRENCY-OR-MARKS COST-OR-BUDGET) *c3*);

  ;; 评分 依据 
  ((MARKS EVIDENCE) *c3*);

  ;; 学分 证明 
  ((CURRENCY-OR-MARKS LEGAL-DOC-IND) *c3*);

  ;; 工作 职场 
  ((JOB-WORD-GONG1-ZUO4 BUILDING-TYPE-IND-NS) *c3*);

  ;; 首善之都 执政权 
  ((CITY-WORD-DU1 RIGHTS) *c3*);

  ;; 金 杯子 
  ((GOLD-OR-COST-IND CUP-OR-COMPETITION) *c3*);

  ;; 研究 分析 
  ((WORD-RESEARCH V-FEN4-XI1) *c3*);

  ;; 三角形 关系 
  ((ADJ-SHAPE RELATIONSHIP-OF) *c3*);

  ;; 装饰 用品 
  ((FURNITURE GOODS-IND) *c3*);

  ;; 世界 方面 
  ((WORD-WORLD ASPECT-IND) *c3*);

  ;; 本初 子午线 
  ((ORDINAL PHYSICAL-LINE) *c3*);

  ;; 铁路 方面 
  ((WORD-ROAD ASPECT-IND) *c3*);

  ;; 公路 方面 
  ((STREET ASPECT-IND) *c3*);

  ;; 南昆 铁路 
  ((PLACE-NAME WORD-ROAD) *c3*);

  ;; 签证 方面 
  ((PAPER-LIKE-EVIDENCE ASPECT-IND) *c3*);

  ;; 工作 指导 
  ((JOB-WORD-GONG1-ZUO4 V-ZHI5-DAO3) *c3*);

  ;; 香港 政局 
  ((CITY-NAME POLITICS-STATE) *c3*);

  ;; 工作 不足 
  ((JOB-WORD-GONG1-ZUO4 V-BU2-ZU2) *c3*);

  ;; 工作 办公室 
  ((JOB-WORD-GONG1-ZUO4 MOSTLY-INDOOR) *c3*);

  ;; 革命 阶段 
  ((BIG-CHANGE-EVENT TIME-WORD-STAGE) *c3*);

  ;; 肉 制品 
  ((EDIBLE-PART GOODS-IND) *c3*);

  ;; 稀土 厂 
  ((LAND-OR-SOIL FACTORY-IND) *c3*);

  ;; 贸 公司 
  ((TRANSACTION-IND WORD-COMPANY) *c3*);

  ;; 古文化 艺术节 
  ((WORD-CULTURE HOLIDAY-DAY) *c3*);

  ;; 碳素 制品 
  ((VEGAN-FOOD-OR-ELEMENT-OF GOODS-IND) *c3*);

  ;; 光缆 厂 
  ((PHYSICAL-LINE FACTORY-IND) *c3*);

  ;; 福建 方面 
  ((PROVINCE-NAME ASPECT-IND) *c3*);

  ;; 东亚 版图 
  ((CONTINENT-NAME TERRITORY) *c3*);

  ;; 产品 品类 
  ((GOODS-PRODUCT GOODS-WORD-IND) *c3*);

  ;; 草案 日期 
  ((PROPOSAL-LIKE TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 工作 期间 
  ((JOB-WORD-GONG1-ZUO4 AD-HOC-TIME-PERIOD-WORD-DURING) *c3*);

  ;; 编钟 复制品 
  ((DRUM GOODS-IND) *c3*);

  ;; 藤竹 制品 
  ((LONG-THIN-PLANT-MATERIAL GOODS-IND) *c3*);

  ;; 欧洲 政局 
  ((CONTINENT-NAME POLITICS-STATE) *c3*);

  ;; 羽绒 制品 
  ((HAIR-LIKE GOODS-IND) *c3*);

  ;; 食品 厂 
  ((EDIBLE FACTORY-IND) *c3*);

  ;; 工作 汇报 
  ((JOB-WORD-GONG1-ZUO4 V-HUI4-BAO4) *c3*);

  ;;;;
  ;; 危机 时候 
  ((FATE-OR-CHANCE WORD-TIME-SHI2-HOU4) *c3*);

  ;; 金龙 玉玺 
  ((GOLD-OR-COST-IND TOOL-STAMP) *c3*);

  ;; 社会 转型期 
  ((SOCIETY TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 文章 开始 
  ((ARTICLE-LIKE WORD-TIME-KAI1-SHI3) *c3*);

  ;; 企业 未来 
  ((ORGANIZATION WORD-TIME-FUTURE) *c3*);

  ;; 恶梦 开始 
  ((DREAM WORD-TIME-KAI1-SHI3) *c3*);

  ;; 生产 周期 
  ;; 项目 周期 
  ;; 生命 周期 
  ((ABSTRACT AD-HOC-TIME-PERIOD-WORD-ZHOU1-QI2) *c3*);

  ;; 暴政 日子 
  ((POLITICS TIME-PT-NUM-OPT-GE-DAY) *c3*);

  ;; 他 神灵 
  ((PRONOUN GOD-LIKE-IND) *c3*);

  ;; 纳粹 德国 
  ((DOCTRINE-NAME COUNTRY-NAME) *c3*);

  ;; 运输业 未来 
  ((INDUSTRY WORD-TIME-FUTURE) *c3*);

  ;; 寿命 周期 
  ((INANIMATE-ATTRIBUTE AD-HOC-TIME-PERIOD-WORD-ZHOU1-QI2) *c3*);

  ;; 图形 界面 
  ((ADJ-SHAPE INTERFACE) *c3*);

  ;; 匿名 变化 
  ((ADJ-ON-HUMAN A-CHANGE) *c3*);

  ;; 社会主义 阳光 
  ((DOCTRINE LIGHT) *c3*);

  ;; 地震 时候 
  ;; 雪灾 时候 
  ((NATURAL-PHENOMENA WORD-TIME-SHI2-HOU4) *c3*);

  ;; 成百上千亿 不义之财 
  ((NUM MONEY-RELATED-ATTRIBUTE) *c3*);

  ;; 回程 日期 
  ((PROCESS-OR-RANGE TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; C D 
  ;; B C 
  ;; A B 
  ((ORDINAL-A-Z ORDINAL-A-Z) *c3*);

  ;; 国家 日本 
  ;; 国家 文莱 
  ;; 国家 科威特 
  ((WORD-COUNTRY COUNTRY-NAME) *c3*);

  ;; 关键 时候 
  ((ABSTRACT-KEY WORD-TIME-SHI2-HOU4) *c3*);

  ;; 资金 周期 
  ((MONEY-RELATED AD-HOC-TIME-PERIOD-WORD-ZHOU1-QI2) *c3*);

  ;; 国家 将来 
  ;; 国家 未来 
  ((WORD-COUNTRY WORD-TIME-FUTURE) *c3*);

  ;; 工作 时候 
  ((JOB-WORD-GONG1-ZUO4 WORD-TIME-SHI2-HOU4) *c3*);

  ;; 生活 开始 
  ((LIFE WORD-TIME-KAI1-SHI3) *c3*);

  ;; 民族 未来 
  ((RACE-IND WORD-TIME-FUTURE) *c3*);

  ;; 文革 时候 
  ((EVENT WORD-TIME-SHI2-HOU4) *c3*);

  ;; 节目 一开始 
  ((DRAMA WORD-TIME-KAI1-SHI3) *c3*);

  ;; 预案 时候 
  ((PROPOSAL-LIKE WORD-TIME-SHI2-HOU4) *c3*);

  ;; 高峰 期间 
  ((climax AD-HOC-TIME-PERIOD-WORD-DURING) *c3*);

  ;; 自愿 原则 
  ((ADJ-ON-HUMAN STANDARD-OF) *c3*);

  ;; 英文 自撰 
  ((ARTICLE-OR-LANGUAGE v-zhuan4-a) *c3*);

  ;; 三级跳远 比赛 
  ((SPORTS COMPETITION) *c3*);

  ;; 新闻 一开始 
  ((NEWS WORD-TIME-KAI1-SHI3) *c3*);

  ;; 棒球 岁月 
  ((BALL-SPORTS AD-HOC-TIME-PERIOD) *c3*);

  ;; 诽谤罪 自诉 
  ((SIN-OR-CRIME v-su4-c) *c3*);
  ((SIN-OR-CRIME v-shang5-su4-b) *c3*);

  ;; ９·２１ 时候 
  ((TIME-DATE WORD-TIME-SHI2-HOU4) *c3*);

  ;; 期限 ８点钟 
  ((LIMIT-OF TIME-PT) *c3*);

  ;; 政治 尴尬期 
  ((POLITICS TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 友好 世纪 
  ((ADJ-ON-ANIMATE TIME-PERIOD-NUM-OPT-GE-IND-CENTURY) *c3*);

  ;; 裂变 发展期 
  ((BIG-CHANGE-EVENT TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 免疫学 领域 
  ((study REGION) *c3*);

  ;; 给排水 公司 
  ((drainage-related WORD-COMPANY) *c3*);

  ;; 暖气 替代品 
  ((GAS GOODS-IND) *c3*);

  ;; 经济体 未来 
  ((economy WORD-TIME-FUTURE) *c3*);

  ;; 研究 调查 
  ((WORD-RESEARCH V-TIAO2-ZHA1) *c3*);

  ;; 系统 日期 
  ((SYSTEM TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 高温 周期 
  ((TEMPERATURE-OF AD-HOC-TIME-PERIOD-WORD-ZHOU1-QI2) *c3*);

  ;; V 频道 
  ((ORDINAL-A-Z ABSTRACT-DAO4) *c3*);

  ;; 幼时 经验 
  ((ad-hoc-time-period PAST-EXPERIENCE) *c3*);

  ;; 历练 采收期 
  ((PAST-EXPERIENCE TIME-PERIOD-NUM-OPT-GE-word-cai3-shou1-qi2) *c3*);

  ;; 成果 采收期 
  ((RESULTING-EFFECT TIME-PERIOD-NUM-OPT-GE-word-cai3-shou1-qi2) *c3*);

  ;; 乐团 讲古 
  ((ORGANIZATION v-jiang3-gu3) *c3*);

  ;; 研究 发展 
  ((WORD-RESEARCH V-FA5-ZHAN3) *c3*);

  ;; e世代 热潮 
  ((TIME-PT TREND-OR-WAVE) *c3*);

  ;; 青春 容颜 
  ((ADJ-WORD-QING1-CHUN1 FACE) *c3*);

  ;; 中程 阶段 
  ((PROCESS-OR-RANGE TIME-WORD-STAGE) *c3*);

  ;; 征兵制 役期 
  ((SYSTEM-POLICY-MECHANISM-MADE TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 兵役 役期 
  ((FORCED-LABOR TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 直觉 反应 
  ((feeling-or-sense reaction) *c3*);

  ;; 关键 阶段 
  ((ABSTRACT-KEY TIME-WORD-STAGE) *c3*);

  ;; 回乡证 有效期 
  ((PAPER-LIKE-EVIDENCE TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 研究 框架 
  ((WORD-RESEARCH framework) *c3*);

  ;; 贸 公司 
  ((TRANSACTION-IND WORD-COMPANY) *c3*);

  ;; 实质性 阶段 
  ((ABSTRACT-SUFFIX TIME-WORD-STAGE) *c3*);

  ;; 公司 高峰期 
  ((WORD-COMPANY TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 联合国 周年 
  ((ORGANIZATION-NAME TIME-PT-NUM-OPT-GE-YEAR) *c3*);

  ;; 贸 技 
  ((TRANSACTION-IND SKILL) *c3*);

  ;; 研究 开发 
  ((WORD-RESEARCH V-KAI1-FA5) *c3*);

  ;; 给排水 便 
  ((drainage-related adj-word-bian4) *c3*);

  ;;;;
  
  ;; 项目 施工期 
  ((PROJECT-OR-ITEM TIME-PERIOD-NUM-OPT-GE) *c3*);

  ;; 水产 加工品 
  ((GOODS-PRODUCT GOODS-IND) *c3*);
  ;;;;
  )
;; end mod-noun-common

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mod de noun

;; for passing the word length of m and n to the generic function
;; mod-de-noun, and mod-noun
(defparameter *L1* 1)
(defparameter *L2* 1)
;; for passing the separator to constraints such as similar-noun
(defparameter *sep* nil)

(defun-conn mod-de-noun-x (m n) mod-de-noun-xx)
(defgeneric mod-de-noun-xx (m n))
(def-comb mod-de-noun-xx
  ;; the fallback case
  (((m t) (n t)) (mod-noun-common m n))

  ;; for other specific biase for mod-de-noun
  (((m adj) (n noun))
   ;; bias against adj-no-mono-de
   (score-add
    (mod-noun-common m n)
    (if (= *L1* 1)
        (not-prefer m 'adj-no-mono-de *c-rare*)
        0)))

  (((m yong4-adj) (n noun))
   (mod-de-noun (yong4-adj-the-verb m) n *L1* *L2*))
  ;;
  ((company word-hand) *c4*) ;; 集团 之 手

  ((occupation-suffix name-of-ind) *c4*) ;; 大师 之 称
  ((human-ind theory-word-dao4) *c2*) ;; e.g. 人 之 道

  ((adj-on-noun currency-or-marks-ind) *c2*) ;; 贵贱 之 分

  ((amount-of adj-more-less) *c1*) ;; XX 之 少
  ((value-of adj-more-less) *c1*) ;; XX 之 少

  ((amount-of v-zeng1-jia1) *c2*) ;; XX 之 增加
  ((value-of v-zeng1-jia1) *c2*) ;; XX 之 增加

  ((verbs time-word-stage) *c1*) ;; e.g. '教育 阶段', '建设 阶段'
  ((verbs opportunity) *c2*) ;; e.g. 开战 时机
  ((verbs wave) *c3*) ;; e.g. 剧变 浪潮
  ((verbs V-BIAO3-ZHANG1) *c3*) ;; e.g. 见义勇为 表彰 
  ((verb martial-arts) *c4*) ;; e.g. 拍照 的 功夫 
  ((verbs STYLE-TREND-IND) *c4*) ;; 崇洋媚外 风 
  ((verbs REASON) *c4*) ;; 变动 因素 
  ((army-landscape army-like) *c2*)
  ((army-landscape battle) *c2*)
  ((verbs word-method) *c3*) ;; 征税 做法 
  ((verbs perspective) *c3*) ;; 扶养 角度 
  ((verbs project-or-item) *c3*) ;; 勘查 项目 
  ((verbs suspicion-for-guilty) *c3*) ;; 诈欺 罪嫌 
  ((verbs abstract-cost-to-pay) *c3*) ;; 嬉闹 代价 
  ((verbs NAME-OR-REPUTATION) *c3*) ;; 诈欺 罪名 
  ((verbs trip-ind) *c1*) ;; 破冰 之 旅
  ((place trip-ind) *c1*) ;; 美國 之 旅
  ((verbs stress) *c3*) ;; 救亡图存 压力 
  ((adj-word-successful key) *c4*) ;; 成功 钥 
  ((verbs point-ind) *c2*) ;; 起點
  ((verbs composition-of) *c3*) ;; 误导 成分
  ((verbs adj-word-important) *c3*) ;; 同心协力 重要
  ((v-suggest non-verb-noun) *c-common*) ;; prefer verb-x 提出

  ((word-nan2-de t) *c-very-rare*) ;; prefer '男 的' as noun + zhi
  ((word-nu3-de t) *c-very-rare*) ;; prefer '女 的' as noun + zhi

  ((verbs process) *c2*) ;; '围堵 的 过程'
  ((verb-n process) *c3*) ;; '(围堵“蓝衣男”) 的 过程'

  ((v-xiao1-zhang1 animate-attribute) *c3*) ;; '嚣张 气焰'
  ((v-xiao1-zhang1 attitude) *c2*) ;; '嚣张 態度'

  ((place population-count) *c1*) ;; 北部 人口
  ((dir-suffix-place population-count) *c1*) ;; 北部 人口

  ((adj-word-lian2-xu4 time-period) *c0*) ;; 连续4天

  ((word-pollution position-of) *c1*) ;; 污染 水平

  ((weather-convergence-divergence strip-or-region-like) *c1*) ;; 辐合带
  ((weather-convergence-divergence region) *c1*) ;; 辐合带

  (((m pressure) region)
   (if (trait-value m 'has-adj-noun-mod)
       ;; 低压 辐合带
       *c1*
       *c3*))
  (((m pressure) strip-or-region-like)
   (if (trait-value m 'has-adj-noun-mod)
       ;; 低压 辐合带
       *c1*
       *c3*))

  ((place situation) *c2*) ;; 地区 环境

  ((info-warning ad-hoc-time-period-word-during) *c1*) ;; 橙色预警 期间

  ((health protection) *c1*) ;; 健康 防护
  ((adj-word-healthy protection) *c4*) ;; prefer '健康' as noun in '健康 防护'

  ((rough-amount (n place))
   ;; '部分 地区', but not prefer '部分 中國'
   (+ *c2* (not-prefer n 'nr-name *c-close-suffix*)))
  ((rough-amount visibility) *c2*) ;; prefer '(部分 地区) 能见度' to '部分 (地区 能见度)'

  ((v-shou4-shang1 abstract-matter) *c-common*) ;; prefer '(NN 受伤) 的 事故'

  ((v-lin2-jin4-b place) *c2*) ;; '临近 的 台湾、日本和韩国'

  ((health influence) *c2*) ;; '健康影响'

  ((word-pollution source) *c2*) ;; 污染 来源

  ((word-pollution v-zhi4-li4-c) *c2*) ;; 'XX 治理'
  ((weather v-zhi4-li4-c) *c2*) ;; 'XX 治理'
  ((natural-phenomena v-zhi4-li4-c) *c2*) ;; 'XX 治理'
  ((gas v-zhi4-li4-c) *c2*) ;; '空气 治理'

  ;; prefer verb form for '過去' when modifying time, because it is
  ;; more flexible, e.g. '在刚过去的冬天'.
  ((word-time-guo4-qu4 time-name) *c-rare*)

  ((inanimate dependency-of) *c2*) ;; '煤炭 依赖'
  ((medicine dependency-of) *c1*)
  ((fuel dependency-of) *c1*) ;; '煤炭 依赖'
  ((oil dependency-of) *c1*)
  ((drink dependency-of) *c1*)
  ((body-part dependency-of) *c4*)

  ((gas concentration-of) *c1*) ;; '细悬浮微粒 浓度'
  ;;;;

  ;; 同心协力 重要 
  ((V-TONG2-ZHU3-XIE2-LI4 adj-word-important) *c3*)

  ;; 转变 影响 
  ((V-ZHUAN4-BIAN4-B influence) *c3*)

  ;; 异曲同工 妙 
  ((V-YI4-QU1-TONG2-GONG1 adj-word-miao4) *c1*)

  ;; 保护 两难 
  ((V-BAO3-HU4 difficulty) *c3*)

  ;; 调改会 指控 
  ((organization V-ZHI5-KONG4) *c2*)
  ((animate V-ZHI5-KONG4) *c2*)

  ;; 大刀阔斧 建设 
  ((V-DAI4-DAO1-KUO4-FU3 FACILITY-LIKE) *c3*)

  ;; 诈欺 超贷 
  ((V-ZHA4 v-chao1-dai4) *c3*)
  ((v-qi1-zha4 v-chao1-dai4) *c3*)

  ;; 不景气 影响 
  ((V-JING3-QI4 influence) *c2*)

  ;; 再生产 角度 
  ((V-ZAI4-SHENG1-CHAN3 perspective) *c2*)

  ;; 工作 便 
  ((JOB-WORD-GONG1-ZUO4 adj-word-bian4) *c1*);

  ;; 夸大 成份 
  ((V-KUA1-DAI4 composition-of) *c3*)

  ;; 实施 步骤 
  ((V-SHI5-SHI1 STEPS) *c3*)

  ;; 微循环 血液 
  ((V-XUN2-HUAN2 LIQUID) *c3*)

  ;; 营林 投入 
  ((V-YING2-LIN2 V-TOU2-RU4) *c3*)

  ;; 连载 一开始 
  ((V-LIAN2-ZAI4 WORD-TIME-KAI1-SHI3) *c3*)

  ;; 宽恕 火花 
  ;; 宽恕 火 
  ((V-KUAN1-SHU4 FIRE) *c3*)

  ;; 音乐 未来 
  ((MUSIC WORD-TIME-FUTURE) *c3*)

  ;; 各种各样 骗税 
  ((V-GE4-ZHONG4-GE4-YANG5 V-PIAN4-SHUI4) *c3*)

  ;; 关系 未来 
  ((RELATIONSHIP-OF WORD-TIME-FUTURE) *c3*)

  ;; 事件 未来 
  ((EVENT-IND WORD-TIME-FUTURE) *c3*)

  ;; 考研 升温 
  ((V-KAO3-YAN2 V-SHENG1-WEN1) *c3*)

  ;; 满目疮痍 场面 
  ((V-MAN3-MU4-CHUANG1-YI2 SITUATION) *c3*)

  ;; 行善 名义 
  ((V-XING2-SHAN4 IN-THE-NAME-OF) *c3*)

  ;; 冷嘲热讽 幽默 
  ((V-LENG3-CHAO2-RE4-FENG3 ADJ-WORD-YOU1-MO4) *c3*)

  ;; 排卵 诱发剂 
  ;; 解毒 冲剂 
  ((VERB-N LIQUID-LIKE-AGENT) *c2*)

  ;; 工作 进行 
  ((JOB-WORD-GONG1-ZUO4 V-JIN4-XING2) *c3*)

  ;; 实事求是 原则 
  ((V-SHI2-SHI4-QIU2-SHI4 STANDARD-OF) *c3*)

  ;; 一体化 深入 
  ((HUA-VERB V-SHEN1-RU4-B) *c3*)

  ;; 再就业 工作 
  ((V-JIU4-YE4 JOB-WORD-GONG1-ZUO4) *c3*)

  ;; 让步 通牒 
  ((V-RANG4-BU4 PAPER-LIKE-EVIDENCE) *c3*)

  ;; 灭顶 灾 
  ((V-MIE4-DING3 NATURAL-PHENOMENA-IND) *c3*)

  ;; 现代化 洗礼 
  ((HUA-VERB V-XI3-LI3) *c3*)

  ;; 悲悯 情 
  ((V-BEI1-MIN3 FEELING-OR-SENSE-OR-SITUATION-IND) *c3*)

  ;; 陆海空 作战 
  ((PHYSICAL-SKY V-ZUO4-ZHAN4-a) *c3*)

  ;; 轮替 意义 
  ((V-LUN2-TI4 MEANING) *c3*)

  ;; 民主化 成功 
  ((HUA-VERB V-CHENG2-GONG1) *c3*)

  ;; 参拜 列 
  ((V-SHEN1-BAI4 SERIES-IND) *c3*)

  ;; 研究 出版 
  ((WORD-RESEARCH V-CHU1-BAN3) *c3*)

  ;; 鼓励 话 
  ((V-GU3-LI4 SPOKEN-LANGUAGE-IND) *c3*)

  ;; 后殖民 孤立 
  ((V-ZHI2-MIN2 V-GU1-LI4) *c3*)

  ;; 工作 执行 
  ((JOB-WORD-GONG1-ZUO4 V-ZHI2-HANG2-C) *c3*)

  ;; 研究 分工 
  ((WORD-RESEARCH V-FEN4-GONG1) *c3*)

  ;; 工作 推动 
  ((JOB-WORD-GONG1-ZUO4 V-TUI1-DONG4) *c3*)

  ;; 工作 展开 
  ((JOB-WORD-GONG1-ZUO4 V-ZHAN3-KAI1) *c3*)

  ;; 欢迎 仪式 
  ((V-HUAN1-YING2 PROCESS) *c3*)

  ;; 起义 爆发 
  ((V-HUO3-YI4 V-BAO4-FA5-B) *c3*)

  ;; 休戚与共 字 
  ((V-XIU1-QI1-YU3-GONG4 WRITTEN-CHARACTER) *c3*)

  ;; 软着陆 市场 
  ((V-RUAN3-ZHUO2-LU4 MARKET-WORD-ind) *c3*)

  ;; 感激 心 
  ((V-GAN3-JI1 HEART-IND) *c3*)

  ;; 抗震救灾 胜利 
  ((V-KANG4-ZHEN4-JIU4-ZAI1 EVENT) *c3*)

  ;; 抗震救灾 斗争 
  ((V-KANG4-ZHEN4-JIU4-ZAI1 BATTLE) *c3*)

  ;; 抗灾救灾 斗争 
  ((V-KANG4-ZAI1 BATTLE) *c3*)

  ;; 社会主义 阳光 
  ((DOCTRINE LIGHT) *c3*)

  ;; 自己 力 
  ((PRONOUN-WORD-SELF STRENGTH-OR-ABILITY-IND) *c3*)

  ;; 关爱 意 
  ((V-GUAN1-AI4 INTENTION-OR-FEEL-IND) *c3*)

  ;; 千丝万缕 关系 
  ((V-QIAN1-SI1-MO4-LV3 RELATIONSHIP-OF) *c3*)

  ;; 精萃 词 
  ((V-CUI4 WRITTEN-CHARACTER-POEM-LIKE) *c3*)

  ;; 依恋 情 
  ((V-JUAN4-LIAN4 FEELING-OR-SENSE-OR-SITUATION-IND) *c3*)

  ;; 拜祭 用 
  ((V-BAI4-JI4 V-USE) *c3*)

  ;; 众人 力 
  ((HUMAN STRENGTH-OR-ABILITY-IND) *c3*)

  ;; 骑虎难下 汹涌 
  ((V-QI2-HU3-NAN4-XIA4 LANDSCAPE) *c3*)

  ;; 现代化 道路 
  ;; 股份化 道路 
  ;; 法制化 道路 
  ((HUA-VERB STREET) *c3*)

  ;; 见义勇为 称号 
  ((V-JIAN4-YI4-YONG3-WEI4 NAME-AS-SIGN) *c3*)

  ;; 蜕变 路 
  ((V-TUI4-BIAN4 ROAD-IND) *c3*)

  ;; 中庸 之 道 
  ((ADJ-ON-HUMAN-NM THEORY-WORD-DAO4-OR-STREET) *c3*)

  ;; 统一 路 
  ((V-TONG3-YI1 ROAD-IND) *c3*)

  ;; 统一 果 
  ((V-TONG3-YI1 FRUIT-OR-RESULTING-EFFECT) *c3*)

  ;; 认识 问题 
  ((V-REN4-SHI5 POSED-QUESTION) *c3*)

;;;;
  )

(defun mod-de-noun (m n L1 L2)
  (let ((*L1* L1)
        (*L2* L2)
        (penalty 0))
    (when (trait-value n 'has-num-unit-suffix)
      ;; prefer the num-unit to be attached to outer scope
      (incf penalty *c-close-sep*))
    (when (trait-value n 'time-with-ind)
      ;; not want '男 的 (在用手机偷拍咱们)' as time
      (incf penalty *c-common*))
    (score-add penalty (mod-de-noun-x m n))))
;; end mod-de-noun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mod-de-noun-tag (m n L1 L2)
  (declare (ignore L2))
  (let ((mt (noun-mod-trait m L1)))
    (if (and mt n)
        (more-trait n
                    mt t
                    'noun-mod-has-de t)
        (more-trait n 'noun-mod-has-de t))))

(defparameter *mod-de-noun-tag-t1-t3*
  (tn (mod-de-noun-tag t1 t3 L1 L3)))
(defparameter *mod-de-time-tag-t1-t3*
  (tn (combine-time-scale-tag
       t1
       (mod-de-noun-tag t1 t3 L1 L3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The current whitelist of class names of verbs that can be mod in mod-noun

(defparameter *verb-in-mod-noun-list*
  '(V-JI4-D V-ZHONG4-B V-SHI4-J V-HUAN2 V-ZHAN4-B V-NENG2 V-XUE2 V-KE1-A V-JI2-E
    V-ZHI4-F V-XIN4 V-ZI1 V-XIAO1-C V-LING2-DAO3 V-DANG1 V-WU3-A V-WANG3-A
    V-BAN4-B V-BIAO1-B V-KNOW V-QI1-B V-LING2 V-YI4-E V-SI5 V-JU1-C V-DAO3-A
    V-XIAN4-D V-YI1-A V-YA1-B V-CHENG2-F V-FEI1-A V-DAO4-D V-XIE2-ZHU4 V-LI4-A
    V-BAO3-A V-LI4-B V-GU4-A VERBS V-FA5-YAN2-B V-SHI2-B V-REN2 V-GUAN1-C V-JING1
    V-XING2 V-TONG2 V-PIN3 V-HANG2-ZHENG4 V-FEN4 V-BI3 V-GONG4 V-DO V-SHU3-A
    V-HANG2-B V-DONG4-B V-TUAN2 V-JIAO1-C V-GUAN1-A V-YI4-H V-CHOOSE V-BIE4
    V-BING4-B V-BEI4-C V-ZHENG4-A V-SHU1-A V-QU1-D V-DAI4-BIAO3 V-YAN2-JIU1
    V-BAO4-B V-KUN4 V-SEE V-YUN4-DONG4 V-ZONG1-A V-DI2-A V-JIAN4-B V-USE V-LIAN2-C
    V-ZHA2 V-WEI4-F V-XIAN2-B V-DAI4-B V-XING1 V-DAI4-C V-GAN3-B V-ZHAO4-B
    V-SHEN1-YI4 V-JING4-ZHENG1 V-KE4-NENG2 V-LUN4 V-WU3-B V-TOU2-ZI1 V-YAN2
    V-HUA4-A V-HUA4-C V-SWIM-IN V-GEN1-A V-JIA4-ZHI2 V-JIE2-A V-XI4-C V-MA3
    V-ZHENG1-C V-YING3-XIANG3 V-SI1-A V-YIN4 V-SNEAK V-GE2-A V-MING4 V-ZHENG1-E
    V-YI2-A V-DA2-A V-JIANG1-A V-YAN2-C V-JI2-ZHUANG1 V-ZHI1-CHI2-B V-ZHU3-CHI2
    V-ZHI5 V-LIU2-A V-SHI5-YAN4 V-GUAN3 V-HU4 V-ZHENG3 V-LV3 V-LIN2-A VERBS
    V-JIAN4-ZHU4 V-CHENG2-E V-YAN3-B V-XUAN1 V-YUAN4-B V-TONG4 V-QU1-ZHU2
    V-GONG1-ZUO4-B V-JIAO1-YU4 V-ZHI2-SHENG1 V-YOU5 V-HAN2-B V-BIAO3 V-PAO1-A
    V-ZHONG4-A V-JIANG3-A V-ZHI1-B V-MAI4-B V-ZUO4-TAN2 V-CHU3 V-CHUANG4
    VERB-DEGREE V-KUANG4 V-ZHAN4-DOU4 V-YUE4-D V-DU2-A VERB-DEGREE V-TIE4
    V-GU4-YONG4-A V-TU2-B V-XI5 V-SHI4-WEI1-B V-GU4-C V-TI2 V-JIAO1
    V-HUAN4-B V-HUI4-A V-JUE2-D V-MAO4-YI4 V-YU2-A V-JI4-NIAN4 V-BA4-B V-ZU2
    V-DE5-ZHEN4 V-ZHI2-B V-JIA1-YOU2 V-GUAN4-B V-ZHENG1-D V-TING2-PO1 V-SHI5-C
    V-YUN4 V-SHENG3 V-WAIT V-PIAN4 V-JIANG1-B V-HU1 V-ZHI4-LIAO2 V-PAO3
    V-CHU1-KOU5 V-QIU1-JIN4 V-CHAO2 V-LIANG5 V-YAN3-CHANG4 V-HAO4-A V-ZHU4-C V-FA5
    V-DER V-TING1-ZHENG4 V-ZU3 V-RAN2-A V-LOU4-B V-LIE4 V-HUN1-B V-TONG3-CE4
    V-YIN3-A V-FU4-WU4 V-TAN2-A V-XIA2 V-SHENG1-CHAN3 V-BIAN4-D V-GUAN1-B
    V-ZHAO1-B V-CE4-B V-RONG2-B V-YI3 V-PING2-A V-LIAN2-YI4 V-HUN1-A V-FEN4
    V-PASS-TO V-ZA4-YE3 V-FAN3-PAN4 V-WAN2-B V-JIN4-B V-ATTRACT V-KA3 V-SHI1-B
    V-DING4-B V-KUAN3 V-YU4-D V-ZHAO1-A V-YU4-F V-GEN1-JU1 V-ZHI5-HUI1 V-ZHU4-D
    V-JIN4-FEI1 V-JIAO1-YI4 V-SU4-B V-QING4 V-XING4-B V-ZOU3 V-ZHAO4-JI2 V-SAI4-A
    V-CHU2-B V-ZU1 V-QI4-E V-IS V-JIAN4-C V-ZHEN4-B V-JIAN4-A V-SHI4-M V-XIAN4-C
    V-YIN3-C V-YOU2-JI1 V-CHENG1-B V-MAI3 V-XING4-CUN2 V-PING2-LUN4 V-YING2-C
    V-SUAN4 V-ZHI2-A V-SI1-FA3 V-ZHAN4-YOU2 V-HE2 V-YA1-A V-XIAO4-B V-PAN4-A
    V-GUAN1-CHA2 V-HUA4-YU3 V-HUA1 V-SHENG4-B V-CHUANG4-ZUO4 V-BAO4-ZHA2 V-SHUN4
    V-TONG4-HUO4-PENG2-ZHANG4 V-ZHU4-HE4 V-ZONG1-B V-NIAN4 V-CHI2-C V-ZE2-REN2-B
    V-QUE1 V-SHE4-C V-SHOU4-A V-SHOU4-NAN4 V-KONG3 V-WANG3-B V-JIAN4-DU1
    V-ZHANG3-A V-HUI4-TU2 V-XUAN1-ZHUAN4-A V-ZU3-ZHI1 V-CHOU2-A V-GE2-D V-YU2-B
    V-SAI4-B V-BAO3-ZHENG4 V-SUO3-ZA4-B V-YU2-YUE4-B V-BAO5 V-XI2-A V-ZHI4-ZUO4
    V-CHEN4-A V-JIAN3-B V-PREVENT V-WANG4-XIANG1 V-JU1-A V-SI3-NAN4 V-ZAO4
    V-SHUI4-FU4 V-PAI4 V-AI4-HAO3 V-FAN4 V-TONG4-HUO4-PENG2-ZHANG4 V-JIAN4-SHE4
    V-BANG1 V-FU4-JIA1 V-CHENG2-JIAN4 VERB-DEGREE V-FU2-C V-YI2-LIU2-XIA4
    V-AN1-ZANG4 V-JI3-JI3-A V-ZHE1-A V-JIE4-A V-TAO3-LUN4 V-BIAN1 V-HAS
    V-YAN2-REN2 V-LI2-NAN4 V-DING4-JU1 V-DENG3-HOU4 V-PAI1 V-CUO4-A V-PI1-A
    V-DIAN3 V-FU4-GAI4 V-BU2 V-ZHUAN4-RAN3 V-FEI4-QI4 V-BAO3-WEI4 V-RUI4 V-JUMP
    V-QIANG3-A V-SHEN1-QING3 V-DIE1 V-SUO3-YOU2 V-SHI1-C V-XU1-QIU2 V-ZE2-JIAO4
    V-FAN3-B V-HUI2-YI4 V-ZUO4-A V-DAI4-LI4 V-GENG1 V-HERD V-KAN1-A V-BU4-A V-FALL
    V-HU4-WEI4 V-ZAO3-CHAN3 V-LIN2-B V-FANG2 V-DANG1-QUAN2 V-SHA1-B V-XUN4-HUA4
    V-BU4-B V-ZHAN3-LAN3 V-CE4-HUA2-B V-HUO3-ZHENG1 V-ZHUAN4-B V-SHENG1-HAI2
    V-BAN3 V-KAO3-B V-SHE4-CE4 V-SHENG1-HUO2 V-CUN2 V-JIAN4-D V-QU1-E V-RELY-ON
    V-YI2-MIN2 V-ZHONG1-ZHI3 V-FA1-GAI3 V-PAI2-HANG2 VERBS V-LIAN2-SUO3 V-FU2
    V-XIE4-SHI4-B V-BING4-SI3 V-YING2-A V-TAN1-A V-FA5-TIE4 V-SHU4-A V-JI4-LU4-A
    V-JU4-B V-QI3-A V-GONG1-YING4-B V-YA3 V-NING2-JU4 V-JING4-C V-QUAN1 V-XIE3
    V-BI4-YUN4 V-WU1-RAN3 V-TAO2 V-JU3 V-ZHUI1-SUI2 V-JIN3-B V-XING4-A V-DAI4-YAN2
    V-SHOU1-FEI4 V-AN1-B V-CU4-JIN4 V-LU4-YIN1 V-ZHENG1-A V-ZUO4-QU1 V-ZU3-HE2
    V-SHI4-YAN4 V-JING1-XIAO1 V-DING4-JIA4 V-FENG1-C V-SHUI4-MING2 V-LI4-PEI2
    V-YUAN2-A VERBS V-SHOU3 V-JIE2 V-CHU4-A V-DA5 V-PING2-B V-WEI2-C V-TONG4-ZHI1
    V-XIAN4-JIA4-A V-JI3-HAO2 V-CHAO3-FANG2 V-ZHEN1-DUI4 V-HUNT V-TAN2-PAN4
    V-TU1-PO4 V-KUI1-SUN3 V-WA3 V-QI3-HUA4 V-QI3-TU2 V-TONG4-HUA4 V-JI4-B V-JIU4-A
    V-BAO4-GAO4 V-XIE2-YI4 V-CHU1-RU4 V-ZHEN3-ZHI4 V-DI1-MING2 V-ZHI4-PIAN4
    V-CHENG2-JIU4 V-NIAO4-B V-LING3 V-XIAN3-SHI4 V-DAI4-MAI4 V-JIAN3-ZHA1 V-CHONG3
    V-ZHAO1-C V-GUAI3 V-BEI4-A V-TAO4-A V-GUO5-LAI2 V-NAO4 V-XUAN3-JU3
    V-XI1-SHENG1 V-CHUANG4-ZAO4 V-DU3-C V-WO1 V-GAI3-GE2 V-DIAO4-B V-YUAN2-C
    V-ZHENG1-DUO2 VERBS V-PREPARE V-BAO4-FA5-A V-JUE2-CE4 V-JIA4-C V-SHEN1-B
    V-SUN3 V-DI1-B V-XUAN2 V-YOU1-B V-YI2-B V-TUO1-LA1 V-JIANG4-JIE3 V-ZHI4-LENG3
    V-CHU2-CHU2 V-ZHANG4-CHAO2 V-CHU1-HAI3 V-LOST-WAY V-YUN2-B V-LIAO3
    V-CHOU2-BEI4 V-CHOU1-SHUI4 V-JI3-JIU4 V-JU1-LIU2-B V-ZHAN3-A V-DU4-B V-JIA3
    V-CAO1 V-SHOU1-CANG2 V-XIA4-SHUI4 V-FANG3 V-DIAO1-KE4 V-TUO1-C V-SU4-SONG4
    V-DANG1-ZHENG4 V-HUA4-ZHUANG1 V-JUE2-YI4-B V-YAO2 V-ZHI4-D V-GEN1-B V-ZAI4-A
    V-XIU4-A V-BAI4-B V-JUAN4 V-QU3-KUAN3 V-SLEEP V-FU4-E V-WANG2 V-FAN3-YING4-A
    V-SHENG1-A V-JIE2-B V-DING4-A V-YAN1-A V-LIAN2-B V-TIE1 V-FU4-E V-YU4-BEI4
    V-DOU4-B V-JIN4-CHU1-KOU5 V-SAN3 V-BAO3-ZHANG4 V-PUSH V-LI2 V-YOU4-A V-PAN4-A
    V-RUSH-THROUGH V-FANG4 V-ZHEN4-A V-FAN1-YI4 V-JI1-B V-ZI1-XUN2-B V-GONG1-SU4
    V-GAN4-SHI4 V-SHOU4-FANG3 V-WEI2-B V-SHI4-K V-ZHEN3 V-MIAN3 V-RIDE-ON
    V-GUANG3-GAO4 V-CHENG2-ZU1 V-QUAN2-SHI4 V-ZHUA1 V-YUE4-DU2 V-JIANG3-B
    V-YI1-CUN2 V-TIAO2 V-WU4-E V-COMMAND V-QI3-ZHONG4 V-HUI2-BAO4 V-JIA4-JU1
    V-FOLLOWING V-BI4-FENG1 V-JING3-TI4 V-JU2-XIAN4 V-YI4-F V-GUO5-LV4 V-BA1-A
    V-LUO3-LOU4 V-ZHUANG1-SHI4 V-SHI2-A V-MO2-B V-YONG1-YOU2 V-GONG1-FANG2
    V-PI1-PING2 V-XING1-FEN4 V-PEI2-LIAO2 V-JIE1-C V-XIAO1-B V-CE4-HUA2-A
    V-ZHUANG4 V-JI3-ZUN1 V-DAI4-TOU5 V-JIA4-A V-ZHU4-G V-GAI4 V-DIAN3-JI1 V-NU4
    V-ZHAN4-C V-JIN4-KOU5-B V-YUAN4-A V-MO5 V-ZHI4-A V-XI3-SHOU5 V-ZHAO1-B
    V-DAI4-KUAN3 V-SHOU4-YI4-B V-SHU3-B V-JIANG4 V-CHI2-JIU3 V-TIAN1-JIA1
    V-SI4-YANG3 V-XIE4-B V-YAN3-ZOU4 V-DENG1-JI4 V-CHEN2-B V-CHI2-YOU2 V-ZHI1-A
    V-ZHUI1 V-DAO3-C V-YANG3 V-QI3-HUA4 V-SHI4-F V-BU3-XI2 V-BING4-FA1 V-ZU3-NI2
    V-CHU2-CANG2 V-JIA1-A V-GE2-C V-JIAN3-YAN4 V-YAO2-TOU5 V-SHENG1-MING2 V-SHU2
    V-ZHAI1-B V-GUAN4-A V-BIAN4-A V-PEI2-SHEN3 V-CONG1 V-JUAN1 V-QIAN1-MING2
    V-FAN2-A V-SAO4 V-BI4-C V-LIAN2-XI5-B V-YING4-DA2 V-ZUAN1 V-TU1-ZAI3 V-YIN3-B
    V-HERD-SHEEP V-SHAN3-GUANG1 V-FU4-YIN4 V-DAO4-B V-FEI1-B V-WAN1 V-HUO4-JIANG3
    V-HUA4-C V-ZHENG1-YI4 V-PIAO2-CHANG1 V-GONG4-RONG2 V-ZHI2-YING2 V-XUAN3-ZE2
    V-FA1-XIANG2 V-FEI3-BANG4 V-GU4-B V-CHU1-RANG4 V-ZHONG1-JIE2 V-SHAN1-DONG4
    V-QI3-QIU2-B V-PAN2 V-JU4-F V-ZHE1-XIU1 V-GUAN1-XI5 V-HUO3-CAO3 V-JIAO3
    V-DONG4-CHA2 V-YUE4-B V-CI4-B V-LA4-JIAO3 V-XIAN4-E V-MOU2-B VERBS V-SONG1
    V-SHENG1-SUO3 V-DA2-B V-YAN3-A V-CHANG4-YI4 V-FU4-C V-CAI3-SHOU1 V-TUO1-A
    V-ZHONG4-CAI2 V-LIAO4 V-LIAN2-JIE1-B V-GUA4 V-JI2-F V-JI3-GEI3 V-BO2-A
    V-CHONG2-SHANG1 V-GONG4-XIAN4 V-CHI2-ZHENG4 V-INSURED V-PAI2-MING2 V-FU4-HE4
    V-BAO3-JIAN4-A V-JIE4-SHAO4 V-BAN4-XIE4-FANG4 V-DUAN4 V-NI4-B V-ZHA1-A
    V-XIU4-C V-RI4-YONG4 V-HA1-TIAN1 V-SHOU3-HU4 V-HUA2-A V-ZHEN1-XUAN3 V-XU4-B
    V-JI1-A V-YU4-FU4 V-XIU4-B V-ZONG4 V-SHI3 V-XIANG3-XIANG4-A V-ZHAN4-SHENG4
    V-CAI3-B V-JIAN4-DING4-B V-YAN3-CHU1 V-TUN2-KEN3 V-DONG4-YI4 V-CHUAN2-ZHEN1
    V-KAI1-LIE4 V-LA1-JU4 V-KENG1-B V-KONG3-XIA4 V-DA2-HUO3 V-HAND-PUSH
    V-GAN3-MAO4 V-ZHEN1-FANG2 V-DAO3-MANG2 V-BANG3 V-DIE2-XIE3 V-CHEN2-LIE4
    V-JIAN4-DING4-B V-KU1-QI4 V-QI1-XI1 V-CHI1-MI3 V-LIAN2-TI3 V-BAO4-A V-FEI1-A
    V-YOU2-LAN3 V-KU1 V-BENG1-B V-QIAN4-B V-ZHI4-SI3 V-DONG4-ZUO4 V-BI4-NAN4
    V-CHAN3-A V-XIE3-ZI4 V-JING4-SAI4 V-RU4-CHANG2 V-ZHUI1-DAO4-A V-JING4-BIAO1
    V-CANG2 V-SONG4-JIAO1 V-QI1-PIAN4 V-SANG1 V-YI4-YU4-B V-JIAN4-ZHENG4-B
    V-GUAI3-WAN1 V-YI4-DING4 V-TI3-GAI3 V-JUAN1-KUAN3 V-YA1-SUO1 V-DAO3-XIA4
    V-DIAO1-SU4 V-LU4-XIANG4 V-PAI2-XIE4-B V-CHA1 V-FU3-FU2 V-SHE4-B V-BU3-B
    V-JI4-A V-TING2-JI1 V-LA4-KUAN3 V-XI1-YOU2 V-SHE4-JI2 V-ZHAN4-LING2-B
    V-GONG1-SHI4 V-JIE4-D V-TI2-KUAN3 V-ZHAO1-YAN3 V-ZHANG3-KONG4 V-XIANG1-GUAN1
    V-CHU2-BEI4 V-PANG2-GUAN1 V-BAO4-C V-SHOU1 V-FA2-C V-DI3-YA1 V-GUI1-SHU3
    V-CHANG4-DAO3 V-ZHI4-YI2-B V-XIAN4-GUANG3 V-SUO3-C V-TIAN2-B V-TI4-ZUI4 V-ZUI4
    V-SHI4-N V-QIU2-A V-JIA4-XIANG3-B V-BAN4-JIAO3 V-ZHI3 V-JIANG4-LA4 V-ZAI3
    V-PAI2 V-PACK-IN V-CUN4 V-YOU4-HUO4 V-ASK-TO V-ZHI4-DING4-B V-DAI4-E
    V-HOU4-HUI3 V-FU4-ZHAI4 V-SU4-C V-GUI1-HUA2-B V-FU2-E V-JI4-LU4-B V-XI3-TAO4
    V-YANG3-ZHU1 V-SHE4-BEI4 V-DUI4-WAI4 V-SHI4-DIAN3 V-RUI4-SHUI4 V-ZENG4
    V-SHAO1-B V-GAI3 V-YI4-HONG2 V-LUN2-HOU4 V-ZI1-XUN2-A V-HUAN4-C V-QIE1 V-SUI2
    V-WEI4-MIAN3 V-SHOU3-YOU2 V-CHOU1 V-JI1-C V-LEI2-CE4 V-HUO2-DONG4 V-ZHU4-JIU3
    V-XIAN4-B V-HEN4 V-YAN4 V-SHEN1-FANG3 V-DAI4 V-YOU2-XI4 V-ZHI4-E V-SHEN3-MEI3
    V-FA5-JIAO4-B V-CHU2-JI1 V-CHU2-D V-MIN3-B V-ZHOU1-ZHUAN4 V-CHU1-SHENG1-B
    V-YU4-YOU4 V-TI3-YAN4 V-CE4-HUA2-B V-KAI1-CHUANG4 V-YIN3-LING2 V-HUI3-B
    V-TIAO1-ZHAN4 V-XIAN3 V-LIAN4-XI2 V-TUO1-ER V-HU1-HAN3 V-KOU4 V-XIE4-SHUI4
    V-YANG3-CHENG2 V-XUN4-LIAN4 V-YING4-A V-WEI4-C V-SHE4-DING4 V-HU1-JIAO4
    V-GAN3-YING4 V-NAI4-A V-BAI3 V-RONG2-A V-NI4-SHI2 V-XI3-B V-ZUO4-YE4
    V-CHONG1-TU1 V-JI4-E V-RU4-CHU1 V-SHOU4-SHI4 V-SHE4-E V-XIE4-A V-SHU1-YE4
    V-LIAN4-A V-YING2-LI3-B V-JIAO1-JIE4 V-ZHENG1-BA4 V-WEI4-PAO1 V-LA1-LA1
    V-YING4-ZHAO4 V-KAI1-DAO1 V-CAI2-JUE2 V-LIU2-LANG4 V-PIN4 V-YOU2-HANG2
    V-HUO3-LA4 V-CU4 V-KAI1-SHAN1 V-BIAN4-DIAN4 V-XIE2-CHI2-B V-FAN4-YING1
    V-LIAO4-LI4 V-JIE2-AN4 V-CHENG2-HUO2 V-YAN3-XI2 V-FAN1-YI4 V-FU2-SHE4
    V-CONQUER V-SUO3-A V-ZHAN4-ZHENG1 V-JIANG4-XUE4 V-TU1-A V-SAI4-MA3 V-YU4-HAI4
    V-GONG1-NUAN3 V-PI1-B V-GUA1 V-PAN4-JUE2 V-JU4-JU1 V-PO4-JI1 V-PO4-XI2
    V-YUAN3-ZHENG1 V-FU3 V-SHU1-B V-YAN4-SHI1 V-ZHUAN4-GAO3 V-SHOU5-SHU4
    V-QIN1-QUAN2 V-WANG4-A V-KUAI4-SHANG1 V-JIU4-SHENG1 V-XIA4-A V-ZHI5-SHI4
    V-TONG2-HANG2 V-XIANG4-ZHENG1-B V-FAN3-GONG4 V-QIAN1-ZHENG4 V-GOU4-A
    V-FU4-JIA1 V-TONG2-LING2 V-JIAO1-SHOU4 V-HOU4-LAI2 V-AI1-DAO4 V-YU4-BAO4
    V-ZAN4 V-XU1-NI3 V-JUE2-YUAN2 V-ZHUANG1-ZAI4 V-BU2-DONG4 V-YIN3-LUO4
    V-YIN3-YI4 V-LIU2-LAN3 V-JUAN1-ZENG4 V-JU4-C V-ZENG1-RONG2 V-QIE4 V-QI3-SHI4
    V-MO2-JIA1-KE4-GUI1 V-GU3 V-ZHAN4-A V-MENG4 V-SHI1-ZONG1 V-DANG3 V-JING4-XUAN3
    V-GE2-MING4 V-TA4 V-CHONG2-BAI4 V-SOU1-ZHA1 V-XIA4-YU3 V-ZHONG1-JI4 VERBS
    V-YAN3-HUA4 V-BIAN4-LUN4 V-JUE2-DING4 V-SHI1-HAI4 V-HAO4-B V-TU1-B V-SHI2-ROU4
    V-XIU1-MIAN2 V-SHUAI1-MEN2 V-JU4-E V-DEI3-YI4 V-FU5 V-ZI4-MAO4 V-MA2-ZUI4
    V-SHOU4-CUO4 V-CHAO3-B V-WEI4-E V-JIAO1-HUO4-B V-LI4-ZU2 V-DUI4-ZHANG4
    V-HE2-YI4 V-HUAN4-BING4 V-WEI4-GUAN1 V-FENG2-A V-NING2-GU4 V-BAO3-YOU2
    V-HUI4-LU4 V-BU4-HANG2 V-PO1 V-RAN3 V-ZHANG3-QUAN2 V-YING4-PIN4 V-JI3-BI4
    V-BIAO3-YAN3 V-ZUN1 V-DIAO4-A V-FENG2-A V-HUI4-ZONG3 V-HUAI4-A V-JI3-ZHU4
    V-LAO2-JIAO1 V-HUAN1 V-HAI4 V-FU4-SHU3 V-FU3-WEI4 V-LING4 V-JI4-SHENG1
    V-TONG2-MENG2 V-LING3-TOU2 V-TU4 V-SIT V-ZHENG1-GUI1 V-KE4-A V-YOU2-C
    V-XIANG4-A V-JIAN4-ZAI4 V-RAN2-B V-DAN1-B V-LIANG3-QUAN2 V-FAN1-SHEN1
    V-SHI1-YI4-B V-XIA4-MA3 V-JI4-REN2 V-BO2-B V-PAN4-GUO2 V-DUAN1 V-CHU4-MO2
    V-SUN3-SHI1 V-DU3-KA3 V-BO1-DA2 V-XIE2-DAI4 V-RU2 V-ZHU4-SU4 V-QUAN2-CAO3
    V-ZHI4-NA4 V-DI3 V-ZENG1-ZI1 V-KAI1-GUAN1-A V-ZHUAN1-YONG4 V-DAI4-SHOU1
    V-PEI4-TAO4 V-ZHI2-SHU3 V-LA1 V-DAO4-ZI1 V-ZHUI1-SUO3 V-QIAN1-DONG4
    V-YAN3-JIANG3 V-ZHAN1-A V-GUI1-JI2 V-PEI2-A V-JIAO1-HUI4 V-JI2-ZI1
    V-JIAO1-ZHU4 V-TU2-A V-SHOU1-LU4 V-PEN1 V-KU1-B V-PEI2-TONG2 V-YUAN2-JIE4
    V-DA2-XIE4 V-GUAN1-CE4 V-TOU2 V-BI3-SAI4 V-ZHANG4-AI4 V-SHI4-SHI1 V-ZHI5-LING3
    V-YIN3-DAO3 V-CHU1-KOU5 V-NIANG4 V-DUI1 V-SHI4-CHA2 V-ZHONG1-SHEN3 V-LU4-YING3
    V-TONG2-ZHU3 V-GUI1 V-ZAI1 V-QING1-A V-ZHU4-E V-QI4-A V-XUN2-A V-XUN2-WEN4
    V-JIAN3-BAO4 V-DAO4-QI1 V-ZHAN4-YONG4 V-PING2-GU1 V-CHU1-XI2 V-JIN4-HUO4
    V-ZHI2-XIAO1 V-BIAN3 V-JING3-SHI4-B V-YU2-A V-QIAN1-A V-DAO4-WEI4 V-FEI4-B
    V-QIAN1-FA5 V-JIE1-TI4 V-WAI4-YUN4 V-CHU2-A V-YU4-A V-BAO3-WEN1 V-CHU1-SHEN1
    V-TIAN2 V-FAN3-A V-ZHAO1-MU4 V-SHEN3 V-BAI4-SU4 V-PANG2-TING1 V-YUE4-YE3
    V-PA2-HANG2 V-ZHI4-JING4 V-BI4-BEI4 V-YOU4-FA5 V-BU3-CHANG2 V-MAN3-ZU2
    V-QU1-DONG4 V-BAO5-BI4 V-TAN1-BO4 V-DU2-SHEN1 V-JIAO1-CHA4-A V-TING2-KAO4
    V-ZHUAN4-XIANG4 V-ZHUAN4-HUAN4-B V-CE4-SHU3 V-XIU1-B V-HUO4-YI4 V-CAO3-NI3
    V-PEI4-DUI4 V-CHENG2-XIAO1 V-SHOU3-XUAN3 V-JIANG3-SHOU4 V-ZU3-CHENG2
    V-BIAN1-JI2 V-BI4-B V-CUN2-KUAN3 V-GONG4-TONG1 V-FANG2-SHUI4 V-XI1-D
    V-CHENG2-ZAI4-B V-SHAO2 V-PEI2-CHANG2 V-ZAI4-YUN4 VERB-DEGREE V-JIU4-B
    V-FENG1-NIAN2 V-SU4-A V-ZAO1-YU4 V-QIU2-B V-JIE1-JIN4 V-SHI1-YI4 V-LING3-HANG2
    V-JUE2-SHENG4 V-QING1-XIN1 V-XIU1-RONG2 V-SHOU1-LIAN3 V-JIE1-NA4 V-CHAN4-HUI3
    V-QING1-DAO4 V-YAN2-B V-JI3-FU4-B V-ZOU3-LUO4 V-KUO4-ZHANG1 V-TUO1-YE4
    V-BU3-YU2 V-BU3-HUO4 V-XIE4-SAN3 V-HAO2-TAO2-DAI4-KU1 V-CUI1-MING4
    V-SHI1-ZHUN3 V-KAN1-HU4 V-TAN4 V-TIAO2-YUE4 V-PAI2-TA1 V-ZHUAN3-LIE4
    V-CHONG2-HUA2 V-QI3-QIU2-B V-LIAN2-MENG2 V-CHU1-CHA4 V-QING3-JIA4
    V-SHEN1-ZHAN3-A V-FANG3-MAO4 V-GONG3 V-HE2-HUO3 V-QI4-HE2-A V-ZHU3-HUN1
    V-TU2-LI3 V-DING4-ZUI4 V-KE4-B V-CHUAN1-TOU4 V-ZHAI2 V-BAI4-A V-CHAI1
    V-XUN2-HUAN1 V-YAN3-MAI2 V-WO4 V-YI3-HUN1 V-KUI1-BEN3 V-CHU2-C V-ZHUAN4-YUE4
    V-XIAO4-A V-JING1-XUAN3 V-QIU2-ZHI1 V-PAO3-BU4 V-ZHAO4-SHE4 V-CHANG2-XIAO1
    V-SHOU3-CAI2 V-ZAN4-TAN4 V-ZHE1-B V-GUAN1-ZHAO4 V-DING4-WEI4-C V-DIAN3-CANG2
    V-PAI1-DA2 V-CHUAI4-A V-JIA1-B V-LOU4-A V-CHU1-XUE2 V-LAO4 V-FEN4-MI4
    V-JIAO1-ZAO4 V-YOU1-A V-ZHOU4 V-TAO3-ZHAI4 V-XUN4-D V-KANG4-YA1 V-SHUAI4
    V-JIAN3-SHI4 V-HANG2-DONG4 V-ZHI4-XI1 V-CHONG1-SHI5 V-BENG1-TA1 V-KAN3-FA2
    V-PAO2 V-YUN4-CANG2 V-LIAN2-XIAN4 V-PEI2-ZANG4 V-GONG1-TOU2 V-YU4-DING4-B
    V-CHAO3-RANG5 V-JUDGE-SENTENCE V-YOU2-XUE2 V-XIE4-D V-FEN1-CHA4 V-PING2-SHEN3
    V-WU2-GEI3 V-KAO3-A V-DAI4-LING2 V-REN3-SHOU4 V-ZHAO1-LI4-B V-BIAN1-YI4
    V-WU4-B V-PI2 V-DAO3-YAN3 V-NIAN3-A V-PU1-A V-JING4-JI4 V-SHI5-B V-XUAN4
    V-DA2-JI1 V-SHOU4-XING2 V-DUI4-TAN2 V-WAN3-A V-ZHI4-HENG2 V-BAO3-LIU2 V-XUN2-C
    V-ZHU4-A V-DIAO1-KE4 V-DI1-CHUI2 V-TUO4 V-SHOU5-CHAO1 V-KAN1-HAO3
    V-SHENG4-CHANG2 V-NIU3 V-XIN4-FENG4 V-BIAO3-XIAN4 V-NIAO3-KAN4 V-YAN1-B
    V-CHUI2-LEI4 V-QI3-SHAO1 V-XUAN1-RANG5 V-PAI3-HAI4 V-LIN2-CHANG3
    V-ZHUANG1-ZHI4 V-JI2-KUAI4 V-XIANG1-TONG2 V-CI4-XIU4 V-CUN2-HUO2 V-BU3-LAO1
    V-SHUA1 V-MEI4-A V-BIAO1-A V-ZU1-PENG2 V-PU4-GUANG1 V-SHOU4-HUI4-B
    V-BAO4-SHUI4 V-JIA1-HAI4 V-TUI1-JIN4 V-KAI1-SHI3 V-GUI1-LING2 V-CE4-SHI2
    V-GUO5-QU4 V-QIAO1-KE4 V-BIAN4-HUA4 V-RAO3 V-JING4-ZHENG1 V-SHOU3-WANG4
    V-SHOU1-KAN1 V-SHOU4-GU4 V-ZHU3-KONG4 V-TOU1-KUI1 V-SAI4-JU1 V-QIN1-JIN4
    V-BAO4-DAO3 VERB-DEGREE V-MI2-HU2 V-KAO4-BEI4 V-TIAN1-XING2 V-FA5-PIAN1
    V-MING2-B V-LIAN4-GANG1 V-YAO2-HUANG4 V-MEI3-FA5 V-SA3 V-BU4-DAO4 V-SUI2-HANG2
    V-CUI1 V-CHUANG4-LI4 V-JIU4-QI3 V-YING2-BIN1 V-CI2 V-CHEN2-DIAN4 V-PO4-AN4
    V-SHANG3 V-TU1-SHA1 V-DONE V-CHEN2-QING2 V-ZHEN1-JIU3 V-TING2 V-YU4-CE4-B
    V-FAN1 V-BO1-BAO4 V-CHEN2-SHU4 V-ZHU4-TUI1 V-JU4-HE2 V-CHANG1 V-SHOU3-FA1
    V-DONG4-A V-SUMMON V-AT V-XI1-SHOU1 V-TIAO2-WEI4 V-SHAO1-SHANG1 V-ZHI4-AN1
    V-BU3-YU2 V-COMPLAIN V-SHEN1-A V-XI1-SHI2 V-ZHUAN4-BO1-A V-BAN1-B
    V-LENG3-DONG4 V-SHANG1-HAI4 V-PAO3-LUO4 VERBS V-YING3-YIN4 V-ZAO1-QIE4
    V-XIANG3-XIANG4-A V-CHU2-CANG2 V-TIAO1 V-ZHUI1-SI5 V-ZHONG4-XUAN3 V-BAN4-C
    V-ZHUAN4-SONG4-A V-QIAN1-C V-XIU1-DAO4-A V-KE1-SHUI4 V-ZHENG4-HUN1 V-LAN2
    V-MO2-A V-FANG4-HUA4 V-WANG3-SHENG1 V-JIAN1-JIAO4 V-CHAN3-LUAN3 V-TE4-NAN3
    V-CHUAN4 V-YUN4-DU2 V-CHUI1 V-CE4-A V-JI4-XU4 V-RONG2 V-XIE4-JUE2-B V-HUI4-KE4
    V-CAN2-LIU2 V-LIAN4-B V-CU4-SI3 V-ZA2 V-KONG4-SU4 V-JIANG3-XI2 V-DAI4-KE4
    V-NUO2 V-SHE4-XIANG4-B V-GOU4-C V-DAI4-TI4 V-MAI2 V-RONG2-C V-NA4 V-JIA1-SU4
    V-ZHI4-YI2-A V-JIE2-JI1 V-DIAN4-HAN4 V-CHUANG4-SHI4 V-BO1-YIN1 V-JIN4-CHENG2
    V-ZHENG4-BIAN4 V-GUAN3-XIA2 V-SHENG4-ZHAN4 V-JU3-HANG2 V-DAI4-BAI2 V-RONG2-ZI1
    V-RU4-KOU5 V-TAN4-QIN1 V-RUAN3-JIN4 V-JIAN3-JU3 V-KANG2 V-ZHI5-SHI5 V-XIE4-C
    V-XING2-XING2 V-CUO1 V-ZA4-TAO2 V-STEAL V-HUI2-YING4 V-ZHI4-YU4 V-SHOU1-YI4
    V-LANG4-FEI4 V-JIE4-KUAN3 V-SHI1-A V-RU4-ZHU4-B VERBS V-TI2-ZHI4 V-ZHA2-CAO3
    V-JIA1-JIANG3 V-BIAN1-ZHUAN4 V-ZU3-GE2-A V-DAO4-DA5 V-JI1-JU4 V-CHANG2-B
    V-HUI3-MIE4 V-GAO4-MI4 V-ZHI2-QIN2-B V-JU1-GONG1-B V-BAO4-DAO4-C V-JIN4-SHI2
    V-FANG3-TAN2 V-JIAO3-ZHENG1 V-QIANG3-PAI3 V-JIAO1-ZAO4 V-YU4-YAN2 V-DAI4-TI4
    V-ZHENG3-LI4 V-BU2-GUAN3 V-XIE4-JIU4 V-ZA4-REN2 V-BU3-LIE4 V-QIAO1-MEN2
    VERB-DEGREE V-CHU3-FA2 V-ZAO4-FAN3 V-JI4-SHI4 V-KAI1-JI1 V-CAN2-CUN2
    V-FANG4-KONG4 V-JI2-JU4 V-ZHUANG1-BEI4 V-XING2-RONG2 V-CHU1-CHANG2-B
    V-JING3-SHI4-A V-YING4-B V-HU4-FU1 V-SHE4-RU4-B V-YOU2-LI2 V-CHENG2-MING2
    V-ZHAO4-XIANG4 V-SUN3-HUAI4 V-LAN4-WEI3 V-CHI4-ZI4 V-YI4-C V-SHU2-QU3
    V-SHOU4-B V-DI1-CHANG4 V-SHI4-WEN1 V-XUAN3-XIU1 V-DING4-SHI2 V-XI3-LIAN3
    V-DAO4-A V-FU3-WEI4 V-ZHAO4-YAO1 V-SHEN1-YIN2 V-CAN3-JIAO4 V-WEI2-GONG1
    V-SHI4-E V-FA5-SONG4 V-WEN2 V-CAN3-JIAO4 V-PEI4-ZHI4-A V-BAO5-ZHUANG1
    V-BAO4-FU4-B V-DOU1 V-WOMAN-MARRY-TO V-DUO1-FA5 V-HONG1-MING2
    V-YI2-HAN4 V-JUAN1-JIAN4 V-JIAN1-SHOU3 V-TOLERATE V-PIAO1 V-SHAN1-A V-QIN1-LI4
    V-ZHU4-B V-SHOU4-NUE4 V-BO1-FANG4 V-YING4-YONG4 V-KE4-C V-ZHU4 V-DA1
    V-SHANG1-WANG2 V-JING1-BAN4 V-LENG3-XIAO4 V-JIAN4-JI1 V-YAN1-GE1 V-DU3-A
    V-JIAN4-GUAN3 V-PAI1-MAI4 V-TIAN2-MAI2 V-DIAN4-B V-HAN4-JIE1 V-SHAN1-DONG4
    V-GUI1-YI1 V-DUI4-BI3 V-BEI4-B V-WEN4-HOU4 V-CANG2-NI4 V-ZHUAN4-XIE3
    V-JIAO3-BAN4 V-TAN4-ZHAO4 V-JU4-KUAI4 V-ZHANG3-GUAN3 V-HONG1-GAN1 V-SHEN3-XUN4
    V-ZAN4-MEI3 V-QI1-SHEN1 VERBS V-LIE4-YAN4 V-SHI4-A V-MO2-BIAN1 V-SHI1-NUE4
    V-ZHENG3-JIU4 V-XU4 V-ZHA1-B V-JU4-JUE2 V-LIAN2-MIN3 V-TUI1-JIAN4
    V-JIA1-QIANG3 V-ZHI4-WO3-MAN3-ZHU2 V-DU2-DUAN4 V-SHUAI1-JIAO1 V-PAN1
    V-GUO5-YE4 V-SHAI1 V-DANG4 V-KAN3-A V-HAN3-JIAO4 V-ZHUI1-GAN3 V-TIAO1-TI1
    V-TUO1-B V-TUI1-JIE4 V-JU4-LI2 V-HU2 V-LU4 V-SHEN1-KUAI4 V-WU3-RU3 V-JIA1-SHI2
    V-QING3 V-SHOU3-FA3 V-SHANG5-DANG1 V-GONG1-KAO3 V-DI1-SHI4 V-TIAO2-HU2 V-YU2-E
    V-SAN3-BU4-B V-GEN1-SUI2 V-XIN4-YANG3 V-HUI4-B V-SHENG4-A V-TAO3-FA2 V-CHAO1
    V-YAN4-ZENG1 V-TAN2-B V-YI2-WANG4 V-TIAN2-CHONG1 V-ZHAN1-HE2 V-HUI1 V-FA5-SAN3
    V-CAO1-ZONG4 V-FU4-ZE2 V-HANG2-SHI3 V-BANG4-A V-CAI2-LIANG5 V-XIAO1-HAO4
    V-CI4-JI1 V-QIU2-SHAN4 V-ZHI4-SHI5-B V-LIAN4-BING1-A V-JIAO1-ZHAN4 V-LI4-JIAO3
    V-YUE1-B V-WANG2-GUO2 V-CHONG1-A V-HUI2-SU4 V-SHOU1-HUO4 V-HUO4-A V-BU2-RU2
    V-HE2-YING3 V-JIAO1-ZAO4 V-DI1-AN4 V-TIAN1-B V-HUI2-FU4 V-JIN4-XING2
    V-XIAN4-GOU4 V-KANG4-HENG2 V-FA5-HUANG1 V-QI4-XIU1 V-YONG4-GONG1-B
    V-TOU1-QING2 V-PAN4-DUAN4 V-QU1-BIE4 V-DOU4-ZHENG1 V-WEN4-ZE2 V-SHI1-ZU2
    V-QING1-SU4 V-BU2-HE2 V-XIAN3-SHI2 V-GONG1-REN4 V-FEN4-BIAN4 V-JIE2-SHU4
    V-SHA1-CHONG2 V-ZI4-YOU3 V-YANG2 V-GIVE V-XIA4-ZAI4 V-FENG1-KOU5 V-SI5-KAO3
    V-SHI3-FA1 V-SHI4-I V-HUAN1-HU1 V-MENG4-XIANG3 VERB-DEGREE V-JIN4-ZHAN4
    V-DRINK V-XU1-B V-YU4-GAO4 V-BAI2-ZHAN3 V-JIAN3-A V-ZHEN4-SHAN1 V-QI1-WANG4
    V-HENG2-WEN1 V-ZAO4-YAO2 V-WEI2-GUAN1 V-LA4-MA3 V-YIN4-CHAO1 V-ZHI2-JIAO1
    V-XIAN4-SHEN1-B V-CHUI1-NIU2 V-SHOU1-SHUI4 V-WU4-GONG1 V-UNDERSTAND VERBS
    V-KONG4-GAO4 V-FAN3-TAN1 V-QING3-SHI4 V-QIAN1-SHE4 V-HUI2-TOU5 V-FU4-ZHAO1
    V-SHEN1-YU2 V-SI4-YANG3 V-WEN3-A V-SHUI2-ZHU3 V-XIE2-ZHU4 V-PENG4 V-ZE2-A
    V-YAN1-XUN4 V-BAN1-C V-HUAI4-YI2 V-HUO4-LI3 V-XIANG1-CHI2 V-JIAN1-HONG1
    V-TI4-ZUI4 V-GONG4-YOU2 V-ZHEN4-SHE4-B V-JIE2-ZHI4 V-PAN4-TAO2 V-MAI4-SHEN1
    V-HUA1-QIAN2 V-GAN4-HUO2 V-XIAO4-FANG3 V-MAI4-YIN3 V-MENG2 V-SHUAI1
    V-ZHAO1-LU4 V-DI4-ZAO4 V-BU4-FANG2 V-PEN1-SHE4 V-PU1-B V-JIAO3-JU2 V-ZUAN1-DE5
    V-BAN4-JIAO3 V-BING3 V-BAO4-FA5-B V-YAN1 V-SHOU4-JIA3 V-GAO3 V-KONG4-ZHI4-A
    V-KEN3-LAO3 V-LEARN-MARTIAL-ART V-TOU2-JIANG4 V-CHONG1-B V-KUO4-DAI4
    V-CUO4-BAI4 V-ZHI4-CHAO1 VERBS V-YI1-FU4 V-ZHENG1-ZHAO4 V-SA1-SHOU5
    V-BEI4-ZENG1 V-YI2-WEN4 V-ZHE1-BI4 V-JUE2-C V-FEN4-FA5-B V-HUO4-QU3 V-JIA1-MI4
    V-WAI4-XIE4 V-LUN2-XUN2 V-YUN4-WEI2 V-CHEN2-CHUAN2 V-XIN1-XING1 V-JIAO1-ZHAO1
    V-TUAN2-GOU4 V-WEI1-HAI4 V-HAI2-JI1 V-SHEN4-TOU4 V-SHUANG1-YING2 V-ZUO4-XIU4-B
    V-QIU2-SHI2 V-QIAN3-ZE2-B V-SHI4-FEI1 V-FAN5-ZHU3 V-SHANG5-FANG3 V-BI4-SAI4
    V-CAO1-KONG4 V-FA5-HUI1 V-MING2-C V-YANG3-YU4 V-HAI2-DAI4 V-BAO3-DI3 V-WAIT
    V-FANG4-DAI4-B V-FU4-SU1 V-WU4-GONG1-B V-QIU2-FU4 V-ZHI4-HOU4-B V-ZHI2-SHU4
    V-HU1-HUAN4 V-WANG3-TUO1 V-SHI5-XI2 V-LIANG4-JIAN4 V-GAI3-ZHUANG1 V-ZENG1-YA1
    V-QIAN2-CHU1 V-BU4-SHU3-A V-ZHUI1-JI1-A V-CHE4-RUI4 V-SHA1-SHANG1
    V-O-I-BODY-PART V-PO4-JIANG4 V-CAI2-MIN3 V-XIAN1-JIN4 V-YING4-JING3
    V-ZHI4-SHENG4-B V-QIANG3-JUN1 V-REN3-RANG4 V-SHOU1-FU4 V-JI1-HUI3-A V-SHI1-SU4
    V-TAN2-SHE4 V-JIAN1-HONG1 V-FA5-DONG4 V-ZHAN3-SHOU3 V-CHU1-DONG4 V-BU3-GEI3
    V-GAN4-SHE4 V-BANG1-ZHU4-B V-CHENG2-JIE4 V-HAI3-XUN2 V-BIAN4-GUI3 V-RE4-AI4
    V-TAO3-KUAN3 V-FU4-ZHA1 V-BIAO3-ZHANG1 V-QIANG3-JIU4 V-MIN3-PAI2 V-GUO4-LU4
    V-SHI1-YA1 V-ZHENG1-YONG4 V-ZHENG1-QIU2 V-WEI1-BO4 V-JI3-SHENG3 V-FANG2-FU3
    V-XIN4-FANG3 V-YONG1-HU4 V-ZHENG4-QIAN2 V-CHUAN1 V-HUA4-HUA4 V-QI1-YA1
    V-ZHUAN4-BAO3 V-ZHI2-XIA2 V-ZHI2-BAN1 V-DA2-JIA4-B V-ZHI4-BEN3 V-SHANG5-TIAO2
    V-LIAN3-LI4 V-WEI2-ZHANG4 V-KONG4-YAN1 V-RONG2-REN3 V-CHENG2-ZHI4 V-BA1-B
    V-DAO3-XIANG4-A V-YOU1-GUO2-YOU1-MIN2 V-JI3-LI4-B V-NI4-FAN3 V-GAI3-SHAN1
    V-ZHI4-AI2 V-ZHANG4-JIA4 V-NIU3-QU1 V-CHANG4 V-ZUO4-D V-MIAO2-HUI4 V-GAI3-JIN4
    V-CHOU1-JIANG3 V-TIAO2-JIE2 V-SHOU4-PIAO4 V-DING4-PIAO4-B V-CHAO2-ZAI4
    V-LIAN2-YIN1 V-SONG4-LI3 V-CAI3-GUANG1 V-DAO4-QIAN4 V-XIA4-DIE1 V-CHU1-XIAN4-B
    V-SU4-KU3 V-WEI4-WEN4 V-SONG4-SI3 V-DUI4-BU2-HUO3 V-YUE1-DING4-SU2-CHENG2
    V-SHA1-FU4-JI3-PIN2 V-GAO1-FA5 V-JIANG4-DI1 V-XIA4-HUA2 V-JIAO3-FEI4
    V-LING2-QU3 V-SHI1-ZHI2 V-CHAO2-BIAO1 V-SU4-CHENG2 V-TUO1-QIAN4 V-ZHEN3-ZHI4
    V-XIAN4-DING4 V-XIA4-JIA4-B V-CHENG2-QING1 V-GUAN1-MEN2 V-YAN2-ZHA1
    V-ZU1-FANG2 V-SHI1-WANG4 V-BIAO1-SHI5 V-DI1-BA2 V-BAN1-BU4 V-BAN1-FA5
    V-JIA1-MENG2 V-HANG2-PIAN4 V-QIAN4-FEI4 V-QIU2-QING2 V-SHOU4-PIN4 V-PIN4-REN2
    V-CHENG1-SAN3 V-ZHI2-YE4 V-JIE1-QIA4 V-HONG1-DONG4 V-MI3-HUO4 V-XIU1-LI4
    V-ZUO4-REN2 V-SI3-JI4-YING4-BEI4 V-LIANG5-XING2 V-TING2-SHEN3 V-XUAN3-REN2
    V-SAO4-HUANG2 V-WAN3-JIU4 V-WU4-PAN4 V-JI2-QUAN2 V-TIAO1-XIN4 V-KAN1-BING4
    V-BIAN4-ZHENG4 V-YOU1-SHENG4-LIE4-BAI4 V-ZHONG4-LI4 V-LENG3-JING4 V-HE2-BAO4
    V-DAI4-CHANG2 V-XIU1-JIA4 V-FU4-ZE2 V-XI5-JIN4 V-MO2-CA1 V-LUN2-HUAN4
    V-GOU4-PIAO4 V-RAO4-HANG2 V-JIAO1-FANG2 V-MAN2-MA4 V-ZU1-SHOU4 V-SHI1-TIAO2
    V-RU4-HU4 V-SHANG5-HANG2 V-SHI4-FANG4 V-YU4-YUE1 V-JIAN3-TAO3 V-BAI4-XUAN3
    V-QIONG2-JIN3 V-GONG4-HU2 V-SHI1-LI3-B V-BEI4-PAN4-B V-XING2-CHENG2
    V-FAN3-PAN4 V-QI4-QUAN2 V-PI3-JUE2 V-WEI1-SHE4 V-SHUAI1-LA4 V-FAN3-BO2
    V-YUN4-CHOU2 V-JU3-SHOU5 V-SHAN1-ZHAI4 V-RU4-KUAI4 V-MO4-HEI1 V-TIAO2-KAN3
    V-GOU4-ZAO4 V-TIAO2-YAN2 V-HUAN3-XIE4 V-BENG1-KUI4 V-KUA3-TAI1 V-SI3-SHANG1
    V-DENG1-LU4-A V-ZHAN4-BAI4 V-HU4-HUAN4 V-WEI1-BI1 V-QIN1-HUA4 V-JIAN4-DANG3
    V-QIAN1-ZHI4-B V-JIAO4-JING4 V-REN3-NAI4 V-HONG2-YANG2-B V-SHANG5-YANG2
    V-FA5-XIE4 V-PIAO2-CHANG1 V-MAI4-YIN3 V-YONG4-JU1 V-XUAN3-YONG4 V-BO2-YI4
    V-JIAO1-FENG1 V-LONG3-DUAN4 V-HUAN4-JIE4 V-CHONG2-ZHUANG1 V-CAI3-YOU2
    V-HUO3-FEI1 V-FAN3-KONG3 V-JI3-WEI4 V-HUI2-JIA1 V-GUO5-JIE2 V-DING4-SUN3
    V-XIU1-JU1 V-SI1-LIE4 V-XUAN3-BA2 V-XIA4-XIAN4-B V-JIN4-HUA4
    V-JIAN4-YI4-YONG3-WEI4 V-ZI4-KONG4 V-JIE4-YAN1 V-YI2-SHI1-B V-SHUI4-JIAO1
    V-SHI2-YI2 V-XUAN3-ZHI3 V-ZHI4-HUAN4 V-LI4-AN4 V-GUAN3-SHU4 V-YIN4-FA5
    V-HUI2-BI4 V-QUAN4-RUI4 V-TONG4-BAO4 V-BU2-GONG1 V-DAO4-QIE4 V-FANG2-ZHI3
    V-SHI1-LING2 V-JING4-YE4 V-TOU1-DAO4-B V-ZHU2-LI3 V-NEI4-HONG4 V-YI2-LIU2
    V-KANG4 V-BAO5-RONG2 V-TOU2-JI1-DAO3-BA3 V-JIE1-FA5 V-QUN2-OU1 V-XIA4-PO1
    V-GUA4-SHI1 V-JU3-BAO4 V-WEI4-ZHENG4 V-TAO2-PAO3 V-JIAO1-ZHI1 V-ZHAO4-HUAN4
    V-WEI2-YUE1 V-SHEN3-AN4 V-ZHAO1-DAI4 V-XIA4-JU1 V-QIN1-MIN2 V-CONG1-YE4
    V-XUN2-HANG2 V-TOU2-DU2 VERBS V-JIA4-SHI3 V-HUAI4-YUN4 V-CHENG2-JIAO1
    V-DA2-BAO5 V-YOU2-JI4 V-HU4-SHEN1 V-BIN4-ZANG4 V-JIU4-JI3 V-CHAO2-YUE4
    V-KUANG2-HUAN1 V-JIAO1-PEI4 V-MI3-XIN4 V-XI1-DU2 V-XIAN3-XIAN4 V-YIN3-YU4
    V-HUA2-JI1-KE4-XIAO4 V-JIAN1-SHOU1-BING4-CHU2 V-DA2-ZHANG4 V-BAI4-JIN1
    V-CHONG2-QI3 V-ZHI2-BO1 V-HE2-HUO3 V-MAO4-XIAN3 V-FU5-XIU3 V-TONG4-QIN2
    V-XUAN2-ZHUAN4 V-PIN1-XIE3 V-NA4-SHUI4 V-YONG1-DU3 V-DIAN3-QI3 V-BEI3-FA2
    V-ZHAO1-GU3 V-DUI4-CHONG1 V-CHAI1-WEI2 V-CHU1-YUAN4 V-WEI2-QUAN2 V-ZAI4-HUN1
    V-FU3-YANG3 V-SHAN4-YANG3 V-SHU1-SAN3 V-CAI3-TA4 V-HAI2-KUAN3 V-BING4-GOU4
    V-GONG1-QI4 V-SHOU4-SUN3 V-AN1-JU1 V-MO4-REN4 V-ZUO4-BI4 V-CUN2-FANG4
    V-SHENG1-CHENG2 V-KAI1-BAN4 V-DENG1-LU4-B V-SHA1-DU2 V-XIAN4-XIE3 V-GAO3-XIAO4
    V-WEI2-HU4 V-YING4-SHI4 V-SAN3-BU4-A V-JIU1-CUO4 V-XIN4-REN2 V-YAO2-GAN3
    V-ZHUAN4-DA5-B V-MAI4-GUO2 V-SHE4-FANG2 V-RU4-DANG3 V-ZI1-GAI3 V-KANG4-ZAI1
    V-DUI4-YING4 V-MIN3-ZAI1 V-SHENG1-LAO3-BING4-SI3 V-DU2-BAO4 V-KONG4-JIANG4
    V-BO1-JI2 V-YING4-JI2 V-KANG4-ZHEN4-JIU4-ZAI1 V-KONG4-TOU2 V-MA4
    V-ZHUAN1-ZHI4-B V-DI1-JIA4 V-ZHUAN4-DAO3 V-CHU1-CHANG3 V-LAI2-HUI2 V-NOTICE
    V-YOU2-JIE1 V-ZHI2-CAO3 V-SHOU4-ZAI1 V-ZHU3-GONG1 V-GONG1-GAO4 V-YING2-JIU4
    V-JIE1-RU4 V-PU1-SHE4 V-HANG2-HUI4 V-TAN1-FU5 V-YIN3-SHEN1-A V-HUI2-GOU4
    V-DA2-ZHE1 V-JI2-YOU2 V-XIANG1-QIN1 V-GENG4-HUAN4 V-PEI2-FU4 V-YOU2-MU4
    V-SHENG1-ZHI2-B V-SHUAI1-RUI4 V-DI3-XIAO1 V-QUN2-FANG3 V-BIAN4-LIAN3
    V-RUAN3-ZHUO2-LU4 V-BI3-JIA4 V-SHI2-YAN2-A V-MEI3-BAI2 V-PEI2-YANG3
    V-TENG2-AI4 V-CHI1-FAN4 V-GENG1-ZUO4 V-JIA1-GU4 V-KANG4-ZHEN4 V-QUN2-CE4
    V-QUN2-FANG2 V-ZOU3-FANG3 V-XIA4-QI2 V-CHU3-SHI4 V-YUN4-NIANG4 V-CHUANG4-JIAN4
    V-CHENG2-BAN4 V-GAN3-ZHAO4 V-SHU2-MAI3 V-JIE1-GUAN3 V-SHE4-XIANG4 V-XUAN1-PAN4
    V-BANG3-JIA4 V-JIU4-ZHI4 V-BAO4-JING3 V-ZHI1-QING2 V-BAO4-LIAO4 V-XIE4-MI4-B
    V-QUE4-REN4 V-ZU3-NAO2 V-YAN3-XI4 V-GAO1-KANG4 V-YI4-YU4-B V-FU4-JUE2
    V-NUE4-DAI4 V-SHUI4-MIAN2 V-SHEN3-WEN4 V-KAO4-WEN4 V-AN1-YUE4-SI3
    V-ZHEN3-DUAN4 V-DUI4-JIANG3 V-QIAN1-XI3 V-FANG2-KONG4-A V-FU2-DONG4
    V-SHI4-YUN4-HANG2 V-GUA1-CENG4 V-ZHUI1-WEI3 V-FA5-BU4 V-KAI1-BO1
    V-TONG4-XIANG4 V-CHU1-HANG2 V-JIN4-GONG1 V-GAN4-YU4 V-SHEN1-BAO4 V-XUN2-SHI4
    V-DUI4-KANG4 V-REN4-DING4 V-SAO4-JIE1 V-RU4-JIE4 V-GAN4-RAO3 V-ZHU4-YUAN4-A
    V-KONG4 V-XUN2-YAN3 V-BIAN4-GENG4 V-KANG4-TIAN1 V-AI4-GUO2 V-NONG2-SUO1
    V-GONG1-NUAN3 V-ZHENG1-FA5-B V-CHOU1-JIAN3 V-GUO2-CHAN3 V-JIANG4-JIA4
    V-FENG1-HE2 V-QIANG3-JIE2 V-LIU2-XUE2 V-DI1-SHUI4 V-ZHENG1-SHUI4
    V-SHANG5-SHENG1 V-YIN3-YONG4-A V-JIE1-BAN1 V-DIAN3-DENG1 V-JIAN3-YI4
    V-FAN3-FU5 V-JUAN1-ZHU4 V-ZHENG4-ZHA1 V-QI2-YU3-B V-XI3-QIAN2 V-QIAN2-TAO2
    V-JU1-YA1 V-SOU1-BU3 V-DUAN3-QUE1 V-ZU1-YONG4 V-QIU2-YUAN2 V-JI1-E4
    V-HUAN1-QING4 V-YAN2-ZHANG3 V-KUAI4-WU4 V-HUI2-XIANG1 V-CAI3-KUANG4
    V-DUO4-TAI1 V-QI2-DAO3-B V-CHI1-HE4 V-CHAO2-FENG3 V-KUAI4-CAO3 V-ZHEN4-YA1
    V-DIAN3-PIAO4 V-HUO3-FU2 V-SHE4-MIAN3 V-SAO4-MIAO2 V-DUI4-JIE1 V-DUO3-CANG2
    V-SHOU3-YE4 V-QIAN1-YI2 V-JIA1-BAN1 V-FEN4-HUA4 V-YIN3-DU4 V-ZHEN1-PO4
    V-XIAO1-XI3 V-JUN1-SHOU4 V-DU2-ZHI2 V-ZHUAN3-LIE4 V-TONG2-YI4 V-JING1-SHANG1
    V-KAN1-SHOU3 V-TUO1-XIAN3 V-JIN4-ZHI3 V-TAO2-NAN4 V-WEI1-XIE2 V-TUO1-YAN2
    V-QING3-YUAN4 V-HU1-YU4 V-AN4-SHA1 V-WU3-BI4 V-REN4-ZUI4 V-SHI4-FA5
    V-FAN3-KANG4 V-BA4-GONG1 V-DUO2-JIA4 V-HUI3-HUAI4 V-FAN3-JI1 V-HUI4-LU4
    V-DI3-ZHI4 V-CHOU2-JI2 V-BAO4-PO4 V-LIAO2-TIAN1 V-JING3-JIAO4-B V-DI3-KANG4
    V-HUO4-MIAN3 V-HUI2-XUAN2 V-BI4-HU4 V-TIAO2-TING2 V-JI3-FEN2 V-XIU1-XING2
    V-YAN3-YI4-B V-CHU1-JING4 V-QIANG1-SHA1 V-YU4-CI4 V-XING4-SAO1-RAO3
    V-TAN1-HUAN4 V-MIE4-JUE2 V-FU5-BAI4 V-ZHI5-KONG4 V-BAO4-KAO3 V-NAO4-SHI4
    V-DAO3-LUAN4 V-XUAN2-FU2 V-TUO3-XIE2 V-SHAN1-HOU4 V-XIU1-JIAN4 V-XI2-JI1
    V-FEN4-LI2 V-FAN4-MAI4 V-ZI1-SHENG1 V-FANG4-FEI1 V-YU3-KUAI4 V-SHEN1-AO4
    V-HUO4-SHENG4 V-REN2-MING4 V-TING2-CHAN3 V-KE1-KAO3 VERB-DEGREE V-YING2-JIE1
    V-DUAN4-LIU2 V-FAN2-YU4 V-JIAN4-CAO3 V-TUO1-LI2 V-LAN2-JIE2 V-TING2-PO1
    V-KONG4-YUN4 V-ZHONG1-ZHI3 V-RU4-SHI4-A V-KAO3-ZHA1 V-JIE4-BEI4 V-TOU1-DU4
    V-YUN4-SONG4 V-DING4-GOU4 V-DING4-HUO4 V-HONG1-ZHA2 V-LA4-CHENG2 V-HUI4-JU4
    V-REN2-YONG4 V-JIN4-JI2 V-KANG4-MEI3-YUAN2-ZHAO1 V-FANG4-SHE4 V-DAO4-NIAN4
    V-WEI2-JI4 V-DI1-SU4 V-ZHI4-YI1 V-ZA4-ZHI2 V-ZHUO2-SHANG1 V-JIAN4-ZHENG4-A
    V-YI2-ZHI2 V-REN4-KE4 V-CI2-ZHI2 V-FAN3-XIANG1 V-CAO3-SHI4-B V-TOU1-QIE4
    V-TAO4-JIN1 V-HUAN1-YUE4 V-BAO4-LIE4 V-MU4-JI1 V-YA1-XIE4 V-JU3-BAN4
    V-CHONG2-JI1 V-TA1-SHA1 V-LA4-SHUI4 V-BA1-QIE4 V-TAN1-DU2 V-ZUO4-AN4
    V-JI1-YA1-B V-XING4-QIN1-HAI4 V-JIAN4-JIN4-A V-HUAN4-ZHUANG1 V-QIANG3-JIAN1
    V-PIAO4-XUAN3 V-TAO2-SHENG1 V-SHANG5-XUE2 V-DA2-REN2 V-WU4-ZUO4-JU4
    V-JING4-ZUO4 V-JI4-PIAO4 V-BU4-XIAN4 V-HUANG1-BAO4-B V-JIA4-YU4 V-GAO4-BIE4
    V-GE2-LI2 V-JIE1-JIAN4 V-ZHA1-ZHENG4 V-QIANG1-JI1 V-ZONG4-QI3 V-KUN3-BANG3
    V-OU1-DA2-B V-BANG3-PIAO4 V-BAO4-CHONG1 V-ZHU3-FAN4 V-TAO2-WANG2
    V-XIU1-LIAN4-A V-AN1-YANG3 V-HUAI4-JIU4 V-JI4-CHENG2-B V-DAO3-GE1 V-XU3-YUAN4
    V-ZHA1-A V-LAN2-ZHA1 V-QU3-DI4-C V-BA4-MIAN3 V-HU4-LI4 V-HU4-HANG2
    V-TONG4-JI1-A V-FEI1-JU1 V-ZHUI1-ZHU2 V-CA1-ZHUANG4 V-SHE4-AN4 V-QIANG3-DUO2
    V-XIA4-TAI1 V-XIONG1-SHA1 V-MIE4-QI3 V-SHE4-JI1 V-DA2-LIE4 V-HUN1-MI3
    V-JI3-QIANG3 V-BIAO1-JU1 V-XIU1-FA3 V-SHOU4-SHANG1 V-ZHEN1-XUN4-B
    V-HANG2-XIONG1 V-ZHONG4-JIANG3 V-ZHA4-LI4 V-TONG4-PAN2 V-AN4-MO2 V-WEI2-XIN1
    V-SOU1-JIU4 V-OU3-TU4 V-DUAN4-DIAN4 V-TING2-DIAN4 V-CHENG2-CHU3 V-PAI1-XI4
    V-YUE1-TAN2 V-JIAN3-CAI3 V-WEI2-JIAN4 V-CHAI1-CHU2 V-SHA1-REN2 V-LE4-SUO3
    VERBS V-QIANG3-BAO4 V-ZHUI1-ZHUANG4 V-GUA4-PAI2 V-WEI4-QI1 V-XIANG1-CHU3
    V-JING4-LAO3 V-BIAN4-TONG4 V-JIN4-CHANG2 V-HUO3-QI3 V-BAO3-XIAN3 V-DI1-QU3
    V-MIN3-MIAN3 V-YANG3-LAO3 V-LI4-XIE4 V-JIAN4-KONG4 V-SHI5-XIAN4 V-FANG2-WEI4
    V-ZHEN1-CHA2 V-JIU1-ZHENG1 V-YONG4-REN2 V-PIAN4-SHUI4 V-DUAN4-LIE4
    V-FANG2-ZHEN4 V-QIANG3-XIAN3 V-ZHONG4-DU2 V-ZHAN4-DE5 V-DAO3-TA1 V-XIU1-ZHU4
    V-BAN4-AN4 V-ZHUI1-JIU1-B V-FAN3-DI4 V-HANG2-HANG2 V-XIU1-LUO4 V-AN4-FANG3
    V-ZHI4-JIA4 V-ZAO4-JIA4 V-DA2-JIA4-A V-BAN4-JIAO4 V-FAN1-CHUAN2 V-KANG4-MEI3
    V-YUAN2-ZHAO1 V-XIAO1-DU2 V-SHI2-YONG4 V-BAN1-QIAN1 V-ZHI4-FU4-B V-ZHI2-QIN2-A
    V-YU4-FANG2 V-QIAN2-JIN4-B V-JI1-LI4 V-TAO4-TAI4 V-GU4-ZHANG4 V-MING4-MING2
    V-TUO1-PIN2 V-JU4-JI2 V-QIN1-LUE4 V-FENG1-SHENG4 V-REN2-ZHI2 VERB-DEGREE
    V-KANG1-FU4 V-TING2-JIAN4 V-SHEN1-BAN4-B V-HU2-TAN2 V-BAO3-SHI4 V-MOU2-SHA1
    V-JUE2-SHI2 V-FEN4-GONG1 V-TING2-LIU2 V-DEI3-JIANG3 V-PAI2-LIE4 V-HANG2-JIN4
    V-XUAN1-JIAO1 V-SHOU4-JIANG3 V-KAI1-TING2 V-SHEN3-LI4 V-JING1-JIAN3
    V-BEI4-ZHAN4 V-TAN1-WU1 V-TING2-ZHI2 V-PING2-JIAN4-B V-RU4-XUAN3 V-YAN4-PIAO4
    V-CE4-PIAO4 V-ZHUI4-JI1 V-JIAN4-GOU4 V-SHI4-SHE4 V-DENG1-SHAN1 V-HUO3-SU4
    V-ZU3-ZHI3 V-DA2-LAO1 V-XU3-KE4 V-XIA4-BAN1 V-YI4-YE4 V-GOU4-BING4-A
    V-JIN4-HANG2 V-CHE4-ZHI2 V-CHU3-ZHI4 V-FENG1-SUO3 V-CHENG2-HANG2-B
    V-XIE2-FANG2 V-GU3-ZHANG3 V-ZHUAN1-ZHENG4 V-ZHUAN1-FANG3 V-DAO3-HANG2
    V-ZAI4-SHENG1 V-CHONG2-JU4 V-LI2-SAN3 V-ZHEN1-XUAN3 V-ZHONG4-ZHUAN4
    V-SAN1-TONG4 V-XIE4-HUO4-A V-TOU2-SU4 V-SHE4-CHANG3 V-SHOU4-XIN4
    V-JIE4-JI2-YONG4-REN3 V-SHOU4-HUI4-A V-ZHEN4-DANG4 V-SHI4-SHI4-B V-HU4-PAN2
    V-FEN4-TAN1 V-FA5-SHENG1-B V-SHOU1-WEI3 V-FANG2-ZAI1 V-PAI2-YAN1 V-FANG2-QI3
    V-SHI5-BIE4 V-DIAN4-HAN4 V-LENG3-QUE4 V-QIAN1-YUE1 V-SHI4-YONG4-A V-PI1-PAN4
    V-JI1-RUO4 V-QI3-MENG2 V-HUA4-B V-JIU4-WANG2-TU2-CUN2 V-CHU1-FA5 V-JIA1-FEN4
    V-SHOU3-YING4 V-FEN2-SHAO1 V-TONG4-TIAN1 V-YAN3-JIN4 V-TONG4-FENG1 V-CHU1-TU3
    V-YIN4-ZHI4 V-CHUANG4-BAN4 V-XIU1-XI1 V-XIN1-ZHUAN4 V-XIU1-FU4 V-FU4-SHI3
    V-ZHUI1-ZONG1 V-LIN2-JIN4-A V-XI3-SHEN4 V-JI3-SHA1 V-SI1-SHA1 V-ZHUAN4-SONG4-D
    V-BO1-YING4 V-CAI3-FANG3 V-SHA1-JIA4 V-TAO3-JIA4 V-HAI2-JIA4 V-BI4-XU1-A
    V-FEI1-HANG2-A V-DA2-GONG1 V-SHI2-MAO2 V-XING1-HUO3 V-QIANG3-GOU4 V-FA5-BING4
    V-XIE4-DONG4 V-XIU1-GAI3 V-HAN2-GAI4-B V-DUAN3-SHI4 V-SHANG5-GUI4 V-SHEN3-HE2
    V-CHONG1-CI4 V-YIN1-YING4 V-HAN4-WEI4-A V-CHOU2-HUA2 V-GE1-CHANG4 V-SHA1-LU4
    V-ZHU4-F V-XI3-ZAO3 V-ZHUAN4-JIAO1-A V-CHAO2-FENG3 V-CHAN3-XIAO1
    V-YAN2-JIN4-HAI3 V-KU1-JIE2 V-QIU2-JIU4 V-HUI4-ZHENG3 V-CHAO3-ZUO4 V-ZHI1-BU4
    V-ZHI4-TAO2 V-ZHAN4-BO5 V-ZHU4-HE4 V-PU3-SHI4 V-DAI4-BU3 V-QIU1-JIN4
    V-DONG4-TU3 V-PAO2-XIAO4 V-CHEN2-DIAN4 V-YAO2-CE4 V-YIN3-SHUI4 V-PAI2-SHUI4
    V-ZHI4-SHUI4 V-JING1-SHENG3 V-RU4-QIN1 V-XIU1-BU3 V-BO1-HAO4 V-YUE1-SHU4
    V-LUO3-LOU4 V-GUO5-LV4 V-SOU1-XUN2 V-SAO4-DANG4 V-FEN4-JI2 V-YU4-SHI4-B
    V-XUN2-ZHAO3 V-ZHUAN4-SHI4 V-XIAO1-ZAI1 V-HONG2-FA3-A V-TUO1-GUI3 V-DA2-QIU2
    V-YI4-MAI4 V-CHU1-JI1 V-BEI4-LI2 V-MI3-FENG1 V-KAI1-GUO2 V-BAN1-YUN4
    V-TONG3-ZHI4 V-WEN2-ZHI4 V-DA4-YI1-TONG3 V-ZAN4-ZHU4 V-TAN4-CE4 V-XUN2-HUI2-A
    V-SHOU1-YIN1 V-GUAN1-KAN1 V-JI1-QIU2 V-HUI1-GAN1 V-ZAO4-SHI4 V-JIANG1-CHI2
    V-RANG4-DU4 V-PAI1-SHE4 V-ZHUAN4-CHENG2 V-LU4-ZHI4 V-ZHONG4-TIAN2 V-AI4-XIANG1
    V-ZHAN3-ZHUAN4 V-FEN4-GE1 V-PING2-FAN3 V-KANG4-ZHENG1 V-LI2-HUN1 V-SONG1-BANG3
    V-XUAN1-XIE4 V-PAN4-DOU4 V-FEI1-YUE4-A V-QIAN2-ZHAN1 V-TUN2-JI1 V-ZAN4-CHENG2
    V-FAN3-TAN2 V-DEI3-PIAO4 V-DANG1-XUAN3 V-XI5-HUA4 V-BI4-YE4 V-PAI1-PIAN1
    V-LISTEN V-SAY V-DU2-B V-DA2-SAO4 V-CUN2-ZA4 V-FANG3-SHI4 V-HUO3-PAO3
    V-XIA4-KE4 V-BU3-JIU4 V-XIAO1-CHU2-A V-XING1-JIAN4 V-YUN4-ZHUAN4
    V-JIAN4-CHANG3 V-XUAN1-DAO3 V-FU4-GONG1 V-GONG4-CHAN3 V-CHANG4-XIAO1
    V-XIANG1-ZHI1 V-FA2-JUE2 V-TI4-DAI4 V-GUAN1-CHANG3 V-ZHENG3-HE2 V-HUAN3-CHONG1
    V-DU2-JU1 V-SHOU1-RONG2 V-SHENG1-ZHANG3 V-FAN2-YAN3 V-SHENG1-ZHI2 V-FEN4-BU4
    V-BU3-ZHUO1 V-FANG3-ZHI4 V-WA1-JUE2 V-KAI1-GUAN3 V-ZHUAN4-KE4 V-PIAO1-YI4
    V-HENG4 V-SHU4-B V-YU1-HUI2 V-SHEN1-SUO1 V-PO4-HUAI4 V-SHI4-XIAN4 V-DU2-CAI2
    V-CAI3-JI2 V-ZAI1-ZHI2 V-ZAI1-PEI2 V-ZAI1-ZHONG4 V-XIE2-BAN4 V-FA5-LI4-A
    V-SUAN4-MING4 V-SHENG1-GUAN1 V-YAO2-QIAN2 V-ZHAO1-LI4-A V-ZHI4-BING4
    V-FA5-YAN2-A V-XI1-SHI4 V-FU4-JIAN4 V-JIAN4-SHEN1 V-DIAN4-DU4 V-BENG1-PAN2
    V-CHU2-CUN2 V-LU4-YING3 V-JIN4-RU4-B V-XIE4-TAO4 V-QIU2-ZHI2 V-GOU4-WU1
    V-DONG4-GONG1 V-JIAO1-HUAN4 V-ZHAO4-XIANG1 V-CHAO2-SHENG4 V-FEI1-TIAN1
    V-JIAN3-SU3 V-HOU4-XUAN3 V-SHEN1-XUAN3 V-XIU1-ZHENG1 V-GAN3-EN1 V-FU4-XING1
    V-TING2-JU1 V-ZHI4-SHENG4-A V-JIE1-SHOU4 V-RUI4-HUO4 V-JU1-JIA1 V-CU4-XIAO1
    V-PIAO1-QIE4 V-HUI4-TU2 V-JIE1-CHAN2 V-GAI3-LIANG2 V-RU4-MEN2 V-YUN4-SUAN4
    V-XUN4-ZANG4 V-GEN1-JIN4 V-ZHAN4-LI4-A V-MAN3-YUE4 V-JIE2-ZHI1 V-KAI1-MU4
    V-HANG2-SHI4 V-SOU1-SUO3 V-DI1-SHENG1 V-XIAO1-JIE2 V-YAO1-QING3 V-SHE4-ZHI4
    V-SHU2-XI1 V-XUAN1-SHI4-A V-LIU2-WANG2 V-TUI1-LI4 V-LAI2-FANG3 V-KEN3-DING4
    V-YU4-JING3 V-SHOU1-JI2 V-ZHEN1-ZHA1 V-CHU1-FANG3 V-SHI1-HANG2 V-ZHUAN1-SI1
    V-DAO3-ZHANG4 V-QIE1-RU4 V-YUAN2-WAI4 V-JIN1-YUAN2 V-YAN2-XU4 V-WEI2-DU3
    V-QING4-HE4 V-ZA2-JIAO1 V-ZHEN1-CE4 V-GAN3-RAN3 V-BIAN4-SHI5 V-FA5-SHAO1
    V-SHU1-C V-TAN1-C V-ZHANG4-B V-LI4-LI4-B V-TE4-SHE4-A V-SHI1-ZHENG4
    V-SHEN1-ZHENG4 V-WEN4-ZHENG4-A V-ZHENG4-ZHENG1 V-JI3-FEI2 V-AN1-FU3
    V-XIU1-XIAN4 V-WEI3-DIAO4 V-QIAN2-SHUI3 V-YOU2-LE4 V-DU4-JIA4 V-ZHA4-PIAN4
    V-TUO1-GUAN3-B V-FA1-YUAN2 V-DU2-SHU1 V-GUANG1-FU4 V-CHUAN1-YI1 V-BAO3-YANG3
    V-YANG3-SHENG1 V-SU4-SHEN1 V-WEI2-SHENG1 V-ZHENG3-XING2 V-ZHENG3-RONG2
    V-MEI3-RONG2 V-BAO3-SHI1 V-LOU4-YING2 V-MO2-SUO3-B V-HANG2-XIAO1 V-JIN4-XIU1
    V-CHONG1-DIAN4 V-SHENG1-XUE2 V-HUI2-LIU2 V-ZHU3-BAN4 V-YAN2-XI2-A
    V-SHENG4-XUAN3 V-QI4-C V-ZHI2-ZHENG4 V-ZHI4-GUO2 V-JIAO1-GAI3 V-CONG1-ZHENG4
    V-LI3-DUO1 V-TAI1-DU2 V-MIN2-XUAN3 V-DUI4-LI4 V-LIAN2-FANG2 V-BAO4-FU4-A
    V-XIA4-ZU3 V-GONG1-JI1 V-FAN3-ZHI4-B V-ZHI4-KONG1 V-GONG1-B V-FANG2-YU4
    V-JUN1-GOU4 V-CHU1-SHU1 V-HU4-DONG4 V-KOU5-SHU4 V-QIU2-XUE2 V-BI3-JIAO4
    V-SHUI4-HUA4 V-JI4-XIU4 V-BAO4-GUAN1 V-YING4-CHOU2 V-SHANG5-KE4 V-JIE4-YAN2
    V-SHENG1-QING3 V-CHOU2-ZI1 V-YAN2-NI3 V-BU3-ZHU4 V-ZHI1-GEI3 V-WEI2-LU2
    V-TUAN2-YUAN2 V-TUAN2-JU4 V-CHENG2-ZHANG3 V-HUAN1-XI3 V-GUO5-NIAN2 V-EAT
    V-RAN2-SHAO1 V-TI2-B V-TONG2-QING2 V-TOU2-SHE4 V-BIAO3-DA5 V-GUAN3-JIAO1
    V-GUAN1-HUAI4 V-JIAN3-TIAO2 V-YUN4-YONG4 V-KAI1-LI4 V-ZHA1-A V-DIY V-ZHI1-ZAO4
    V-TOU4-SHI1 V-RAN3-SE4 V-TANG4-SHANG1 V-REN4-TONG2 V-YAN2-FA5 V-GOU1-TONG4
    V-ZHI2-MIN2 V-BAO4-MING2 V-YI2-JIAO1 V-DAO3-BI4 V-CHONG2-JIAN4 V-KAI1-HU4
    V-FU4-ZHI4 V-CE4-LIANG5 V-TAN4-XIAN3 V-JI3-ZHUAN4 V-DAN4-SHENG1 V-DONG4-WU3
    V-KONG4-XI2 V-ZUO4-ZHAN4-a V-KANG4-YI4 V-FEN1-FANG2 V-JU1-ZHU4 V-SHUN4-CHA4
    V-LIE4-SHI2 V-GOU4-MAI3 V-HE2-BING4 V-ZU1-LIN4 V-JIN4-CHU1 V-ZAO4-CHUAN2
    V-FENG1-DING3 V-TONG4-HANG2-A V-ZHENG3-ZHI4 V-LIANG4-XIE4 V-JIE1-SHOU1
    V-BAN4-XUE2 V-MU4-LIN2 V-SHEN1-JIA1 V-JIE1-CHU4 V-CUO1-SHANG1 V-GUO5-JING4
    V-HU4-FANG3 V-GENG1-ZHONG4 V-JIU4-ZAI1 V-ZHEN4-ZAI1 V-REN4-JUAN1 V-YAN3-SHENG1
    V-SHANG5-WANG3 V-PIAN4-HUI4 V-ZHI5-DING4 V-LIAN2-WANG3 VERB-DEGREE
    V-RU4-SHI4-B V-QIANG3-HUA4 V-CHI2-CANG1 V-JIN3-SUO1 V-JIE1-DAI4 V-GEN1-ZONG1
    V-TUO1-YUN4 V-LENG3-ZHAN4-A V-JI2-JIU4 V-DING4-DIAN3 V-LIU2-XIE3 V-HU2-XIE4
    VERB-DEGREE V-YONG4-YAO4 V-BAO4-XIAO1 V-CHAN2-YI1 V-HANG2-JU1 V-XUN2-LUO2
    V-SHEN3-DING4 V-HUI4-BAO4 V-FA5-BIAO3 V-GONG1-JIAN1 V-XUN2-HUAN2 V-FA5-FANG4
    V-FENG1-BI4 V-BO1-DONG4 V-CHU1-TAI1 V-ZHUAN3-YUN4 V-DUI4-HUAN4 V-JIE2-NENG2
    V-SHEN1-KAO3 V-GOU4-FANG2 V-JUN4-GONG1 V-PAI2-FANG4 V-JIAN1-LI3 V-ZHI2-FA3
    V-TU1-JI1 V-PAI2-WU1 V-SHANG1-TAO3 V-DA5-BIAO1 V-CHU1-GUO2 V-BAO3-ZHI2
    V-YU4-NAN4 V-QU3-ZHENG4 V-ZHAO4-SHI4 V-XIA4-JIANG4 V-DIAN1-FU4 V-FANG2-KONG4-B
    V-TOU2-PIAO4 V-KANG4-HONG2 V-ZHI1-FU4 V-RUI4-XIU1 V-SHANG5-ZHANG4 V-YUAN2-JIU4
    V-SHI1-SHI4 V-ZHEN4-DANG4 V-WA1-QIAN2 V-GOU4-WU4 V-JIAO1-HUO4-A V-FU4-KUAN3
    VERB-DEGREE V-YIN4-SHUA1 V-ZHAN3-SHI4 V-LIFT-WEIGHT V-LA4-SHI5 V-ZOU3-XIANG4
    V-MO2-FANG3 V-KAI1-TUO4 V-KE4-LONG2 V-QU1-TONG2 V-RU4-WANG3 V-CHOU1-YANG5
    V-TING2-YE4 V-FA5-YU4 V-BIAO3-JUE2 V-ZHAO1-SHENG1 V-SHEN3-YI4 V-BI4-MU4-B
    V-CHU1-SHI4-C V-LAO2 V-SHENG1-CUN2 V-ZHUAN4-KUAI4 V-MIN3-CHAN3 V-WEN4
    V-TIAO2-DUO2 V-FEN4-DOU4 V-LUE1-DUO2 V-JIAN4-CHA2 V-XIAO1-LI4 V-JIU4-YUAN2
    V-SHEN1-SAI4 V-QI1-ZHA4 V-TAN2-HE2 V-HE2-ZHA1 V-SHOU1-GOU4 V-HUA2-XUE4
    V-GENG4-XIN1 V-SHOU4-HAI4 V-SHOU4-LI4 V-GUAN4-GAI4 V-BIAN4-HU4 V-SHEN3-PAN4
    V-YING4-DUI4 V-SHEN1-YU3 V-TONG3-A V-DUI4-ZHI4-A V-JING3-JIE4 V-HU1-XI1
    V-JIAN4-HU4 V-TAN4-SUO3 V-FENG1-SHAN1 V-RUI4-GENG1 V-XU4-JIAN4 V-QIU2-ZHU4
    V-JI3-LV4 V-JIA4-MAO4 V-FAN4-DU2 V-JI1-DU2 VERBS V-BO1-FU4 V-GUAN1-SHANG3
    V-BAO3-CUN2 V-HUO3-BU4 V-CHENG2-XING2-A V-NI4-CHA4 V-YOU1-SHENG4 V-GAI3-ZU3
    V-BAO3-JIAN4-B V-JI2-SAN4 V-TUO4-ZHAN3 V-TONG4-GUO5 V-GONG1-HUO4 V-FEN4-ZU3
    V-ZHUANG1-XIU1 V-YUAN2-JIAN4 V-FANG2-FAN4 V-CHU1-ZI1 V-TONG4-JU1 V-SOU1-JI2
    VERB-DEGREE V-RU4-GU3 V-CHONG1-JI1 V-KUO4-ZHAN3 V-YING2-YUN4 V-NEI4-XIAO1
    V-KUO4-CHANG3 V-WAI4-XIAO1 V-KUO4-CHONG1 V-BIAN3-ZHI2 V-HAO4-ZHAO4 V-JIA1-RU4
    V-YUE4-DU2 V-ZENG1-JIA1 V-SI3-WANG2 V-XI1-YAN1 V-ZHU4-CHONG2 V-YU4-QI1
    V-FEN4-XIANG3 V-DENG1-JI1-B V-LI2-JING4 V-GONG3-GU4 V-SHENG1-XIAO4
    V-LV3-HANG2-A V-LIAN2-LUO4 V-JIAN4-JIAO4 V-TONG3-CHOU2-B V-ZHUI1-SU4
    V-XIU1-DING4-B V-ZHUAN1-ZE2 V-HE2-SHI5 V-ZHENG4-MING2 V-GONG1-BU4 V-JIE2-HUN1
    V-SHEN1-GUAN1 V-ZHI1-YUAN2 V-JIAN4-DING4-B V-GU4-YONG4-B V-LIAN2-ZHENG4
    V-SHEN1-SU4 V-JIAN3-KONG4 V-YAN2-QI1 V-SHANG5-SU4-B V-ZHUAN4-BO1-B
    V-SHENG1-JIANG4 V-ZHUAN4-RANG4 V-JIE2-ZHI3 V-WEI2-XIU1 V-QING1-XIE4
    V-CHAI1-XIE4 V-ZU1-ZHU4 V-CHU1-ZU1 V-ZI1-ZHU4 V-RU4-XUE2 V-YAN2-DU2 V-FU3-DAO3
    V-SHANG5-BAN1 V-CHU3-LI4 V-FEN4-PAN4 V-FINISHED V-ZHUANG1-SHE4 V-YING2-SHANG1
    V-ZENG1-ZHI2-C V-JU1-LIU2-A V-RU4-JING4 V-QI2-FU2 V-SONG4-ZENG4 V-CHENG2-LI4
    VERB-DEGREE V-TUI1-XUAN3 V-XUAN1-SHI4-B V-JIU4-ZHI2 V-PEI2-XUN4 V-GONG1-ZHENG4
    V-SHU1-SONG4 V-YAN4-SHOU1 V-TOU2-BIAO1 V-FU2-ZHI2 V-XIU1-XIAN2 V-GUAN1-GUANG1
    V-FA5-MING2 V-ZHUANG4-JU1 V-ZHI5-DAO3 V-KAI1-CAI3-B V-SUO3-PEI2 V-QING1-XIE2
    V-PING2-BI3 V-PAI2-XU4 V-LA4-HOU4 V-TONG4-HANG2-B V-KAI1-GONG1 V-WANG3-LAI2
    V-YANG3-ZHI2 V-BU3-LAO1 V-FANG3-WEN4 V-ZHUAN4-YI2 V-DI1-GONG1 V-JIE2-HUI4
    V-YING2-LI3-A V-KAI1-YE4 V-ZENG1-SHOU1 V-FA5-JIAO4-B V-ZHUAN4-XING2
    V-ZU3-JIAN4 V-JIE1-LI4 VERB-DEGREE V-LIU2-HANG2 V-PEI4-HE2 V-XIE2-SHANG1
    V-RUI4-RANG4 V-TING2-ZHAN4 V-JU3-ZHENG4 V-ZHA1-JI1-B V-PI1-ZHUN3
    V-JIAN4-SHI4-B V-JIAN4-CE4 V-DIAN4-JI1 V-TUI1-HANG2 V-SHI5-JIAN4 V-CU4-JIN4
    V-BAN1-JIANG3 V-LIN2-MO2 V-SHE4-YING3 V-XUE2-XI2 V-JIAO1-XUE2 V-JIU4-ZHU4
    V-SHENG1-YU4 V-JIAN4-ZAO4 V-FA5-DIAN4 V-XIN4-TUO1 V-BU3-CHONG1 V-ZHAO1-PIN4
    V-QIA4-TAN2 V-SHEN1-ZHAN3-B V-QIAN3-FAN3 V-JIE4-DAI4 V-GUAN3-ZHI4
    V-ZHUANG4-JI1-A V-PENG4-ZHUANG4 V-XIE4-FANG4 V-JI3-ZHI4-A V-CHU1-BAN3
    V-ZHI4-XIAN4 V-CHUANG4-SHI3 V-ZHI4-LIAO2 V-DI1-GAO1 V-MO2-NI3 V-FAN4-ZUI4
    V-JIAN4-JIAO1 V-CAI2-JUN1 V-CHAN3-SHENG1 V-TING2-QI3 V-REN2-MIAN3 V-SHEN3-ZHA1
    V-SHEN3-CE4 V-MAI2-CANG2 V-FAN2-ZHI2 V-KAO3-KU3 V-CHAN3-B V-GONG1-C
    V-CHE4-JUN1 V-YUE4-BING1 V-HUA2-FEN4 V-MU4-JUAN1 V-KAI1-XUE2 V-HUI1-FU4
    V-LU4-QU3 V-QIANG3-XIU1 V-SHI4-FAN4 V-PEI2-YU4 V-HUI2-SHOU1 V-FEI1-HANG2-B
    V-FA5-SHE4 V-ZOU3-SI1 V-XIE4-JUE2-C V-SHU1-XIE3-B V-PING2-XUAN3 V-YOU2-YONG3
    V-TIAO4-SHUI4 V-SHI2-SHI4-QIU2-SHI4 V-TUI1-DONG4 V-SHU1-RU4 V-DONG4-YUAN2
    V-SHOU4-QUAN2 V-FAN2-RONG2 V-CHOU2-ZU3 V-BAN4-GONG1 V-ZHU4-CE4 V-ZI1-XUN2-B
    V-ZHAO1-BIAO1 V-TONG4-XUN4 V-ZHU4-LUO4 V-JIAO1-JIE1 V-LING2-XIAN1 V-JIAN3-CE4
    V-CHUANG4-HUI4 V-ZHUANG1-PEI4 V-RU4-ZHU4-A V-YI1-LAI4 V-ZU1-JU1 V-MU4-JI2
    V-REN4-GOU4 V-HUI2-GUI1 V-JIAN4-GUO2 V-FANG1-GAI3 V-FEN4-PEI4 V-CHENG2-SHOU4
    V-FEN4-LIU2 V-CHONG2-ZU3 V-TIE1-XI1 V-ZHUAN4-SHU1 V-CAO1-ZUO4 V-TOU2-JI1
    V-ZHA1-CHU3 V-JIA4-JIE1 V-PING2-DING4-A V-FA5-ZHAI4 V-SHI5-YONG4 V-JIE2-HE2
    VERBS V-XIE2-ZUO4 V-SHAI1-XUAN3 V-DAN1-BAO3 V-CHANG2-HAI2 V-NIAN2-CHAN3
    V-LI3-YONG4 V-DIAN1-BO3 V-TIAO2-KONG4 V-LA1-DONG4 V-SHI1-YE4 V-XIE4-KUN4
    V-ZHI4-YUE1 V-ZENG1 V-AN1-ZHI4 V-ZHUAN4-YE4 V-JIAN1-BING4 V-HUN1-HE2
    VERB-DEGREE V-JIU4-YE4 V-YING4-BIAN4 V-ZHAN4-C V-DAI4-DONG4 V-YIN3-JIN4
    V-TOU2-CHAN3 V-ZHUI1-JIA1 V-XIA4-GANG3 V-NIU3-KUI1 V-KAN1-TAN4 V-GONG1-DIAN4
    V-ZHU3-DAO3 V-KAO3-CHA2 V-KAI1-TONG4 V-KUO4-DAI4 V-QI3-DONG4 V-HU4-BU3
    V-ZENG1-CHAN3 V-XIAO1-FEI4 V-XIAN4-ZHI4 V-GAI3-ZHI4 V-LI4-SHU3-A V-CHU1-SHOU4
    V-PAN1-DENG1 V-TUO1-SHOU1 V-TIAO2-ZHA1 V-PO4-CHAN3 V-TIAO2-XIE4 V-TONG4-GUAN1
    V-CHUANG4-YE4 V-JIN4-BU4 V-ZHENG1-SHOU1 V-BAO3-QUAN2 V-ZHI2-HANG2-A
    V-GUANG3-BO1 V-YUAN2-ZHU4 V-LAO2-DONG4 V-PIAN2-LI3 V-SHANG5-SHI4-B
    V-ZHONG4-ZHI2 V-TIAO2-ZHENG3 V-YI2-DONG4 V-TOU2-RU4 V-XIAN2-JIE1 V-ZHI4-CAI2
    V-NIANG4-ZHI4 V-XIAO1-HUA4 V-DI1-LIAN4 V-GAI3-ZAO4 V-BIAN1-ZHI1 V-BEI4-AN4
    V-WEI1-TUO1 V-YUN4-YING2 VERB-DEGREE V-FEN4-LIE4-B V-GUAN4-CHE4 V-ZHI4-YAO4
    V-BAO3-MI4 V-FANG2-ZHI4 V-YAN2-ZHI4-C V-LI4-FA3 V-ZHI1-CHENG1 V-CHUANG4-XIN1
    V-QI2-SHI4 V-KUO4-JIAN4 V-FEN4-XI1 V-LUN4-ZHENG4-B V-JIAO1-LIU2 V-FU2-CHI2
    V-ZHI4-LI4-C V-YUN4-ZAI4 V-WANG3-FAN3 V-CE4-SUAN4-B V-YOU2-SHUI4 V-CHOU2-JIAN4
    V-ZHUANG1-XIE4 V-JIAO1-WANG3 V-TUI1-XIAO1 V-HUO3-JIANG4 V-KONG4-ZHI4-B
    V-TIAO2-SHI4 V-SHENG1-JI2 V-ZHI4-ZAO4 V-GUAN1-ZHU4 V-ZHI2-HANG2-C
    V-TONG4-SHANG1 V-QIAN1-ZI4 V-YING2-YE4 V-SHEN3-PI1 V-BAN4-SHI4 V-ZHENG3-DUN4
    V-YING2-XIAO1 V-YUN4-HANG2 V-KAI1-ZHAN3 V-FA5-HANG2 V-YIN3-ZI1 V-SHEN1-ZHAO4
    V-JIAN4-GUAN3 V-YUN4-ZUO4 V-LIU2-TONG4 V-ZHEN4-XING1 V-HE2-ZUO4 V-JIN4-ZHAN3
    V-JIA1-GONG1 V-ZHAO1-SHANG1 V-XIAO1-SHOU4 V-TOU2-BAO3 V-SHU1-CHU1
    V-AN1-ZHUANG1 V-YUN4-SHU1 V-LV3-YOU2 V-FU2-PIN2 V-FA5-ZHAN3 V-HE2-XIAO1
    V-JING1-YING2 V-CHENG2-BAO5 V-ZENG1-ZHANG3 V-KAI1-FANG4 V-GUAN3-LI4
    V-CHAI1-QIAN1 V-SHI5-SHI1 V-TONG4-XIN4 V-SHI1-GONG1 V-YI1-LIAO2 V-CAI3-GOU4
    V-KAI1-FA5))

(defparameter *verb-in-mod-noun-hash*
  (let ((h (make-hash-table)))
    (dolist (v *verb-in-mod-noun-list* h)
      (setf (gethash v h) t))))

(defun verb-in-mod-noun-p (v)
  ;; v is an verb instance
  (gethash (type-of v) *verb-in-mod-noun-hash*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The current whitelist of verbs that can be followed by pause, when
;;; used on verb-p and subj-pred.
(defparameter *verbs-with-pause*
  ;; TODO: get a more complete list
  '(V-BIAO3-MING2 V-WEN4-DAO4-B V-XIE3-DAO4-B V-XIE3-DAO4-A V-ZHI5-MING2 V-SAY
    V-DISCOVER V-HUI2-DAO4-A V-CHENG1-DAO4 V-CHANG4-DAO4 V-RANG5-DAO4
    V-XIAO4-DAO4-A V-SHUI4-DAO4-A V-YELL V-JIAO4-DAO4-B V-SHENG1-MING2
    V-BAO4-DAO4-C V-DAO4-D V-XIANG3-DAO4 V-REN4-WEI4 V-YI3-WEI4 V-GU1-CE4-B
    V-FA5-XIAN4 V-XIANG1-XIN4 V-XUAN1-BU4 V-YU4-CE4-A  V-YU4-CE4-B V-XIE4-SHI4-B
    v-gao4-su4 v-biao3-shi4 v-zhi5 v-cheng1-b v-xuan1-cheng1 v-ji3-cheng1
    v-zhi5-cheng1 v-hao4-cheng1 v-xi4-cheng1 v-kou5-cheng1 v-tan3-cheng1
    v-gong1-cheng1 v-xiao4-cheng1 v-huang1-cheng1 v-bian4-cheng1
    v-jian1-cheng1-b v-ju1-cheng1 v-sheng1-cheng1
    ))

(defparameter *verbs-with-pause-hash*
  (let ((h (make-hash-table)))
    (dolist (v *verbs-with-pause* h)
      (setf (gethash v h) t))))

(defun verbs-with-pause-p-x (v)
  ;; v is an verb instance
  (gethash (type-of v) *verbs-with-pause-hash*))

(defun verbs-with-pause-p (v)
  (typecase v
    (conn-thing
     (and (verbs-with-pause-p-x (conn-thing-first v))
          (verbs-with-pause-p-x (conn-thing-second v))))
    (combine-verb
     (or (verbs-with-pause-p-x (combine-verb-first v))
         (verbs-with-pause-p-x (combine-verb-second v))))
    (verb0
     (verbs-with-pause-p-x (verb0-verb v)))
    (t (verbs-with-pause-p-x v))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric mod-noun-dui4-noun (n d)
  ;; n is a noun being modified
  ;; d is the noun in dui4-noun
  )
(def-comb mod-noun-dui4-noun
  ;; fallback case
  ((t t) *c-rare*)

  ;; e.g. '大连 (对韩 出口)'
  ;; (对 华) 投资
  ;; (对 外) 贸易
  ;; (对 外) 经贸
  ((transaction country-name) *c0*)
  ((transaction dir-inside-outside) *c1*)
  ((transaction province-abbr) *c1*)
  ((transaction place) *c1*)
  ((transaction race-name) *c2*)

  ;; e.g. '澳 ((对 亚洲) 政策)'
  ((policy country-name) *c0*)
  ((policy continent-name) *c0*)
  ((policy province-abbr) *c1*)
  ((policy dir-inside-outside) *c1*)
  ((policy place) *c2*)
  ((policy race-name) *c2*)

  ;; (对 非洲) 援助
  ;; (对 台) 军售
  ;; (对 美) 博弈
  ;; (对 伊朗) 石油 制裁
  ;; (对 美) 贸易 顺差
  ;; (对 外) 联络
  ;; (对 外) 合作
  ;; (对 外) 开放
  ;; (对 华) 经营
  ;; TODO: these are verbs, whether to leave it as verb-p?

  ;; (对 台) 军售额
  ;; (对 俄) 出口额
  ((amount-of country-name) *c0*)
  ((amount-of continent-name) *c2*)
  ((amount-of province-abbr) *c1*)
  ((amount-of dir-inside-outside) *c1*)
  ((amount-of place) *c3*)

  ;; (对 台) 问题
  ((posed-question country-name) *c0*)
  ((posed-question continent-name) *c2*)
  ((posed-question province-abbr) *c1*)
  ((posed-question dir-inside-outside) *c1*)
  ((posed-question place) *c3*)

  ;; (对 台) 策略
  ((strategy country-name) *c0*)
  ((strategy continent-name) *c0*)
  ((strategy province-abbr) *c1*)
  ((strategy dir-inside-outside) *c1*)
  ((strategy place) *c3*)
  ((strategy race-name) *c2*)

  ;; (对 日) 关系
  ((relationship-of country-name) *c0*)
  ((relationship-of continent-name) *c2*)
  ((relationship-of province-abbr) *c1*)
  ((relationship-of dir-inside-outside) *c1*)
  ((relationship-of place) *c3*)
  ((relationship-of race-name) *c2*)

  ;; (对 中国 政府) 态度
  ((attitude country-name) *c0*)
  ((attitude continent-name) *c2*)
  ((attitude province-abbr) *c1*)
  ((attitude dir-inside-outside) *c1*)
  ((attitude organization) *c2*)
  ((attitude human) *c2*)
  ((attitude place) *c3*)
  ((attitude race-name) *c2*)

  ;; (对 华) 投资 信心
  ((abstract-heart-attribute country-name) *c1*)
  ((abstract-heart-attribute province-abbr) *c1*)
  ((abstract-heart-attribute dir-inside-outside) *c1*)
  ((abstract-heart-attribute organization) *c1*)
  ((abstract-heart-attribute human) *c0*)
  ((abstract-heart-attribute place) *c3*)
  ((abstract-heart-attribute race-name) *c1*)

  ;; (对 外) 承包 工程
  ((process dir-inside-outside) *c0*)
  ((process place) *c3*)

  ;; (对 越) 自卫 反击战
  ((battle country-name) *c0*)
  ((battle dir-inside-outside) *c1*)
  ((battle province-abbr) *c1*)
  ((battle race-name) *c1*)

  ;; (对 他) 评价
  ((evaluation-of country-name) *c0*)
  ((evaluation-of continent-name) *c2*)
  ((evaluation-of province-abbr) *c1*)
  ((evaluation-of dir-inside-outside) *c1*)
  ((evaluation-of organization) *c2*)
  ((evaluation-of human) *c2*)
  ((evaluation-of place) *c3*)
  ((evaluation-of race-name) *c2*)

  ;; (对 岛屿) 影响
  ((influence country-name) *c0*)
  ((influence continent-name) *c1*)
  ((influence province-abbr) *c1*)
  ((influence dir-inside-outside) *c1*)
  ((influence organization) *c2*)
  ((influence human) *c2*)
  ((influence place) *c3*)
  ((influence race-name) *c2*)

  ;; (对 台) 教育
  ((teaching country-abbr) *c1*)
  ((teaching province-abbr) *c1*)
  ((teaching dir-inside-outside) *c1*)
  ((teaching human) *c1*)

  ;; (对 外) 交往 活动
  ((arranged-event dir-inside-outside) *c0*)

  ;; default
  (((n abstract) (d t))
   (+ (if (also-verb-p n)
          *c-prefer*
          *c-close-suffix*)
      (typecase d
        (country-name *c2*)
        (continent-name *c3*)
        (province-abbr *c3*)
        (dir-inside-outside *c3*)
        (place *c4*)
        (t *c-rare*))))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-conn mod-noun-x (m n) mod-noun-xx)
(defgeneric mod-noun-xx (m n))
(def-comb mod-noun-xx
  ;; the fallback case
  (((m t) (n t)) (mod-noun-common m n))

  ;; bias of number of characters for mod
  ((verb-n noun)
   (if (= *L1* 2)
       *c-verb-x-noun-mod-noun*
       *c-rare*))

  (((m a-subj-pred) (n non-verb-noun))
   (cond ((= *L1* 2) *c2*)
         ((trait-value (a-subj-pred-pred m) :verb-mod-suo3)
          ;; e.g. '((黔江 地区) 所辖) (行政 区域)'
          (verb-noun (a-subj-pred-pred m) n))
         (t *c-common*)))

  (((v verb) (n noun))
   (cond ((and (<= *L1* 2)
               (verb-in-mod-noun-p v))
          ;; most verbs should be used in verb-noun context instead of
          ;; mod-noun context, so changed to use a whitelist approach
          ;; instead of a blacklist approach.
          
          ;; Of course, individual verbs can be listed separately to
          ;; override this default.
          (+ *c2*
             ;; prefer a noun not (N zhi N)
             (if (trait-value n 'noun-mod-has-de)
                 *c4*
                 0)))
         ((has-trait v 'verb-SP) *c-fallback*)
         ((has-trait v :verb-mod-CC) *c-rare*)
         (t *c-common*)))

  (((m yong4-adj) (n noun))
   (mod-noun (yong4-adj-the-verb m) n *L1* *L2*))

  ((verb-action non-verb-noun)
   ;; prefer '(美国 人) 的 (就业 机会)' to '((美国 人) 的 就业) 机会'
   *c-very-rare*)

  ((non-verb-noun de-noun-mod) *c-common*) ;; e.g. not want '自身 ((不利， 影响美国商人) 的)'
  
  ((verbs verb-n) *c-rare*) ;; want '推进 舱', not '推 进舱'
  
  ((adj noun-suffix) *c-rare*)
  ((verb noun-suffix) *c-rare*) ;; since the tag would be the first item, so discourage this combination
  
  (((v verb0) (n noun))
   (mod-noun-x (verb0-verb v) n))

  (((v verb-n) (n inanimate))
   (score-add (subj-verb n v)
              (if (= *L1* 2) 0 *c-very-rare*)))

  (((m de-noun-mod) (n noun))
   (let ((v (de-noun-mod-pred m)))
     (score-add
      *c-close-sep*
      (score-add
       ;; especially '中', '日', '印', '法', '美', '台', '意'
       (not-prefer n 'country-abbr *c-close-suffix*)
       (cond ((a-subj-pred-p v)
              (let ((rs (let ((v0 (a-subj-pred-pred v)))
                          ;; TODO: can v0 be a conn-thing of verbs?
                          (if (typep v0 'verb0)
                              (verb-noun (verb0-verb v0) n)
                              (mod-de-noun-x v n)))))
                (if (or (typep n 'reason)
                        (typep n 'word-culprit)
                        ;; e.g. '数十辆车连环撞 的 交通事故'
                        (typep n 'abstract-matter))
                    ;; e.g. '北部人口寿命缩减 的 元凶'
                    (score-max rs *c1*)
                    rs)))
             ;; 'verb-p de'
             (t (subj-verb n v)))))))
  (((m de-noun-mod) time-name)
   (let ((v (de-noun-mod-pred m)))
     ;; 'XXX 的 time', e.g. '通常因燃煤供暖导致雾霾天增多 的 冬季供暖期间'
     (if (or (trait-value v 'verb-mod-time)
             (is-type-p v 'v-zhi4-b)
             (and (a-subj-pred-p v)
                  (trait-value (a-subj-pred-pred v) 'verb-mod-time)))
         *c-common*
         *c3*)))

  ;;
  (((m pp-verb-mod) (n noun))
   ;; from verb-mod as noun-zhi-mod
   ;; e.g. '(有关 当地空气污染) 的 照片'
   (mod-noun (pp-verb-mod-object m) n (max 0 (- *L1* 2)) *L2*))
   
  ;;;
  ;; but in some cases, omitting the 'de' is not preferred

  ((v-ji3-ji3-a non-verb-noun) *c-very-rare*) ;; prefer '自己' as pronoun

  ((v-yin3-dao3 money-related) *c-rare*) ;; e.g. prefer '引导 外资' as verb-noun
  
  ((v-qu3-a noun) *c-rare*) ;; prefer 取 as verb-noun
  
  ((v-huo4-b place) *c-very-rare*) ;; prefer 获 as verb-noun
  ((v-huo4-b non-verb-noun) *c-very-rare*) ;; prefer 获 as verb-noun

  ((animate country-abbr-america) *c-rare*) ;; prefer 美 as beauty
  ((inanimate country-abbr-america) *c-rare*)
  ((abstract country-abbr-america) *c-rare*)
  ((place country-abbr-america) *c-rare*)
  ((p-pronoun country-abbr) *c-fallback*) ;; 本意

  ((v-jia1-qiang3 noun)
   ;; prefer verb-noun
   (if (> *L2* 1) *c-very-rare* *c-close-sep*))

  ((surname shop-like)
   *c-common*)
  ((surname county-ind)
   ;; e.g. '赵县'
   (if (= *L1* *L2* 1)
       *c1*
       *c-very-rare*))
  ((surname village-ind)
   ;; e.g. '陳村'
   (if (= *L1* *L2* 1)
       *c1*
       *c-very-rare*))

  ((abstract country-name) *c-rare*)

  ((organization shop-like) *c2*) ;; to encourage parallel form such as "评级机构  日本公社债研究所"

  ((place creative-work-ind) *c4*)
  ((electric-appliance goods-product) *c1*)

  ((v-jin4-ru4-b noun) *c-rare*) ;; prefer verb-noun
  ((v-do non-verb-noun) *c-rare*) ;; prefer verb noun

  ((v-at place)
   ;; For '在 place', prefer as verb-mod if used before verb-p
   ;; and prefer as verb-noun if used alone as verb-p.
   ;; If place is mono, prefer '在 place' verb-x
   *c-very-rare*)
  ((v-at non-verb-noun) *c-very-rare*) ;; prefer '在' as verb, or place ind.

  ((dir-verbs place) *c-fallback*) ;; prefer verb-noun

  ((v-shi4-wei4-a noun) *c-very-rare*) ;; extra black list, prefer verb-noun

  ((non-verb-noun army-like-word-general-ind) *c-rare*) ;; "將" alone is usually adverb or verb-mod

  ((transaction-ind noun) *c-common*) ;; "貿" is not usually used alone as prefix

  ((adv (n time-name))
   (if (has-trait n 'time-with-ind) *c-rare* *c-common*))
  ((adv-on-num-unit (n time-name))
   (if (has-trait n 'time-with-ind) *c-common* *c-close-suffix*))

  ((v-li3-yong4 money-related) *c-rare*) ;; prefer verb-noun

  ((v-shi5-shi1 (n noun))
   (if (typep n 'ind)
       *c-close-suffix*
       *c-rare*))
  ((v-shi5-shi1 import-export) *c-rare*) ;; prefer verb-noun or even (verb verb-p), e.g. 实施 出口

  ;; "將" alone is usually adverb or verb-mod
  ((non-verb-noun army-like-word-general-ind) *c-rare*)
  ((place army-like-word-general-ind) *c-rare*) ;; e.g. prefer adv in '浙江 将'

  ((v-ju1-a noun) *c-rare*) ;; prefer 据 as PP

  ((p-pronoun-ah noun) *c-rare*)
  ((p-pronoun-ah num) *c-rare*) ;; has special rule animate-a already
  ((p-pronoun-ah number) *c-rare*) ;; has special rule animate-a already
  ((p-pronoun-ah animate) *c1*)
  ((p-pronoun-ah human) *c0*)

  ;; dynasty often are also surnames, and are confusing. Most of the
  ;; time, they should be surnames
  ((dynasty occupation) *c4*)

  ((transaction style-or-process) *c2*) ;; prefer '转让 等 (投资 方式)' to '(转让 等 投资) 方式'
  ((transaction transaction) *c4*)
  ((transaction import-export) *c2*) ;; '外贸 出口'
  ((import-export transaction) *c2*) ;; '外贸 出口'

  ((v-tong3-ce4 information-word-zi1-liao4) *c0*) ;; '统计 资料'

  ;; (对 N1) N2
  (((m dui4-noun) (n t))
   (mod-noun-dui4-noun n (dui4-noun-noun m)))
  ;;

  ((place-boundary-ind non-verb-noun) *c-common*) ;; prefer '(经济 界) 人士' to '经济  (界 人士)'
  ((place-boundary-ind animate) *c-common*) ;; prefer '(经济 界) 人士' to '经济  (界 人士)'
  ((place-boundary-ind human-suffix) *c-common*) ;; prefer '(经济 界) 人士' to '经济  (界 人士)'
  ((domain-circles-ind non-verb-noun) *c-common*) ;; prefer '(经济 界) 人士' to '经济  (界 人士)'
  ((domain-circles-ind animate) *c-common*) ;; prefer '(经济 界) 人士' to '经济  (界 人士)'
  ((domain-circles-ind human-suffix) *c-common*) ;; prefer '(经济 界) 人士' to '经济  (界 人士)'

  ((v-hua4-b non-verb-noun) *c-rare*) ;; prefer "(工业 化) 状态" to "工业 (化 状态)"

  ((sun-word moon-word) *c1*) ;; 日 月
  ((moon-word sun-word) *c1*) ;; 月 日

  ((ordinal-A-Z ordinal-A-Z)
   ;; prefer two A-Z as name
   (if (and (= *L1* 1) (= *L2* 1))
       *c-fallback*
       *c-common*))

  ((human-ind theory-word-dao4) *c3*) ;; e.g. 人道

  ((adj doctrine-ind) *c1*)
  ((adj-word-ren2-dao4 doctrine-ind) *c0*)

  ((adj-color country) *c-common*) ;; 金国
  ((gold-or-cost-ind country) *c4*) ;; 金国

  ((v-yu4-fu4 cost-or-budget-ind) *c0*) ;; 预付费

  ((verbs s-lun4) *c1*) ;; e.g. '对不起 论', '杀富济贫 论'

  ((pollution-gas-PM time-name) *c-common*) ;; PM2.5 and PM10 may be confused with time

  ((verbs time-word-stage) *c1*) ;; e.g. '教育 阶段', '建设 阶段'

  ((doctrine s-fen1-zi3) *c1*) ;; e.g. 分离主义 分子

  ((verbs opportunity) *c2*) ;; e.g. 开战 时机

  ((verbs wave) *c3*) ;; e.g. 剧变 浪潮

  ((verbs name-as-number-or-sign-ind) *c4*) ;; e.g. 保钓 号
  ((verb-n news) *c4*) ;; e.g. XX 丑闻 

  ((verbs STYLE-OR-PROCESS) *c4*) ;; 枪决 方式 

  ((verbs EVENT-IND) *c4*) ;; 咬伤 事件 

  ((verbs MACHINE) *c4*) ;; 传动 装置 

  ((verbs STANDARD-OF) *c4*) ;; 汇改 原则 

  ((verbs SYSTEM-POLICY-MECHANISM-MADE) *c4*) ;; 按需分配 制度 

  ((verbs PLAN) *c4*) ;; 播出 计划 
  ((verbs STYLE-TREND-IND) *c4*) ;; 崇洋媚外 风 
  ((verbs WORD-METHOD) *c4*) ;; 救人 办法 
  ((verbs POWER) *c4*) ;; 值守 力量 
  ((verbs REASON) *c4*) ;; 变动 因素 
  ((verbs LEGAL-CASE-OR-PROPOSAL-IND) *c4*) ;; 性骚扰 案 
  ((verbs LEGAL-CASE) *c4*) ;; 性侵害 案件 
  ((verbs RESPONSIBILITY) *c4*) ;; 判案 职务 
  ((verbs SERIES) *c4*) ;; 认养 行列 
  ((verbs OBSTACLE) *c4*) ;; 进入 障碍 
  ((verbs RESULTING-EFFECT) *c4*) ;; 受精 效果 
  ((verbs parts-ind) *c4*) ;; 冲压 件
  ((verbs machine-ind) *c2*) ;; 控制 器
  ((verbs canon-ind) *c2*) ;; 迫击 炮
  ((organization-ind s-zhang3) *c0*) ;;
  ((organization s-zhang3) *c3*) ;; want the two separate in '专卖局 局长'
  ((word-organization-ju2 s-zhang3) *c0*) ;; e.g. 局长
  ((adj-word-zhuan1 human-suffix) *c-rare*) ;; not prefer 专长
  ((unit s-zhang3) *c-fallback*) ;; e.g. prefer '成长' as one word

  ((v-xie4-d liquid-like-agent) *c-common*) ;; want '解毒 冲剂', not '解 毒冲剂'
  ((explanation-ind liquid-like-agent) *c-common*) ;; want '解毒 冲剂', not '解 毒冲剂'
  ((v-cui1-lei4 bullet-like) *c1*) ;; 催泪弹
  ((human book-like-record) *c1*) ;; 输卒 手记
  ((verbs PAST-EXPERIENCE) *c4*) ;; 豪赌 经验 
  ((verbs ABSTRACT-KEY) *c4*) ;; 开店 关键 
  ((verbs ABILITY) *c3*) ;; 吞吐 能力 
  ((army-landscape army-like) *c2*)
  ((army-landscape battle) *c2*)
  ((v-shou4-hai4 human-ind) *c1*) ;; 受害人
  ((verbs word-method) *c4*) ;; 征税 做法 
  ((verbs perspective) *c4*) ;; 扶养 角度 
  ((verbs project-or-item) *c4*) ;; 勘查 项目 
  ((verbs suspicion-for-guilty) *c3*) ;; 诈欺 罪嫌 
  ((verbs abstract-cost-to-pay) *c4*) ;; 嬉闹 代价 
  ((verbs NAME-OR-REPUTATION) *c4*) ;; 诈欺 罪名 
  ((verbs stress) *c4*) ;; 救亡图存 压力 
  ((verbs space-of) *c3*) ;; 施展 空间 
  ((verbs noodle-word-mian4-ind) *c-common*)
  ((verbs face-word-mian4-ind) *c-common*)
  ((verbs word-act) *c3*) ;; 握手 动作 
  ((verbs door-like-or-abstract-threshold) *c4*) ;; 工业化 门槛 
  ((verbs prize-ranks) *c4*) ;; 小回转 冠军 
  ((verbs technology) *c3*) ;; 转化 科技 
  ((v-zhuan4-b money-related-can-be-fang4) *c4*) ;; want 转贷 as verb or verb-n
  ((verbs targeted-subject) *c3*) ;; 治平 对象
  ((verbs design-of) *c3*) ;; 防洪 设计 
  ((v-zhi2-a governance-power) *c-common*) ;; want '执政 权', not '执 政权'
  ((verbs PROCESS) *c3*) ;; 防洪 工程 
  ((verbs V-GUAN3-LI4) *c3*) ;; 招投标 管理 

  ((race-name verb) *c-rare*) ;; want '唐纳' as name. Some race-name are also surnames.
  ((confusing-race-name verb)
   ;; want '唐纳' as name.
   (+ *c-very-rare* *c-close-suffix*))

  ((v-yan2 non-verb-noun) *c-common*) ;; prefer verb-x, e.g. 沿海
  ((v-suggest non-verb-noun) *c-common*) ;; prefer verb-x 提出
  ((district suo3-ind) *c-rare*) ;; not want '(XX 地区 所) (辖 行政 区域)'
  ((suo3-ind administration) *c-rare*) ;; not want '(XX 地区 所) (辖 行政 区域)'
  ((v-xia2 administration) *c-rare*) ;; not want '(XX 地区 所) (辖 行政 区域)'

  ((v-jin4-xing2 verbs) *c-common*) ;; prefer verb-n or verb-c for '进行'
  ((v-jin4-xing2 (n abstract))
   ;; e.g. want verb-n form of '进行 辩论'
   (+ *c2* (if (also-verb-p n) *c4* 0)))
  ((v-jin4-xing2 arranged-event) *c-rare*) ;; e.g. want verb-n form of '进行 辩论'

  (((m v-xiang1-guan1) (n noun))
   ;; '相关' could be used as adj
   (adj-noun m n))

  ((verbs road-ind) *c3*) ;; many roads have a verb as name!!??
  ((verbs street-place-ind) *c3*) ;; many roads have a verb as name!!??
  (((m place) road-ind)
   ;; prefer '省会 (中山 路)' to '(省会 中山) 路'
   (+ *c2*
      (if (> (trait-value m 'n-base-nouns 1) 1)
          *c-long-sep*
          0)
      (prefer m 'name *c-close-suffix*)))
  (((m place) street-place-ind)
   (+ *c2*
      (if (> (trait-value m 'n-base-nouns 1) 1)
          *c-long-sep*
          0)
      (prefer m 'name *c-close-suffix*)))

  ((transaction word-xin4-yong4) *c1*) ;; '出口 信用 保险 制度'
  ((import-export word-xin4-yong4) *c1*) ;; '出口 信用 保险 制度'

  ((verbs consciousness) *c2*) ;; 保护 意识

  ((word-nan2-de t) *c-very-rare*) ;; prefer '男 的' as noun + zhi
  ((word-nu3-de t) *c-very-rare*) ;; prefer '女 的' as noun + zhi
  ((v-wei4-yu2-b t) *c-rare*) ;; prefer '位于 XX' as verb-noun

  ((v-gao4-su4 animate) *c-rare*) ;; prefer verb-noun

  ;; e.g. 管治下, 確認下.
  ;; Note that the verb has to be turned into noun with a penalty, so this penalty need not be too severe.
  ;; Also, some verbs may not be applicable, add exceptions as needed.
  ((verbs dir-word-down) *c-close-suffix*)
  ((v-at dir-word-down) *c-common*) ;; prefer '在' as place ind for this case, e.g.  '在 XXX 管治下'
  ((verb-action dir-word-down) *c-close-suffix*)

  ((order dir-word-down) *c1*) ;; '命令 之 下', '命令 下'

  ;; '賽事 中', '賽事 之 中'
  ((event dir-middle) *c1*)

  ;; '過程 中', '過程 之 中'
  ((process dir-middle) *c1*)

  ;; '問題 上', '議題 上'
  ((posed-question dir-word-up) *c1*)
  ((topic dir-word-up) *c1*)

  ;; '事情 (之) 上', '事件 (之) 上'
  ((abstract-matter dir-word-up) *c1*)
  ((event-ind dir-word-up) *c1*)
  ((event dir-word-up) *c2*)
  ((strategy dir-word-up) *c2*) ;; '政策 上'

  ((strategy dir-word-down) *c1*) ;; '政策 (之) 下', '戰略 (之) 下'

  ((v-chu1-shou5 machine-ind) *c-common*) ;; prefer '出 手机' to '出手 机'
  ((v-chu1 word-mobile-phone) *c-common*) ;; prefer '出 手机' as verb-n

  ((v-is time-period)
   ;; in ancient chinese, '是' could mean 'this', so '是晚' , '是日' could mean tonight and today, respectively.
   ;; both this usage is much rarer in modern chinese now, and could confuse with other word segmentation, e.g. prefer '是 (晚上) (來 (商场 附近) 玩)' to '(是 晚) (上來 (商场 附近) 玩)'
   (if (= 1 *L1* *L2*)
       *c-common*
       *c-rare*))

  ((v-xiao1-zhang1 animate-attribute) *c3*) ;; '嚣张 气焰'
  ((v-xiao1-zhang1 attitude) *c2*) ;; '嚣张 態度'

  ((v-bu2-ru2 t) *c-rare*) ;; prefer '不如' as verb in verb-noun, or adv
  ((adj-more-less number) *c-common*) ;; not want '少一' as one noun
  ((number abstract-matter-ind) *c2*) ;; prefer '一事' as noun

  ((adj-word-fang1 adj) *c-rare*) ;; '方' should be used as adj, and normally should not be followed by adj as noun

  ((p-pronoun (n time-name))
   ;; want '(其 被带到派出所) 时', not '其 (被带到派出所 时)'
   (+ *c-very-rare* (not-prefer-trait n 'time-with-ind))
   )

  ((v-jie2-he2 animate) *c-rare*) ;; prefer verb-n for '结合'

  ((country-abbr-america word-girl)
   ;; prefer '美' as adj in '美女'
   (if (= *L2* 1)
       *c4*
       *c2*))
  (((m adj-word-mei3) (n word-girl))
   (if (= *L2* 1)
       *c1*
       (+ *c1* (adj-noun m n))))
  
  ((influence abstract) *c-common*) ;; prefer verb-noun
  ((influence animate) *c-rare*) ;; prefer verb-noun '影响 民众'

  ((v-ping2-jun1 noun) *c-common*) ;; prefer adj or verb-n for 平均

  ((word-research word-ren2-yuan2) *c0*) ;; 研究人员

  ((place population-count) *c1*) ;; 北部 人口
  ((dir-suffix-place population-count) *c1*) ;; 北部 人口

  ((adj-word-lian2-xu4 time-period) *c-close-sep*) ;; 连续4天

  ((v-xian4-ru4 noun) *c-rare*) ;; prefer verb-n '陷入'

  ((style-trend-ind strength-or-ability-ind) *c-common*) ;; prefer '风力' as wind power

  ((place pollution-gas) *c1*) ;; 北京 雾霾

  ((v-yu2-d time-name) *c-rare*) ;; prefer '于' as time-ind for time

  ((word-pollution position-of) *c1*) ;; 污染 水平

  ((weather-convergence-divergence strip-or-region-like) *c1*) ;; 辐合带
  ((weather-convergence-divergence region) *c1*) ;; 辐合带

  (((m pressure) region)
   (if (trait-value m 'has-adj-noun-mod)
       ;; 低压 辐合带
       *c1*
       *c3*))
  (((m pressure) strip-or-region-like)
   (if (trait-value m 'has-adj-noun-mod)
       ;; 低压 辐合带
       *c1*
       *c3*))

  ((place situation) *c2*) ;; 地区 环境

  ((situation v-jian4-ce4) *c0*) ;; 环境 监测

  ((info-warning ad-hoc-time-period-word-during) *c1*) ;; 橙色预警 期间

  ((adv-word-ri4 time-name) *c4*) ;; not prefer '工作 (日 高峰时段)' with '日' being adv

  ((climax time-period-num-opt-ge-ind) *c1*) ;; 高峰 时段

  ((v-jin4-zhi3 organization) *c-rare*) ;; prefer verb-n '禁止'

  ((health protection) *c1*) ;; 健康 防护
  ((adj-word-healthy protection) *c4*) ;; prefer '健康' as noun in '健康 防护'

  ((shop-like time-name) *c4*) ;; prefer separated '研究所 (周一（11日）)' to '(研究所 周一)（11日）'

  ;; e.g. '大陆 十余 省'
  ((number (n place))
   (+ *c1*
      (prefer n 'ind *c-close-suffix*)))
  ((num (n place))
   (+ *c1*
      (prefer n 'ind *c-close-suffix*)))

  ((place num) *c4*) ;; prefer '大陆 (十余 省)'
  ((place number) *c4*) ;; prefer '大陆 (十余 省)'

  ;; some '盆地' in China
  ((province-si4-chuan1 land-pen2-di4) *c0*)
  ((basin-name land-pen2-di4) *c0*)

  ((weather organization-or-platform-ind) *c1*) ;; '气象 台'

  ((rough-amount (n place))
   ;; '部分 地区', but not prefer '部分 中國'
   (+ *c2* (not-prefer n 'nr-name *c-close-suffix*)))
  ((rough-amount visibility) *c2*) ;; prefer '(部分 地区) 能见度' to '部分 (地区 能见度)'

  ((body-part mask-like-ind)
   ;; '口罩' and some common ones are already special cases
   ;; so this is for unanticipated cases
   *c2*)

  ((v-ying4-ji2 emergency-response) *c0*) ;; '应急 响应'

  ((v-fa5-bu4 adj) *c-rare*)
  ((v-fa5-bu4 (n abstract))
   ;; prefer verb-n for '发布'
   (+ *c2* (prefer n 'ind *c-close-suffix*)))

  ((city-name market-or-city-ind) *c1*) ;; '重庆 市', want '市' as city in this case

  ((verbs number-of) *c1*) ;; '死亡 人数'

  ((v-jin3-ci4-yu2 place) *c-common*) ;; prefer verb-noun

  ((num reason) *c2*) ;; '这一因素'
  ((number reason) *c2*) ;; '这一因素

  ((health influence) *c1*) ;; '健康影响'

  ((health word-research-lab) *c2*) ;; '健康 研究所'
  (((m influence) word-research-lab)
   ;; '健康影响 研究所'
   (if (or (trait-value m 'has-adj-noun-mod)
           (> (trait-value m 'b-base-nouns 1) 1))
       *c1*
       *c4*))
  ((country word-research-lab) *c2*) ;; '美国 研究所'

  ((place health) *c-common*) ;; prefer '美国 (健康影响 研究所)' to '(美国 健康) 影响 (研究所)'

  ((word-pollution source) *c2*) ;; 污染 来源

  ((word-pollution v-zhi4-li4-c) *c2*) ;; 'XX 治理'
  ((weather v-zhi4-li4-c) *c2*) ;; 'XX 治理'
  ((natural-phenomena v-zhi4-li4-c) *c2*) ;; 'XX 治理'
  ((gas v-zhi4-li4-c) *c2*) ;; '空气 治理'
  
  ((occupation human-name)
   (+ *c-close-sep* *c-close-suffix*))
  ((can-be-title human-name)
   (+ *c-close-sep* *c-close-suffix*))

  ((verbs human-name) *c-very-rare*) ;; '供暖季 却 ...' mistaken as '(供暖) (季却) ...'

  ((adv-time-at time-name) *c-common*) ;; prefer time-at-to

  ((city city-name) *c2*) ;; e.g. '首都 北京'

  ((v-jian4-dao4-b noun) *c-rare*) ;; prefer verb-noun

  ((v-kong4-zhi4-b word-pollution) *c4*) ;; prefer verb-noun
  ((v-kong4-zhi4-b pollution-gas) *c4*) ;; prefer verb-noun
  ((v-kong4-zhi4-b steps-word-cuo4-shi1) *c1*) ;; '控制 措施'
  (((m verb-n) steps-word-cuo4-shi1)
   ;; '(控制 污染) 措施'
   (if (and (or (typep (verb-n-verb m) 'v-kong4-zhi4-b)
                (typep (verb-n-verb m) 'v-kong4))
            (typep (verb-n-obj m) 'abstract))
       *c0*
       *c4*))

  ((economy plan) *c2*) ;; '经济 规划'

  ((inanimate dependency-of) *c2*) ;; '煤炭 依赖'
  ((medicine dependency-of) *c1*)
  ((fuel dependency-of) *c1*) ;; '煤炭 依赖'
  ((oil dependency-of) *c1*)
  ((drink dependency-of) *c1*)
  ((body-part dependency-of) *c4*)

  ((situation ns-chan3) *c-common*) ;; '环境 产' ?
  (((m situation) s-sheng1)
   (if (trait-value m 'with-noun-suffix)
       ;; not prefer '(环境 产) 生'
       *c-common*
       *c4*))

  ((v-xuan2-fu2 pollutant-particles) *c1*) ;; 细悬浮微粒

  ((gas concentration-of) *c1*) ;; '细悬浮微粒 浓度'

  ((adv-yu4-exceed (n time-date))
   ;; e.g. '逾1/5' is more likely number
   (+ *c2* (not-prefer-trait n 'confusing-time-date *c-common*)))

  ((pollution-gas time-period-num-ind-day) *c1*) ;; '雾霾 天'

  ((fuel v-gong1-nuan3) *c1*) ;; '燃煤 供暖'

  ((word-representative arranged-event) *c2*) ;; e.g. '全国 代表 大会'

  ((time-period-num-opt-ge-ind-year amount-of) *c2*) ;; '年度 (煤炭 消耗量)'

  ((country-name amount-of) *c2*) ;; '中国 (年度 煤炭 消耗量)'
  ((country-name time-period-num-opt-ge-ind-year)
   ;; not want '(中国 年度) (煤炭 消耗量)'
   *c-rare*)

  ((v-chuang4 book-like-record) *c-common*) ;; prefer verb-noun for '创 纪录'
  ((v-po4 book-like-record) *c-common*) ;; prefer verb-noun for '创 纪录'

  ((industry word-huo2-dong4) *c1*) ;; '重工业 活动'

  ((material production) *c1*) ;; '钢铁和水泥 生产'

  ((v-huan3-xie4 (n adj))
   ;; not want '缓解 或许只是暂时' as mod-noun
   (if (trait-value n 'adv-with-shi4)
       *c-rare*
       *c4*))

  ((v-chu1 number) *c-very-rare*) ;; prefer verb-noun

  ((v-at verbs) *c-rare*) ;; not want mod-noun '在 (冬季通常燃煤供暖)', but verb-p '(在 冬季) (通常 燃煤 供暖)'
  ((v-at adj) *c-rare*) ;; not want mod-noun '在 (冬季通常燃煤供暖)', but verb-p '(在 冬季) (通常 燃煤 供暖)'

  ((number army-like-word-tian1-jiang1) *c-common*) ;; e.g. '4天将' is often '(..4天) (将 ..)'
  ((num army-like-word-tian1-jiang1) *c-common*) ;; e.g. '4天将' is often '(..4天) (将 ..)'

  ((situation station) *c2*) ;; e.g. '环境 监测总站'
  ((situation organization) *c2*) ;; e.g. '环境 保护局'
  ((situation protection) *c2*) ;; e.g. '环境 保护'

  ((v-fa5-bu4 word-pollution) *c-common*) ;; prefer verb-noun
  ((v-fa5-bu4 (n noun))
   ;; ok for e.g. '发布權', but not want '发布 重污染天气'
   (if (or (= *L2* 1) (typep n 'ind))
       *c2*
       *c-common*))

  ((steps-word-cuo4-shi1 place) *c-rare*) ;; '(措施) (结束后，3月的北京)' ??
  ((steps-word-cuo4-shi1 inanimate) *c-common*) ;;

  ((v-dang1 noun) *c-common*) ;; want '当' as time-ind in '当 扩散条件 ...'

  ((v-shi5-shi1 work-ind) *c-common*) ;; not want '(实施 工) 作...'

  (((m num) word-growth)
   (if (trait-value m 'num-is-percentage)
       ;; e.g. '逾11% 成长'
       *c1*
       *c-rare*))
  ;;;; semi-automatically extracted from CTB.
  ;;;; record those with verbs as mod here, because they are shadowed
  ;;;; if put in mod-noun-common. Later change to lower penalty.
  
  ;; 保护 意识
  ((v-bao3-hu4 consciousness) *c2*)
  ((protection consciousness) *c1*) ;; prefer the noun form of '保护'
  
  ;; 就业 机会
  ((v-jiu4-ye4 opportunity) *c1*)
  
  ;; (单点 系泊) (原油 装卸 系统)
  ((v-xi5-po1 system) *c2*)

  ;; 招投标 管理 
  ((V-ZHAO1-BIAO1 V-GUAN3-LI4) *c2*)

  ;; 改扩建 工程 
  ((V-KUO4-JIAN4 PROCESS) *c2*)

  ;; 防洪 工程 
  ((V-FANG2-HONG2 PROCESS) *c2*)

  ;; 防洪 设计 
  ((V-FANG2-HONG2 design-of) *c2*)

  ;; 改建 工程 
  ((V-GAI3-JIAN4 PROCESS) *c3*)

  ;; 治平 专案 
  ((v-zhi4-ping2 project-or-item) *c2*)

  ;; 国家 日本 
  ;; 国家 文莱 
  ;; 国家 科威特 
  ((WORD-COUNTRY COUNTRY-NAME) *c1*);

  ((v-shi1-gong1 time-period-num-opt-ge-ind) *c2*) ;; 施工期

  ;; 后加工 项目 
  ((V-JIA1-GONG1 project-or-item) *c3*)

  ;; 转化 科技 
  ((V-HUA4-B technology) *c3*)

  ;; 分拨 中心 
  ((v-fen1-bo1 PLACE) *c3*)

  ;; 保护 工程 
  ((V-BAO3-HU4 process) *c3*)

  ;; 照顾 人员 
  ((V-ZHAO4-GU4 OCCUPATION) *c3*)

  ;; 误导 成分 
  ((V-WU4-DAO3 composition-of) *c2*)

  ;; 顶推 轮驳 
  ((V-DING3-tui1 ship) *c3*)

  ;; 配售 部分 
  ((v-shou4-a ROUGH-AMOUNT) *c3*)

  ;; 小回转 冠军 
  ((V-HUI2-ZHUAN4 prize-ranks) *c3*)

  ;; 工业化 门槛 
  ((HUA-VERB door-like-or-abstract-threshold) *c3*)

  ;; 握手 动作 
  ((V-WO4-SHOU5 word-act) *c2*)

  ;; 反对 势力 
  ((V-FAN3-DUI4 a-power) *c3*)

  ;; 诈欺 超贷 
  ((V-ZHA4 v-chao1-dai4) *c3*)
  ((v-qi1-zha4 v-chao1-dai4) *c3*)

  ;; 反对 立场 
  ((V-FAN3-DUI4 stance) *c3*)

  ;; 治水 防洪 
  ((V-ZHI4-SHUI4 v-fang2-hong2) *c3*)

  ;; 再生产 角度 
  ((V-ZAI4-SHENG1-CHAN3 perspective) *c3*)

  ;; 施展 空间 
  ((V-SHI1-ZHAN3 space-of) *c2*)

  ;; 救亡图存 压力 
  ((V-JIU4-WANG2-TU2-CUN2 stress) *c3*)

  ;; DIY 现场 
  ((V-DIY place-word-xian4-chang3) *c3*)

  ;; 诈欺 罪名 
  ((V-ZHA4 NAME-OR-REPUTATION) *c3*)
  ((v-qi1-zha4 NAME-OR-REPUTATION) *c3*)

  ;; 复传 事业 
  ((V-ZHUAN4-C JOB) *c3*)

  ;; 烧焦 味道 
  ((V-SHAO1-JIAO1 smell-or-flavor) *c3*)

  ;; 嬉闹 代价 
  ((V-XI1-NAO4-a abstract-cost-to-pay) *c3*)

  ;; 带审 转播 
  ((V-DAI4-F V-ZHUAN4-BO1-A) *c3*)

  ;; 拉法夜立案 部分 
  ((V-LI4-AN4 ROUGH-AMOUNT) *c3*)

  ;; 基本化 精神 
  ((V-HUA4-B spirit-or-mind) *c3*)
  
  ;; 勘查 项目 
  ((V-KAN1-ZHA1 project-or-item) *c3*)

  ;; 包装用 菜板儿 
  ((V-BAO5-ZHUANG1 PLATE-LIKE) *c3*)

  ;; 夸大 成份 
  ((V-KUA1-DAI4 composition-of) *c3*)

  ;; 输供电 系统 
  ((v-gong1-dian4 SYSTEM) *c3*)

  ;; 性虐待 受害人 
  ((V-XING4-NUE4-DAI4 HUMAN) *c3*)

  ;; 现代化 玻壳 
  ((HUA-VERB SHELL-OR-CASING) *c3*)

  ;; 一体化 合作 
  ((HUA-VERB V-HE2-ZUO4) *c3*)

  ;; 优化 重组 
  ((HUA-VERB V-CHONG2-ZU3) *c3*)

  ;; 送递 号码 
  ((V-GIVE-TO ID-NUMBER) *c3*)

  ;; 优化 升级 
  ((HUA-VERB V-SHENG1-JI2) *c3*)

  ;; 产业化 开发 
  ((HUA-VERB V-KAI1-FA5) *c3*)

  ;; 公司化 改组 
  ((HUA-VERB V-GAI3-ZU3) *c3*)

  ;; 催化 重整 
  ((HUA-VERB V-CHONG2-ZHENG3) *c3*)

  ;; 货币化 分配 
  ((HUA-VERB V-FEN4-PEI4) *c3*)

  ;; 实施 步骤 
  ((V-SHI5-SHI1 STEPS) *c3*)

  ;; 微循环 血液 
  ((V-XUN2-HUAN2 LIQUID) *c3*)

  ;; 适应 能力 
  ((V-SHI4-YING4 ABILITY) *c3*)

  ;; 现代化 制药 
  ((HUA-VERB V-ZHI4-YAO4) *c3*)

  ;; 响应 能力 
  ((V-XIANG3-YING4 ABILITY) *c3*)

  ;; 结售汇 业务 
  ((V-JIE2-SHOU4-HUI4 BUSINESS) *c3*)

  ;; 保护 总局 
  ((V-BAO3-HU4 ORGANIZATION) *c3*)

  ;; 营林 投入 
  ((V-YING2-LIN2 V-TOU2-RU4) *c3*)

  ;; 保护 权利 
  ((V-BAO3-HU4 RIGHTS-IND) *c3*)

  ;; 首善之都 执政权 
  ((CITY-WORD-DU1 RIGHTS) *c3*)

  ;; 生鲜 区 
  ((ADJ-WORD-XIAN1 DISTRICT-IND) *c3*);

  ;; 开店 关键 
  ((V-KAI1-DIAN4 ABSTRACT-KEY) *c3*)

  ;; 遮荫 树 
  ((V-ZHE1-YIN1 TREE-IND) *c3*)

  ;; 扶助 基金会 
  ((V-FU2-ZHU4 ORGANIZATION) *c3*)

  ;; 连载 一开始 
  ((V-LIAN2-ZAI4 WORD-TIME-KAI1-SHI3) *c3*)

  ;; 白热化 阶段 
  ((HUA-VERB TIME-WORD-STAGE) *c3*)

  ;; 催泪 瓦斯 
  ((V-CUI1-LEI4 FUEL-GAS) *c3*)

  ;; 到访 时期 
  ((V-DAO4-FANG3 TIME-PERIOD-NUM-OPT-GE-IND-SHI2-QI2) *c3*)

  ;; 各种各样 骗税 
  ((V-GE4-ZHONG4-GE4-YANG5 V-PIAN4-SHUI4) *c3*)

  ;; 免疫 证明书 
  ((V-MIAN3-YI4 BOOK-LEGAL-DOC-CHARACTER-STYLE) *c3*)

  ;; 免疫 证书 
  ;; 免疫 证明 
  ((V-MIAN3-YI4 LEGAL-DOC-IND) *c3*)

  ;; 塌方 现场 
  ((V-TA1-FANG1 PLACE) *c3*)

  ;; 豪赌 经验 
  ((V-HAO2-DU3 PAST-EXPERIENCE) *c3*)

  ;; 免疫 系统 
  ((V-MIAN3-YI4 SYSTEM) *c3*)

  ;; 保护 能力 
  ((V-BAO3-HU4 ABILITY) *c3*)

  ;; 满目疮痍 场面 
  ((V-MAN3-MU4-CHUANG1-YI2 SITUATION) *c3*)

  ;; 行善 名义 
  ((V-XING2-SHAN4 IN-THE-NAME-OF) *c3*)

  ;; 宽容 社会 
  ((V-KUAN1-RONG2 SOCIETY) *c3*)

  ;; 保护 税额 
  ((V-BAO3-HU4 AMOUNT-OF) *c3*)

  ;; 长途跋涉 部分 
  ((V-BA2-SHE4 ROUGH-AMOUNT) *c3*)

  ;; 半殖民 国家 
  ((V-ZHI2-MIN2 WORD-COUNTRY) *c3*)

  ;; 排卵 诱发剂 
  ;; 解毒 冲剂 
  ((VERB-N LIQUID-LIKE-AGENT) *c2*)
  
  ;; 保护 管理 
  ((V-BAO3-HU4 V-GUAN3-LI4) *c3*)

  ;; 蒸馏 酒 
  ((V-ZHENG1-LIU4 DRINK-IND) *c3*)

  ;; 扩改建 技术 
  ((V-KUO4-DAI4 SKILL) *c3*)

  ;; 核算 企业 
  ((V-HE2-SUAN4-B ORGANIZATION) *c3*)

  ;; 保护 药 
  ((V-BAO3-HU4 MEDICINE-ORAL-IND) *c3*)

  ;; 发售 期间 
  ((V-FA5-SHOU4 AD-HOC-TIME-PERIOD-WORD-DURING) *c3*)

  ;; 联运 方面 
  ((V-LIAN2-YUN4 ASPECT-IND) *c3*)

  ;; 到站 位置 
  ((V-DAO4-ZHAN4 POSITION-OF) *c3*)

  ;; 失衡 因素 
  ((V-SHI1-HENG2 REASON) *c3*)

  ;; 实事求是 原则 
  ((V-SHI2-SHI4-QIU2-SHI4 STANDARD-OF) *c3*)

  ;; 聘用 手续 
  ((V-PIN4-YONG4 PROCESS) *c3*)

  ;; 保真 效果 
  ((V-BAO3-ZHEN1 RESULTING-EFFECT) *c3*)

  ;; 储运 公司 
  ((V-CHU2-YUN4 WORD-COMPANY) *c3*)

  ;; 改建 工作 
  ((V-GAI3-JIAN4 JOB-WORD-GONG1-ZUO4) *c3*)

  ;; 现代化 机场 
  ((HUA-VERB BUILDING) *c3*)

  ;; 深加工 基地 
  ((V-JIA1-GONG1 PLACE) *c3*)

  ;; 适应 课程 
  ((V-SHI4-YING4 LESSON) *c3*)

  ;; 失聪 条例 
  ((V-SHI1-CONG1-A LEGAL-DOCUMENT) *c3*)

  ;; 存取 记忆体 
  ((V-CUN2-QU3 LITTLE-ELECTRONICS) *c3*)

  ;; 多元化 经营 
  ;; 产业化 经营 
  ;; 集约化 经营 
  ((HUA-VERB V-JING1-YING2) *c3*)

  ;; 总装机 容量 
  ((V-ZHUANG1-JI1 VOLUME-OF) *c3*)

  ;; 脱困 责任 
  ((V-TUO1-KUN4 RESPONSIBILITY) *c3*)

  ;; 对阵 结果 
  ((V-DUI4-ZHEN4 RESULTING-EFFECT) *c3*)

  ;; 酗酒 现象 
  ((V-XU4-JIU3 APPEARANCE) *c3*)

  ;; 修纂 体系 
  ((V-XIU1-ZUAN3 ABSTRACT-TYPE-OF) *c3*)

  ;; 修纂 工作 
  ((V-XIU1-ZUAN3 JOB-WORD-GONG1-ZUO4) *c3*)

  ;; 欢迎 酒会 
  ((V-HUAN1-YING2 ORGANIZATION-OR-EVENT) *c3*)

  ;; 受精 效果 
  ((V-SHOU4-JING1 RESULTING-EFFECT) *c3*)

  ;; 深加工 农副产品 
  ((V-JIA1-GONG1 GOODS-PRODUCT) *c3*)

  ;; 现代化 城市 
  ((HUA-VERB BUILDING-TYPE-IND) *c3*)

  ;; 实施 细则 
  ((V-SHI5-SHI1 STANDARD-OF) *c3*)

  ;; 回转 比赛 
  ;; 小回转 比赛 
  ((V-HUI2-ZHUAN4 COMPETITION) *c3*)

  ;; 不织布 领域 
  ((V-ZHI1-BU4 REGION) *c3*)

  ;; 陆海空 作战 
  ((PHYSICAL-SKY V-ZUO4-ZHAN4-a) *c3*)

  ;; 轮替 意义 
  ((V-LUN2-TI4 MEANING) *c3*)

  ;; 欣赏 层次 
  ((V-XIN1-SHANG3 POSITION-OF) *c3*)

  ;; 直选 依据 
  ((V-ZHI2-XUAN3 EVIDENCE) *c3*)

  ;; 代谢 研究 
  ((V-XIN1-CHEN2-DAI4-XIE4 WORD-RESEARCH) *c3*)

  ;; 反对 党 
  ((V-FAN3-DUI4 POLITICAL-PARTY-IND) *c3*)

  ;; 营造 计画 
  ((V-YING2-ZAO4 PLAN) *c3*)

  ;; 征选 过程 
  ((V-ZHENG1-XUAN3-A PROCESS) *c3*)

  ;; 保护 风气 
  ((V-BAO3-HU4 STYLE-TREND) *c3*)

  ;; 剧变 序曲 
  ((V-JU4-BIAN4-A MUSIC) *c3*)

  ;; 反对 声 
  ((V-FAN3-DUI4 SOUND-IND) *c3*)

  ;; 进入 障碍 
  ((V-JIN4-RU4-B OBSTACLE) *c3*)

  ;; 面谈 方式 
  ((V-CAO3-TAN2 STYLE-OR-PROCESS) *c3*)

  ;; 认养 行列 
  ((V-REN4-YANG3 SERIES) *c3*)

  ;; 发掘 报告 
  ((V-FA5-JUE2 INFO-REPORT) *c3*)

  ;; 备询 程序 
  ((V-BEI4-XUN2-A PROCESS) *c3*)

  ;; 求变 实验 
  ((V-QIU2-BIAN4 EXPERIMENT) *c3*)

  ;; 猎捕 调查 
  ((V-LIE4-BU3 V-TIAO2-ZHA1) *c3*)

  ;; 后制作 水准 
  ((V-HOU4-ZHI4-ZUO4 STANDARD-OF) *c3*)

  ;; 推展 影片 
  ((V-TUI1-ZHAN3 VIDEO) *c3*)

  ;; 扩散 点 
  ((V-KUO4-SAN3 POINT-IND) *c3*)

  ;; 融合 美 
  ((V-RONG2-HE2 WORD-BEAUTY) *c3*)

  ;; 大一统 帝国 
  ((V-DA4-YI1-TONG3 COUNTRY-IND) *c3*)

  ;; 观赛 礼仪 
  ((V-GUAN1-SAI4 PROCESS) *c3*)

  ;; 疏迁 过程 
  ((V-SHU1 PROCESS) *c3*)

  ;; 疏迁 草图 
  ((V-SHU1 PLAN-PICTURE) *c3*)

  ;; 沈淀 池 
  ((V-SHEN3-DIAN4 LARGE-CONTAINER) *c3*)

  ;; 支助 企业 
  ((V-ZHI1-ZHU4 ORGANIZATION) *c3*)

  ;; 沿近海 渔业 
  ((V-YAN2-JIN4-HAI3 INDUSTRY) *c3*)

  ;; 核准 程序 
  ((V-HE2-ZHUN3 PROCESS) *c3*)

  ;; 关机 状态 
  ((V-GUAN1-JI1 SITUATION) *c3*)

  ;; 鼓励 话 
  ((V-GU3-LI4 SPOKEN-LANGUAGE-IND) *c3*)

  ;; 关心 点 
  ((V-GUAN1-ZHU3 POINT-IND) *c3*)

  ;; 结盟 关系 
  ((V-JIE2-MENG2 RELATIONSHIP-OF) *c3*)

  ;; 反对 运动 
  ((V-FAN3-DUI4 ARRANGED-EVENT-OR-SPORTS) *c3*)

  ;; 反对 联盟 
  ((V-FAN3-DUI4 ORGANIZATION-IND) *c3*)

  ;; 实施 期限 
  ((V-SHI5-SHI1 LIMIT-OF) *c3*)

  ;; 得分 情形 
  ((V-DEI3-FEN4 SITUATION) *c3*)

  ;; 统合 方面 
  ((V-TONG3-HE2 ASPECT-IND) *c3*)

  ;; 判案 职务 
  ((V-PAN4-AN4 RESPONSIBILITY) *c3*)

  ;; 研究 产出 
  ((WORD-RESEARCH V-CHAN3-B) *c3*)

  ;; 保护 令状 
  ((V-BAO3-HU4 LEGAL-DOCUMENT) *c3*)

  ;; 认识 方面 
  ((V-REN4-SHI5 ASPECT-IND) *c3*)

  ;; 脱困 攻坚战 
  ((V-TUO1-KUN4 BATTLE) *c3*)

  ;; 工厂化 基地 
  ;; 石化 基地 
  ;; 现代化 中心 
  ((HUA-VERB PLACE) *c3*)

  ;; 保护 力度 
  ((V-BAO3-HU4 DEGREE-OF) *c3*)

  ;; 正常化 进行 
  ((HUA-VERB V-JIN4-XING2) *c3*)

  ;; 追查 过程 
  ((V-ZHUI1-ZHA1 PROCESS) *c3*)

  ;; 集结 方式 
  ((V-JI2-JIE2 STYLE-OR-PROCESS) *c3*)

  ;; 查封 工作 
  ((V-ZHA1-FENG1 JOB-WORD-GONG1-ZUO4) *c3*)

  ;; 保命 要件 
  ((V-BAO3-MING4 DOCUMENT) *c3*)

  ;; 围捕 过程 
  ((V-WEI2-BU3 PROCESS) *c3*)

  ;; 恶作剧 事件 
  ((V-WU4-ZUO4-JU4 EVENT-IND) *c3*)

  ;; 掏空 案 
  ((V-TAO1-KONG4 LEGAL-CASE-OR-PROPOSAL-IND) *c3*)

  ;; 性侵害 案件 
  ((V-XING4-QIN1-HAI4 LEGAL-CASE) *c3*)

  ;; 集结 行动 
  ((V-JI2-JIE2 PROCESS) *c3*)

  ;; 戒急用忍 政策 
  ((V-JIE4-JI2-YONG4-REN3 STRATEGY) *c3*)

  ;; 陪伴 声 
  ((V-PEI2-BAN4 SOUND-IND) *c3*)

  ;; 工作 访问 
  ((JOB-WORD-GONG1-ZUO4 V-FANG3-WEN4) *c3*)

  ;; 抗美援朝 战争 
  ((V-KANG4-MEI3-YUAN2-ZHAO1 BATTLE) *c3*)

  ;; 抗美援朝 周年 
  ((V-KANG4-MEI3-YUAN2-ZHAO1 TIME-PT-NUM-OPT-GE-YEAR) *c3*)

  ;; 降生 过程 
  ((V-JIANG4-SHENG1 PROCESS) *c3*)

  ;; 侵犯 问题 
  ((V-QIN1-FAN4 POSED-QUESTION) *c3*)

  ;; 欢迎 宴会 
  ((V-HUAN1-YING2 MEAL-LIKE-EVENT) *c3*)

  ;; 保护 计划 
  ((V-BAO3-HU4 PLAN) *c3*)

  ;; 议政 院 
  ((V-YI4-ZHENG4 SHOP-LIKE-IND) *c3*)

  ;; 实施 阶段 
  ((V-SHI5-SHI1 TIME-WORD-STAGE) *c3*)

  ;; 自动化 插件机 
  ;; 石化 机械 
  ;; 自动化 设备 
  ;; 石化 装置 
  ((HUA-VERB MACHINE) *c3*)

  ;; 自动化 研究院 
  ;; 自动化 研究所 
  ((HUA-VERB SHOP-LIKE) *c3*)

  ;; 跪射 俑 
  ((V-GUI4-A WORD-STATUE) *c3*)

  ;; 扫黄打非 办公室 
  ((V-SAO4-HUANG2 MOSTLY-INDOOR) *c3*)

  ;; 性骚扰 案 
  ((V-XING4-SAO1-RAO3 LEGAL-CASE-OR-PROPOSAL-IND) *c3*)

  ;; 炮击 训练 
  ((V-PAO4-JI1 TRAINING) *c3*)

  ;; 保护 制度 
  ((V-BAO3-HU4 SYSTEM-POLICY-MECHANISM-MADE) *c3*)

  ;; 取暖 油 
  ((V-QU3-NUAN3 OIL-IND) *c3*)

  ;; 现代化 浪潮 
  ;; 全球化 浪潮 
  ((HUA-VERB WAVE) *c3*)

  ;; 保护 部队 
  ((V-BAO3-HU4 TEAM) *c3*)

  ;; 逗留 期间 
  ((V-DOU4-LIU2 AD-HOC-TIME-PERIOD-WORD-DURING) *c3*)

  ;; 欢迎 仪式 
  ((V-HUAN1-YING2 PROCESS) *c3*)

  ;; 保护 盔甲 
  ((V-BAO3-HU4 ARMOR) *c3*)

  ;; 保护 工作 
  ((V-BAO3-HU4 JOB-WORD-GONG1-ZUO4) *c3*)

  ;; 关心 程度 
  ((V-GUAN1-ZHU3 DEGREE-OF-IND) *c3*)

  ;; 让步 迹象 
  ((V-RANG4-BU4 APPEARANCE) *c3*)

  ;; 保护 活动 
  ((V-BAO3-HU4 ARRANGED-EVENT) *c3*)

  ;; 力挺 回报 
  ((V-LI4-TING3 MONEY-RELATED-ATTRIBUTE) *c3*)

  ;; 变动 因素 
  ((V-BIAN4-DONG4 REASON) *c3*)

  ;; 起义 爆发 
  ((V-HUO3-YI4 V-BAO4-FA5-B) *c3*)

  ;; 变幻 考验 
  ((V-BIAN4-HUAN4-A V-KAO3-YAN4) *c3*)

  ;; 保护 领域 
  ((V-BAO3-HU4 REGION) *c3*)

  ;; 放缓 迹象 
  ((V-FANG4-HUAN3 APPEARANCE) *c3*)

  ;; 孤立 状态 
  ((V-GU1-LI4 SITUATION) *c3*)

  ;; 突发 预案 
  ((V-TU1-FA5 PROPOSAL-LIKE) *c3*)

  ;; 保护 行为 
  ((V-BAO3-HU4 BEHAVIOR) *c3*)

  ;; 关切 问题 
  ((V-GUAN1-QIE1 POSED-QUESTION) *c3*)

  ;; 开讲 节目 
  ((V-KAI1-JIANG3 DRAMA) *c3*)

  ;; 吞吐 总量 
  ((V-TUN1 AMOUNT-OF-IND) *c3*)

  ;; 吞吐 问题 
  ((V-TUN1 POSED-QUESTION) *c3*)

  ;; 石化 改造 
  ;; 电子化 改造 
  ((HUA-VERB V-GAI3-ZAO4) *c3*)

  ;; 重视 程度 
  ((V-CHONG2-SHI4 DEGREE-OF-IND) *c3*)

  ;; 保护 基金 
  ((V-BAO3-HU4 FUND-IND) *c3*)

  ;; 照顾 方式 
  ((V-ZHAO4-GU4 STYLE-OR-PROCESS) *c3*)

  ;; 过剩 周期 
  ((V-PASS AD-HOC-TIME-PERIOD) *c3*)

  ;; 照顾 政策 
  ((V-ZHAO4-GU4 STRATEGY) *c3*)

  ;; 保护 作用 
  ((V-BAO3-HU4 RESULTING-EFFECT) *c3*)

  ;; 保护 系统 
  ((V-BAO3-HU4 SYSTEM) *c3*)

  ;; 感激 心 
  ((V-GAN3-JI1 HEART-IND) *c3*)

  ;; 诈欺 案 
  ((V-ZHA4 LEGAL-CASE-OR-PROPOSAL-IND) *c3*)

  ;; 抗震救灾 胜利 
  ((V-KANG4-ZHEN4-JIU4-ZAI1 EVENT) *c3*)

  ;; 抗震救灾 斗争 
  ((V-KANG4-ZHEN4-JIU4-ZAI1 BATTLE) *c3*)

  ;; 抗灾救灾 斗争 
  ((V-KANG4-ZAI1 BATTLE) *c3*)

  ;; 救人 办法 
  ((V-JIU4-REN2-B WORD-METHOD) *c3*)

  ;; 祈愿 画面 
  ((V-QI2-YUAN4 SCENERY) *c3*)

  ;; 震动 模式 
  ((V-ZHEN4-DONG4 STYLE-TREND) *c3*)

  ;; 工作 成交 
  ((JOB-WORD-GONG1-ZUO4 V-CHENG2-JIAO1) *c3*)

  ;; 反对 决议 
  ;; 反对 意见 
  ((V-FAN3-DUI4 IDEA-OPINION) *c3*)

  ;; 企业化 操作 
  ((HUA-VERB V-CAO1-ZUO4) *c3*)

  ;; 显像 诊断仪 
  ;; 抗风 阻尼器 
  ;; 催泪弹 发射器 
  ;; 制毒 设备 
  ;; 护法 机关 
  ;; 刮水器 电机 
  ((VERB-N MACHINE) *c3*)

  ;; 保护 手段 
  ((V-BAO3-HU4 WORD-METHOD) *c3*)

  ;; 欣赏 课 
  ((V-XIN1-SHANG3 LESSON) *c3*)

  ;; 人称 游戏 
  ((V-REN2-CHENG1 GAME-LIKE) *c3*)

  ;; 鉴赏 观念 
  ((V-JIAN4-SHANG3 ANIMATE-ATTRIBUTE) *c3*)

  ;; 兼收并蓄 主义 
  ((V-JIAN1-SHOU1-BING4-CHU2 DOCTRINE-IND) *c3*)

  ;; 许诺 地 
  ((V-XU3-NUO4 LAND-IND) *c3*)

  ;; 挡风 玻璃 
  ((VERB-N MATERIAL) *c3*)

  ;; 长途跋涉 部分 
  ((V-BA2-SHE4 word-part-of) *c3*)

  ;; 暴虐 本性 
  ((V-NUE4 personality) *c3*)

  ;; 虐囚 丑闻 
  ((v-nue4-qiu2 NEWS) *c3*)

  ;; 攀岩 公路 
  ((VERB-N STREET) *c3*)

  ;; 咒骂 语 
  ((V-ZHOU4-MA4 SPOKEN-LANGUAGE-IND) *c3*)

  ;; 拍照 功夫 
  ((V-PAI1-ZHAO4 MARTIAL-ARTS) *c3*)

  ;; 现代化 道路 
  ;; 股份化 道路 
  ;; 法制化 道路 
  ((HUA-VERB STREET) *c3*)

  ;; 对向 情况 
  ((DUI4-NOUN SITUATION) *c3*)

  ;; 曹 营 
  ((SURNAME ARMY-LIKE-YING2) *c3*);

  ;; 燃气 石油 
  ((VERB-N OIL) *c3*);

  ;; 实事求是 高度 
  ((V-SHI2-SHI4-QIU2-SHI4 DEGREE-OF) *c3*)

  ;; 转弯 题 
  ((V-ZHUAN4-WAN1 TOPIC-OR-QUESTION) *c3*)

  ;; 见义勇为 称号 
  ((V-JIAN4-YI4-YONG3-WEI4 NAME-AS-SIGN) *c3*)

  ;; 见义勇为 行为 
  ((V-JIAN4-YI4-YONG3-WEI4 behavior) *c3*)

  ;; 合法化 保护 
  ((HUA-VERB V-BAO3-HU4) *c3*)

  ;; 认识 高度 
  ((V-REN4-SHI5 degree-of) *c3*)

  ;; 变革 呼声 
  ((V-BIAN4-GE2 SOUND) *c3*)

  ;; 变革 力量 
  ((V-BIAN4-GE2 POWER) *c3*)

  ;; 协调 组织 
  ;; 协调 委员会 
  ((V-XIE2-TIAO2 ORGANIZATION-IND) *c3*)

  ;; 协调 机构 
  ((V-XIE2-TIAO2 ORGANIZATION) *c3*)

  ;; 休克 疗法 
  ((v-xiu1-ke4 method-therapy) *c3*)

  ;; 统一 纲领 
  ((V-TONG3-YI1 OUTLINE) *c3*)

  ;; 统一 言论 
  ((V-TONG3-YI1 SPEECH-LIKE) *c3*)

  ;; 统一 名号 
  ((V-TONG3-YI1 NAME-AS-SIGN) *c3*)

  ;; 统一 模式 
  ((V-TONG3-YI1 STYLE-TREND) *c3*)

  ;; 统一 法 
  ((V-TONG3-YI1 METHOD-OR-LAW-IND) *c3*)

  ;; 统一 主张 
  ((V-TONG3-YI1 BELIEF) *c3*)

  ;; 发迹 细节 
  ((V-FA5-JI4 details) *c3*)

  ;; 准入 条件 
  ((V-RU4 ABSTRACT) *c3*)

  ;; 违规 成本 
  ((V-WEI2-GUI1 COST-OR-BUDGET) *c3*);

  ;; 安居乐业 问题 
  ((V-AN1-JU1-YUE4-YE4 POSED-QUESTION) *c3*)

  ;; 投机倒把 罪 
  ((V-TOU2-JI1-DAO3-BA3 SIN-OR-CRIME) *c3*)

  ;; 讹诈 军演 
  ((V-E4-ZHA4 ARRANGED-EVENT) *c3*)

  ;; 优胜劣汰 规律 
  ((V-YOU1-SHENG4-LIE4-BAI4 REGULARITY) *c3*)

  ;; 跨越 状态 
  ((V-KUA4-YUE4-B SITUATION) *c3*)

  ;; 跨越 阶段 
  ((V-KUA4-YUE4-B TIME-PT-NUM-OPT-GE-WITHIN-DAY) *c3*)

  ;; 贫穷 帽子 
  ((V-PIN2-QIONG2 HAT-IND) *c3*)

  ;; 巡查 力度 
  ((V-XUN2-ZHA1 DEGREE-OF) *c3*)

  ;; 协调 能力 
  ((V-XIE2-TIAO2 ANIMATE-ATTRIBUTE) *c3*)

  ;; 死记硬背 功夫 
  ((V-SI3-JI4-YING4-BEI4 MARTIAL-ARTS) *c3*)

  ;; 认识 间隔 
  ((V-REN4-SHI5 FENCE-OR-LAYOUT) *c3*)

  ;; 过江 线 
  ((VERB-N LINE-IND) *c3*)

  ;; 保护 监测 
  ((V-BAO3-HU4 V-JIAN4-CE4) *c3*)

  ;; 冒进 苦头 
  ((V-MAO4 SUFFERING) *c3*)

  ;; 斗殴 黑点 
  ((V-DOU4-OU1 POINT) *c3*)

  ;; 征地 证明 
  ((V-ZHENG1-DE5 LEGAL-DOC-IND) *c3*)

  ;; 变革 牺牲品 
  ((V-BIAN4-GE2 SACRIFICE) *c3*)

  ;; 连横 目的 
  ((V-LIAN2-HENG2 GOAL) *c3*)

  ;; 合纵 目的 
  ((V-HE2-ZONG4 GOAL) *c3*)

  ;; 示弱 正解 
  ((V-SHI4-RUO4 EXPLANATION) *c3*)

  ;; 新陈代谢 规律 
  ((V-XIN1-CHEN2-DAI4-XIE4 REGULARITY) *c3*)

  ;; 标准化 出现 
  ((HUA-VERB V-CHU1-XIAN4-B) *c3*);

  ;; 商品化 生产 
  ;; 国产化 工程 
  ;; 石化 项目 
  ;; 产业化 项目 
  ;; 产业化 工程 
  ;; 造化 安排 
  ;; 非商业化 宗旨 
  ;; 现代化 交通 
  ;; 机械化 作业 
  ;; 全球化 影响 
  ;; 泡沫化 危机 
  ;; 多元化 社会 
  ;; 国际化 挑战 
  ;; 全球化 竞争 
  ;; 工业化 潮流 
  ;; 全球化 挑战 
  ;; 信息化 条件 
  ;; 习惯化 训练 
  ;; 全球化 方向 
  ;; 智能化 方向 
  ;; 股份化 大气候 
  ;; 全球化 缺点 
  ;; 电气化 工程 
  ;; 合法化 象征 
  ;; 全球化 潮流 
  ;; 一体化 设计 
  ;; 数字化 链接 
  ;; 法制化 社会 
  ;; 法治化 社会 
  ;; 自动化 系统 
  ((HUA-VERB ABSTRACT) *c3*);

  ;; 现代化 管理 
  ;; 绿化 管理 
  ;; 自动化 管理 
  ((HUA-VERB V-GUAN3-LI4) *c3*);

  ;; 工业化 状态 
  ;; 多元化 格局 
  ;; 现代化 格局 
  ;; 贫困化 状态 
  ;; 同质化 格局 
  ((HUA-VERB SITUATION) *c3*);

  ;; 产业化 进程 
  ;; 工业化 过程 
  ;; 正常化 进程 
  ;; 一体化 过程 
  ;; 国际化 进程 
  ;; 一体化 进程 
  ;; 净化 仪式 
  ;; 民主化 进程 
  ;; 现代化 进程 
  ;; 现代化 过程 
  ;; 城市化 进程 
  ;; 全球化 进程 
  ((HUA-VERB PROCESS) *c3*);

  ;; 现代化 设施 
  ;; 城镇化 建设 
  ;; 现代化 建设 
  ((HUA-VERB FACILITY-LIKE) *c3*);

  ;; 国际化 事实 
  ((HUA-VERB TRUTH) *c3*);

  ;; 本地化 问题 
  ;; 无核化 问题 
  ;; 优化 问题 
  ;; 社会化 问题 
  ;; 透明化 问题 
  ((HUA-VERB POSED-QUESTION) *c3*);

  ;; 生化 武器 
  ((HUA-VERB WEAPON) *c3*);

  ;; 透明化 要求 
  ((HUA-VERB REQUEST) *c3*);

  ;; 现代化 工业 
  ;; 现代化 农业 
  ;; 石化 工业 
  ;; 石化 行业 
  ;; 日化 行业 
  ((HUA-VERB INDUSTRY) *c3*);

  ;; 日化 品牌 
  ((HUA-VERB BRAND) *c3*);

  ;; 多元化 市场 
  ;; 日化 市场 
  ((HUA-VERB MARKET-WORD-IND) *c3*);

  ;; 专业化 团队 
  ((HUA-VERB TEAM) *c3*);

  ;; 数字化 时代 
  ;; 全球化 时代 
  ;; 虚拟化 时代 
  ((HUA-VERB DYNASTY-OR-PERIOD-IND) *c3*);

  ;; 年轻化 趋势 
  ;; 国际化 趋势 
  ;; 本土化 趋势 
  ;; 多极化 趋势 
  ;; 全球化 趋势 
  ((HUA-VERB TENDENCY-OR-GESTURE) *c3*);

  ;; 绿化 体系 
  ;; 信息化 体系 
  ((HUA-VERB ABSTRACT-TYPE-OF) *c3*);

  ;; 数字化 军队 
  ((HUA-VERB ARMY-LIKE-IND) *c3*);

  ;; 信息化 作战 
  ((HUA-VERB V-ZUO4-ZHAN4-a) *c3*);

  ;; 腐化 堕落 
  ((HUA-VERB V-DUO4-LA4) *c3*);

  ;; 产业化 国家 
  ;; 工业化 国家 
  ((HUA-VERB WORD-COUNTRY) *c3*);

  ;; 市场化 本质 
  ((HUA-VERB QUALITY) *c3*);

  ;; 保洁 公司 
  ((V-BAO3-JIE2 WORD-COMPANY) *c3*);

  ;; 在线 贷款 
  ((V-ZA4-XIAN4 MONEY-RELATED) *c3*);

  ;; 在线 平台 
  ((V-ZA4-XIAN4 ORGANIZATION-OR-PLATFORM) *c3*);

  ;; 无知 表现 
  ((V-MO2-ZHI1 ANIMATE-ATTRIBUTE) *c3*);

  ;; 鼓励 政策 
  ((V-GU3-LI4 STRATEGY) *c3*);

  ;; 免费 形式 
  ((V-MIAN3-FEI4 STYLE-TREND) *c3*);

  ;; 和谐 感恩 
  ((V-HU2-XIE2-B V-GAN3-EN1) *c3*);

  ;; 稳定 基金 
  ((V-WEN3-DING4 FUND-IND) *c3*);

  ;; 协调 规范 
  ((V-XIE2-TIAO2 LAW) *c3*);

  ;; 团结 力量 
  ((V-TUAN2-JIE2 POWER) *c3*);

  ;; 包抄 形式 
  ((V-BAO5-CHAO1 STYLE-TREND) *c3*);

  ;; 优胜劣汰 巨浪 
  ((V-YOU1-SHENG4-LIE4-BAI4 WAVE) *c3*);

  ;; 对称 原理 
  ((V-DUI4-CHENG4 REASON-OR-ETHICS) *c3*);

  ;; 崛起 迹象 
  ((V-JUE2-HUO3 APPEARANCE) *c3*);

  ;; 合力 真实性 
  ((V-HE2-LI4 ABSTRACT-SUFFIX) *c3*);

  ;; 连任 路 
  ((V-LIAN2-REN2 ROAD-IND) *c3*);

  ;; 说理 方式 
  ((V-SHUI4-LI4 STYLE-OR-PROCESS) *c3*);

  ;; 海投送 力量 
  ((V-TOU2-SONG4 POWER) *c3*);

  ;; 在线 贷款 
  ((V-ZA4-XIAN4 MONEY-RELATED) *c3*);

  ;; 在线 平台 
  ((V-ZA4-XIAN4 ORGANIZATION-OR-PLATFORM) *c3*);

  ;; 无知 表现 
  ((V-MO2-ZHI1 BEHAVIOR) *c3*);

  ;; 无耻 表现 
  ((V-HOU4-YAN2-WU2-CHI3 BEHAVIOR) *c3*);

  ;; 鼓励 政策 
  ((V-GU3-LI4 STRATEGY) *c3*);

  ;; 免费 形式 
  ((V-MIAN3-FEI4 STYLE-TREND) *c3*);

  ;; 网购 概念 
  ((V-WANG3-GOU4 CONCEPT) *c3*);

  ;; 和谐 感恩 
  ((V-HU2-XIE2-B V-GAN3-EN1) *c3*);

  ;; 协调 规范 
  ((V-XIE2-TIAO2 LAW) *c3*);

  ;; 团结 力量 
  ((V-TUAN2-JIE2 POWER) *c3*);

  ;; 包抄 形式 
  ((V-BAO5-CHAO1 STYLE-TREND) *c3*);

  ;; 统一 问题 
  ((V-TONG3-YI1 POSED-QUESTION) *c3*);

  ;; 防守 情况 
  ((V-FANG2-SHOU3 SITUATION) *c3*);

  ;; 自私 层级 
  ((V-JI3-SI1 POSITION-OF) *c3*);

  ;; 反对 票 
  ((V-FAN3-DUI4 TICKET-OR-VOTE-IND) *c3*);

  ;; 反对 态度 
  ((V-FAN3-DUI4 ATTITUDE) *c3*);

  ;; 违法 犯罪 
  ((V-WEI2-FA3 V-FAN4-ZUI4) *c3*);

  ;; 在意 自豪感 
  ((V-ZA4-YI4 FEELING-OR-SENSE) *c3*);

  ;; 污蔑 谣言 
  ((V-WU1-MIE4-B RUMOR) *c3*);

  ;; 恶心 难爱 
  ((V-WU4-ZHU3 EMOTION) *c3*);

  ;; 压制 存在 
  ((V-YA1-ZHI4 V-CUN2-ZA4) *c3*);

  ;; 和谐 稳定 
  ((V-HU2-XIE2-B V-WEN3-DING4) *c3*);

  ;; 征地 补偿 
  ((V-ZHENG1-DE5 MONEY-RELATED) *c3*);

  ;; 保护 法规 
  ((V-BAO3-HU4 LAW) *c3*);

  ;; 腐化 现象 
  ((HUA-VERB APPEARANCE) *c3*);

  ;; 就学 当年 
  ((V-JIU4-XUE2 AD-HOC-TIME-PT-YEAR) *c3*);

  ;; 违法 处罚 
  ((V-WEI2-FA3 PRIZE-OR-PENALTY) *c3*);

  ;; 违法 事实 
  ((V-WEI2-FA3 TRUTH) *c3*);

  ;; 节约 美德 
  ((V-JIE2-YUE1 ETHICS) *c3*);

  ;; 保护 问题 
  ;; 保护 命题 
  ((V-BAO3-HU4 POSED-QUESTION) *c3*);

  ;; 忧国忧民 意识 
  ((V-YOU1-GUO2-YOU1-MIN2 CONSCIOUSNESS) *c3*);

  ;; 阶级化 力量 
  ;; 物化 力量 
  ((HUA-VERB POWER) *c3*);

  ;; 和谐 教育 
  ((V-HU2-XIE2-B TEACHING) *c3*);

  ;; 播出 管理 
  ((V-BO1-C V-GUAN3-LI4) *c3*);

  ;; 保护 申请 
  ((V-BAO3-HU4 REQUEST) *c3*);

  ;; 民主化 经验 
  ;; 现代化 步伐 
  ;; 现代化 目标 
  ;; 硬化 能力 
  ;; 贫困化 境地 
  ((HUA-VERB ANIMATE-ATTRIBUTE) *c3*);

  ;; 积累 政策 
  ((V-JI1-LEI2 STRATEGY) *c3*);

  ;; 违法 成本 
  ((V-WEI2-FA3 COST-OR-BUDGET) *c3*);

  ;; 违规 失职 
  ((V-WEI2-GUI1 V-SHI1-ZHI2) *c3*);

  ;; 理化 指标 
  ((HUA-VERB A-SIGN) *c3*);

  ;; 保护 协会 
  ;; 保护 委员会 
  ((V-BAO3-HU4 ORGANIZATION-IND) *c3*);

  ;; 保护 举措 
  ;; 保护 措施 
  ((V-BAO3-HU4 STEPS) *c3*);

  ;; 千丝万缕 联系 
  ((V-QIAN1-SI1-MO4-LV3 CONTACT-AND-COMMUNICATION) *c3*);

  ;; 迷失 悲剧 
  ((V-MI3-SHI1 DRAMA) *c3*);

  ;; 多元化 发展 
  ;; 城市化 发展 
  ;; 产业化 发展 
  ((HUA-VERB V-FA5-ZHAN3) *c3*);

  ;; 产业化 集群 
  ((HUA-VERB WORD-CLUSTER) *c3*);

  ;; 膨胀 表现 
  ((V-PENG2-ZHANG4 BEHAVIOR) *c3*);

  ;; 恭维 里面 
  ((V-GONG1-WEI2 DIR-INSIDE-OUTSIDE) *c3*);
;;;;
  )

(defun mod-noun (m n L1 L2)
  (let ((*L1* L1)
        (*L2* L2)
        (penalty 0))
    ;; currently only bias mod-noun for some traits of m
    (when (and (not (typep n 'policy))
               (has-trait m 'noun-mod-has-unit))
      ;; allow '(一个 中国) 政策'
      (incf penalty *c-close-suffix*))
    (when (and (not (typep m 'city-word-du1))
               (has-trait m 'noun-mod-has-de))
      ;; allow for '首善之都 执政权'
      (incf penalty *c-close-suffix*))
    (when (trait-value n 'time-with-ind)
      ;; e.g. want '(国民党 政府) (于 一九三七年) VV', not '((国民党 政府)(于 一九三七年))'
      ;; not want '男 的 (在用手机偷拍咱们)' as time
      (incf penalty *c-common*))
    (when (trait-value m 'time-with-at-ind)
      ;; e.g. want '(于 一九三七年) (西 ...', not '((于 一九三七年) 西)'
      (incf penalty *c-common*))
    ;;
    (when (trait-value m 'has-num-unit-suffix)
      ;; prefer 'N1 (num-unit N2)' to '(N1 num-unit) N2'
      (incf penalty *c-common*))
    (when (trait-value n 'has-num-unit-suffix)
      ;; prefer the num-unit to be attached to outer scope
      (incf penalty *c-long-sep*))
    ;;
    (when (has-trait m 'has-non-passive-verb-mod)
      (incf penalty *c-very-rare*))
    (when (has-trait m 'verb-vp)
      (incf penalty *c-very-rare*))
    (when (has-trait m 'place-with-ind)
      (incf penalty *c-rare*))
    (when (has-trait n 'place-with-ind)
      ;; e.g. not want '她们 (在 商场 试衣间 内)' as place
      (incf penalty *c-common*))
    (when (trait-value n 'noun-mod-has-subj-pred-de)
      ;; prefer '((N1 N2) V) de N3' to 'N1 ((N2 V) de N3)'
      (incf penalty *c-close-sep*))
    (when (and (has-trait n 'noun-mod-has-long-verb-n)
               (not (has-trait n 'noun-mod-has-de)))
      (incf penalty *c-common*))
    ;; to discourage 'N1 (N2 conn N3) N4'
    (when (and (conn-thing-p m)
               (has-trait m 'noun-mod-is-noun))
      (incf penalty *c-common*))
    (when (and (conn-thing-p m) (typep n 'friend))
      ;; prefer '(冯 女士) 和 (两名 (女性 好友))' to '((冯 女士) 和 (两名 女性))好友'
      (incf penalty *c-close-sep*))
    (when (typep n 'trip-ind)
      ;; '旅' is not usually used as noun in mod-noun
      (incf penalty *c-close-suffix*))
    (when (and (typep n 'ind)
               (> (trait-value m 'n-base-nouns 1) 1))
      ;; prefer '中央 (气象 台)' to '(中央 气象) 台'
      (incf penalty *c-close-sep*))
    (when (typep n 'sound-literal)
      ;; especially things like '的', '丁', '習' which may have meaning
      ;; sound literal is not usually used as noun in mod-noun
      (incf penalty *c-common*))
    (when (trait-value m 'quoted)
      ;; e.g. want '『雾』它' separated in '我叫一声『雾』 它敢答应么'
      (incf penalty *c4*))
    (when (and (typep n 'time-name)
               (typep m 'de-noun-mod)
               (trait-value (de-noun-mod-pred m) 'verb-mod-zai4))
      ;; prefer '在' as time-at-to rather than adv in  '在刚过去的冬天'
      (incf penalty (+ *c-close-sep*
                       *c-close-suffix*)))
    ;;;;
    ;; to prefer (adj (noun ind)) to ((adj noun) ind) ??
    ;; but prefer "金砖 国" to  "金 砖国" ??
    ;; prefer '大米 价格' to '大 (米 价格)'
    ;;(when (and (typep n 'ind)
    ;;           (has-trait m 'has-adj-noun-mod))
    ;;  (incf penalty *c-close-suffix*))
    ;;
    (score-add penalty (mod-noun-x m n))))
;; end mod-noun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mod-noun-tag (m n L1 L2)
  (declare (ignore L2))
  (let ((mt (noun-mod-trait m L1))
        (n-vals nil))
    ;; add to the traits
    (when (and mt n)
      (setf n-vals (cons2 mt t
                          n-vals)))
    (when (and (typep m 'de-noun-mod)
               (a-subj-pred-p (de-noun-mod-pred m)))
      (setf n-vals (cons2 'noun-mod-has-subj-pred-de t
                          n-vals)))
    (when (or (typep m 'ju4-sentence)
              (typep m 'sheng1))
      ;; indicate sound, which some verbs such as vc-speak favors
      (setf n-vals (cons2 'has-sheng1-or-ju4 t
                          n-vals)))
    (when (or (typep m 'num)
              (typep m 'number))
      ;; not prefer verb-noun '死亡 人', but prefer '死亡 2人'
      (setf n-vals (cons2 'noun-mod-is-num t
                          n-vals)))
    ;;
    (cond ((and (typep m 'place) (typep m 'nr-name))
           ;; country name would propagate, so that
           ;; 中國 (雞蛋 和 雞肉) is preferred to (中國雞蛋) 和 雞肉
           ;; with proper bias in similar-noun, we should have
           ;; 中國雞蛋和美國雞肉 as (中國 雞蛋) 和 (美國 雞肉)
           (setf n-vals (cons2 'noun-mod-has-place-name t
                               n-vals)))
          ((and (or (typep m 'place) (typep m 'race))
                (typep n 'human))
           ;; e.g. '赵县 人', which could be used alone as pred for person, use trait to mark it
           ;; e.g. '回族 人 '
           (setf n-vals (cons2 'place-or-race-human t
                               n-vals)))
          ((conn-thing-p n)
           ;; to discourage 'N1 (N2 conn N3) N4' where N1 is not country-name
           (setf n-vals (cons2 'noun-mod-is-noun t
                               n-vals)))
          ((has-trait m 'noun-mod-has-place-name)
           ;; this propagation is after checking conn-thing so that
           ;; 'P1 N1 (N2 conn N3) N4' cannot get away with the penalty
           ;; by first grouping (P1 N1)
           (setf n-vals (cons2 'noun-mod-has-place-name t
                               n-vals)))
          ((typep m 'adj)
           (setf n-vals (cons2 'has-adj-noun-mod t
                               n-vals)))
          (t nil))
    ;;
    (more-trait-with n n-vals)
    ))

(defparameter *mod-noun-tag-t1-t2*
  (tn (mod-noun-tag t1 t2 L1 L2)))
(defparameter *mod-time-tag-t1-t2*
  (tn (combine-time-scale-tag
       t1
       (mod-noun-tag t1 t2 L1 L2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sum-count-base-nouns (t1 t2 val)
  (more-trait
   val
   'n-base-nouns (+ (trait-value t1 'n-base-nouns 1)
                    (trait-value t2 'n-base-nouns 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *count-mod-time-tag-t1-t2*
  (tn (sum-count-base-nouns
       t1 t2
       (combine-time-scale-tag
        t1
        (mod-noun-tag t1 t2 L1 L2)))))
(defparameter *count-mod-de-time-tag-t1-t3*
  (tn (sum-count-base-nouns
       t1 t3
       (combine-time-scale-tag
        t1
        (mod-de-noun-tag t1 t3 L1 L3)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *mod-noun-t1-t2*
  (tn (mod-noun t1 t2 L1 L2)))

(defparameter *count-mod-noun-t1-t2*
  (tn (score-add
       (mod-noun t1 t2 L1 L2)
       (prefer-less-base-noun t2))))

(defparameter *mod-de-noun-t1-t3*
  (tn (mod-de-noun t1 t3 L1 L3)))

(defparameter *count-mod-de-noun-t1-t3*
  (tn (score-add
       (mod-de-noun t1 t3 L1 L3)
       (prefer-less-base-noun t3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; relative preference of some verbs on verb-p or subj-pred, when it
;; is both v-VP and v-SP.
(defun-conn verb-on-phrase (v p) verb-on-phrase-x)
(defparameter *c-prefer-verb-on-verb-p* *c0*)
(defparameter *c-not-prefer-verb-on-subj-pred* *c1*)
(defgeneric verb-on-phrase-x (v p))
(def-comb verb-on-phrase-x
  ((t t) *c-fallback*)
  ;; default for e.g. '看到'
  (((v combine-verb) (p t))
   (verb-on-phrase (combine-verb-first v) p))
  (((v verb-n) (p t))
   ;; for v-BOP in verb-may-pause, which could be a verb-n
   (verb-on-phrase (verb-n-verb v) p))
  ;; default to make the penalty same as now in verb-c2
  ((verb t) *c-verb-verb-p*) ;; 'verb verb-p' case
  ((verb a-subj-pred) *c-verb-subj-pred*) ;; 'verb subj-pred' case
  ;;
  ((v-jue2-ding4 verb) *c-prefer-verb-on-verb-p*)
  ((v-jue2-ding4 a-subj-pred) *c-not-prefer-verb-on-subj-pred*)

  ((v-zhi2-c (p a-subj-pred))
   (+ *c-verb-subj-pred* *c-close-suffix*))

  ((v-zhi4-b (p a-subj-pred))
   (+ *c-verb-subj-pred* *c-common*
      (not-prefer-trait p 'has-time *c-common*)
      (not-prefer-trait p 'has-place *c-close-suffix*)))

  ((v-tan4-suo3 a-subj-pred)
   (+ *c-common*
      *c-not-prefer-verb-on-subj-pred*))

  ;; '统计' prefers noun more than either verb-p or subj-pred
  ((v-tong3-ce4 (p a-subj-pred))
   ;; TODO: constrains that the subj in subj-pred should has
   ;; q-pronoun?, e.g. "有 多少"?
   (+ (not-prefer (a-subj-pred-subj p) 'information-word-zi1-liao4 *c-common*)
      ;; bias against '统计 (资料表明)', prefer '(统计资料) 表明'
      *c-close-suffix* *c-verb-subj-pred*))
  ((v-tong3-ce4 verb)
   (+ *c-close-suffix* *c-verb-verb-p*))

  ;; '进行' seems rarely used on subj-pred?
  ((v-jin4-xing2 a-subj-pred)
   (+ *c-rare*
      *c-not-prefer-verb-on-subj-pred*))
  ((v-jin4-xing2 (v verb))
   ;; may need to add other cases
   (not-prefer-verb v 'v-xiang1-guan1 *c-rare*)
   )

  ((v-zeng1-qiang3 a-subj-pred)
   ;; not want '自我保护意识' as subj-pred in '增强 自我保护意识'
   (+ *c-close-suffix*
      *c-not-prefer-verb-on-subj-pred*))

  ;; '看' is rarely on verb, but '有' is often followed by subj-pred
  ;; e.g. '看到 (有 (保安人员前来))'
  ((v-see v-has) *c-prefer-verb-on-verb-p*)
  ((v-see verbs) (+ *c-rare* *c-verb-verb-p*))

  ((v-gao4-su4 verbs) *c-prefer-verb-on-verb-p*)

  ;; '面对' could be used on noun, and prefer it to both verb-p or subj-pred
  ((v-cao3-dui4-b a-subj-pred)
   (+ *c-close-sep* *c-not-prefer-verb-on-subj-pred*))

  ;; at first it seems '求' seems less likely to be used on
  ;; passive-voice, e.g. '求 遭拒' ?  but would say '求 (被 選為 冠軍)',
  ;; or '求 (别 遭拒)'. So cannot yet give useful constraints here.

  ((v-yuan4-yi4 verb) *c-prefer-verb-on-verb-p*)

  ((v-is verb) *c-prefer-verb-on-verb-p*)
  ((v-is a-subj-pred)
   ;; want '是 (晚上 ((来 (商场附近)) 玩))', not '是 ((晚上来) (商场附近) 玩)'
   (+ *c-verb-subj-pred* *c-close-suffix*))

  ((v-ying3-xiang3 a-subj-pred)
   ;; want verb-noun '影响民众健康'
   (+ *c-not-prefer-verb-on-subj-pred* *c-long-sep*))

  ((v-yan2-jiu1 (v a-subj-pred))
   ;; want '(研究人员) (分析中国154个城市自1981年至2012年的空气质素数据)', not '研究 (人员分析中国154个城市自1981年至2012年的空气质素数据)'
   (+
    (not-prefer (a-subj-pred-subj v) 'word-ren2-yuan2 *c-common*)
    *c-not-prefer-verb-on-subj-pred*))

  ((v-dang1 a-subj-pred)
   ;; prefer '当' as time-ind
   (+ *c-common*
      *c-not-prefer-verb-on-subj-pred*))

  ((v-sheng3 a-subj-pred)
   ;; '省' could mean province, and even as verb, not often used on subj-pred
   (+ *c-common* *c-not-prefer-verb-on-subj-pred*)
   )

  ((v-fa5-sheng1-b a-subj-pred)
   ;; prefer '发生 (多人受伤 的 交通事故)' to '(发生 (多人受伤)) 的 交通事故'
   (+ *c-common* *c-not-prefer-verb-on-subj-pred*))

  ((v-sheng1-ji2 (p verb-n))
   ;; '升级 (发布 重污染天气橙色预警信号)'
   (+ *c-prefer-verb-on-verb-p*
      ;; prefer '等级 ((升级 为) 橙色预警) ...' to '等级 (升级 (为 橙色预警) ...)'
      (prefer-verb p 'v-fa5-bu4 (+ *c1* *c-close-suffix*))))

  ((v-gui1-hua2-b (v verb))
   ;; '规划 而变动' is strange
   (not-prefer-trait v 'verb-mod-adv-but *c-rare*))
  )
;; end verb-on-phrase

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-conn similar-noun-mod (m n) similar-noun-mod-x)
(defgeneric similar-noun-mod-x (m n))
(def-comb similar-noun-mod-x
  ((t t) *c-rare*)
  ;;
  ((unit unit) *c-short-conn*)
  ((adj adj) *c-short-conn*)
  ;;
  ((q-pronoun q-pronoun) *c-short-conn*)
  ((p-pronoun p-pronoun) *c-short-conn*)
  ((pronoun pronoun) *c-short-conn*)
  ;;
  ((verb verb) *c-long-conn*)
  ((verb-n verb-n) *c-short-conn*)
  ;;
  ((a-subj-pred a-subj-pred) *c-short-conn*)
  )
;; end similar-noun-mod

;;;;
(defun both-or-neither (a b)
  (cond (a b)
        (b nil)
        (t t)))

(defun-conn similar-noun (m n) similar-noun-xx)
(defun similar-noun-xx (m n)
  (let ((penalty 0))
    ;; more parallels in traits
    (unless (both-or-neither
             ;; not want great penalty for '广西' and '四川 盆地'
             (and (has-trait m 'noun-mod-has-place-name)
                  (not (typep m 'place)))
             (and (has-trait n 'noun-mod-has-place-name)
                  (not (typep n 'place))))
      (incf penalty *c-common*))
    (unless (both-or-neither
             (typep m 'ind)
             (typep n 'ind))
      (incf penalty *c-common*))
    (unless (both-or-neither
             (trait-value m 'noun-mod-has-de)
             (trait-value n 'noun-mod-has-de))
      (incf penalty *c-close-suffix*))
    (unless (both-or-neither
             (trait-value m 'place-with-ind)
             (trait-value n 'place-with-ind))
      (incf penalty *c-close-suffix*))
    (score-add penalty (similar-noun-x m n))))
(defgeneric similar-noun-x (m n))
;; TODO: to get more fine grained similarity
(def-comb similar-noun-x
  ((t t) *c-very-rare*)
  ;;
  ((unit unit) *c-short-conn*)
  ((adj adj) *c-short-conn*)
  ;;
  ((q-pronoun q-pronoun) *c-short-conn*)
  ((p-pronoun p-pronoun) *c-short-conn*)
  ((pronoun pronoun) *c-short-conn*)
  ;;
  ((verb verb) *c-long-conn*)
  ((verb-n verb-n) *c-short-conn*)
  ;;
  ((verb verb-action) (+ *c-long-conn* *c-close-sep*))
  ((verb-action verb) (+ *c-long-conn* *c-close-sep*))
  ;; sometimes the noun form is preferred, but can also be verb
  ((verb (a abstract))
   (if (also-verb-p a)
       *c-long-conn*
       *c-very-rare*))
  (((a abstract) verb)
   (if (also-verb-p a)
       *c-long-conn*
       *c-very-rare*))
  ;;
  ((a-subj-pred a-subj-pred) *c-short-conn*)
  ;;
  ((physical physical) *c-common*)

  ((animate animate) *c-long-conn*)
  ((human human) *c-short-conn*)
  ((animal animal) *c-short-conn*)
  ((human animal) *c-long-conn*)

  ;; to discourage non-parallel conn
  ((human-name human-name) *c-close-suffix*)
  ((human-name human) *c-long-conn*)
  ((human human-name) *c-long-conn*)
  ((animate human-name) *c2*)
  ((human-name animate) *c2*)

  ;; the single word 旅 is confusing
  ((army-like army-like) *c-short-conn*)
  ((army-like-lu3 noun) *c-very-rare*)
  ((animate army-like-lu3) *c-very-rare*)

  ((inanimate inanimate) *c-long-conn*)
  ((plant plant) *c-short-conn*)
  ((edible edible) *c-short-conn*)

  ;; sometimes these are treated as belongings
  ((animal inanimate) *c4*)
  ((animal money-related) *c4*)
  ((inanimate animal) *c4*)
  ((inanimate money-related) *c4*)
  ((money-related inanimate) *c4*)
  ((money-related animal) *c4*)

  ((abstract abstract) *c-long-conn*)
  ;; 教育 和 惩戒
  ((teaching prize-or-penalty) *c-short-conn*)
  ((prize-or-penalty teaching) *c-short-conn*)
  ;;
  ((attribute attribute) *c-short-conn*)

  ((place place) *c-long-conn*)
  ((place-with-dir place-with-dir) *c-short-conn*)
  ((place place-with-dir) (+ *c-long-conn* *c-close-suffix*))
  ((place-with-dir place) (+ *c-long-conn* *c-close-sep*))

  ((district district) (- *c-short-conn* *c-close-sep*))
  ((word-place-center word-place-center) *c-short-conn*)
  ;; prefer '(京津冀 及 周边地区) (省级环境监测 中心)'
  ((place word-place-center) (+ *c-long-conn* *c-close-sep*))
  ((word-place-center place) (+ *c-long-conn* *c-close-sep*))

  (((t1 time-name) (t2 time-name))
   (+ *c-long-conn*
      ;; prefer '(周日26日零时 至 周三28日) 期间'
      ;; NOTE: sometimes t1 may have smaller time than t2, e.g. '周日26日零时 至 周三28日'
      (if (member *sep* '(:comma :short-comma :semi-colon))
          ;; if there is a pause, less penalty, e.g. '去年11月至今年3月，在通常因燃煤供暖导致雾霾天增多的冬季供暖期间'
          0
          (+
           (not-prefer t2 'ad-hoc-time-period-word-during *c-close-sep*)
           (similar-time-scale t1 t2 *c2* *c-long-sep*)))))
  (((t1 dynasty) (t2 dynasty))
   (+ *c-short-conn*
      (similar-time-scale t1 t2)))

  ;; '建築' and '生产' can both be verbs, and can be a kind of '活动'
  ((word-building-jian4-zhu2 production) *c-short-conn*)
  ((production word-building-jian4-zhu2) *c-short-conn*)
  )
;; end similar-noun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric similar-verb-x (v1 v2))
(def-comb similar-verb-x
  ((t t) *c-common*)
  ((verb verb) *c-prefer*)
  ;;
  ((verb0 verb-n) *c-common*)
  (((v1 verb-n) (v2 verb0))
   ;; some combinations receives less penalty
   (if (and (is-type-p v1 'v-ran2-a)
            (is-type-p v2 'v-gong1-nuan3))
       ;; e.g. '燃煤 供暖'
       *c-close-sep*
       *c-common*))
  ;;
  (((v1 combine-verb) (v2 combine-verb))
   (score-min (similar-verb (combine-verb-first v1)
                            (combine-verb-first v2))
              (similar-verb (combine-verb-second v1)
                            (combine-verb-second v2))))
  (((v1 combine-verb) (v2 t))
   (score-add *c-close-sep*
              (score-min (similar-verb (combine-verb-first v1) v2)
                         (similar-verb (combine-verb-second v1) v2))))
  (((v1 t) (v2 combine-verb))
   (score-add *c-close-sep*
              (score-min (similar-verb v1 (combine-verb-first v2))
                         (similar-verb v1 (combine-verb-second v2)))))
  ;;
  (((v1 passive-voice) (v2 passive-voice))
   (similar-verb (passive-voice-verb v1) (passive-voice-verb v2)))
  (((v1 passive-voice) (v2 t))
   (score-add *c4*
              (similar-verb (passive-voice-verb v1) v2)))
  (((v1 t) (v2 passive-voice))
   (score-add *c4*
              (similar-verb v1 (passive-voice-verb v2))))
  ;;
  (((v1 verb0) (v2 verb0))
   (if (and (is-type-p v1 'v-bao4-gao4)
            (is-type-p v2 'v-yu4-ce4-b))
       ;; '报告 并 预测' could also be a subj-pred
       *c2*
       (similar-verb (verb0-verb v1) (verb0-verb v2))))
  ;;
  )

(defparameter *similar-supp-traits*
  '(verb-der verb-supp verb-ge-supp
    verb-der-supp verb-bu-supp
    verb-le-supp verb-supp-adj
    verb-le verb-le-time
    verb-de-shi
    ;;
    verb-n-adj verb-n-n verb-der-vp
    verb-OP verb-VP der-verb verb-SP
    ;;
    has-non-passive-verb-mod
    ))
(defun similar-supp-trait (v1 v2)
  (let ((res-score 0))
    (dolist (trait *similar-supp-traits* res-score)
      ;; if one has, both the other does not, then penalize
      (cond ((has-trait v1 trait)
             (unless (has-trait v2 trait)
               (incf res-score *c-close-suffix*)))
            ((has-trait v2 trait)
             (incf res-score *c-close-suffix*))))))

(defun similar-verb-xx (v1 v2)
  (let ((s (similar-verb-x v1 v2)))
    (if s
        (+ s (similar-supp-trait v1 v2))
        nil)))
(defun-conn similar-verb (v1 v2) similar-verb-xx)
;; end similar-verb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric verb-noun-x (v n))
(def-comb verb-noun-x
  ((t t) *c-ignore*)
  ((verb t) *c-fallback*)

  ((adj verb) *c-unknown-pair*) ;; often prefer the adj as adv
  ((adj noun) *c-unknown-pair*) ;; default for adj, give a high penalty, to avoid many adjs used as verb
  ((adj time-period) *c-close-suffix*)
  ((adj num) *c-close-suffix*)
  ((adj number) *c-close-suffix*)
  ((adj unit) *c2*)
  ((adj-word-tall unit-length) *c-close-sep*)
  ((adj-on-length unit-length) *c-close-sep*)
  ((adj-size unit-length) *c-close-sep*)

  ((verb noun) *c-very-rare*) ;; may be *c-fallback* is too severe, because our verb-noun pairs is currently not quite enough
  ((verb verb) *c-fallback*) ;; prevent unwanted verb -> noun
  ;;
  ((verb unit) *c-rare*)
  ((verb tong1) *c-common*) ;; e.g. 大罵一通
  ((verb bian4) *c-common*) ;; e.g. 大罵一遍
  ;;
  ;; verb-close-supp, not necessarily verb
  ((verb-close-supp-mark-place place) *c-close-suffix*)
  ((verb-close-supp-from-to place) *c-close-suffix*)
  ((verb-close-supp-from-to animate) *c-close-suffix*)
  ;;
  (((v combine-verb) (n t))
   (score-max (verb-noun (combine-verb-first v) n)
              (verb-noun (combine-verb-second v) n)))
  ;;
  (((v verb0) (n t))
   (if (has-trait v 'verb-supp-adj)
       *c-very-rare*
       (verb-noun (verb0-verb v) n)))
  ;;
  (((v verb-n) (n t))
   (let ((penalty (second-obj-trait-penalty v))
         (v0 (verb-n-verb v)))
     (incf penalty (second-obj-trait-penalty v0))
     (score-add
      penalty
      (typecase v0
        ;; see pp-verb-mod-verb-basic-penalty
        (combine-verb (verb-noun (combine-verb-first v0) n))
        ;; v-beneficiary can take more than one object
        (v-beneficiary (verb-noun v0 n))
        (t *c-fallback*))))
   )
  ;;
  ;; TODO: whether to have case for passive-voice?   
  ;;
  ((verb num) *c-rare*)
  ((verb point-ind) *c-rare*) ;; prefer verb-mod-noun
  ;;
  ((v-O-I-physical physical) *c-prefer*)
  ((v-O-I-animate animate) *c-prefer*)
  ((v-O-I-inanimate inanimate) *c-prefer*)
  ((v-O-I-body-part body-part) *c-prefer*)
  ;; 打; 纏
  ((v-O-I-animate-inanimate animate) *c-prefer*)
  ((v-O-I-animate-inanimate inanimate) *c-prefer*)
  ;;
  ((v-g-O-I-inanimate inanimate) *c-prefer*)
  ((v-g-O-I-physical physical) *c-prefer*)
  ;;;;
  ((dir-verbs place) *c-close-suffix*)

  ;;; as a quick default
  ((verb-degree animate) *c-close-suffix*)

  ;;
  ((v-jin4-xing2 (n abstract))
   (if (also-verb-p n)
       *c-close-suffix*
       *c-common*))

  ((adj-time-period time-name) *c-common*) ;; '周' as adj could be used as verb, not prefer it followed by time

  ((verb-close-supp-yu1 time-name)
   ;; prefer '於' as time-ind
   (+ *c-close-suffix* *c-rare*))

  ((v-scold organization) *c-close-suffix*)
  ((v-scold place) *c-close-suffix*)
  ((v-scold country) *c-close-suffix*)
  ((v-scold animate) *c-close-sep*)
  ((v-scold foul-language) *c-close-suffix*)

  ((V-WEN4 ABSTRACT) *C-COMMON*)
  ((V-WEN4 STRATEGY-OR-MEASURING-TOOL-IND) *C-CLOSE-SUFFIX*)
  ((V-WEN4 ANIMATE-ATTRIBUTE) *C-CLOSE-SUFFIX*)
  ((V-WEN4 MARKS) *C-CLOSE-SUFFIX*)
  ((V-WEN4 PRICE) *C-CLOSE-SUFFIX*)
  ((V-WEN4 POSED-QUESTION) *C-CLOSE-SUFFIX*)
  ((V-WEN4 RESULTING-EFFECT) *C-CLOSE-SUFFIX*)
  ((V-WEN4 REASON) *C-CLOSE-SUFFIX*)
  ((V-WEN4 SKY-OR-GOD-LIKE-OR-PLACE) *C-CLOSE-SUFFIX*)
  ((V-WEN4 PLACE-NAME) *C-CLOSE-SUFFIX*)
  ((V-WEN4 SITUATION) *C-CLOSE-SUFFIX*)
  ((V-WEN4 METHOD) *C-CLOSE-SUFFIX*)
  ((V-WEN4 HUMAN) *C-CLOSE-SUFFIX*)
  ((V-WEN4 DEGREE-OF) *C-CLOSE-SUFFIX*)
  ((V-WEN4 ABSTRACT-MATTER) *C-CLOSE-SUFFIX*)

  ((vc-speak (n noun))
   (if (or (trait-value n 'quoted)
           (trait-value n 'has-sheng1-or-ju4))
       *c-long-sep*
       *c-fallback*))

  ((v-si3-wang2 (n human))
   ;; not prefer verb-noun '死亡 人', but prefer '死亡 2人'
   (if (trait-value n 'noun-mod-is-num)
       *c-close-suffix*
       *c-common*))

  ((v-long3-zhao4 (n dir-middle))
   ;; e.g. '笼罩 (在)雾霾中'
   (if (> (trait-value n 'n-base-nouns 1) 1)
       *c-close-suffix*
       *c-common*))
  ((v-long3-zhao4 (n dir-inside-outside))
   (if (> (trait-value n 'n-base-nouns 1) 1)
       *c-close-suffix*
       *c-common*))

  ((vc-quantity-more-less dependency-of)
   ;; '减少 煤炭依赖'
   *c-close-suffix*)
  ((vc-quantity-more-less num) *c-close-suffix*)
  ((vc-quantity-more-less number) *c-close-suffix*)

  ((v-chu1-xian4-b (n time-period-num-ind-day))
   ;; '出现 雾霾天'
   (if (> (trait-value n 'n-base-nouns 1) 1)
       *c-close-suffix*
       *c-very-rare*))
  )
;; end verb-noun-x

;; The other part of verb-noun is in
;; chinese_parser_verb_noun_costs.lisp

(defun verb-noun-xx (v n)
  (let ((v-n (if (symbolp v) v (type-of v)))
        (n-n (if (symbolp n) n (type-of n))))
    (multiple-value-bind (cost got)
        (get-cost-comb *verb-noun-costs* v-n n-n)
      (if got cost (verb-noun-x v n)))))
(defun verb-noun-xxx (v n)
  (if (and (typep v 'verb0)
           (has-trait v 'verb-supp-adj))
      ;; seems "(V der adj) N" has quite a lot of advantage over "(V der) (adj N)"
      
      ;; NOTE: this penalty is too severe, consider '(講 得 清楚) 規則'
      ;; or '(講 清楚) 規則' which are preferred to '(講 得) (清楚 規
      ;; 則)' or '講 (清楚 規則)'

      ;; NOTE: also consider '(把 事) (講 得 簡單)' is ok, but '(講 得
      ;; 簡單) 事' is not ok. So need to be more relaxed in BA taking object.
      
      ;; NOTE: it seems the way out is to do it case-by-case, keep
      ;; this penalty for verb0 and verb-supp-adj combination for
      ;; uncommon 'V adj', while for the common and valid one, use
      ;; only the verb and verb-supp-adj, so that this penalty does
      ;; not apply.
      (score-add (+ *c-very-rare* *c-close-sep*)
                 (verb-noun (verb0-verb v) n))
      (verb-noun-xx v n)))

(defun-conn verb-noun (v n) verb-noun-xxx)

;; end verb-noun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric subj-verb-tag (n v))
(def-comb subj-verb-tag
  (((n t) (v t))
   (make-a-subj-pred :subj n :pred v)) ;; default case
  ;; V 的 是 N --> V N, so this subj-pred is like a verb-p
  (((n de-noun-mod) (v v-is))
   n)
  )

;;
(defparameter *v-jiao4-ask* (make-instance 'v-ask-to))
(defparameter *v-jiao4-teach* (make-instance 'v-jiao1-c))

(defun prefer-animate-like (n)
  (typecase n
    (animate 0)
    (organization 0)
    (country 0)
    (province 0)
    (county 0)
    (city-word-du1 *c-rare*)
    (city 0)
    (village 0)
    (building 0)
    (word-place-center *c-close-sep*)
    (station *c-close-suffix*)
    (t *c4*)))

(defun subj-verb (n v)
  ;; to handle traits
  (let ((c-s (trait-value v 'verb-mod-change-subject))
        (penalty 0))
    (when (has-trait v 'verb-mod-prefer-animate-like)
      (incf penalty (prefer-animate-like n)))
    ;;
    (score-add
     ;; penalty due to verb-mod-prefer-animate-like is added even for
     ;; verb-mod-change-subject case.
     penalty
     (cond (c-s
            ;;
            (score-max
             ;; e.g. 樹 叫人 砍了 --> verb-noun(砍, 樹)
             ;; subj-verb(人, 砍) is already checked in adv-verb
             (verb-noun v n)
             ;; e.g. 我 叫人 打他 --> subj-verb(我, 叫),
             ;; subj-verb(人, 打) is already checked in adv-verb
             (subj-verb-xx
              n
              (if (eq c-s :jiao4-teach)
                  *v-jiao4-teach*
                  *v-jiao4-ask*))))
           ;;
           ((trait-value v :verb-mod-gei3)
            ;; check :verb-mod-gei3 still in effect, not just present.
            ;; "給 verb-p" used alone, treated as passive-voice
            ;; note: this is checked after verb-mod-change-subject, so ignored if change subject
            (verb-noun v n))
           ;;
           (t (subj-verb-xx n v))))
    ))

;;;;
(defgeneric subj-verb-noun-x (v s n))
(def-comb subj-verb-noun-x
  ;; default is to fall back on subj-verb
  (((v t) (s t) t) (subj-verb s v))
  ;;
  ;; '燃煤 「贡献」 40%的PM2.5'
  ((vc-cause fuel-like-ore pollution-gas) *c-close-suffix*)
  ((v-gong4-xian4 fuel-like-ore pollution-gas) *c-close-suffix*)

  ;; e.g. '冬天 过去' not usually followed by place
  ((v-guo5-qu4 time-name place) *c-common*)
  ((v-guo5-qu4 time-name dir) *c-common*)
  ((v-lai2 time-name place) *c-common*)
  ((v-lai2 time-name dir) *c-common*)

  ;; '笼罩雾霾中 的 北京', '北京 笼罩 雾霾中'
  ;; '雾霾 笼罩 北京'
  ((v-long3-zhao4 place dir) *c-close-suffix*)
  ((v-long3-zhao4 place pollution-gas) *c-close-suffix*)
  ((v-long3-zhao4 place gas) *c-close-suffix*)
  ((v-long3-zhao4 gas place) *c-close-suffix*)
  ((v-long3-zhao4 pollution-gas place) *c-close-suffix*)
  
  ;; '(减少 煤炭依赖) (对环境 产生 积极影响)'
  ((v-chan3-sheng1 verb-n influence) *c-close-suffix*)
  ((v-chan3-sheng1 (s a-subj-pred) influence)
   ;; '(中国将重工业迁出北京、减少煤炭依赖) (对环境 产生 积极影响)'
   (+ (prefer-conn (a-subj-pred-pred s) 'verb-n *c-common*)
      *c-long-sep*))

  ;; XXX 创 纪录
  ;; e.g. '创纪录 的 逾11%成长'
  ((vc-create word-growth book-like-record) *c-close-suffix*)
  ((vc-create event book-like-record) *c-close-suffix*)
  ((vc-create amount-of book-like-record) *c-close-suffix*)
  ((vc-create degree-of book-like-record) *c-close-suffix*)
  ;; similar for '破 纪录'
  ((v-po4 word-growth book-like-record) *c-close-suffix*)
  ((v-po4 event book-like-record) *c-close-suffix*)
  ((v-po4 amount-of book-like-record) *c-close-suffix*)
  ((v-po4 degree-of book-like-record) *c-close-suffix*)
  ;;
  )

(defun subj-verb-noun (v s n)
    ;; verb, subject, object
  ;; for hopefully a small number of cases which need more specific
  ;; constraints relating the subject with the object through the
  ;; verb.
  (if (conn-thing-p s)
      (score-min (subj-verb-noun v (conn-thing-first s) n)
                 (subj-verb-noun v (conn-thing-second s) n))
      (subj-verb-noun-x v s n)))
;; end subj-verb-noun
;;;;

(defun subj-verb-xx (n v)
  ;; due to the default ordering of parameters for generic function,
  ;; we use a wrapper to handle some cases

  ;; check the penalty for traits first
  (let ((penalty 0))
    ;;
    (when (and (is-type-p v 'v-wait)
               (not (trait-value v 'has-non-passive-verb-mod)))
      ;; '等' is easily confused with etc, so penalize 'N 等 ...' when the '等' has no other adv
      (incf penalty *c-common*))
    (when (and (typep n 'place)
               (trait-value v 'verb-mod-place))
      ;; prefer the two place to be pieced together if possible.
      ;; sometimes prefer the second place to be subj instead of verb-mod.
      ;; increased the penalty from *c-close-suffix*
      (incf penalty *c2*))
    ;;
    (score-add
     penalty
     ;;
     (typecase v
       (combine-verb
        (score-max (subj-verb n (combine-verb-first v))
                   (subj-verb n (combine-verb-second v))))
       (conn-thing
        (score-min (subj-verb n (conn-thing-first v))
                   (subj-verb n (conn-thing-second v))))
       (passive-voice
        (verb-noun (passive-voice-verb v) n))
       (verb-n
        ;; Seems a binary subj-verb is not quite adequate in constraining many cases.
        
        ;; e.g. '燃煤 「贡献」 40%的PM2.5', where we do not want
        ;; subj-verb('燃煤', '贡献') in many cases, but want subj-verb('
        ;; 燃煤', '贡献', 'PM2.5') and some other pollutants.
        (subj-verb-noun (verb-n-verb v) n (verb-n-obj v)))
       (verb0
        (subj-verb n (verb0-verb v)))
       (verb-bian4-adj
        ;; '變 + adj', it seems basically any subject is ok.
        ;; currently only require the adj to agree with the subject.
        (adj-noun (verb-bian4-adj-adj v) n))
       (v-beneficiary
        ;; for verb which we can say (V s o), sometimes it can also be
        ;; written as (o V s), e.g. '交給 他 任務' and '任務 交給 他'
        (verb-noun v n))
       (verb-close-supp
        ;; ignore, but since verb-close-supp uses combine-verb,
        ;; give high penalty
        *c-fallback*)
       (a-subj-pred
        ;; "N1 (N2 V)" case where the second is subj-pred, is better
        ;; handled as obj-pred
        *c-fallback*)
       ;; special cases, because they can be applied to many nouns
       (v-is
        (typecase n
          (time-name *c-prefer*)
          (pronoun-word-ji3 *c-common*) ;; 己 is often not used as subject
          (q-pronoun *c-prefer*)
          ;; TODO: checked consistency between A and B in 'A is B'? Especially when A is pred like?
          (de-noun-mod *c-close-sep*)
          (a-subj-pred *c-close-suffix*)
          (number *c-close-suffix*)

          (v-SP *c-common*)
          (v-VP *c-common*)
          ;;(v-geng4 *c-common*) ;; prefer 更 as adv, verb '更' already removed
          (v-ming2-c *c4*) ;; prefer '明明' or '明' as adv
          (adj-word-yao4 *c-rare*) ;; '要是' could be like an adv
          (verb *c-prefer*) ;; for VP 是 ..
          (noun *c-prefer*)
          (t *c-fallback*)
          ))
       (v-bian4-d *c-close-suffix*)
       (v-bian4-wei4-b *c-close-suffix*)
       (v-zhuan4-bian4-b *c-close-suffix*)
       (v-bian4-huan4-b *c-close-suffix*)
       ;;
       (t (if (conn-thing-p n)
              (score-add
               (min-over-conn
                v
                ;; want '报告 并 预测' as subj-pred, instead of verb-p as subj
                #'(lambda (x)
                    (not-prefer x 'verb0 *c-close-suffix*)))
               (score-min (subj-verb (conn-thing-first n) v)
                          (subj-verb (conn-thing-second n) v)))
              (subj-verb-x n v)))))
    ))

(defgeneric subj-verb-x (n v))
(def-comb subj-verb-x
  ;; default penalty to save us trouble
  ((t t) *c-unknown-pair*)
  ;;
  ((abstract verb) *c-common*)
  (((n abstract) (v verb0))
   (verb-noun v n))
  ;;
  ((creative-work-ind verb) *c-rare*)
  ((suo3-ind verb) *c-very-rare*)
  ((pronoun-zhi verb) *c-rare*) ;; "之" as pronoun is used in object, but not subject position
  ;;
  ((unit verb) *c-common*) ;; sometimes num-unit is used as a subject, with an implicit noun, e.g. 一個個
  ((noun verb) *c-fallback*)
  ((number verb) *c-fallback*)

  ;;
  (((n noun) (u unit))
   ;; when num-unit is pred.
   (noun-unit n u))
  (((n number) (u unit))
   ;; when num-unit is pred.
   (noun-unit n u))
  (((n num) (u unit))
   ;; when num-unit is pred.
   (noun-unit n u))
  (((n t) (u unit))
   ;; when num-unit is pred.
   (noun-unit n u))
  ;;
  ((human (n human))
   ;; e.g. '“蓝衣男”三十多岁，赵县人'
   (if (or (trait-value n 'place-or-race-human)
           (typep n 'occupation))
       *c-close-suffix*
       *c-common*))
  ;; "N1 (N2 V)" case where the second is subj-pred, is better handled
  ;; as obj-pred.
  ;; the penalty is given in subj-verb-xx
  
  ((noun adj) *c-common*)
  ((number adj) *c-common*)
  (((n non-verb-noun) (a adj))
   (+ (adj-noun a n) *c1*))
  (((n non-verb-noun) (a adj-has-de))
   (+ (adj-noun (adj-has-de-the-adj a) n) *c1*))
  (((n time-name) (v verb))
   (+ *c-rare*
      ;; time as 'at ...' is less likely as subject
      (if (has-trait n 'time-with-ind) *c-close-suffix* 0)
      ;; further penalty if the verb has time already
      (if (has-trait v 'verb-mod-time) *c-close-suffix* 0)))
  ((dynasty verb)
   ;; dynasty is more commonly the subject of subj-pred
   *c-common*)
  ((a-subj-pred verb) *c-fallback*) ;; only some verb would take a subj-pred as subject
  ;; e.g. '(美国国会 每年 就这个问题 进行辩论) 实际上只有对美国自身不利，影响美国商人的对华投资信心，从而也影响到美国人的就业机会'
  ((a-subj-pred v-bu2-li3) *c4*)
  ((a-subj-pred v-ying3-xiang3) *c4*)

  ((a-subj-pred (v adj-has-de))
   (if (trait-value (adj-has-de-the-adj v) 'adv-with-shi4)
       ;; e.g. '雾霾缓解 (只是 暂时的)'
       *c2*
       *c-unknown-pair*))
  
  ;; most verbs should have an animate object as subject
  ((animate verb) *c-prefer*)

  ;; organizations are usually used like an animate entity
  ((organization verb) *c-close-suffix*)
  ((word-place-center verb) *c-close-suffix*) ;; '中心' could mean a organization
  ((station verb) *c-close-suffix*) ;; 'XX 站' could also be an organization
  ((station-building-type-ind verb) *c-rare*) ;; prefer '站' alone as verb
  ((shop-like verb) *c-close-suffix*)
  ((money-related-organization-short-hand verb) *c-close-suffix*)
  ;; also country, province, county, city, village
  ;; these are often used like an animate entity
  ((country verb) *c-close-suffix*)
  ((province verb) *c-close-suffix*)
  ((county verb) *c-close-suffix*)
  ((city-word-du1 verb) *c-rare*)
  ((city verb) *c-close-suffix*)
  ((village verb) *c-close-suffix*)
  ((district verb) *c-close-suffix*)
  ((district-ind verb) *c-common*)

  ((p-pronoun verb) *c-common*)
  ((q-pronoun verb) *c-common*)
  ((q-pronoun-human verb) *c-prefer*)
  ;; special cases
  ;; the v-is case is moved to subj-verb-xx
  ;;
  ((noun v-at) *c-prefer*)
  ((noun v-has) *c-prefer*)

  ((plant v-grow) *c-prefer*)
  ((place v-grow) *c-close-sep*) ;; e.g. 世界盡頭 長著 生命之樹

  ((abstract v-maintained) *c-prefer*)
  ((light v-zhao4-b) *c-prefer*) ;; 光 照
  ((light v-into) *c-prefer*) ;; 光 照進 花園
  ((physical v-into) *c-close-suffix*) ;; 一個金蘋果 掉進了 他的手裡
  ((animate-attribute v-goto) *c-prefer*) ;; 嫌疑 落到 ...

  (((s place) (v verb))
   (+
    (if (has-trait s 'place-with-ind) *c-common* 0)
    (if (has-trait v 'verb-mod-place)
        ;; already has place in verb-p, the subject should not be place
        *c-fallback*
        *c-rare*)))
  
  ((place v-pack-in) *c-prefer*) ;; 裡面 裝 什麼
  ((q-pronoun-place v-pack-in) *c-close-sep*)

  ((inanimate v-peng4) *c-close-suffix*) ;; e.g. 蛇肉剛碰到他的舌頭

  ((place v-fully-filled) *c-prefer*)
  ((q-pronoun-place v-fully-filled) *c-close-sep*)

  ((non-verb-noun v-fully-filled) *c-close-suffix*) ;; 她的心 便充滿了 對他的愛

  ((word-body-part-ti3 v-cai3-si3) *c-close-sep*) ;; 蹄子踩死...
  ((leg v-cai3-si3) *c-close-sep*)

  ((army-like-lu3 v-fei4-b) *c-common*) ;; prefer 旅費 as money

  ((word-method v-shake-off) *c-close-suffix*) ;; 這辦法擺脫自己厄運
  ((word-method v-xie4-jue2-a) *c-close-suffix*)

  ;; TODO: special handling of 'Noun 想 Verb-p'? to have 'Noun Verb-p' agreement?
  ((body-part v-want-to) *c-close-suffix*) ;; 嘴一張一張地想喝水

  ((plant v-chan2) *c-close-suffix*) ;; e.g. 讓蘆葦纏住了
  ((long-thin v-chan2) *c-close-suffix*)

  ((non-verb-noun v-lost-way) *c-close-suffix*) ;; e.g. noun 迷住了 noun

  ((wave v-tun1-mo4) *c-close-suffix*) ;; 浪濤把你吞沒
  ((wave v-tun1) *c-close-suffix*)

  ((job v-qu3-a) *c-close-suffix*) ;; 工作取得显著成绩

  ((army-like-word-general-ind verb) *c-rare*) ;; "將" used alone if often adverb or verb-mod

  ((arranged-event verb) *c-rare*) ;; prefer obj-pred form

  ;; e.g. 贸易额 大幅增长
  ;; TODO: classify verbs into classes, e.g. increase/decrease. But
  ;; substantial work needed
  ((amount-of v-zeng1-zhang3) *c-close-suffix*)
  ((amount-of v-zeng1-zhi4) *c-close-suffix*)
  ((amount-of v-zeng1-wei4) *c-close-suffix*)
  ((amount-of v-zeng1-duo1) *c-close-suffix*)
  ((amount-of v-zeng1-zhang4) *c-close-suffix*)
  ((amount-of v-zeng1-tian1) *c-close-suffix*)
  ((amount-of v-zeng1-dai4) *c-close-suffix*)
  ((amount-of v-zeng1-jia1) *c-close-suffix*)
  ((amount-of v-zeng1-jin4-b) *c-close-suffix*)
  ((amount-of v-zeng1) *c-close-suffix*)

  ((amount-of v-min3-ban4) *c-close-suffix*)
  ((amount-of v-min3-rui4) *c-close-suffix*)
  ((amount-of v-min3-sun3) *c-close-suffix*)
  ((amount-of v-min3-di1) *c-close-suffix*)
  ((amount-of v-min3-wei4) *c-close-suffix*)
  ((amount-of v-min3-shao4-a) *c-close-suffix*)
  ((amount-of v-min3-b) *c-close-suffix*)
  ((amount-of v-min3-shao4-b) *c-close-suffix*)

  ((abstract-dao4 v-li3-yong4) *c-close-suffix*)

  ;; also appear in subj-verb-xx because dispatch on the first argument
  ((noun v-bian4-d) *c-close-suffix*)
  ((noun v-bian4-wei4-b) *c-close-suffix*)
  ((noun v-zhuan4-bian4-b) *c-close-suffix*)
  ((noun v-bian4-huan4-b) *c-close-suffix*)
  
  ((abstract v-bian4-geng4) *c-close-suffix*)
  ((abstract v-bian4-qian1) *c-close-suffix*)

  ((organization v-bian4-ge2) *c-close-suffix*)
  ((country v-bian4-ge2) *c-close-suffix*)

  ((abstract v-bian4-zhi4) *c-close-suffix*)
  ((inanimate v-bian4-zhi4) *c-common*)
  ((edible v-bian4-zhi4) *c-close-suffix*)
  ((drink v-bian4-zhi4) *c-close-suffix*)

  ((inanimate v-bian4-wei4-a) *c-common*)
  ((edible v-bian4-wei4-a) *c-close-suffix*)
  ((drink v-bian4-wei4-a) *c-close-suffix*)

  ((abstract v-dao4-d) *c-rare*) ;; don't want '(企业 产权 转让) 道'
  ((transaction v-dao4-d) *c-very-rare*) ;; don't want '(企业 产权 转让) 道'

  ((information v-biao3-ming2) *c-close-suffix*) ;; '资料 表明'
  ((information v-xian3-shi4) *c-close-suffix*) ;; '资料 显示'

  ((amount-of v-da5) *c-close-suffix*) ;; '數量 达'
  ((amount-of v-da5-dao4) *c-close-suffix*)
  ((number-of v-da5) *c-close-suffix*)
  ((number-of v-da5-dao4) *c-close-suffix*)

  ((transaction v-da5) *c-close-suffix*) ;; '出口 已 达 一亿多美元'
  ((transaction v-da5-dao4) *c-close-suffix*)

  ((army-like-lu3-ind verbs) *c-common*) ;; want '破冰 之 旅' as trip

  ((industry v-qian1-a) *c-close-suffix*)
  ((industry v-yi2-d) *c-close-suffix*)

  ((place v-hua2-gui1) *c-close-suffix*) ;; e.g. '区域 划归 重庆市管辖'
  ((region v-hua2-gui1) *c-close-suffix*) ;; e.g. '区域 划归 重庆市管辖'

  ((posed-question v-jin4-xing2) *c-very-rare*) ;; not want '这个问题 进行辩论'

  ((word-zi4-wo3 verb) *c-common*) ;; prefer '自我' as adv to form verb-x

  ((animate v-di1-xing3) *c-long-sep*) ;; '民警 提醒 ...', want it to be at outer scope

  ((place-with-dir v-wei4-yu2-b) *c-close-suffix*)
  ((place v-wei4-yu2-b) *c-close-suffix*)
  ((building v-wei4-yu2-b) *c-close-suffix*)

  ((pronoun-word-ji3 verb) *c-common*) ;; '己' not usually used as subject

  ((a-subj-pred v-bu2-ru2) *c-close-suffix*) ;; '多一事 不如 少一事'
  ((verb-n v-bu2-ru2) *c-close-suffix*) ;; '多一事 不如 少一事'
  ((verb v-bu2-ru2) *c-close-suffix*) ;; '多一事 不如 少一事'
  
  ((country-abbr-singapore v-bu2-ru2) *c3*) ;; '新 不如 舊'
  ((adj-word-new v-bu2-ru2) *c-close-suffix*) ;; '新 不如 舊'

  ((surname t) *c-rare*) ;; do not want '赵县人' split into '赵 (县人)' with '赵' as subject

  ((pollution-gas v-si4-nue4) *c-close-sep*) ;; 雾霾 肆虐
  ((disease v-si4-nue4) *c-close-sep*)
  ((disaster v-si4-nue4) *c-close-suffix*)

  ((pollution-gas v-ying3-xiang3) *c-close-sep*) ;; 雾霾 影响
  ((disease v-ying3-xiang3) *c-close-sep*)
  ((disaster v-ying3-xiang3) *c-close-suffix*)
  ((abstract v-ying3-xiang3) *c-close-suffix*)

  ((animate v-jian4-kang1) *c3*) ;; prefer noun '民众 健康'

  ((animate v-duan3-shou4) *c-close-sep*)
  ((animate v-zhang3-shou4) *c-close-sep*)

  ;; many attributes (animate-attribute and inanimate-attribute) can be quantified, and used on verbs that are about amounts

  ;; NOTE: added one verb class for (roughly) quanity increase or decrease. May classify other verb classes for more succinct constraints.

  ;; TODO: to classify nouns into quantifiable or not
  ((animate-attribute vc-quantity-more-less) *c-close-sep*) ;; 寿命 减少, etc
  ((inanimate-attribute vc-quantity-more-less) *c-close-sep*) ;; 寿命 减少, etc
  ((life-expectancy vc-lengthen-shorten) *c-close-sep*) ;; 寿命 延长
  ((degree-of vc-quantity-more-less) *c-close-suffix*) ;; '平均浓度 下降 逾1/5'

  ((degree-of v-gao1-chu1) *c-close-suffix*) ;; '污染程度高出46%'
  ((amount-of v-gao1-chu1) *c-close-suffix*)
  ((life-expectancy v-gao1-chu1) *c-close-sep*)

  ;;((v-geng4 verb) *c-common*) ;; prefer 更 as adv, verb '更' already removed

  ((word-research v-zhi5) *c-close-sep*) ;; '研究 指出'

  ((gas v-lai2) *c-close-suffix*) ;; 重雾霾 又来了

  ((abstract v-gei3-li4) *c-close-suffix*) ;; 气象条件 尤其是风力 不给力

  ((pollution-gas verb) *c-common*) ;; '雾霾' etc are often used like animate
  ((pollution-gas vc-other-attack) *c-close-suffix*) ;; 雾霾 来袭
  ((pollution-gas v-juan4-tu3-chong2-lai2) *c-close-suffix*) ;; 阴霾再度卷土重来
  ((disaster vc-other-attack) *c-close-suffix*)
  ((natural-phenomena vc-other-attack) *c-close-suffix*)

  ((news v-say) *c-close-suffix*) ;; '报道 说'
  ((news v-cheng1-b) *c-close-suffix*)
  ((information v-cheng1-b) *c-close-suffix*) ;; '消息 称'

  ((pollution-gas v-long3-zhao4) *c-close-suffix*)

  ((amount-of v-bu2-zu2) *c-close-suffix*) ;; '局地能见度 不足50米'

  ((province-name v-sheng3) *c-rare*) ;; prefer it as a province, e.g. '山东 省'

  ((place v-fa5-sheng1-b) *c-close-suffix*)
  ((street v-fa5-sheng1-b) *c-close-suffix*) ;; '高速公路发生...'
  ((road v-fa5-sheng1-b) *c-close-suffix*)

  ;; basically all inanimate could collide with things
  ((inanimate vc-collide) *c1*)
  ;; but some things are more often used on colliding verbs
  ((transportation vc-collide) *c-close-suffix*)

  ((transportation v-zhui1-wei3) *c1*)
  ((wheeled v-zhui1-wei3) *c-close-suffix*)
  ((train-like v-zhui1-wei3) *c-long-sep*) ;; often used on trains

  ((abstract-matter vc-cause) *c-close-suffix*) ;; '交通事故 已致1人死亡'
  ((reason vc-cause) *c-close-suffix*) ;; '(这一因素 造成的空气污染) 导致36.6万人过早死亡'
  ((word-pollution vc-cause) *c-close-suffix*) ;; '(这一因素 造成的空气污染) 导致36.6万人过早死亡'

  ((abstract v-for) *c-close-suffix*) ;; '為' to have meaning similar to 'is'
  ((inanimate v-for) *c-close-suffix*) ;; '為' to have meaning similar to 'is'

  ((economy-can-be-job v-tui1-dong4) *c-close-suffix*) ;; '经济 推动 ...'
  ((economy-can-be-job adj-word-di1-mi2) *c-close-suffix*) ;; 经济 低迷
  ((economy-can-be-job v-huo3-fei1) *c-close-suffix*)
  ((economy-can-be-job v-shang5-yang2) *c-close-suffix*)

  ((number-of v-pai2-ming2) *c-close-suffix*) ;; '死亡人数 排名全球第一'
  ((number-of-ind v-pai2-ming2) *c-common*) ;; not prefer '数 排名全球第一'
  ((number-of v-chao2) *c-close-suffix*) ;; '死亡人数 远超仅次印度的62万人'
  ((number-of-ind v-chao2) *c-common*) ;; '死亡人数 远超仅次印度的62万人'

  ((place v-si3-wang2) *c3*)
  ((country v-si3-wang2) *c3*)

  ((event v-zhen4-jing3) *c-close-suffix*) ;; '事件 震惊中外'

  ((info-report v-yu4-ce4-b) *c-long-sep*) ;; '报告 并预测'

  ((time-name v-guo5-qu4) *c-close-suffix*) ;; '冬天 过去'
  ((time-name v-lai2) *c-close-suffix*) ;; '冬天 來了'

  ((animate v-long3-zhao4) *c-common*)

  ((dt-demonstrative v-xian3-shi4) *c-close-suffix*) ;; '这 显示...'

  ((time-period-num-ind-day v-zeng1-duo1) *c-close-suffix*) ;; '雾霾天 增多'

  ((cost-or-budget vc-cause) *c-close-suffix*) ;; '支出 导致 ...'

  ;; TODO: whether to put in subj-verb-noun?
  ((arranged-event v-zhao4-kai1) *c-close-suffix*) ;; '中共第19次全国代表大会 召开'

  ((amount-of (v v-chu1-xian4-b))
   ;; e.g. '煤炭消耗量 出现 3年来首次成长'
   (if (trait-value v 'verb-vp)
       *c-close-suffix*
       *c-common*))

  ((release-of v-liang5) *c-rare*) ;; want '排放 量' as noun

  ((situation v-xia4-c) *c-common*) ;; want '通常情况 下' as place-p as verb-mod

  ;; not want '(建筑、钢铁和水泥生产等重工业) 活动' as subj-pred
  ((industry v-huo2-dong4) *c-rare*)
  ((word-building-jian4-zhu2 v-huo2-dong4) *c-rare*)
  ((production v-huo2-dong4) *c-rare*)

  ((pollution-gas v-huan3-xie4) *c3*) ;; '雾霾 缓解'

  ((steps-word-cuo4-shi1 v-qu3-xiao1) *c-close-suffix*) ;; '措施 取消'

  ((website v-xian3-shi4) *c-close-suffix*) ;; '网站 显示'

  ((adj-word-yao4 verb) *c-rare*) ;; '要' is also a verb, and may act on a verb-p

  ((help-verb-hui4 v-has) *c-very-rare*) ;; prefer '仍 (会 (有...))'
  (((n verb0) v-has)
   ;; prefer '仍 (会 (有...))'
   (not-prefer-verb n 'help-verb-hui4))

  ((natural-power v-bu2-li3-yu2-b) *c-close-suffix*) ;; '风力 不利于 污染扩散'
  ((natural-power v-bu2-li3) *c-close-suffix*) ;; '风力 不利于 污染扩散'
  )
;; end subj-verb-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-conn verb-verb (v1 v2) verb-verb-x)
(defgeneric verb-verb-x (v1 v2))
(def-comb verb-verb-x
  ((t t) *c-ignore*)
  ;; default allow union of verbs
  ((verb verb) (+ *c-close-sep*
                  *c-close-verb-verb*))
  ;;
  ((v-gei3 v-give) *c-close-suffix*)
  ;;
  ((verb v-at) *c-fallback*)
  ((v-movement v-at) *c-close-suffix*)

  ;; *+书, e.g. 借书
  ((verb v-shu1-a) *c-rare*)
  ;; *+弯, e.g. 急弯
  ((verb v-wan1) *c-rare*)
  ;; 燃料
  ((v-ran2-a v-liao4) *c-rare*)
  ;; 利润
  ((v-li3-b v-run4) *c-rare*)
  ;; V + 熱, prefer 熱 as trend
  ((verb v-re4) *c-rare*)
  ;; V + 照, prefer 照 as photo
  ((verb v-zhao4-b) *c-rare*)
  ;; 防 + 检
  ((v-fang2 v-jian3-b) *c-close-suffix*)
  ;; 印 + 染
  ((v-yin4 v-ran3) *c-close-suffix*)
  ((v-ran3 v-yin4) *c-close-suffix*)
  
  ((v-jue2-d v-suan4-shu3) *c-fallback*) ;; prefer 决算 数 to 决 算数

  ((verb v-shang4) *c4*) ;; prefer *+上 if possible
  ((verb v-xia4-c) *c4*) ;; prefer *+下 if possible

  ((verb v-bao5) *c-common*) ;; prefer 包 as bag

  ((v-fang2 verb) *c-close-suffix*) ;; e.g. 防晒
  ((v-fang2 v-zhang3-b) *c-common*) ;; prefer animate 防长

  ((v-shang4 v-swim-in) *c-common*) ;; prefer place 上,下游
  ((v-xia4-c v-swim-in) *c-common*)

  ((verb v-xin4) *c-rare*) ;; prefer noun 證明+信

  ((verb v-tong2-bu4) *c-common*) ;; e.g. 建设同步

  ((v-dui4-wai4 verb) *c-fallback*) ;; e.g. 对外 开放

  ((v-shi4-wei4-a verb) *c-very-rare*) ;; e.g. 是为 进入

  ((verb v-liang5) *c-rare*) ;; e.g. want amount-of in "载运 量"

  ((v-dun1 v-jump) *c-close-sep*) ;; 蹲跳

  ((v-fa5-zhan3 v-yuan2-zhu4) *c-close-sep*)

  ((v-xing4-nue4-dai4 v-shou4-hai4) *c-common*) ;; want '性虐待 受害人', not '(性虐待 受害) 人'

  ((v-shu1-c v-gong1-c) *c-close-sep*) ;; 输供电
  ((v-gong1-c v-shu1-c) *c-close-sep*) ;; 输供电
  ((v-gong1-c v-gong1-dian4) *c-close-sep*) ;; 输供电

  ((verbs help-verb-hui4) *c-common*) ;; '会' easily confused with organization or meeting

  ((verbs v-dian3) *c-rare*) ;; 起点

  ((v-ji4-gai3 v-ban1-qian1) *c-close-sep*) ;; 技改 搬迁 
  ((v-ban1-qian1 v-ji4-gai3) *c-close-sep*) ;; 搬迁 技改

  ((v-jin4-xing2 verb) *c-common*) ;; prefer verb-c for '进行'

  ((v-xiang1-guan1 verb) *c-rare*) ;; prefer '相关' used like adj in mod

  ((adj verb) *c-common*) ;; prefer the adj as adv

  ((verb adj-word-shui3-ping2) *c-common*) ;; prefer '水平' as noun

  ((v-wu1-ran3 t) *c-common*) ;; prefer '污染' as noun

  ((verb adj-word-chief) *c-rare*) ;; prefer '监测 (总 站)' to '(监测 总) 站'

  ((verb adj-time-period) *c-rare*) ;; prefer '工作 日' as time

  ((v-chao2 v-jin3-ci4-yu2) *c-rare*) ;; not prefer '(超 仅次) (印度的62万人)'

  ((v-chuang4 v-ji4-lu4-a) *c-rare*) ;; prefer '创 纪录' as verb-noun

  ((v-dang1 verb) *c-common*) ;; '当' could be used as conditional, meaning 'when', e.g. '当 (扩散条件 自北向南逐步改善)'
  ) ;; end verb-verb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric verb-supp-tag (v s))
(def-comb verb-supp-tag
  (((v t) t) v) ;; default case
  ((verb (s v-give)) s) ;; TODO: combine-verb?
  (((v verb) (s v-for))
   ;; TODO: whether to introduce a new type similar to combine-verb, but use the second verb for 'verb-noun', and use the first verb for :BA?
   ;; e.g. '升级 为'
   (make-combine-verb :first v :second s))
  (((v v-movement) (s v-goto)) s)
  (((v verb) (s v-goto))
   (make-combine-verb :first v :second s))
  (((v verb) (s verb-close-supp-mark-place))
   (make-combine-verb :first v :second s))
  ;;
  ;;(((v verb) (s v-dir-supp)) s)
  ;;(((v v-movement) (s v-dir-O)) s)
  (((v verb) (s dir-verbs))
   (make-combine-verb :first v :second s))
  (((v verb) (s verb-close-supp-cheng2))
   ;; e.g. '变 成'
   (make-combine-verb :first v :second s))
  (((v verb) (s v-at))
   ;; e.g. '写 在', the '在' to indicate the place
   (make-combine-verb :first v :second s))
  (((v v-listen) v-goto) v) ;; 聽到 should not take place as noun
  (((v verb) (s combine-verb))
   (make-combine-verb :first v :second s))
  ;;
  ;; e.g. (V der adj) usually does not further take object
  ;; But for some V and adj combination, they can further take object.
  ;; Do it case-by-case, may add other cases in the future.
  ;; Default wrap as verb0 and trait 'verb-supp-adj so there would be penalty
  (((v verb) (s adj))
   (more-trait-to (make-instance 'verb0 :verb v)
                  'verb-supp-adj s))
  ;; special case '變 + adj', see verb-bian4-adj
  ((v-bian4-d (s adj))
   (make-instance 'verb-bian4-adj :adj s))
  ;;
  ;; for some valid cases, only trait 'verb-supp-adj, so no penalty
  ;; '講, 說, 聽, 想 ...' on '清楚, 明白 ...'
  (((v verb) adj-word-qing1-chu3)
   (more-trait v 'verb-supp-adj t))
  (((v verb) adj-word-ming2-bai2)
   (more-trait v 'verb-supp-adj t))
  ;; '吃, 掃, 洗 ...' on '乾淨 ...'
  (((v verb) adj-word-gan1-jing4)
   (more-trait v 'verb-supp-adj t))
  ;; '放' on '整齊'
  (((v verb) adj-word-zheng3-qi2)
   (more-trait v 'verb-supp-adj t))
  ;;
  )

(defun verb-supp-tag-trait (v s trait)
  (more-trait (verb-supp-tag v s) trait t))

(defun-conn verb-supp (v s) verb-supp-x)
(defgeneric verb-supp-x (v s))
(def-comb verb-supp-x
  ((t t) *c-ignore*)
  ;;
  ((verb adj) *c-common*)
  ((v-chong2 adj-word-new) *c-rare*) ;; prefer the adv 重新
  ((v-jiu4-b t) *c-rare*) ;; prefer 就 as adv
  ;;
  ((verb verb-close-supp-dong4) *c-common*) ;; 動 is quite specific, usually one piece
  ((v-movement verb-close-supp-dong4) *c-close-suffix*) ;; 動 marks movement
  ;;
  ((verb verb-close-supp-yi3) *c-common*) ;; 以 is quite specific, usually one piece
  ;;
  ((v-lock v-shang4) (- *c-close-suffix* *c-close-sep*))
  ((v-guan1-b v-shang4) (- *c-close-suffix* *c-close-sep*))
  ;;
  ((verb dir-verbs) *c-long-sep*)
  ((v-movement dir-verbs) *c-close-sep*)
  ((help-verb dir-verbs) *c-rare*)
  ((verb verb-close-supp) *c-close-sep*)
  ((verb v-goto) *c-close-sep*)
  ((verb v-give) *c-close-sep*)
  ((verb v-for) *c-close-sep*)
  ((verb v-at) *c-long-sep*)

  (((v verb) (s verb)) (verb-verb v s))
  ;;
  ((v-qing3-qiu2 v-give) *c-fallback*) ;; separate 請求 給

  ((v-yi3 v-to) *c-rare*) ;; separate 以去

  ((v-qu3-a v-der) *c-close-suffix*) ;; 取得
  ((v-qu3-b v-der) *c-close-suffix*) ;; 娶得
  ((v-huo4-b v-der) *c-close-suffix*) ;; 获得
  ((v-qiu2-b v-der) *c-close-suffix*) ;; 求得
  ((v-ying2-c v-der) *c-close-suffix*) ;; 赢得
  ;;
  ((verb verb-close-supp-dong3) *c-common*)
  ((V-WAN2-B VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ((V-SEE VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ((V-LISTEN VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ((V-WANT-TO VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ((V-XUE2 VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ((V-JIAO1-C VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ((V-NONG5 VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ((V-GAO3 VERB-CLOSE-SUPP-DONG3) *C-CLOSE-SUFFIX*)
  ;;
  ((verb verb-close-supp-zuo4) *c-rare*) ;; currently prefer one piece verb

  ((v-zu1-lin4 verb-close-supp-xiang4) *c-rare*) ;; not prefer "(租赁) 向"

  ((verb help-verb-hui4) *c-common*) ;; '会' easily confused with organization or meeting

  ((v-wan3-b v-shang4) *c3*) ;; '晚上' could be time
  ((v-wan3-b v-shang5-lai2) *c4*) ;; prefer '晚上 来' as 'time + verb' to '晚 上来' as 'verb + supp'

  ((v-zeng1-qiang3 verb-close-supp-from-to) *c3*) ;; prefer '增强 自我 ...' to '(增强 自) 我 ...'

  ((v-ji3-a adj) *c-rare*) ;; not want '经 (济 低迷)'

  ((v-huan3-xie4 (s adj))
   ;; not want '缓解 (或许只是暂时)' as verb with supp
   (+ *c-rare* (not-prefer-trait s 'adv-with-shi4)))
  )
;; end verb-supp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-conn verb-adj-supp (v a) verb-adj-supp-x)
(defgeneric verb-adj-supp-x (v a))
(def-comb verb-adj-supp-x
  ;; used mainly in 'verb-c -> verb noun-p adj-p' rule for verb and adj-p
  ;; seems not many pairs of verb and adj-p would be needed, so add as needed.
  ((t t) *c-common*)
  ;;
  )
;; end verb-adj-supp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to encourage the 'sizes' goes from large to small when combining time-periods
;; Note: seems like time-period-period does not need to handle conn-thing, change when appropriate
(defgeneric time-period-period (p1 p2))
(def-comb time-period-period
  ((t t) *c4*)
  ;; mostly from large to small
  ((time-century time-year) *c-long-sep*)
  ((time-year time-month) *c-long-sep*)
  ((time-month time-day) *c-long-sep*)
  ((time-day time-within-day) *c-long-sep*)
  ((time-day time-hour) *c-long-sep*)
  ((time-within-day time-hour) *c-long-sep*)
  ((time-hour time-minute) *c-long-sep*)
  ((time-minute time-second) *c-long-sep*)
  )
;; end time-period-period

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
