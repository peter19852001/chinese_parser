(in-package :chinese-parser)

(defconstant +priority-scale+ 10)
(defconstant +sub-scale+ 2)

(defmacro def-priority (&body vars)
  ;; for defining a bunch of priority values.
  ;; starting from 0, each entry has an amount added to the previous one
  ;;
  ;; vars is a list, each entry can be (v1 ... vm amount), where the
  ;; last item in the list is the amount, and the previous items are
  ;; the variable names receiving the same value.
  ;; an entry can also be a single symbol, in which case it is equivalent to (v1 0)
  (let ((vs nil)
        (prev-sym 0))
    (dolist (v vars)
      (cond ((symbolp v)
             (push `(defparameter ,v ,prev-sym) vs)
             (setf prev-sym v))
            ((listp v)
             (let* ((amt (last v))
                    (amount (car amt)))
               (do ((ls v (cdr ls)))
                   ((eq ls amt))
                 (push `(defparameter ,(car ls) (+ ,prev-sym
                                                   (* +priority-scale+
                                                      ,amount)))
                       vs))
               (setf prev-sym (car v))))
            (t (error "Expect variable entry, but got ~A" v))))
    `(progn ,@(nreverse vs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;; for more easily tune the penalties of different grammar rules
(def-priority
  ;; preference of different part of speech from raw word list
  (*c-fraction-half*
   ;; set this to be higher so that the following POS types are that different
   -15)
  (*c-dir* *c-adv* *c-front-adv*
   *c-conn* *c-exclaim* *c-prep* 
   *c-abstract-suffix*
   *c-human-suffix* *c-human-prefix*
   *c-noun-suffix* *c-noun-ind*
   *c-unit* *c-dir-suffix* *c-num*
   *c-serial* *c-double-char*
   *c-verb-supp*
   -1)
  ;; 
  (*c-time* *c-decimal*
   *c-pronoun* *c-q-pronoun* *c-p-pronoun*
   *c-animate* *c-inanimate*
   *c-abstract* *c-place*
   *c-adj* *c-verb* *c-dir-verb* *c-help-verb*
   -1)
  ;; 
  (*c-pre-approx*
   -1)
  ;; 
  (*c-surname*
   *c-whole-name-as-animate*
   *c-human-name*
   *c-Eng-word*
   -1)
  (*c-sound*
   *c-dash*
   -1)
  ;; for one character
  (*c-likely-name* -14)
  (*c-unlikely-name* -5)
  )

(def-priority
  ;; use unknown as different part of speech (only some are allowed)
  ;; TODO: this seems very inefficient for the current parser, need to rethink
  (*c-name-as-place* -4)
  (*c-unknown-as-noun* -4)
  (*c-unknown-as-adj* -1)
  (*c-unknown-as-verb* -1)
  (*c-unknown-as-adv* -1)
  (*c-unknown-as-front-adv* -1)
  )

(def-priority
  ;; turn things into noun and noun-p
  ;; contract these
  (*c-pronoun-as-noun*
   *c-animate-as-noun*
   *c-inanimate-as-noun*
   *c-place-as-noun*
   *c-time-as-noun*
   *c-abstract-as-noun*
   -1)
  
  (*c-animate-as-noun-p*
   *c-num-as-noun*
   *c-inanimate-as-noun-p*
   *c-abstract-as-noun-p*
   *c-noun-p-noun*
   *c-place-as-noun-p*
   *c-verb-as-place*
   *c-noun-as-noun-p*
   *c-p-pronoun-as-noun*
   -1)
  (*c-noun-de-noun-p*
   *c-pronoun-noun*
   *c-noun-mod-as-noun-p*
   *c-num-unit-as-noun-p*
   -1) ;;
  (*c-adj-p-as-noun*
   -2)
  (*c-verb-as-noun*
   -1)
  ;;
  (*c-subj-pred-or-verb-p-as-subject*
   -9)
  ;;
  (*c-clause-as-noun-p* -21)
  )

(def-priority
  ;; different things to received dir-suffix to become a place
  (*c-inanimate-as-noun-place* -3)
  (*c-place-as-noun-place* -1)
  (*c-noun-as-noun-place* -1)
  )

(def-priority
  ;; different preference of noun-mod, and verb-mod
  (*c-adj-as-noun-mod*
   *c-dui-pronoun-as-noun-mod*
   -1)
  (*c-adj-p-as-noun-mod*
   *c-dui-noun-p-as-noun-mod*
   -1)
  ;; note that *c-verb-p-as-noun-mod* need to be tuned with
  ;; *c-verb-noun* to prevent verb-p from turning into nouns through
  ;; noun-mod
  (*c-noun-p-as-noun-mod*
   *c-adj-p-as-noun-mod-zhi*
   *c-place-as-clause-front*
   -1) ;;
  (*c-verb-x-as-noun-mod*
   *c-subj-pred-as-noun-mod*
   -1)
  (*c-noun-p-as-mod*
   *c-verb-p-as-noun-mod-zhi*
   *c-verb-mod-as-noun-mod-zhi*
   *c-time-as-verb-mod*
   *c-place-as-verb-mod*
   -2) ;;
  (*c-verb-mod-front-as-clause-front*
   -3)
  (*c-verb-p-as-noun-mod*
   -7)
  )

(def-priority
  ;; close combination of word types
  (*c-ignore* 0)
  (*c-prefer* 0)

  ;; TODO: may need to start with a larger penalty so that the relative differences between the following is small
  (*c-close-sep* ;; used to +/- to base penalty for small adjustment
   *c-surname-mr*
   *c-verb-x-mod-noun*
   -1)
  (*c-adj-adj*
   *c-adj-mod-adj-p*
   *c-adj-as-verb*
   *c-long-sep*
   *c-verb-x-noun-mod-noun*
   -2)

  (*c0*
   *c-verb-mod-verb-p*
   *c-close-suffix*
   *c-name-close-title*
   *c-adj-as-adv*
   *c-time-place-subj-pred*
   -2)
  (*c0-1*
   *c-short-conn*
   -1)
  (*c1*
   *c-time-far-combine*
   *c-long-conn*
   *c-name-title*
   -1)
  (*c1-2*
   *c-title-name*
   -1)
  (*c2* -1)
  (*c2-3*
   *c-at-to-time*
   -1)
  (*c3*
   *c-close-verb-verb*
   *c-place-no-ind-as-verb-mod*
   -1)
  (*c3-4* -1)
  (*c4* -1)
  ;;
  (*c-short-close-suffix*
   *c-surname-name*
   *c-num-range*
   -2)
  (*c-pronoun-animate*
   *c-approx-num-range*
   *c-dynasty-animate*
   *c-inanimate-suffix*
   *c-common*
   -1)
  (*c-approx-int-int*
   -1)
  (*c-adj-adv-supp*
   -1)
  (*c-job-animate* -1)
  (*c-rare* -1)
  (*c-attribute-zhe3*
   *c-noun-mod-noun-p*
   -1)
  (*c-very-rare* -2)
  (*c-unknown-pair* -10)
  (*c-fallback* -10)
  )

(defparameter *c-one-prep* (- *c-prep* *c-close-sep*))
(defparameter *c-two-prep* (- (+ *c-prep* *c-prep*)
                              *c-close-sep*))

(def-priority
  ;; preference of pred
  (*c-subj-pred-as-pred* -3)
  (*c-adj-p-as-pred* -3)
  ;; obj-pred penalty has to be severe enough to avoid many unwanted
  ;; 'noun verb-p' phrases be recognized as obj-pred
  (*c-obj-pred-as-pred* -7)
  )

(def-priority
  ;; other conversions
  (*c-dir-verb-as-verb*
   *c-help-verb-as-verb*
   *c-place-ind-as-verb*
   *c-front-adv-subj-pred-as-clause*
   -2) ;;
  (*c-subj-pred-as-clause* -1)
  (*c-noun-p-as-pred* -14)
  (*c-noun-p-as-clause* -4)
  )
;;;
(def-priority
  ;; verb supplements level 0
  *c-close-suffix-as-verb-close-supp*
  *c-dir-verb-as-verb-close-supp* ;; 0
  (*c-verb-as-verb-close-supp* -1) ;; -1
  (*c-verb-close* -1)
  (*c-place-ind-place* -1) ;;
  (*c-adj-as-verb-close-supp* -1)
  )

(def-priority
  ;; verb supplements level 1
  (*c-verb-then-verb-p* *c-verb-animate-noun-p* -1)
  (*c-verb-noun* *c-help-verb-verb-p-de* -1)
  (*c-verb-noun-verb-p* *c-verb-OP* -2) ;;
  (*c-verb-subj-pred* -1)
  (*c-verb-and-verb-p* -3)
  (*c-verb-noun-pred* -1)
  (*c-de-verb-p* -6)
  (*c-verb-verb-p* -2) ;; need to be servere enough to prevent "verb noun-p" to somehow become "verb verb-p" by shifting verbs
  (*c-verb-de-verb-p* -1) ;;
  )

(def-priority
  ;; verb supplements level 2
  (*c-verb-inner-supp* -2) ;; -2
  (*c-verb-outer-supp* -1)
  )

(def-priority
  ;; for until pred
  (*c-until-clause-upto* -1)
  (*c-until-clause* -1)
  (*c-clause-upto* -1)
  )

(def-priority
  ;; for noun-mod for time
  (*c-noun-mod-period* *c-noun-mod-pt* -1)
  (*c-noun-mod-close-period* -2)
  (*c-noun-mod-close-pt* -1)
  (*c-noun-mod-far-pt* -1)
  )
(def-priority
  ;; number used in absolute time point
  (*c-integer-as-time-num* *c-digits-as-integer* -1)
  (*c-digits-as-time-num* -2)
  (*c-period-no-num* -3)
  (*c-period-ge-no-num* -1)
  )
(def-priority
  ;; something attached with time suffix to become time point
  (*c-time-period-time-suffix* -1)
  (*c-time-pt-time-suffix*
   *c-event-time-suffix*
   *c-verb-p-time-suffix*
   -1)
  (*c-subj-pred-time-suffix* -7)
  (*c-clause-time-suffix* -1)
  )

(def-priority
  ;; verb-mod: 為 *** 而
  (*c-for-subj-pred-so* -1)
  (*c-for-noun-p-so* -1)
  )

(def-priority
  (*c-adv-time-pt* -3)
  )
;;;;;;;;;;;
(def-priority
  ;; preference of individual parts
  (*c-sentences-alone* -1)
  (*c-sentence-alone* -1)
  (*c-clauses-alone* -1)
  (*c-clause-alone* -1)
  (*c-noun-p-alone*
   *c-passage-footer-alone*
   -1)
  (*c-time-p-alone* -1)
  (*c-num-unit-alone* -1)
  (*c-ordinal-alone* -1)
  (*c-integer-alone* -1)

  (*c-pause-alone* *c-end-alone* -1)
  (*c-sep-alone* -1)

  (*c-dir-alone* -1)
  (*c-unit-alone* -1)
  (*c-pronoun-alone* -1)
  (*c-q-pronoun-alone* -1)
  (*c-p-pronoun-alone* -1)
  (*c-adv-alone* -1)
  (*c-conn-alone* -1)
  (*c-exclaim-alone* -1)
  (*c-sound-alone* -1)
  (*c-punctuation-alone* -1)
  (*c-prep-alone* -1)
  (*c-unknown-alone* -1)
  )
;;;
(def-priority
  ;;
  (*c-cont-unknown* -50)
  (*c-item* *c-start-unknown* -50) ;;
  (*c-null-grammar-word* *c-null-input-word* *c-mismatch* -80) ;;
  (*c-total-limit* -50000) ;; very large
  )
