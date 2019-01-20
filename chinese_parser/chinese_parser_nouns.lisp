(in-package :chinese-parser)

;; for sbcl when loading utf-8 encoded file
(setf sb-impl::*default-external-format* :utf-8)

;; Note: Some words are both verbs and nouns.

;; If a word is a single noun, it should be preferred in noun context
;; (e.g. noun phrase).

;; Otherwise, a single verb can be turned into a noun, which should
;; represent the action in noun form.

;; However, if the word is preferred as a noun (which means something
;; other than the action), but is composed of atomic parts, then it
;; will have a higher penalty than the single verb, then a single noun
;; should be created for the word.

;; nouns grouped into sub-groups: animate, inanimate, place, etc.

;; NOTE: some words are in more than one group.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; While basically all verbs is also noun, whose meaning is the action
;; itself, some words are both verbs and nouns, but with different yet
;; related meanings.
;; e.g. 關鍵: verb => '(關鍵 到) 國家未來'; noun => '是 (成功 的 關鍵)'.
;; e.g. 融資: verb => '(利用 資本市場) (直接 融資)'; noun => '獲 (巨額 境外 融資)'.
;; e.g. 貸款: verb => '(向 銀行) 貸款'; noun => '這筆 貸款'.

;; With limited resources, we could not categorize all the verbs
;; properly, so the verbs are only very coarsely categorized. Also,
;; with the current grammar and constraint functions, the noun form is
;; preferred, but the verb form would be more appropriate, e.g. '金融
;; 融资、信息传递、产业转换', where the parallel structure suggests
;; that all three are of the form 'noun verb', but '金融 融资' is
;; locally preferred as 'noun noun', leading to penalty in
;; similar-noun between '融资', '传递' and '转换', resulting in a
;; strange parse.

;; As a hack to remedy this, we would mark some nouns (currently only
;; abstracts) with 'also-verb-p' flag to indicate it has a related verb
;; form, and modify similar-noun to check for this accordingly.

;; NOTE: do this automatically, due to the large number of nouns, and
;; that the verbs list may be updated from time to time.
(def-word-class non-verb-noun (noun) ())
(def-word-class abstract (non-verb-noun)
  ((also-verb-p
    ;; this word is also a verb, with related meaning
    :initarg :also-verb-p
    :accessor also-verb-p
    :initform nil)
   ))

(def-word-class num (abstract)
  ((val
    ;; to hold the numerical value, if any
    :initarg :val
    :accessor num-val
    :initform nil)
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is a dummy list, to define some common classes, without
;; including words.
(def-tagged-words *noun-list* *noun-hash*
  ;; also define other main classes for convenient forward declaration
  (@ non-verb-noun
   (de-noun)
   (@ abstract
    (can-be-fang4 (study (work)))
    (can-be-job)
    (@ num (ordinal (ordinal-A-Z)) (rough-amount) (ask-amount))
    (cavity-of)
    (abstract-suffix (belief (belief-or-argument)))
    (past-record)
    (audible)
    (field-or-column)
    (resulting-effect)
    (money-related (cost-or-budget)
                   (currency))
    (sports)
    (event (game-like (competition))
           (argument)
           (disaster)
           (arranged-event (meal-like-event)))
    (organization (company ((company-name nr-name))
                           ))
    (a-sign (sign-signal))
    (evidence)
    (info-report)
    (written-character (character-style))
    (creative-work
     ((sound audible)
      (music))
     ((video audible))
     (article-like (chapter)
                   (legal-document))
     )
    (brand)
    (plan)
    (fallback-or-preparation)
    (long-thin (abstract-line))
    (foundation)
    (essence-of)
    (element-of)
    (abstract-heart)
    (aspect)
    (law
     ((chapter-or-law chapter)
      ;; * + 章 could mean a chapter-like of something, or some
      ;; regulations or laws.
      ;; 章 as a stamp is elsewhere.
      )
     )
    (product)
    (attribute
     (appearance ((appearance-ind ind)))
     (inanimate-attribute)))
   (physical
    (animate
     (human (occupation (army-like)))
     (god-like)
     (animal (bird)))
    (inanimate
     (edible)
     (body-part
      ((body-cavity cavity-of))
      )
     (star-like)
     (valuable-treasure)
     (paper-like
      ((book-like creative-work)
       ((book-like-record past-record)))
      )
     (road (street))
     (material (metal)
               (soil))
     (word-material)
     (hole)
     (physical-twist)
     (tool (container (large-container)))
     (beam-like)
     (furniture)
     (net-like)
     (tent-like)
     (layer-like)
     (wave)
     (plant-part)
     (fluid-like (smell))))
   (@ attribute
      (animate-attribute
       (reply)
       (martial-arts)
       (trace-of)
       (religion-like
        ((religion-name nr-name)
         ((religion-or-human human)))
        )))
   (@ currency
      ((metal-currency metal))
      )
   (@ abstract
      ((race human)
       ((race-name nr-name)))
      )
   (place (region)
          ((place-name nr-name)))
   (time-name)
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-word-class de-noun-mod (de-noun)
  ((pred ;; could be verb-p, or subj-pred, or more
    :initarg :pred
    :initform nil
    :reader de-noun-mod-pred)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-tagged-words *noun-suffix-list* *noun-suffix-hash*
  ;; these suffix are supplementary, and the tag for "noun + suffix" should
  ;; be that of the noun
  ((noun-suffix traited-word)
   (general-suffix
    ;; used in abstract, inanimate-attribute, animate-attribute,
    ;; inanimate, animate
    (ns-er2 "兒")
    (ns-zi3 "子")
    (ns-yu2 "餘")
    (ns-zhi1 "隻")
    (ns-xiang4 "項")
    (ns-lei4 "類")
    (ns-miao2 "苗")
    ;;(ns-shang4-xia4 "上下" "上上下下") ;; TODO: kept, or use the place rule?
    (ns-men2 "們")
    (ns-liang3 "倆")
    )
   (ns-fang1 "方")
   ;; inanimate
   (ns-dui4 "隊")
   (ns-bu4 "部")
   (ns-fen4 "分")
   (ns-chan3 "產")
   (ns-jian4 "件")
   (ns-mu4 "目")
   (ns-zuo4 "作")
   ;; material-ind
   (ns-material-ind
    (ns-cai2 "材")
    (ns-gan1 "乾")
    (ns-qi4 "器"))
   ;;
   (ns-ping3 "品")
   ;; shape-ind
   (ns-shape-ind
    ;; TODO: also allow some of these to be used alone?
    ;; or used with adjectives?
    (ns-tuan2 "團")
    (ns-dui1 "堆")
    (ns-pian4 "片")
    (ns-kuai4 "塊")
    (ns-ban3 "板")
    (ns-li4 "粒")
    (ns-si1 "絲")
    (ns-xie4 "屑")
    (ns-fen3 "粉")
    (ns-sui4 "碎")
    (ns-hua1 "花")
    (ns-wan2 "丸" "圓")
    (ns-shu4 "束")
    (ns-di1 "滴")
    (ns-tiao2 "條")
    (ns-chuan4 "串")
    (ns-huan2 "環")
    (ns-duan4 "段")
    (ns-qiu2 "球"))
   ;;
   (ns-qun2 "群" "集群")
   (ns-long2 "龍")
   (ns-ji2 "集")
   (ns-luo4 "落")
   ;;
   ))

;; end noun-suffix-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

