(in-package :chinese-parser)

(def-tagged-words *pronouns-list* *pronouns-hash*
  ((pronoun human)
   (pronoun-word-self
    "自己" "自家" "自個兒" "自個"
    ;; "自已" is common typo for "自己"
    "自已" )
   (pronoun-zhi "之") ;; used as object, but not subject
   (pronoun-word-bi4 "敝")
   ;;(pronoun-word-yu2 "余") ;; TODO: this could be confusing
   "我" "你" "您" "他" "她" "它" "咱"
   "鄙人" "在下"
   "吾人" "吾" "汝"
   (word-ben3-ren2 "本人")
   "俺" "偶"
   "我們" "咱們" "你們" "您們" "他們" "她們" "它們"
   "大家" "別人" "他人" "人家" "對方" "雙方"
   ;; TODO: distinguish those not usually used to modify other nouns
   "舉座" "一己"
   (pronoun-word-ji3 "己")
   (word-zi4-wo3 "自我")
   ;; sometimes some English is mixed in
   "I" "you" "we" "they" "he" "she" "it"
   "me" "us" "them" "him" "her"
   "my" "your" "our" "their" "his" "its"
   ))

(def-tagged-words *q-pronouns-list* *q-pronouns-hash*
  ((q-pronoun traited-word)
   ;; use not-single to mark those that do not normally used as a noun
   ;; on its own.
   ((q-pronoun-human human) "誰")
   ((q-pronoun-place place) "哪" "哪兒" "哪裏")
   "孰" "啥" "哪些"
   (q-dt-quantifier
    "甚麼" "什麼"
    ((q-dt-quantifier-ns not-single)
     "何" "任何"))
   ))

(def-tagged-words *p-pronouns-list* *p-pronouns-hash*
  ((p-pronoun non-verb-noun)
   ;; use not-single to mark those that do not normally used as a noun
   ;; on its own.
   (p-pronoun-all
    (p-pronoun-all-people "人人" "個個"))
   ;;
   (p-pronoun-dt
    (dt-demonstrative
     "這" "這些" "此" "那" "那些"
     "其餘" "其他" "其它" "某些"
     ;;
     "this" "that" "these" "those"
     ;;
     ((dt-demonstrative-ns not-single)
      "尤其" "尤其是"
      "該" "本" "別" "另" "某")
     )
    ;;
    (dt-quantifier
     "全體" "全部" "有的" "有些"
     ((dt-quantifier-all p-pronoun-all)
      "一切" "所有")
     ((dt-quantifier-ns not-single)
      ;; note that '各' and some other are also adv
      "各" "諸" "每" "整" "全" "同")
     ))
   (p-pronoun-qi2-zhong1 "其中")
   ;;
   ((p-pronoun-place place)
    "這兒" "那兒" "這裏" "那裏")
   ((p-pronoun-time time-name)
    "這會兒" "那會兒" "其間")
   "一些"
   "本身" "彼此" "自身" "這樣"
   "以上" "個中"
   "其" "以下" "前列" "彼" "爾"
   ;;(p-pronoun-you3 "有") ;; TODO: whether to keep 有 as p-pronoun?
   ((p-pronoun-ns not-single)
    (p-pronoun-ah
     ;; e.g. "阿哥", "阿 (陳 經理)", seems it can repeat, and modify a
     ;; noun phrase of animate

     ;; NOTE: seems CTB has not such construct, so not sure of the
     ;; tval
     "阿" "啊")
    ;; 分 to mean 'branch', where "總" is now an adj
    (p-pronoun-fen1 "分"))
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
