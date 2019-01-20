;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chinese-parser-asd
  (:use :cl :asdf))

(in-package :chinese-parser-asd)

(defsystem chinese-parser
  :name "chinese-parser"
  :version "0.1"
  :maintainer "Dr. Peter Lo"
  :author "Dr. Peter Lo"
  :description "Experimental Chinese Parser"
  :long-description "An experimental Chinese parser with integrated word segmentation, POS-tagging, seamless traditiona/simplified Chinese handling, with semantic tags and semantic constraints."
  :components ((:file "earley_parser")
               (:file "package" :depends-on ("earley_parser"))
               (:file "chinese_parser_utils" :depends-on ("package"))
               ;;
               (:file "chinese_parser_words"
                      :depends-on ("package" "chinese_parser_utils"))
               (:file "chinese_parser_nouns"
                      :depends-on ("package" "chinese_parser_words"))
               ;; the main nouns
               (:file "chinese_parser_animates"
                      :depends-on ("package"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"))
               (:file "chinese_parser_inanimates"
                      :depends-on ("package"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"))
               (:file "chinese_parser_abstracts"
                      :depends-on ("package"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"))
               (:file "chinese_parser_places"
                      :depends-on ("package"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"))
               (:file "chinese_parser_times"
                      :depends-on ("package"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"))
               (:file "chinese_parser_pronouns"
                      :depends-on ("package"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"))
               ;;
               (:file "chinese_parser_units"
                      :depends-on ("package"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"))
               ;;
               (:file "chinese_parser_verbs"
                      :depends-on ("package" "chinese_parser_words"))
               (:file "chinese_parser_adjs"
                      :depends-on ("package" "chinese_parser_words"))
               (:file "chinese_parser_advs"
                      :depends-on ("package" "chinese_parser_words"))
               ;;
               (:file "chinese_parser_penalty"
                      :depends-on ("package"))
               (:file "chinese_parser_verb_noun_costs"
                      :depends-on ("package"))
               (:file "chinese_parser_constraints_tag"
                      :depends-on ("package"
                                   "earley_parser"
                                   "chinese_parser_utils"
                                   "chinese_parser_penalty"
                                   "chinese_parser_verb_noun_costs"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"
                                   "chinese_parser_animates"
                                   "chinese_parser_inanimates"
                                   "chinese_parser_abstracts"
                                   "chinese_parser_places"
                                   "chinese_parser_times"
                                   "chinese_parser_pronouns"
                                   "chinese_parser_units"
                                   "chinese_parser_verbs"
                                   "chinese_parser_adjs"
                                   "chinese_parser_advs"))
               ;;
               (:file "chinese_parser_char"
                      :depends-on ("package"))
               ;;
               (:file "chinese_parser_grammar"
                      :depends-on ("package"
                                   "earley_parser"
                                   "chinese_parser_utils"
                                   "chinese_parser_penalty"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"
                                   "chinese_parser_animates"
                                   "chinese_parser_inanimates"
                                   "chinese_parser_abstracts"
                                   "chinese_parser_places"
                                   "chinese_parser_times"
                                   "chinese_parser_pronouns"
                                   "chinese_parser_units"
                                   "chinese_parser_verbs"
                                   "chinese_parser_adjs"
                                   "chinese_parser_advs"
                                   "chinese_parser_constraints_tag"))
               (:file "chinese_parser"
                      :depends-on ("package"
                                   "earley_parser"
                                   "chinese_parser_utils"
                                   "chinese_parser_penalty"
                                   "chinese_parser_words"
                                   "chinese_parser_nouns"
                                   "chinese_parser_animates"
                                   "chinese_parser_inanimates"
                                   "chinese_parser_abstracts"
                                   "chinese_parser_places"
                                   "chinese_parser_times"
                                   "chinese_parser_pronouns"
                                   "chinese_parser_units"
                                   "chinese_parser_verbs"
                                   "chinese_parser_adjs"
                                   "chinese_parser_advs"
                                   "chinese_parser_constraints_tag"
                                   "chinese_parser_grammar"
                                   "chinese_parser_char"))
               )
  )
