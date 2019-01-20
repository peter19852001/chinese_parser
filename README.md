# What is it?
This is the archive of a prototype Chinese parser that I developed
while at the Institute of Future Cities, The Chinese University of
Hong Kong. The paper to describe the prototype parser in more details
is under preparation.

Chinese parsing presents interesting challenges due to the great
syntactic flexibility, lack of explicit marking by morphological
inflections, absence of word delimiters between words, and two
overlapping sets of characters (traditional and simplified).

This constituency parser is intended to explore a unified framework
for integrating word segmentation, POS-tagging, ambiguity resolution,
seamless handling of traditional and simplified Chinese characters, by
using symbolic semantic constraints on words categorized into
ontological classes with multiple inheritance.

Our prototype parser currently has

* 1931 grammar rules
* 451 non-terminals
* 19939 classes for atomic words organized in a multiple inheritance hierarchy (about 18100 of which are for individual verbs)
* atomic words, from which more words could be composed
    - 23451 words for different types of nouns
    - 362 measure words
    - 1825 adjectives
    - 1650 adverbs
    - 18163 verbs

The prototype parser also has a few types of semantic constraints, but
the number of constraints is not enough relative to the number of
words, so this parser remains a prototype, and not intended for
production use.

Also included is a simple webpage for visualization of the parse tree,
which was written by Mr. Sunny Lai as part of his Final Year Project.

# Dependencies

## Required

The prototype parser is written in Common Lisp, but has been tested on
[SBCL](http://www.sbcl.org/) implementation of Common Lisp only. The
main parser has been tested both on Windows 7 and Linux ([Ubuntu
16.04](http://releases.ubuntu.com/16.04/) and [Slackware
14.2](https://mirrors.slackware.com/slackware/slackware-14.2/)), both
other versions and other Linux distros should work fine. The
visualization has been tested on Linux (Ubuntu 16.04) only.

## Optional Dependencies for Visualization

The code here also includes a simple visualization of the parser
output for easier testing, which requires the following besides SBCL:

* [Quicklisp](https://www.quicklisp.org/beta/) for easier loading of
  the parser, and the code to support the visualization.

* either UNIX, GNU Linux, Mac, *BSD as the Operating System: required by Woo.

* [libev](http://libev.schmorp.de/): required by Woo.

* the [Woo server](https://github.com/fukamachi/woo): most easily
  installed using Quicklisp by typing the following in SBCL

```Lisp
(ql:quickload :woo)
```

* the [Clack](https://github.com/fukamachi/clack) web application
  environment: most easily installed using Quicklisp by typing the
  following in SBCL

```Lisp
(ql:quickload :clack)
```

* the [Ningle](https://github.com/fukamachi/ningle) web application
  framework: most easily installed using Quiclisp by typing the
  following in SBCL
  
```Lisp
(ql:quickload :ningle)
```

# Running

## To parser a sentence in SBCL

The parser needs quite a lot of RAM, so you may want to start SBCL with more memory allocated:

```Bash
cd chinese_parser
sbcl --dynamic-space-size 2048
```

Then in SBCL (with Quicklisp properly setup. If not, you may use
[ASDF](http://www.sbcl.org/asdf/Using-asdf-to-load-systems.html) to
load it), type the following to load the parser and switch to the
proper package:

```Lisp
(load (compile-file "chinese_parser.asd"))
(ql:quickload :chinese-parser)

(in-package :chinese-parser)
```

Then you may parse a sentence by calling `test-parse`, e.g.

```Lisp
(test-parse "我愛香港")
```

to get the output
```
Input: 我愛香港
Parse: ((SUBJ-PRED (PRONOUN 我) (NOT-PREFER-PAUSE)
         (VERB-C (VERB-A 愛) (PLACE-A (香 港)))))

((SUBJ-PRED (PRONOUN #\U6211) (NOT-PREFER-PAUSE)
  (VERB-C (VERB-A #\U611B) (PLACE-A (#\U9999 #\U6E2F)))))
-1790
NIL
```

As another example:
```Lisp
(test-parse "北京空气重污染应急指挥部3月24日晚发布空气重污染橙色预警，并于周日26日零时至周三28日期间实施橙色预警措施。")
```
to get the output
```
Input: 北京空气重污染应急指挥部3月24日晚发布空气重污染橙色预警，并于周日26日零时至周三28日期间实施橙色预警措施。
Parse: ((SENTENCE
         (SUBJ-PRED
          (NOUN-T
           (NOUN-T (NOUN-T (PLACE-A (北 京)) (INANIMATE-A (空 气)))
            (NOUN-B (ADJ-A 重) (ABSTRACT-A (污 染))))
           (NOUN-B (VERB-A (应 急)) (ABSTRACT-A (指 挥 部))))
          (NOT-PREFER-PAUSE)
          (VERB-P-CONN-A
           (VERB-P-A
            (VERB-MOD
             (AD-HOC-TIME-PT (NOUN-B (TIME-PT-A (INTEGER 3) 月 (INTEGER 24) 日))
              晚)
             (MAY-PAUSE))
            (VERB-C (VERB-A (发 布))
             (NOUN-T
              (NOUN-T (INANIMATE-A (空 气))
               (NOUN-B (ADJ-A 重) (ABSTRACT-A (污 染))))
              (NOUN-B (ADJ (ADJ-A 橙) 色) (ABSTRACT-A (预 警))))))
           (L-CONN (PAUSE ，) (CONN 并))
           (VERB-P-A
            (VERB-MOD
             (TIME-PT-H (TIME-AT-TO 于)
              (AD-HOC-TIME-PT
               (NOUN-B
                (TIME-PT-D-CONN-A
                 (TIME-PT-D (TIME-PT-A (周 日))
                  (TIME-PT-D (TIME-PT-A (2 6) 日) (TIME-PT-A (INTEGER 0) 时)))
                 至 (TIME-PT-D (TIME-PT-A (周 三)) (TIME-PT-A (2 8) 日))))
               (期 间)))
             (MAY-PAUSE))
            (VERB-C (VERB-A (实 施))
             (NOUN-T (NOUN-B (ADJ (ADJ-A 橙) 色) (ABSTRACT-A (预 警)))
              (ABSTRACT-A (措 施)))))))
         (END 。)))

((SENTENCE
  (SUBJ-PRED
   (NOUN-T
    (NOUN-T
     (NOUN-T (PLACE-A (#\U5317 #\U4EAC)) (INANIMATE-A (#\U7A7A #\U6C14)))
     (NOUN-B (ADJ-A #\U91CD) (ABSTRACT-A (#\U6C61 #\U67D3))))
    (NOUN-B (VERB-A (#\U5E94 #\U6025)) (ABSTRACT-A (#\U6307 #\U6325 #\U90E8))))
   (NOT-PREFER-PAUSE)
   (VERB-P-CONN-A
    (VERB-P-A
     (VERB-MOD
      (AD-HOC-TIME-PT
       (NOUN-B (TIME-PT-A (INTEGER 3) #\U6708 (INTEGER 24) #\U65E5)) #\U665A)
      (MAY-PAUSE))
     (VERB-C (VERB-A (#\U53D1 #\U5E03))
      (NOUN-T
       (NOUN-T (INANIMATE-A (#\U7A7A #\U6C14))
        (NOUN-B (ADJ-A #\U91CD) (ABSTRACT-A (#\U6C61 #\U67D3))))
       (NOUN-B (ADJ (ADJ-A #\U6A59) #\U8272) (ABSTRACT-A (#\U9884 #\U8B66))))))
    (L-CONN (PAUSE #\FULLWIDTH_COMMA) (CONN #\U5E76))
    (VERB-P-A
     (VERB-MOD
      (TIME-PT-H (TIME-AT-TO #\U4E8E)
       (AD-HOC-TIME-PT
        (NOUN-B
         (TIME-PT-D-CONN-A
          (TIME-PT-D (TIME-PT-A (#\U5468 #\U65E5))
           (TIME-PT-D (TIME-PT-A (2 6) #\U65E5)
            (TIME-PT-A (INTEGER 0) #\U65F6)))
          #\U81F3
          (TIME-PT-D (TIME-PT-A (#\U5468 #\U4E09)) (TIME-PT-A (2 8) #\U65E5))))
        (#\U671F #\U95F4)))
      (MAY-PAUSE))
     (VERB-C (VERB-A (#\U5B9E #\U65BD))
      (NOUN-T
       (NOUN-B (ADJ (ADJ-A #\U6A59) #\U8272) (ABSTRACT-A (#\U9884 #\U8B66)))
       (ABSTRACT-A (#\U63AA #\U65BD)))))))
  (END #\IDEOGRAPHIC_FULL_STOP)))
-9001
NIL
```

Note that any spurious whitespaces may confuse the prototype parser,
so they should be removed first. Also note that longer sentences take
much longer time to parse.

## To access through web request and use visualization

The `test-parse` function could optionally be accessed through either
GET or POST requests, by loading the file `test_server.lisp` as
follows:

```Bash
cd chinese_parser
sbcl --dynamic-space-size 2048 --load test_server.lisp
```

If things go well, the server should be running and listening to port
5000 (the default) at localhost. You may try to access this url in the
browser:
  
```
localhost:5000/parse?q=我愛香港
```

You should see a returned JSON for the resulting parse.

### To use the visualization

Following the above to start the server, if the web request can return
JSON properly, we may open `chinse_parser/visual/index.html` in your
browser, then type sentence to the "Input Text:" box, and submit to try.

# Supplementary Materials
