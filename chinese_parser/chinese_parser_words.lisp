(in-package :chinese-parser)

;; record the direct super-classes of the classes ourselves
;; the class names are normally upper case (the default in common lisp)
(defparameter *word-super-classes* (make-hash-table :test 'equal)
  "class name to list of names of super classes, all in strings.")

(defun word-super-class-of (name)
  (gethash name *word-super-classes* nil))

(defmacro def-word-class (name direct-superclasses direct-slots &rest options)
  `(progn
     (setf (gethash ,(symbol-name name) *word-super-classes* nil)
           ',(mapcar #'symbol-name direct-superclasses))
     (defclass ,name ,direct-superclasses ,direct-slots ,@options)))

;;
(defun word-list-hash (words)
  ;; words is a list of (class-name . w1 w2 ...), where each wi is a list of characters,
  ;; and class-name could be a class-instance instead.
  ;; turn it into a hash table indexed by the first character of each word
  ;; so the hash is first-char -> ((w1 . class-instance) (w2 . class-instance) ...)
  ;; Also, add trait to indicate the number of characters of each word.
  (let ((h (make-hash-table)))
    (dolist (name-lst words h)
      (let* ((class-name (car name-lst))
             (word-lengths (remove-duplicates (mapcar #'length (cdr name-lst))))
             (tag (if (symbolp class-name)
                      (make-instance class-name)
                      class-name))
             (tag-with-lens ;; an assoc list of (length . traited-tag)
              (mapcar #'(lambda (L)
                          (cons L
                                (more-trait tag 'word-len L)))
                      word-lengths)))
        (dolist (word (cdr name-lst))
          (let ((L (length word)))
            (push (cons word (cdr (assoc L tag-with-lens :test #'eql)))
                  (gethash (first word) h nil))))))))

(defun for-word-list-hash (hash func)
  ;; func is (lambda (pair) ...) where pair is (word . class-instance)
  (loop for pairs being the hash-values in hash
       do (dolist (pair pairs)
            (funcall func pair))))

;;
(defmacro def-tagged-words (var-list-name var-hash-name &body specs)
  ;; To help define an assoc list of classes and words (will be transformed by strs-to-lists),
  ;; and the hash table indexing the first character for a subset of lists.
  ;; The format is as follows:
  ;; specs -> spec*
  ;; spec -> (class-spec word*)
  ;; class-spec -> @ name ;; not defining new class, but to use the named class as the implicit super class for the enclosind word*
  ;; class-spec -> name ;; as class name, with the outer class as super class
  ;; class-spec -> (name super-class* ) ;; as class name and explicitly specified super classes, together with the implicit super class
  ;; word -> string ;; constituent word, will be transformed by strs-to-lists
  ;; word -> spec ;; can specify subclass with its words, where the current class is the implicit super class (unless explicitly overriden)

  (let ((class-def-list nil)
        (assoc-list nil))
    (labels ((parse-spec (spec super)
               (if (eq '@ (car spec))
                   (let* ((cont-name (second spec))
                          (cont-words (remove-if #'consp (cddr spec)))
                          (entry (assoc cont-name assoc-list)))
                     ;; append to the assoc-list, remove dups in the last step
                     (unless entry
                       (setf entry (list cont-name))
                       (push entry assoc-list))
                     (setf (cdr entry)
                           (append (strs-to-lists cont-words)
                                   (cdr entry)))
                     ;;
                     (dolist (s (cddr spec))
                       (when (consp s)
                         (parse-spec s (list cont-name)))))
                   (parse-spec-h spec super)))
             ;;
             (parse-spec-h (spec super)
               ;; super is the list of name of super class, if not nil
               (let* ((class-spec (car spec))
                      (class-name (if (consp class-spec)
                                      (car class-spec)
                                      class-spec))
                      (super-classes (if (consp class-spec)
                                         (append super (cdr class-spec))
                                         super))
                      (words (remove-if #'consp (cdr spec))))
                 ;; the class, currently no slots, may change later
                 (push `(def-word-class ,class-name ,super-classes ())
                       class-def-list)
                 ;; special case for ind, insert the name of its
                 ;; direct superclass into *ind-category*
                 (when (member 'ind super-classes :test #'eq)
                   (push `(setf (gethash ',class-name *ind-category*)
                                ',(first super))
                         class-def-list))
                 ;; get the raw words first
                 (when words
                   (push (cons class-name (strs-to-lists (no-dups words)))
                         assoc-list))
                 ;; now subclass specs, if any
                 (dolist (s (cdr spec))
                   (when (consp s) (parse-spec s (list class-name)))))))
      ;; walk through the specs
      (dolist (s specs) (parse-spec s nil))
      ;;
      `(progn
         ;; define the classes
         ,@(nreverse class-def-list)
         ;; generate the assoc list
         (defparameter ,var-list-name
           (mapcar #'(lambda (x) (cons (make-instance (car x)) (no-dups (cdr x))))
                   ',(nreverse assoc-list)))
         (defparameter ,var-hash-name
           (word-list-hash ,var-list-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Traits, for attaching key-value pairs to tags
;;
(def-word-class traited-word ()
  ((traits
    ;; holds an assoc list to add attributes to a word, but these
    ;; cannot be used for auto dispatching in the scoring functions.
    :initarg :traits
    :initform nil)
   ))

(def-word-class noun (traited-word) ())

(defmethod print-object ((n traited-word) out)
  (print-unreadable-object (n out :type t :identity t)
    (format out "traits: ~s" (slot-value n 'traits))))

;;; Previously overlooked the possibility of having traits in
;;; structure when struct is used as tag.

;;; So the remedy is to also allow structure in more-trait and related
;;; functions
(defstruct has-traits
  (traits nil))
(defmacro def-trait-struct (name &body slot-descriptions)
  `(defstruct (,name (:include has-traits))
     ,@slot-descriptions))

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  ;; copied on 9th Jan, 2018
  ;; from https://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defun can-have-traits-p (n)
  (or (has-traits-p n)
      (typep n 'traited-word)))
(defun copy-noun-or-struct (n)
  (if (typep n 'traited-word)
      (copy-instance n)
      (copy-structure n)))

(defun traits-of (n)
  (cond ((typep n 'traited-word)
         (slot-value n 'traits))
        ((has-traits-p n)
         (has-traits-traits n))
        (t nil)))

(defun adjoin-one-trait (name val old-traits)
  ;; old-traits is a-list of (name . val)
  ;; allow overriding trait value by adding new pair
  (let ((entry (assoc name old-traits)))
    (if (and entry
            (equalp val (cdr entry)))
        ;; seen, same value
        old-traits
        ;; either not seen before, or different value
        (cons (cons name val) old-traits))))

(defun adjoin-traits (old-traits name-vals)
  ;; Now allow overriding trait values
  (loop for (name val . rest) on name-vals by #'cddr
     do (setf old-traits (adjoin-one-trait name val old-traits)))
  old-traits)

(defun adjoin-traits-to (n name-vals)
  (if (typep n 'traited-word)
      (setf (slot-value n 'traits)
            (adjoin-traits (slot-value n 'traits) name-vals))
      (setf (has-traits-traits n)
            (adjoin-traits (has-traits-traits n) name-vals)))
  n)

(defun replace-traits-to (n new-traits)
  (if (typep n 'traited-word)
      (setf (slot-value n 'traits) new-traits)
      (setf (has-traits-traits n) new-traits))
  n)

(defun more-trait-to (n &rest name-vals)
  ;; NOTE: this is destructive, so be very careful, otherwise, some
  ;; tags may be modified and the results would be very mysterious!
  (if (can-have-traits-p n)
      (adjoin-traits-to n name-vals)
      n))

;; NOTE: currently, tag equality is compared using equalp only, to
;; prevent making many objects unnecessarily and cause spurious
;; ambiguity, we memoize more-trait-with.
(defparameter *more-trait-with-hash* (make-hash-table :test 'equalp))
(defun clear-more-trait-with-hash ()
  (clrhash *more-trait-with-hash*))
(defun more-trait-with-h (n name-vals)
  ;; n could be either noun (or its subclass) or (sub-)structure of has-traits
  ;; name-vals are 'name1 val1 name2 val2 ...'
  ;; avoid copying n if nothing new to add
  (if (can-have-traits-p n)
      (let* ((old-traits (traits-of n))
             (new-traits (adjoin-traits old-traits name-vals)))
        (if (eq new-traits old-traits)
            n
            (replace-traits-to (copy-noun-or-struct n)
                               new-traits)))
      n))
(defun more-trait-with (n name-vals)
  (let ((arg (cons n name-vals)))
    (or (gethash arg *more-trait-with-hash*)
        (setf (gethash arg *more-trait-with-hash*)
              (more-trait-with-h n name-vals)))))

(defun more-trait (n &rest name-vals)
  (more-trait-with n name-vals))

(defun has-trait (n trait-name)
  (assoc trait-name (traits-of n)))

(defun trait-value (n trait-name &optional default-value)
  (let ((z (assoc trait-name (traits-of n))))
    (if z (cdr z) default-value)))

(def-word-class verb (noun) ())


;;; Some categories have a word as indicator, which represents the
;;; class, e.g. 市 (city), and some cities can be a name with the
;;; indicator, e.g. 北京市, but the name alone is also commonly used
;;; to represent the city, e.g. 北京. But, some words in a category
;;; would have a name not necessarily ending with the indicator, or
;;; the name is not commonly used alone, or to avoid too much
;;; ambiguity, we want some words to stand alone. So we have a
;;; category 'ind' to signal the indicators and 'name' to indicate the
;;; name of the respective category.

;;; To use properly for a category A, there should be a subclass
;;; (e.g. A-ind) having immediate superclasses A and ind (with A
;;; first), and a subclass (e.g. A-name) having superclasses A and
;;; name (not necessarily immediate superclass).

(def-word-class name (noun) ()) ;; it represents a name, can be suffixed with a type
(def-word-class nr-name (name) ()) ;; it represents an NR

;; to indicate a word consists of two parts, so its penalty should be doubled
(def-word-class compound () ())
;; the suffix of a particular type
(def-word-class ind () ())

;; don't know how to get the first direct parent of this class, so
;; include it ourself in a hash table, indexed by the class name of
;; the ind, and gives the class name of its direct parent class. And
;; modify def-tagged-words to input the direct parent class properly
;; if a class derives from ind
(defparameter *ind-category* (make-hash-table :test 'eq))
(defun category-of-ind (ind)
  ;; ind should be of class ind
  (gethash (type-of ind) *ind-category*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-word-class not-single () ()) ;; do not usually as a single noun, should be prefixed or suffixed with something

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-trait-struct adj-has-de
  ;; NOTE: adj-has-de is not subclass of noun, but can appear in
  ;; noun-p context, so need to take care in defining the constraints
  the-adj)

(def-trait-struct yong4-adj
  ;; e.g. 包装用, 殺敵用, 驅蚊用
  ;; the verb could be a verb, or verb-n
  the-verb)

(def-trait-struct dui4-noun
  ;; as noun-mod
  ;; e.g. "对韩 出口"
  noun)

(def-trait-struct a-subj-pred
  subj
  pred)
(def-trait-struct combine-verb
  ;; want to have both properties of the two or more verbs
  first
  second)
(def-trait-struct passive-voice
  ;; either could be nil, if it is not yet specified
  verb
  object)
(def-trait-struct conn-thing
  ;; contain two things, maybe noun, verbs, subj-pred, adj, ...

  ;; in a constraint, want both first and second to be constrained,
  ;; i.e. take the min of applying to first and second.
  first
  second
  sep ;; the separator, could be nil
  )
(def-trait-struct pp-verb-mod
  ;; for some verb-mod (usually PP in CTB) that also has argument
  
  ;; the PP, should decide whether they (non-exclusive):
  ;; -- introduce additional object for verb
  ;; -- introduce additional subject for verb
  ;; -- change subject for verb
  ;; -- non-stackable
  ;; -- prefer animate-like subject
  ;; -- are preferred or unwanted by some verbs, e.g. v-is
  pp
  ;; the object, could be a noun, or a verb phrase, or subj-pred,
  ;; depending on the PP
  object
  ;; whether to prefer pause after this PP
  prefer-pause
  )

(defun conn-thing-last (c)
  (if (conn-thing-p c)
      (conn-thing-last (conn-thing-second c))
      c))
