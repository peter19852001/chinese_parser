(in-package :chinese-parser)

;; to help collapsing nodes and changing POS tags
(defun pr-tree-strs (stream tree)
  (cond ((or (stringp tree) (characterp tree))
         (format stream "~A" tree))
        ((consp tree)
         (pr-tree-strs stream (car tree))
         (pr-tree-strs stream (cdr tree)))
        (t nil)))

(defun collapse-str (&rest trees)
  ;; return the string of the tree
  (with-output-to-string (str)
    (dolist (tree trees)
      (pr-tree-strs str tree))))

(defun strs-len (tree)
  (cond ((stringp tree) (length tree))
        ((characterp tree) 1)
        ((consp tree)
         (+ (strs-len (car tree))
            (strs-len (cdr tree))))
        (t 0)))

(defmacro t-str (POS-tag)
  ;; args is accessible in POS-tag, so can decide the POS tag
  ;; depending on its contents
  `#'(lambda (args tags lens)
       (declare (ignore tags lens))
       (list ,POS-tag (collapse-str args))))

(defmacro re-tag (POS-tag)
  ;; change tag
  ;; args is accessible in POS-tag, so can decide the POS tag
  ;; depending on its contents
  `#'(lambda (args tags lens)
       (declare (ignore tags lens))
       (cons ,POS-tag (cdr args))))

(defun tag-as (POS-tag &rest trees)
  `(,POS-tag ,(apply #'collapse-str trees)))

;;;
(defun strs-to-lists (strings)
  (mapcar #'(lambda (s) (coerce s 'list)) strings))

(defun strs (ns strings &optional (arrow '->) &rest others)
  ;; each string is converted into list of characters
  (mapcar #'(lambda (s) `(,ns ,arrow ,@(coerce s 'list) ,@others))
          strings))

(defun str-tags (ns str-tags &optional (arrow '->) &rest others)
  ;; str-tags is list of either:
  ;; string
  ;; (tag string1 ...)
  ;; each string is converted into list of characters
  (let ((res nil))
    (dolist (x str-tags (nreverse res))
      (if (consp x)
          (dolist (s (cdr x))
            (push `(,ns ,arrow ,@(coerce s 'list)
                        :tag ,(car x)
                        ,@others)
                  res))
          (push `(,ns ,arrow ,@(coerce x 'list) ,@others)
                res)))))

;;
;; in score, nil means prohibited, similar to -inf
(declaim (inline score-min score-max score-add
                 no-dups cons2))
(defun score-min (x y)
  (cond ((null x) nil)
        ((null y) nil)
        (t (min x y))))
(defun score-max (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (max x y))))
(defun score-add (x y)
  (cond ((null x) nil)
        ((null y) nil)
        (t (+ x y))))

(defun no-dups (x) (remove-duplicates x :test #'equalp))

(defun cons2 (a b tail)
  (cons a (cons b tail)))

(defmacro aif (con then &optional else)
  `(let ((it ,con))
     (if it ,then ,else)))

(defmacro awhen (con &body body)
  `(let ((it ,con))
     (when it ,@body)))
