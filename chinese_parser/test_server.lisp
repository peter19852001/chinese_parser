;;; To provide a primitive service for parsing a sentence, and returning the parse tree in JSON form for display.

;;; our Chinese parser
;;(load (compile-file "earley_parser.lisp"))
;;(load (compile-file "test_parser.lisp"))
(load (compile-file "chinese_parser.asd"))

(ql:quickload :chinese-parser)

;;; Uses WOO as the server, ningle as framework on clack
;;;
(ql:quickload :woo)
(ql:quickload :clack)
(ql:quickload :ningle)

(in-package :chinese-parser)

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      "Welcome to ningle!")

(defun handle-parse (params)
  (let ((input-sentence (cdr (assoc "q" params :test #'string=))))
    `(200 (:content-type "application/json; charset=utf-8"
           :access-control-allow-methods "POST"
           :access-control-allow-origin "*"
           :access-control-allow-headers "content-type")
          (,(handler-case (parse-tree-to-json
                           (let ((*output-detail* t))
                             (car (test-parse input-sentence nil))))
                          (error (c) (format nil "\"ERROR: ~A~%\"" c)))))))

(defun handle-super (params)
  (let ((name (cdr (assoc "n" params :test #'string=))))
    `(200 (:content-type "application/json; charset=utf-8"
           :access-control-allow-methods "POST"
           :access-control-allow-origin "*"
           :access-control-allow-headers "content-type")
          (,(handler-case (class-names-to-json
                           (word-super-class-of name))
                          (error (c) (format nil "[]")))))))

(setf (ningle:route *app* "/parse" :method :post) #'handle-parse)
(setf (ningle:route *app* "/parse" :method :get) #'handle-parse)

(setf (ningle:route *app* "/super" :method :post) #'handle-super)
(setf (ningle:route *app* "/super" :method :get) #'handle-super)

(clack:clackup *app* :server :woo)
