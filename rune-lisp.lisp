
(defpackage :rune/hash
  (:use :cl)                             ; inherit CL’s external symbols
  (:export :hash-helper :define-hash-verb)) ; public API

(in-package :rune/hash)


(defun pop-rest (lst)
  (cdr lst))

(defun at-least-n-elements (lst n)
  (if (= n 1)
      (not (endp lst))
      (at-least-n-elements (rest lst) (- n 1))
      ))

(defmacro >> (spec &rest body)
  ;; SPEC is like: ((a b . rest) :from xs)
  (destructuring-bind (pattern &key from) spec
    `(destructuring-bind ,pattern ,from
       ,@body)))

(defmacro bind-values-of (ex &key into ((and-then body)))
  `(multiple-value-bind ,into ,ex ,@body))

(defmacro $ (lst)
  (let ((helper NIL))
  (setf helper (lambda (&rest body)
    (append '(list-helper ,lst) body)))
    (funcall helper 
  )))

(defmacro $ (lst &rest body)
  (let ((argslist NIL))
    (setf argslist (append (list lst) body))
    argslist
  `(funcall #'list-helper ,@argslist)))


(defun hash-set (hasht &rest rest)
  (>> ((key value) :from rest) (setf (gethash key hasht) value)))


(defun hash-keys (ht)
  (let ((out NIL))
    (maphash (lambda (k _) (declare (ignore _))
	       (push k out))
	     ht)
    out))



(defun hash-helper (hasht message &rest rest)
  (let ((keyword (intern (string-upcase message) :keyword)))
     (case keyword
       ((:set) (>> ((key value) :from rest) (hash-set hasht key value)))
       ((:get) (>> ((key) :from rest) (gethash key hasht)))
       ((:is) (hash-table-p hasht))
       ((:keys) (hash-keys hasht))
       ((:help) (list :set :get :is :keys :help))
       (otherwise "did not understand"))))




(defun hash-helper2 (hasht message &rest rest)
  (let ((keyword (intern (string-upcase message) :keyword))
	(funcs (make-hash-table))
	(func-obj NIL))
    (setf (gethash :set funcs) (lambda (hasht rest) (>> ((key value) :from rest)
							(hash-set hasht key value))))
    (setf func-obj (gethash keyword funcs))
    (if (not func-obj)
	(format t "keyword ~s not understood" keyword)
	(apply func-obj (list hasht rest)))))


(defun $ (lst)
  (let ((list lst))
    
    ))


(defvar *old-readtable* nil)

(defun %read-splice (stream char)
  (declare (ignore char))
  ;; Read the next form and wrap it so macros can notice it later
  (let ((form (read stream t nil t)))
    `(%splice ,form)))
