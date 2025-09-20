
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



(defparameter *hash-helper-registry-2* (make-hash-table :test 'eq))





(defmacro define-hash-verb (name (h &rest args) docstring &body body)
  "Register a hash-helper verb NAME with a handler (H &rest ARGS) and DOCSTRING."
  `(progn
     (setf (gethash ,name *hash-helper-registry*)
           (list (lambda (,h &rest ,args) ,@body)
                 ,docstring))
     ',name))

(define-hash-verb :get (h key) "Lookup KEY; returns value and present?"
  (gethash key h))

(defun hash-helper (hasht message &rest rest)
  (let ((keyword (intern (string-upcase message) :keyword)))
     (case keyword
       ((:set) (>> ((key value) :from rest) (hash-set hasht key value)))
       ((:get) (>> ((key) :from rest) (gethash key hasht)))
       ((:is) (hash-table-p hasht))
       ((:keys) (hash-keys hasht))
       ((:help) (list :set :get :is :keys :help))
       (otherwise "did not understand"))))

;; Internal (not exported) variable by convention uses earmuffs
(defparameter *hash-helper-registry* (make-hash-table :test 'eq))
(pdef (make-hash-table) -> *hash-helper-help*)

(defun set-hash (hasht key value)
  (setf (gethash key hasht) value))

(set-hash *hash-helper-help* :set "usage `hash-obj :set key value`: sets the specified key-value pair")


(set-hash *hash-helper-help* :keys "usage `hash-ob :keys`: returns a list of the hash table's keys")

(defun define-hash-helper (key lambda-func help-text)
  (set-hash *hash-helper-registry* key (list lambda-func help-text)))

(define-hash-helper :set
    (lambda (hasht rest) (>> ((key value) :from rest) (set-hash hasht key value)))
  "usage `hash-obj :set key value`: sets the specified key-value pair")

(define-hash-helper :get
    (lambda (hasht rest) (>> ((key) :from rest) (gethash hasht key)))
 "usage `hash-obj :set key value`: sets the specified key-value pair")

(define-hash-helper :size
    (lambda (hasht rest) (declare (ignore rest)) (hash-table-count hasht))
 "usage `hash-obj :size`: get the number of pairs in the hash table")

(defun get-hash-helper-map ()
  (let ((funcs (make-hash-table))
	(help-doc-list NIL)
	(set-func NIL)
	(get-func NIL))


    
    (setf set-func (lambda (hasht rest)
		     (>> ((key value) :from rest)
			 
    (set-hash funcs :set (list set-func (gethash :set *hash-helper-help*)))
    (setf get-func (lambda (hasht rest) (declare (ignore rest)) (hash-keys hasht)))
    (setf (gethash :keys funcs) (list get-func
    				      "usage `hash-ob :keys`: returns a list of the hash table's keys"))
    (setf (gethash :help funcs) (list (lambda (hasht rest) (declare (ignore hasht) (ignore rest)) (dolist (x help-doc-list) (format t x)))
    				      "usage `hash-ob :help`: prints this help info"))
    (list funcs help-doc-list)
    ))

(defun build-hash-helper-docs (hash-helper-registry help-doc-list)
  (maphash (lambda (k v)
	       (setf help-doc-list (push (format NIL "~s ; ~a~%" k (second v)) help-doc-list)))
	     hash-helper-registry))

(defun hash-helper2 (hasht message &rest rest)
  (let ((keyword (intern (string-upcase message) :keyword))
	(func-obj NIL))

    
    (>> ((funcs help-doc-list) :from (get-hash-helper-map))
	(build-hash-helper-docs funcs help-doc-list)
	(setf func-obj (first (gethash keyword funcs)))
	(if (not func-obj)
	    (format t "keyword ~s not understood" keyword)
	    (apply func-obj (list hasht rest))))))


(defun hash-helper3 (hasht message &rest rest)
  (let ((keyword (intern (string-upcase message) :keyword))
	(func-obj NIL)
	(hash-helper-map *hash-helper-registry*)
	(help-doc-list NIL))
	(build-hash-helper-docs hash-helper-map help-doc-list)
    (setf func-obj (first (gethash keyword hash-helper-map)))
    (if (eq keyword :help)
	(dolist (x help-doc-list) (format t x))
	(if (not func-obj)
	    (format t "keyword ~s not understood" keyword)
	    (apply func-obj (list hasht rest))))))
	


(defun $ (lst)
  (let ((list lst))
    
    ))


(defvar *old-readtable* nil)

(defun %read-splice (stream char)
  (declare (ignore char))
  ;; Read the next form and wrap it so macros can notice it later
  (let ((form (read stream t nil t)))
    `(%splice ,form)))
