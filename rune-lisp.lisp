
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



(defun hash-helper (hasht message &rest rest)
  (let ((keyword (intern (string-upcase message) :keyword)))
     (case keyword
       ((:set) (>> ((key value) :from rest) (setf (gethash key hasht) value)))
       (otherwise "did not understand"))))

  
    

(defun list-helper (lst message &rest body)
   (let ((keyword (intern (string-upcase message) :keyword)))
     (case keyword
       ((:length) (length lst))
       ((:len-at-least) (at-least-n-elements lst (first body)))
       (otherwise "did not understand"))))


(defun $ (lst)
  (let ((list lst))
    
    ))


(defvar *old-readtable* nil)

(defun %read-splice (stream char)
  (declare (ignore char))
  ;; Read the next form and wrap it so macros can notice it later
  (let ((form (read stream t nil t)))
    `(%splice ,form)))
