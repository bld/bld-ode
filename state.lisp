(in-package :bld-ode)

;; State arithmetic
(defgeneric +x2 (a b)
  (:documentation "Add two states together, using per-class methods"))
(defgeneric -x2 (a b)
  (:documentation "Subtract one state from another"))
(defgeneric *xs (x s)
  (:documentation "Multiply all elements of a state by a scalar using per-class methods"))
(defgeneric norminfx (x)
  (:documentation "Infinity norm of a state"))
;; Numeric state arithmetic
(defmethod +x2 ((a number) (b number))
  (+ a b))
(defmethod -x2 ((a number) (b number))
  (- a b))
(defmethod *xs ((s number) (x number))
  (* s x))
(defmethod norminfx ((x number))
  (abs x))
;; Vector state arithmetic
(defmethod +x2 ((a vector) (b vector))
  (map 'vector #'+x2 a b))
(defmethod -x2 ((a vector) (b vector))
  (map 'vector #'-x2 a b))
(defmethod *xs ((x vector) (s number))
  (map 'vector #'(lambda (xi) (*xs xi s)) x))
(defmethod norminfx ((x vector))
  (reduce #'max (map 'vector #'norminfx x)))
;; Hash table state arithmetic
(defmethod +x2 ((a hash-table) (b hash-table))
  (let ((result (make-hash-table)))
    (loop for k being the hash-keys in a
       for va being the hash-values in a
       for vb being the hash-values in b
       do (setf (gethash k result) (+x2 va vb)))
    result))
(defmethod -x2 ((a hash-table) (b hash-table))
  (let ((result (make-hash-table)))
    (loop for k being the hash-keys in a
       for va being the hash-values in a
       for vb being the hash-values in b
       do (setf (gethash k result) (-x2 va vb)))
    result))
(defmethod *xs ((x hash-table) (s number))
  (let ((result (make-hash-table)))
    (loop for k being the hash-keys in x
       for v being the hash-values in x
       do (setf (gethash k result) (*xs v s)))
    result))
(defmethod norminfx ((x hash-table))
  (loop for v being the hash-values in x
     maximize (norminfx v)))

;; Define PRINT-OBJECT method for hash tables
(defmethod print-object ((h hash-table) stream)
  (format stream "#<HASH-TABLE")
  (maphash #'(lambda (k v) (format stream " ~a ~a" k v)) h)
  (format stream ">"))

;; Define methods on state classes
(defmacro defstatemethod-2arg (meth class slots)
  `(defmethod ,meth ((a ,class) (b ,class))
     (make-instance 
      ',class
      ,@(loop for slot in slots
	   collect (make-keyword slot)
	   collect `(,meth (slot-value a ',slot)
			   (slot-value b ',slot))))))
(defmacro def*xsmethod (class slots)
  `(defmethod *xs ((x ,class) (s number))
     (make-instance 
      ',class
      ,@(loop for slot in slots
	   collect (make-keyword slot)
	   collect `(*xs (slot-value x ',slot) s)))))
(defmacro defnorminfxmethod (class slots)
  `(defmethod norminfx ((x ,class))
     (max ,@(loop for slot in slots
	       collect `(norminfx (slot-value x ',slot))))))
(defmacro defstatemethods (class slots)
  `(progn
     (defstatemethod-2arg +x2 ,class ,slots)
     (defstatemethod-2arg -x2 ,class ,slots)
     (def*xsmethod ,class ,slots)
     (defnorminfxmethod ,class ,slots)))
(defmacro defstate (class slots)
  `(progn
     (defclass ,class ()
       (,@(loop for slot in slots
	     collect `(,slot :initarg ,(make-keyword slot) :reader ,slot))))
     (defmethod print-object ((x ,class) stream)
       (format stream "#<~a" ',class)
       ,@(loop for slot in slots
	    collect `(format stream " ~a ~a" ',slot (slot-value x ',slot)))
       (format stream ">"))))
