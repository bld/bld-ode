#|
Generic state arithmetic for ODE solvers
Includes addition, subtraction, negation, and scalar multiplication
Includes vector & hash table arithmetic
Planned: macro to define methods on custom classes
|#

(in-package :bld-ode)

;; Infinity norm of an ODE state
(defgeneric norminfx (x)
  (:documentation "Infinity norm of a state"))

;; Infinity norm of a number
(defmethod norminfx ((x number))
  (abs x))

;; Vector state arithmetic

(defmeth2 + ((a vector) (b vector)) 
  (map 'vector #'+ a b))

(defmeth12 - ((a vector) (b vector)) 
  ((map 'vector #'- a)) 
  ((map 'vector #'- a b)))

(defmeth2 * ((x vector) (s number))
  (map 'vector #'(lambda (xi) (* xi s)) x))

(defmeth2 * ((s number) (x vector))
  (* x s))

(defmethod norminfx ((x vector))
  (reduce #'max (map 'vector #'norminfx x)))

;; Hash table state arithmetic

(defmeth2 + ((a hash-table) (b hash-table))
  (let ((result (make-hash-table)))
    (loop for k being the hash-keys in a
       for va being the hash-values in a
       for vb being the hash-values in b
       do (setf (gethash k result) (+ va vb)))
    result))

(defmeth12 - ((a hash-table) (b hash-table))
  ((bld-utils:maphash2 #'(lambda (k v) (- v)) a))
  ((let ((result (make-hash-table)))
     (loop for k being the hash-keys in a
	for va being the hash-values in a
	for vb being the hash-values in b
	do (setf (gethash k result) (- va vb)))
     result)))

(defmeth2 * ((x hash-table) (s number))
  (let ((result (make-hash-table)))
    (loop for k being the hash-keys in x
       for v being the hash-values in x
       do (setf (gethash k result) (* v s)))
    result))

(defmeth2 * ((s number) (x hash-table))
  (* x s))

(defmethod norminfx ((x hash-table))
  (loop for v being the hash-values in x
     maximize (norminfx v)))

;; Define PRINT-OBJECT method for hash tables
(defmethod print-object ((h hash-table) stream)
  (format stream "#<HASH-TABLE")
  (maphash #'(lambda (k v) (if (keywordp k) 
			       (format stream " :~a ~a" k v)
			       (format stream " ~a ~a" k v))) h)
  (format stream ">"))

;; Macro to define state arithmethic on a custom class
(defmacro defstatearithmetic (class slots &key (initargs (mapcar #'make-keyword slots)))
  "Defines state arithmetic for specified class and slots.
Assumes state arithmetic already defined for slot types, including NORMINFX.
By default, assumes INITARGS of the same name as slots already defined."
  `(progn
     ;; Inifinity norm
     (defmethod norminfx ((x ,class))
       (max
	,@(loop for slot in slots
	     collect `(norminfx (slot-value x ',slot)))))
     ;; Addition
     (defmeth2 + ((a ,class) (b ,class))
       (make-instance 
	',class
	,@(loop for slot in slots
	     for initarg in initargs
	     collect initarg
	     collect `(+ (slot-value a ',slot) (slot-value b ',slot)))))
     ;; Negation / Subtraction
     (defmeth12 - ((a ,class) (b ,class))
       ((make-instance
	 ',class
	 ,@(loop for slot in slots
	      for initarg in initargs
	      collect initarg
	      collect `(- (slot-value a ',slot)))))
       ((make-instance
	 ',class
	 ,@(loop for slot in slots
	      for initarg in initargs
	      collect initarg
	      collect `(- (slot-value a ',slot) (slot-value b ',slot))))))
     ;; Scalar multiplication
     (defmeth2 * ((x ,class) (s number))
       (make-instance 
	',class
	,@(loop for slot in slots
	     for initarg in initargs
	     collect initarg
	     collect `(* (slot-value x ',slot) s))))
     (defmeth2 * ((s number) (x ,class))
       (* x s))))

