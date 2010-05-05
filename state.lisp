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
  `(defclass ,class ()
     (,@(loop for slot in slots
	   collect `(,slot :initarg ,(make-keyword slot) :reader ,slot)))))

