(in-package :bld-ode)

;; Stepping functions
(defun ki (fun tm i h x k c a param)
  "Calculate intermediate k value"
  (setf (aref k i) (* (aref k 0) (aref a i 0)))
  (loop for j from 1 below i
     do (setf (aref k i) (+ (aref k i) (* (aref k j) (aref a i j)))))
  (funcall
   fun
   (+ tm (* h (aref c i)))
   (+ (* (aref k i) h) x)
   param))
(defun kall (fun tm h x s c a param)
  "Calculate vector of k values"
  (let ((k (make-array 7)))
    (setf (aref k 0) (funcall fun tm x param))
    (loop for i from 1 below s
       do (setf (aref k i)
		(ki fun tm i h x k c a param)))
    k))
(defun xnext (x h k b)
  "Calculate next state"
  (+
   (*
    (loop for bj across b
       for kj across k
       for res = (* kj bj)
       then (+ res (* kj bj))
       finally (return res))
    h)
   x))

;; Dormand Prince coefficients
(defparameter *a-dp*
  (make-array 
   '(7 7)
   :element-type 'double-float
   :initial-contents
   (list (list 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
	 (list (/ 1d0 5d0) 0d0 0d0 0d0 0d0 0d0 0d0)
	 (list (/ 3d0 40d0) (/ 9d0 40d0) 0d0 0d0 0d0 0d0 0d0)
	 (list (/ 44d0 45d0) (/ -56d0 15d0) (/ 32d0 9d0) 0d0 0d0 0d0 0d0)
	 (list (/ 19372d0 6561d0) (/ -25360d0 2187d0) (/ 64448d0 6561d0) (/ -212d0 729d0) 0d0 0d0 0d0)
	 (list (/ 9017d0 3168d0) (/ -355d0 33d0) (/ 46732d0 5247d0) (/ 49d0 176d0) (/ -5103d0 18656d0) 0d0 0d0)
	 (list (/ 35d0 384d0) 0d0 (/ 500d0 1113d0) (/ 125d0 192d0) (/ -2187d0 6784d0) (/ 11d0 84d0) 0d0))))
(defparameter *bl-dp*
  (make-array 7 :element-type 'double-float :initial-contents (list (/ 5179d0 57600d0) 0d0 (/ 7571d0 16695d0) (/ 393d0 640d0) (/ -92097d0 339200d0) (/ 187d0 2100d0) (/ 1d0 40d0))))
(defparameter *bh-dp*
  (make-array 7 :element-type 'double-float :initial-contents (list (/ 35d0 384d0) 0d0 (/ 500d0 1113d0) (/ 125d0 192d0) (/ -2187d0 6784d0) (/ 11d0 84d0) 0d0)))
(defparameter *c-dp*
  (make-array 7 :element-type 'double-float :initial-contents (list 0d0 (/ 2d0 10d0) (/ 3d0 10d0) (/ 8d0 10d0) (/ 8d0 9d0) 1d0 1d0)))

;; Runge Kutta w/adaptive stepsize driver function
(defun rka (fun t0 tf x0 &key (a *a-dp*) (bl *bl-dp*) (bh *bh-dp*) (c *c-dp*) (tol 1d-6) (hmax (/ (- tf t0) 4)) (h0 (/ (- tf t0) 200d0)) (hmin (/ (- tf t0) 1d12)) param &aux (s (length bl)))
  "Runge-Kutta method with adaptive stepsize. States are generic. They may be numbers, arrays, classes, etc., so long as they have state addition and scalar multiplication functions.  Results are collected into a list of independant variables & states."
  (loop
     with flag = t ; flag indicating whether integration completed
     with pow = (/ 1d0 6d0)
     with tm = t0 ; independant variable (e.g. time)
     with h = h0 ; stepsize
     with x = x0 ; state
     until (or (>= tm tf) (< h hmin)) ; end: final time, h too small
     for k = (kall fun tm h x s c a param) ; k values used in state estimates
     for xl = (xnext x h k bl) ; lower order estimate
     for xh = (xnext x h k bh) ; higher order estimate
     for err = (norminfx (- xh xl)) ; error estimate
     for allowed = (* tol (max (norminfx x) 1.0)) ; allowable error
     when (<= err allowed) do ; error acceptable
       (setq tm (+ tm h)) ; update dependant variable
       (setq x xh) ; update state
     and collect (list tm x) into out ; collect into return value list
     do (setq h (min hmax (* 0.8d0 h (expt (/ allowed (max err 1d-16)) pow)))) ; update stepsize
     when (> (+ tm h) tf) do (setq h (- tf tm)) ; reduce stepsize if it exceeds tf
     finally (when (< tm tf) ; check if integration finished
	       (setq flag nil) ; flag NIL if TF not reached: h too small
	       (warn "Stepsize ~a smaller than minimum ~a." h hmin))
       (return (values (cons (list t0 x0) out) flag))))

;;; RKA with stopping condition

(defun bisect (fun a b tol nmax)
  "Perform bisection to find 0 of a function
FUN: function (x) to find root FUN(X) = 0
A: minimum X
B: maximum X
TOL: tolerance to find 0
NMAX: max iterations"
  (loop for n below nmax
     for c = (/ (+ a b) 2) ; midpoint of A and B
     for f_c = (funcall fun c) ; f at c
     for f_a = (funcall fun a) ; f at a
     until (or (zerop f_c) (< (/ (- b a) 2) tol)) ; f_c zero or difference of a & b within tol
     if (= (signum f_c) (signum f_a)) do (setq a c) ; update limits
     else do (setq b c)
     finally
       (return
	 (if (< n nmax) c
	     (error "BISECT failed to converge within ~a iterations." nmax)))))

;; Newton-Raphson
(defun nr (fun x-left x-right 
	   &key (accuracy (* 10 double-float-epsilon))
	     (max-iter 20)
	     (prevent-bracket-jump t))
  "Newton Raphson algorithm, where fun(x) is a function that returns
both the function value and it's derivative"
  (assert (< x-left x-right))
  (let ((x (/ (+ x-left x-right) 2))
	delta-x
	denom-for-acc-test)
    (dotimes (j max-iter (if (not (cerror "returns solution so far"
					  "exceeding max iter"))
			     (values x max-iter)))
      (multiple-value-bind (f f-prime) (funcall fun x)
	(setf delta-x (/ f f-prime))
	(setf denom-for-acc (+ (abs x) (abs (decf x delta-x))))
	(cond
	  (prevent-bracket-jump
	   (if (< x x-left) (setf x x-left))
	   (if (> x x-right) (setf x x-right))
	   (if (< (/ (abs delta-x) denom-for-acc) accuracy)
	       (return (values x (1+ j)))))
	  ((<= x-left x x-right)
	   (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
	       (return (values x (1+ j)))))
	  (t
	   (error "jumped out of brackets")))))))

;; Runge Kutta w/adaptive stepsize driver function with stopping condition
(defun rka-stop (fun t0 tfmax x0 &key (a *a-dp*) (bl *bl-dp*) (bh *bh-dp*) (c *c-dp*) (tol 1d-6) (hmax (/ (- tfmax t0) 4)) (h0 (/ (- tfmax t0) 200d0)) (hmin (/ (- tfmax t0) 1d12)) (stopfn #'(lambda (tm x p) tm)) (stopval tfmax) (stoptest #'>=) param &aux (s (length bl)))
  "Runge-Kutta method with adaptive stepsize. States are generic. They may be numbers, arrays, classes, etc., so long as they have state addition and scalar multiplication functions.  Results are collected into a list of independant variables & states."
  (loop
     with flag = t ; flag indicating whether integration completed
     with pow = (/ 1d0 6d0)
     with tm = t0 ; independant variable (e.g. time)
     with h = h0 ; stepsize
     with x = x0 ; state
     for stopvaltm = (funcall stopfn tm x param)
     ;; end: stop value reached, h too small
     until (or (funcall stoptest stopvaltm stopval) (< h hmin))
     for k = (kall fun tm h x s c a param) ; k values used in state estimates
     for xl = (xnext x h k bl) ; lower order estimate
     for xh = (xnext x h k bh) ; higher order estimate
     for err = (norminfx (- xh xl)) ; error estimate
     for allowed = (* tol (max (norminfx x) 1.0)) ; allowable error
     when (<= err allowed) do ; error acceptable
       (setq tm (+ tm h)) ; update dependant variable
       (setq x xh) ; update state
     and collect (list tm x) into out ; collect into return value list
     ;; update step size
     do (setq h (min hmax (* 0.8d0 h (expt (/ allowed (max err 1d-16)) pow))))
     finally (when ; check if stopval reached
		 (not (funcall stoptest stopvaltm stopval))
	       (setq flag nil) ; flag NIL if TF not reached: h too small
	       (warn "Stepsize ~a smaller than minimum ~a." h hmin))
       (return
	 ;; Perform bisection to match stop value in last time interval
	 (destructuring-bind (tf-1 xf-1) (car (last out 2))
	   (destructuring-bind (tf+1 xf+1) (car (last out))
	     ;; Bisection function: take an RK step
	     (labels ((rkstep (t0 tm x0)
			(let* ((h (- tm t0))
			       (k (kall fun t0 h x0 s c a param)))
			  (xnext x0 h k bh)))
		      (bisect-fun (tm)
			(let ((xh (rkstep tf-1 tm xf-1)))
			  (- (funcall stopfn tm xh param)
			     stopval))))
	       ;; Bisect using RK step to find TF
	       (let* ((tf (bisect #'bisect-fun tf-1 tf+1 1d-6 100))
		      ;; Calculate RK step to find XF
		      (xf (rkstep tf-1 tf xf-1)))
		 (values
		  ;; (t0 x0), accumulated (tm x), and (tf xf) replacing last
		  (cons (list t0 x0) 
			(replace out (list (list tf xf)) :start1 (1- (length out))))
		  flag))))))))

;; Runge Kutta w/adaptive stepsize driver function with Newton-Raphson stopping condition
(defun rka-stop-nr (fun t0 tfmax x0 &key (a *a-dp*) (bl *bl-dp*) (bh *bh-dp*) (c *c-dp*) (tol 1d-6) (hmax (/ (- tfmax t0) 4)) (h0 (/ (- tfmax t0) 200d0)) (hmin (/ (- tfmax t0) 1d12)) (stopfn #'(lambda (tm x p) tm)) (stopval tfmax) (stoptest #'>=) param &aux (s (length bl)))
  "Runge-Kutta method with adaptive stepsize. States are generic. They may be numbers, arrays, classes, etc., so long as they have state addition and scalar multiplication functions.  Results are collected into a list of independant variables & states."
  (loop
     with flag = t ; flag indicating whether integration completed
     with pow = (/ 1d0 6d0)
     with tm = t0 ; independant variable (e.g. time)
     with h = h0 ; stepsize
     with x = x0 ; state
     for stopvaltm = (funcall stopfn tm x param)
     ;; end: stop value reached, h too small
     until (or (funcall stoptest stopvaltm stopval) (< h hmin))
     for k = (kall fun tm h x s c a param) ; k values used in state estimates
     for xl = (xnext x h k bl) ; lower order estimate
     for xh = (xnext x h k bh) ; higher order estimate
     for err = (norminfx (- xh xl)) ; error estimate
     for allowed = (* tol (max (norminfx x) 1.0)) ; allowable error
     when (<= err allowed) do ; error acceptable
       (setq tm (+ tm h)) ; update dependant variable
       (setq x xh) ; update state
     and collect (list tm x) into out ; collect into return value list
     ;; update step size
     do (setq h (min hmax (* 0.8d0 h (expt (/ allowed (max err 1d-16)) pow))))
     finally (when ; check if stopval reached
		 (not (funcall stoptest stopvaltm stopval))
	       (setq flag nil) ; flag NIL if TF not reached: h too small
	       (warn "Stepsize ~a smaller than minimum ~a." h hmin))
       (return
	 ;; Perform bisection to match stop value in last time interval
	 (destructuring-bind (tf-1 xf-1) (car (last out 2))
	   (destructuring-bind (tf+1 xf+1) (car (last out))
	     ;; NR function: take an RK step
	     (labels ((rkstep (t0 tm x0)
			(let* ((h (- tm t0))
			       (k (kall fun t0 h x0 s c a param)))
			  (xnext x0 h k bh)))
		      (nr-fun (tm)
			(let ((x (rkstep tf-1 tm xf-1))
			      (dx (funcall fun tm x param)))
			  (values
			   (- (funcall stopfn tm x param)
			      stopval)
			   (funcall stopfn tm dx param)))))
	       ;; Newton Raphson to find TF & XF
	       (let* ((tf (nr #'nr-fun tf-1 tf+1))
		      (xf (rkstep tf-1 tf xf-1)))
		 (values
		  ;; (t0 x0), accumulated (tm x), and (tf xf) replacing last
		  (cons (list t0 x0) 
			(replace out (list (list tf xf)) :start1 (1- (length out))))
		  flag))))))))
