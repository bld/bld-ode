(in-package :bld-ode)

;; Bogacki-Shampine coefficients

(defparameter *c-bs*
  (make-array
   4 :element-type 'double-float
   :initial-contents (list 0d0 (/ 2d0) (/ 3d0 4d0) 1d0))
  "Bogacki-Shampine C coefficients")

(defparameter *a-bs*
  (make-array
   '(4 4) :element-type 'double-float
   :initial-contents
   (list (list 0d0 0d0 0d0 0d0)
	 (list (/ 2d0) 0d0 0d0 0d0)
	 (list 0d0 (/ 3d0 4d0) 0d0 0d0)
	 (list (/ 2d0 9d0) (/ 3d0) (/ 4d0 9d0) 0d0)))
  "Bogacki-Shampine A coefficients")

(defparameter *bl-bs*
  (make-array
   4 :element-type 'double-float
   :initial-contents (list (/ 2d0 9d0) (/ 3d0) (/ 4d0 9d0) 0d0))
  "Bogacki-Shampine 2nd order B coefficients")

(defparameter *bh-bs*
  (make-array
   4 :element-type 'double-float
   :initial-contents (list (/ 7d0 24d0) (/ 4d0) (/ 3d0) (/ 8d0)))
  "Bogacki-Shampine 3rd order B coefficients")

(defun rk23 (fun t0 tf x0
	     &key (tol 1d-6)
	       (hmax (/ (- tf t0) 4))
	       (h0 (/ (- tf t0) 200d0))
	       (hmin (/ (- tf t0) 1d12))
	       param)
  "Adaptive stepsize Runge-Kutta algorithm with 2nd/3rd order Bogacki-Shampine coefficients"
  (rka fun t0 tf x0 :a *a-bs* :bl *bl-bs* :bh *bh-bs* :c *c-bs* :hmax hmax :h0 h0 :hmin hmin :param param))
