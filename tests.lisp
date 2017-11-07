(asdf:load-system :bld-ode)
(asdf:load-system :fiveam)

(defpackage :bld-ode-tests
  (:use :cl :bld-ode :fiveam))

(in-package :bld-ode-tests)

(def-suite :bld-ode)

(in-suite :bld-ode)

(test rka
  (is-true (let* ((tol 1d-11)
		  (results (rka #'(lambda (tm x p) (cos tm)) 0 pi 0 :tol tol)))
	     (every #'identity
		    (loop for (tm x) in results
		       collect (< (abs (- x (sin tm))) tol))))))
