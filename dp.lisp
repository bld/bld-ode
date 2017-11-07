(in-package :bld-ode)

;; Dormand-Prince 45 coefficients from GMAT Math Spec

(defparameter *a-dp45*
  (make-array
   '(7 7)
   :element-type 'double-float
   :initial-contents
   (list
    (list 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 2d0 9d0) 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 12d0) (/ 4d0) 0d0 0d0 0d0 0d0 0d0)
    (list (/ 55d0 324d0) (/ -25d0 10d0) (/ 50d0 81d0) 0d0 0d0 0d0 0d0)
    (list (/ 83d0 330d0) (/ -13d0 22d0) (/ 61d0 66d0) (/ 9d0 110d0) 0d0 0d0 0d0)
    (list (/ -19d0 28d0) (/ 9d0 4d0) (/ 7d0) (/ -27d0 7d0) (/ 22d0 7d0) 0d0 0d0)
    (list (/ 19d0 200d0) 0d0 (/ 3d0 5d0) (/ -243d0 400d0) (/ 33d0 40d0) (/ 7d0 80d0) 0d0)))
  "A-matrix Prince-Dormand 45 coefficients from GMAT Math Spec, called b matrix")

(defparameter *bl-dp45*
  (make-array
   7
   :element-type 'double-float
   :initial-contents
   (list (- (/ 19d0 200d0) (/ 431d0 5000d0))
	 0d0
	 (- (/ 3d0 5d0) (/ 333d0 500d0))
	 (+ (/ -243d0 400d0) (/ 7857d0 10000d0))
	 (- (/ 33d0 40d0) (/ 957d0 1000d0))
	 (- (/ 7d0 80d0) (/ 193d9 2000d0))
	 (/ 50d0)))
  "BL-vector Prince-Dormand 45 coefficients from GMAT Math Spec, called e vector")

(defparameter *bh-dp45*
  (make-array
   7
   :element-type 'double-float
   :initial-contents
   (list (/ 19d0 200d0)
	 0d0
	 (/ 3d0 5d0)
	 (/ -243d0 400d0)
	 (/ 33d0 40d0)
	 (/ 7d0 80d0)
	 0d0))
  "BH-vector Prince-Dormand 45 coefficients from GMAT Math Spec, called c vector")

(defparameter *c-dp45*
  (make-array
   7
   :element-type 'double-float
   :initial-contents
   (list 0d0
	 (/ 2d0 9d0)
	 (/ 3d0)
	 (/ 5d0 9d0)
	 (/ 2d0 3d0)
	 1d0
	 1d0))
   "C-vector Prince-Dormand 45 coefficients from GMAT Math Spec, called a vector")

;; Dormand-Prince 45 coefficients from GMAT 2017a Math Spec

#|
(defparameter *a-dp45*
  (make-array
   '(13 13)
   :element-type 'double-float
   :initial-contents
   (list
    (list 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 18d0) 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 48d0) (/ 16d0) 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 32d0) 0d0 (/ 3d0 32d0) 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 5d0 16d0) 0d0 (/ -75d0 64d0) (/ 75d0 64d0) 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 3d0 80d0) 0d0 0d0 (/ 3d0 16d0) (/ 3d0 20d0) 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 29443841d0 614563906d0) 0d0 0d0 (/ 77736538d0 692538347d0) (/ -28693883d0 1125000000d0) (/ 23124283d0 1800000000d0) 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 16016141d0 946692911d0) 0d0 0d0 (/ 61564180d0 158732637d0) (/ 22789713d0 633445777d0) (/ 545815736d0 2771057229d0) (/ -180193667d0 1043307555d0) 0d0 0d0 0d0 0d0 0d0 0d0)
    (list (/ 39632708d0 573591083d0) 0d0 0d0 (/ -433636366d0 683701615) (/ -421739975d0 2616292301d0) (/ 100302831d0 723423059d0) (/ 790204164d0 839813087d0) (/ 800635310d0 3783071287d0) 0d0 0d0 0d0 0d0 0d0)
    ;; TBD
    )
  "A-matrix Prince-Dormand 78 coefficients from GMAT Math Spec, called b matrix")
|#
