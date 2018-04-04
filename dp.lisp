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

;; Dormand-Prince 78 coefficients
;; See: Prince & Dormand, "High order embedded Runge-Kutta formulae",
;; Journal of Computational and Applied Mathematics, Vol 7, Issue 1, March 1981, Pages 67-75
;; Available from: http://www.sciencedirect.com/science/article/pii/0771050X81900103

(defparameter *a-dp78*
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
    (list (/ 246121993d0 1340847787d0) 0d0 0d0 (/ -37695042795d0 15268766246d0) (/ -309121744d0 1061227803d0) (/ -12992083d0 490766935d0) (/ 6005943493d0 2108947869d0) (/ 393006217d0 1396673457d0) (/ 123872331d0 1001029789d0) 0d0 0d0 0d0 0d0)
    (list (/ -1028468189d0 846180014d0) 0d0 0d0 (/ 8478235783d0 508512852d0) (/ 1311729495d0 1432422823d0) (/ -10304129995d0 1701304382d0) (/ -48777925059d0 3047939560d0) (/ 15336726248d0 1032824649d0) (/ -45442868181d0 3398467696d0) (/ 3065993473d0 597172653d0) 0d0 0d0 0d0)
    (list (/ 185892177d0 718116043d0) 0d0 0d0 (/ -3185094517d0 667107341d0) (/ -477755414d0 1098053517d0) (/ -703635378d0 230739211d0) (/ 5731566787d0 1027545527d0) (/ 5232866602d0 850066563d0) (/ -4093664535d0 808688257d0) (/ 3962137247d0 1805957418d0) (/ 65686358d0 487910083d0) 0d0 0d0)
    (list (/ 403863854d0 491063109d0) 0d0 0d0 (/ -5068492393d0 434740067d0) (/ -411421997d0 543043805d0) (/ 652783627d0 914296604d0) (/ 11173962825d0 925320556d0) (/ -13158990841d0 6184727034d0) (/ 3936647629d0 1978049680d0) (/ -160528059d0 685178525d0) (/ 248638103d0 1413531060d0) 0d0 0d0)))
  "Runge-Kutta A-matrix from Prince & Dormand, 'High order embedded Runge-Kutta formulae', Journal of Computational and Applied Mathematics, Vol 7, Issue 1, March 1981, Pages 67-75. Available from: http://www.sciencedirect.com/science/article/pii/0771050X81900103)")

(defparameter *c-dp78*
  (make-array
   13
   :element-type 'double-float
   :initial-contents
   (list 0d0
	 (/ 18d0)
	 (/ 12d0)
	 (/ 8d0)
	 (/ 5d0 16d0)
	 (/ 3d0 8d0)
	 (/ 59d0 400d0)
	 (/ 93d0 200d0)
	 (/ 5490023248d0 9719169821d0)
	 (/ 13d0 20d0)
	 (/ 1201146811d0 1299019798d0)
	 1d0
	 1d0))
  "Runge-Kutta C-vector from Prince & Dormand, 'High order embedded Runge-Kutta formulae', Journal of Computational and Applied Mathematics, Vol 7, Issue 1, March 1981, Pages 67-75. Available from: http://www.sciencedirect.com/science/article/pii/0771050X81900103)")

(defparameter *bl-dp78*
  (make-array
   13
   :element-type 'double-float
   :initial-contents
   (list (/ 14005451d0 335480064d0)
	 0d0
	 0d0
	 0d0
	 0d0
	 (/ -59238493d0 1068277825d0)
	 (/ 181606767d0 758867731d0)
	 (/ 561292985d0 797845732d0)
	 (/ -1041891430d0 1371343529d0)
	 (/ 760417239d0 1151165299d0)
	 (/ 118820643d0 751138087d0)
	 (/ -528747749d0 2220607170d0)
	 0d0))
  "Runge-Kutta B-low-vector from Prince & Dormand, 'High order embedded Runge-Kutta formulae', Journal of Computational and Applied Mathematics, Vol 7, Issue 1, March 1981, Pages 67-75. Available from: http://www.sciencedirect.com/science/article/pii/0771050X81900103)")

(defparameter *bh-dp78*
  (make-array
   13
   :element-type 'double-float
   :initial-contents
   (list (/ 13451932d0 455176623d0)
	 0d0
	 0d0
	 0d0
	 0d0
	 (/ -808719846d0 976000145d0)
	 (/ 1757004468d0 5645159321d0)
	 (/ 656045339d0 26589118d0)
	 (/ -3867574721d0 1518517206d0)
	 (/ 465885868d0 322736535d0)
	 (/ 53011238d0 667516719d0)
	 (/ 2d0 45d0)
	 0d0))
  "Runge-Kutta B-high-vector from Prince & Dormand, 'High order embedded Runge-Kutta formulae', Journal of Computational and Applied Mathematics, Vol 7, Issue 1, March 1981, Pages 67-75. Available from: http://www.sciencedirect.com/science/article/pii/0771050X81900103)")
