(defpackage :bld-ode
  (:use :cl)
  (:shadowing-import-from :bld-gen
    + - * / expt
    sin cos tan
    atan asin acos
    sinh cosh tanh
    asinh acosh atanh
    log exp sqrt abs
    min max signum)
  (:import-from :bld-utils make-keyword maphash2)
  (:import-from :bld-gen defmeth2 defmeth1 defmeth12)
  (:export :norminfx
	   :rka))
