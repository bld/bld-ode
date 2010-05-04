(defpackage :bld.ode.system 
    (:use :asdf :cl))
(in-package :bld.ode.system)

(defsystem :bld-ode
  :name "bld-ode"
  :author "Benjamin L. Diedrich <ben@solarsails.info>"
  :version "0.0.1"
  :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
  :license "MIT"
  :description "Ordinary Differential Equation solvers"
  :depends-on ("bld-utils")
  :components
  ((:file "package")
   (:file "state" :depends-on ("package"))
   (:file "rk" :depends-on ("package" "state"))))
