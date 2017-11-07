(asdf:defsystem :bld-ode
  :name "bld-ode"
  :author "Benjamin L. Diedrich <ben@solarsails.info>"
  :license "MIT"
  :description "Ordinary Differential Equation solvers"
  :depends-on ("bld-utils" "bld-gen")
  :components
  ((:file "package")
   (:file "state" :depends-on ("package"))
   (:file "rk" :depends-on ("package" "state"))
   (:file "rk23" :depends-on ("rk"))
   (:file "dp" :depends-on ("rk"))))
