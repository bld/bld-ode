bld-ode
=======

Ordinary differential equations solvers in Common Lisp
Currently includes adaptive stepsize Runge-Kutta method.
Define a function of independant variable & state object as the differential equation to solve.
State may be any object with generic arithmetic methods for addition, subtraction, and multiplication by a scalar number defined. Number, vector, and hash table state methods are provided. An infinity norm method is also required, which is defined in the generic function NORMINFX.

Usage
-----

    (rka fun t0 tf x0 :a *a-dp* :bl *bl-dp* :bh *bh-dp* :c *c-dp* :tol 1d-6 :hmax 0.25 :h0 (/ (- tf t0) 200d0) :hmin (/ (- tf t0) 1d12) :param parameter)

Arguments
---------

- FUN: function of independant variable & state, e.g. (fun tn x)
- T0: starting independant variable
- TF: final independant variable
- X0: state at T0

Optional keyword arguments
--------------------------

- :A - A matrix (default Dormand-Prince)
- :BL - lower order B vector (default Dormand-Prince)
- :BH - higher order B vector (default Dormand-Prince)
- :C - C vector (default Dormand-Prince)
- :TOL - relative tolerance
- :HMAX - max stepsize
- :H0 - initial stepsize
- :HMIN - minimum stepsize
- :PARAM - parameter to pass to ODE function

Examples
--------

Integrate (cos (* p tn)) over tn range 0 to 2pi where p = 2:

    (defun testfun (tn x &optional p)	
      (cos (* p tn)))
    (rka #'testfun 0d0 (* 2d0 pi) 0d0 :param 2)

Custom State Classes
--------------------

Vector and hash tables are provided as state types you can use. If you
define a custom class as a state, the macro DEFSTATEARITHMETIC can be
used to define methods on your class for the state arithemetic used in
BLD-ODE. If your :INITARG entries are different from the slot names,
you can define them - in the same order as SLOTS - using the keyword
argument :INITARGS

You can choose which slots of your class to use in the state algebra,
leaving some out to serve other purposes.

Usage:

    (defclass state ()
       ((x :initarg :x)
        (dx :initarg :dx)))
    (defstatearithmetic state (x dx))

Optionally specify the INITARG entries:

    (defclass state ()
       ((x :initarg :xarg)
        (dx :initarg :dxarg)))
    (defstatearithmetic state (x dx) :initargs (:xarg :dxarg))

Also, you can specify data slots that are simply copied and not used
in integration:

    (defclass state ()
       ((x :initarg :x)
        (dx :initarg :dx)
	(data1)
	(data2)))
    (defstatearithmetic state (x dx) :oslots (data1 data2))
