(load "~/quicklisp/setup.lisp")
(load "./gen.lisp")
; (ql:quickload :veq)
(ql:quickload :evl)
(defvar *s* 1000.0)
(defvar *m* (* 0.5 *s*))

(rnd:set-rnd-state)

(veq:fvdef main ()

  (let* ((code (rnd-code *exprs* '!f2))
         (vv (veq:proc-vv code))
         (expanded-expr (veq:replace-varg vv)))
    (lqn:out code)
    (lqn:out "---")
    (lqn:out "---")
    (veq:vpr (evl:evl* expanded-expr
               (evl:new-env `((x . 1.0) (y . 2.0)
                              (vx . 3.0) (vy . 4.0)
                              ,@evl:+std-env+))))

    (veq:vpr (evl:evl*
               '(+
                 1 (evl:evl* '1)
                 )))
    ))

(main)

