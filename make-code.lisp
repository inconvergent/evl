
; (in-system :auxin)

(load "~/quicklisp/setup.lisp")
(load "./gen.lisp")
(ql:quickload :evl)
(defvar *s* 1000.0)
(defvar *m* (* 0.5 *s*))

(rnd:set-rnd-state)
; (ql:quickload :evl) ; must be available locally. eg. in ~/common-lisp/evl
; (in-package :evl)

; xprt prints the expression and the output

(veq:fvdef compile-agent (a*)
  (labels ((wrap-pfx () `(veq:fvprogn
                           (lambda (a)
                             (declare (agent a))
                             (veq:xlet ((f!x (agent-x a)) (f!y (agent-y a))
                                        (f!vx (agent-vx a)) (f!vy (agent-vy a)))
                               (f2.@trunc ,(agent-pfx a*))))))
           (wrap-vfx () `(veq:fvprogn
                           (lambda (all-pos all-vel a)
                             (declare (veq:fvec all-pos all-vel) (agent a))
                             (veq:xlet ((f!x (agent-x a)) (f!y (agent-y a))
                                        (f!vx (agent-vx a)) (f!vy (agent-vy a)))
                              (f2.@trunc ,(agent-vfx a*) 100f0))))))
    (setf (agent-pfxx a*) (eval (wrap-pfx))
          (agent-vfxx a*) (eval (wrap-vfx))))
  a*)

(veq:fvdef make-agents (n)
  (21%@$init (f2!@$+ (veq:f2$polygon n 50f0) (?@ (f2!@+ *m* *m* (rnd:2in-circ 10f0))))
             ((x y) (compile-agent (m@-make-agent x y
                                      (rnd:2in-circ 1f0)
                                      (init-pfx) (rnd-code *exprs* '!f2))))))

(veq:fvdef main (fn)
  (veq:vpr (evl:evl*
    ; (print (veq:replace-varg (veq:proc-vv (rnd-code *exprs* '!f2))))
    ; '(~ 1 2 (~ 3 4 5 6))
    ; '(~ 1 2 (~ 3 4 5))
    '(values (+ 1 2) -2 -3)
    ))

           )


(main (or (second (auxin:cmd-args)) "tmp"))
