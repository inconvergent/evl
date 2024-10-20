
; (in-system :auxin)

(load "~/quicklisp/setup.lisp")
(load "./gen.lisp")
; (ql:quickload :veq)
(ql:quickload :evl)
(defvar *s* 1000.0)
(defvar *m* (* 0.5 *s*))

; (rnd:set-rnd-state)
(rnd:set-rnd-state 1)
; (ql:quickload :evl) ; must be available locally. eg. in ~/common-lisp/evl
; (in-package :evl)


(veq:fvdef main (fn)

  (let* ((code (rnd-code *exprs* '!f2))
        (vv (veq:proc-vv code))
        (expanded-expr (veq:replace-varg vv))
        )
    (lqn:out code)
    (lqn:out "---")
    (lqn:out "---")
   (veq:vpr  (evl:evl* expanded-expr
             (evl:new-env `((x . 1.0) (y . 2.0)
                            (vx . 3.0) (vy . 4.0)
                            ,@evl:+std-env+))
    ))

   ; (veq:vpr (evl:evl* '(multiple-value-list (multiple-value-bind (a b c)
   ;                       (values 1 2 (values 3 4)) (list b a c)))))
   (veq:vpr (evl:evl* '(multiple-value-list (values 1 2 (values 3 4)))))

   ; (veq:vpr (evl:evl* '))
   ;   (veq:vpr (funcall (evl:evl* '(lambda () (values 1 2 3))
   ;                                        )))
   ))



; (let ((a 1)
;       (b 2))
;   (list b a))

; ; ==

; ((lambda (a b)
;    (list b a))
;  1 2)

(main (or (second (auxin:cmd-args)) "tmp"))

    ; '(~ 1 2 (~ 3 4 5 6))
    ; '(~ 1 2 (~ 3 4 5))
    ; '(values (+ 1 2) -2 -3)
    ; '(~ (+ 1 2) -2 -3)
    ; '(~ (+ 1 2) -2 (~ 899 99))
