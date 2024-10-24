(load "~/quicklisp/setup.lisp")

; (ql:quickload :veq)
(ql:quickload :evl)
; (defvar a 88888)

(defvar *exprs*
  '(
    ; (!f1 (f!@+ !f1 !f1)) (!f1 (f!@* !f1 !f1)) (!f1 (f!@- !f1 !f1))
    (!f1 x) (!f1 y)
    (!f1 vx) (!f1 vy)
    ; (!f1 (f.@rnd !f1)) (!f1 (rnd:rnd* 1.0))
    (!f2 (f2!@* !f2 !f2))
    (!f2 (f2!@*. !f2 !f1))
    ; (!f2 (f2!@+ !f2 !f2)) (!f2 (f2!@+. !f2 !f1)) (!f2 (f2!@- !f2 !f2))
    ; (!f2 (f2!@-. !f2 !f1)) (!f2 (rnd:2in-circ !f1)) (!f2 (rnd:2in-circ 1.0))
    ; (!f2 (veq:f2 vx vy)) (!f2 (veq:f2 x y))
    ; (!f2a (f2!@$* !f2a !f2)) (!f2a (f2!@$*. !f2a !f1)) (!f2a (f2!@$+ !f2a !f2))
    ; (!f2a (f2!@$+. !f2a !f1)) (!f2a (f2!@$- !f2a !f2)) (!f2a (f2!@$-. !f2a !f1))
    ; (!f2a all-pos) (!f2a all-vel)

    ; (@l1 (cons * !l1))

    (@f1 'vx)
    (@f1 'vy)
    (@f1 'x)
    (@f1 'y)
    ; (@f1 (rnd:rnd* 1.0)) (@f2 (rnd:2in-circ 1.0))
    (@f2 (values 'vx 'vy))
    (@f2 (values 'x 'y))
    ; (@f2a 'all-vel) (@f2a 'all-pos)
    ))

(rnd:set-rnd-state)


(veq:fvdef main ()

  (let* ((code (evl/gen:gen (evl/gen:signatures *exprs*) '!f2))
         (vv (veq:proc-vv code))
         (a 3333)
         (expanded-expr (veq:replace-varg vv))
         )

    ; (xx)

    ; (print code)
    (lqn:out "---")
    ; (print expanded-expr)
    ; (lqn:out code)
    (lqn:out "---")
    (veq:vp (evl:evl
              ; expanded-expr
              '(progn
                 (list 1)
                 (print (cond ((< 1 2))))
                 (print (cond ((< 2 1))))
                 (print (cond ((< 1 2) :x)))
                 (print (cond ((< 2 1) :y)))

                 (print (cond ((< 1 2)) ((< 2 4))))
                 (print (cond ((< 2 1)) ((> 4 3))))


                 )
              (evl:env/new `((x . 1.0) (y . 2.0)
                             (vx . 3.0) (vy . 4.0)
                             ,@evl:+std-env+))))

    ; (veq:vp (eval `(veq:fvprogn
    ;                  (let ((x 1.0) (y 2.0) (vx 3.0) (vy 4.0))
    ;                    ,code))))
    ))

; (cond ())
; (if )

(main)

(print (when 1))

