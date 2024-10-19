
; (in-system :auxin)

(load "~/quicklisp/setup.lisp")
(load "./gen.lisp")
(ql:quickload :evl)
(defvar *s* 1000.0)
(defvar *m* (* 0.5 *s*))

(rnd:set-rnd-state)
; (ql:quickload :evl) ; must be available locally. eg. in ~/common-lisp/evl
; (in-package :evl)


(veq:fvdef main (fn)
  (veq:vpr (evl:evl*
    (print (veq:replace-varg (veq:proc-vv (rnd-code *exprs* '!f2))))
    ; '(~ 1 2 (~ 3 4 5 6))
    ; '(~ 1 2 (~ 3 4 5))
    ; '(values (+ 1 2) -2 -3)
    ; '(~ (+ 1 2) -2 -3)
    ; '(~ (+ 1 2) -2 (~ 899 99))
    ))

           )


(main (or (second (auxin:cmd-args)) "tmp"))
