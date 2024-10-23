(load "~/quicklisp/setup.lisp")

(ql:quickload :evl)
(in-package :evl)

(veq:fvdef main ()

  (evl:with-env ()

    (labels (
             ; (dat (a) (lambda () (if a (values (car a) (cdr a))
             ;                           (~ nil nil))))
             (dat (i) (lambda () (if (< i 10) (~ i (1+ i))
                                              (~ nil nil))))
             (gen (val-fx res-fx)
               (lambda () (mvb (val nxt) (val-fx)
                            (res-fx val)
                            (when nxt
                                  (gen (dat nxt) res-fx)))))

             (g/doall  (gen-fx) (when gen-fx (g/doall (gen-fx))))
             (g/do1 (gen-fx) (g/don 1 genfx))
             (g/don (n gen-fx)
               (when (and gen-fx (> n 0))
                     (g/don (1- n) (gen-fx)))))
     (g/don 2 (gen (dat 0) print))
     (print (or nil 2))
     (print (and ))

     ; (g/don 3 (gen (dat '( :aa :bb :abc 34)) print))

     )
    ))

(main)


