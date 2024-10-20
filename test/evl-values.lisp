(in-package #:evl-tests)

(plan 1)

(subtest "evl-values"
   (is (multiple-value-list
         (evl* '(values 1 2 3))) '(1 2 3))
   (is (multiple-value-list
         (evl* '(values 1 (values 2 3) 4))) '(1 2 4))
   (is (multiple-value-list
         (evl* '(~ 1 (values 2 3) 4))) '(1 2 3 4))

   (is (multiple-value-list
         (evl* '(~ 1 (~ 2 (~ 3 4)) 5))) '(1 2 3 4 5))
   (is (multiple-value-list
         (evl* '(~ 1 2 (~ (+ 3 4) (~ 3 4)) 5))) '(1 2 7 3 4 5))

   (is (evl* '(multiple-value-bind (a b)
                (values 1 2) (list b a)))
       '(2 1))

   (is (evl* '(multiple-value-bind (a b c)
                (values 1 2 (values 3 4)) (list b a c)))
       '(2 1 3))
   (is (multiple-value-list
         (funcall (evl:evl* '(lambda () (values 1 2 3)))))
       '(1 2 3)

    (is (multiple-value-list
          (evl* '(multiple-value-bind (a b)
                   (values 1 2 ) (values b a))))
        '(2 1))
    (is (evl* '(multiple-value-list (values 1 2))) '(1 2))
    (is (evl* '(multiple-value-list
                   (multiple-value-bind (a b)
                     (values 1 2 ) (values b a))))
        '(2 1))

    (is (evl* '(evl:mvl (evl:mvb (a b) (~ 1 2 ) (~ b a)))) '(2 1))
    (is (multiple-value-list (evl* '(progn (values 3 2 1)
                                           (values -3 -2 -1))))
        '(-3 -2 -1))
    (is (evl* '(multiple-value-list (progn (values 3 2 1)
                                           (values -3 -2 -1))))
        '(-3 -2 -1))
    ))

(unless (finalize) (error "error in evl-values"))
