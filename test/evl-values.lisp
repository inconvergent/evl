(in-package #:evl-tests)

(plan 1)

(subtest "evl-values"
   (is (multiple-value-list (evl* '(values 1 2 3))) '(1 2 3))
   (is (multiple-value-list (evl* '(values 1 (values 2 3) 4))) '(1 2 4))
   (is (multiple-value-list (evl* '(~ 1 (values 2 3) 4))) '(1 2 3 4))

   (is (multiple-value-list (evl* '(~ 1 (~ 2 (~ 3 4)) 5))) '(1 2 3 4 5))
   (is (multiple-value-list (evl* '(~ 1 2 (~ (+ 3 4) (~ 3 4)) 5))) '(1 2 7 3 4 5)))


(unless (finalize) (error "error in evl-values"))
