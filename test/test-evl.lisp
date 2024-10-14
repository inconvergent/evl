(in-package #:evl-tests)

(plan 1)

(subtest "evl"

  (let* ((s 104) (k 107)
         (kv `((s . ,s) (k . ,k)
               (myfx . ,(lambda (k) (+ (sin k) (cos (- k)))))
               (10+ . ,(lambda (x) (+ 10 x)))
               ,@+std-env+)))

    (labels ((env (x &aux (res (assoc x kv)))
               (if res (cdr res) (error "EVL: undefined variable: ~a" x))))

      (is (evl '1 #'env) 1)
      (is (evl 's #'env) 104)
      (is (evl '(list s m) (evl/extenv #'env '(s m) '(105 106))) '(105 106))

      (is (evl '(+ s k) #'env) (+ s k))
      (is (evl '(myfx 1) #'env) 1.3817732)

      (is (evl '(quote 1) #'env) 1)
      (is (evl '(quote s) #'env) 's)
      (is (evl '(quote (+ 1 2)) #'env) '(+ 1 2))
      (is (evl '(quote (+ some-symbol 2)) #'env) '(+ some-symbol 2))
      (is (evl '(list s k) #'env) `(,s ,k))
      (is (evl '(progn s k :progn) #'env) :progn)

      (is (evl '(if (< 4 1) 2 3) #'env) 3)
      (is (evl '(if (< 4 1) 2) #'env) nil)
      (is (evl '(cond ((< 2 1) 7) ((< 1 2) 8) (t :aa))
               #'env)
          8)
      (is (evl '(cond ((< 1 2) 7) ((< 2 1) 8) (t :aa))
               #'env)
          7)
      (is (evl '(cond ((< 2 1) 7) ((< 3 1) 8) (t :aa))
               #'env)
          :aa)

      (is (funcall (evl '(lambda (x) x) #'env) 99) 99)

      (is (evl '((lambda () 77)) #'env) 77)
      (is (evl '((lambda (x) (+ s x -10000)) 888) #'env) (+ s 888 -10000))
      (is (evl '((lambda (x y) (+ s x y)) 888 999) #'env) (+ s 888 999))
      ; (is (evl '((lambda (&rest rest) rest) 999 333) #'env) :aaaa) ; TODO

      (is (evl '(let ((a 1) (b 20)) (+ a b)) #'env) (+ 1 20))
      (is (evl '(let ((a 1) (b 20)) (+ a b) (- a b)) #'env) (- 1 20))

      (is-error (evl '(let ((a 1) (b (1+ a)))
                     (list a b)) #'env)
                error)

      (is (evl '(let ((fx (lambda (x) (+ 1 x))))
                     (fx (fx 1)))
                  #'env)
          3)

      (is (evl '(labels ((fact (x) (if (= x 0)
                                       1
                                       (* x (fact (1- x))))))
                 (fact 7))
               #'env)
          5040)

      (is (evl '(labels ((add0 (x) (add1 (sub1 x)))
                         (add1 (x) (1+ x))
                         (sub1 (x) (1- x)))
                  (add0 7))
               #'env)
          7)

      (is (evl '(destructuring-bind ((xxx yyy) &rest zzz)
                      '((:a :b) :c :d)
                  (list :ok yyy xxx zzz))
             #'env)
          '(:ok :b :a (:c :d)))
      (is (evl '(destructuring-bind ((xxx yyy) &rest zzz)
                      (list (list :a :b) :c :d (10+ 1))
                  (list :ok xxx yyy zzz))
               #'env)
          '(:ok :a :b (:c :d 11)))

      (is (evl '(destructuring-bind (aa &optional b&b)
                      (list 2)
                  (list :ok aa b&b 3))
               #'env)
          '(:ok 2 nil 3))
      )))

(unless (finalize) (error "error in test-evl"))
