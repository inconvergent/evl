(in-package #:evl-tests)

(plan 1)

(subtest "evl"

  (let* ((s 104) (k 107)
         (kv `((+ . +) (- . -) (/ . /) (* . *)
               (= . equal) (< . <) (> . >)
               (1+ . 1+) (1- . 1-)
               (t . t)
               (print . print) (list . list)
               (s . ,s) (k . ,k))))

    (labels ((env (x &aux (res (assoc x kv)))
               (if res (cdr res) (error "EVL: undefined variable: ~a" x))))

      (is (evl '1 #'env) 1)
      (is (evl 's #'env) 104)
      (is (evl '(+ s k) #'env) (+ s k))

      (is (evl '(list s k) #'env) `(,s ,k))
      (is (evl '(progn s k :progn) #'env) :progn)

      (is (evl '(if (< 4 1) 2 3) #'env) 3)
      (is (evl '(if (< 4 1) 2) #'env) nil)

      (is (funcall (evl '(lambda (x) x) #'env) 99) 99)

      (is (evl '((lambda () 77)) #'env) 77)
      (is (evl '((lambda (x) (+ s x -10000)) 888) #'env) (+ s 888 -10000))
      (is (evl '((lambda (x y) (+ s x y)) 888 999) #'env) (+ s 888 999))

      (is (evl '(let ((a 1) (b 20)) (+ a b)) #'env) (+ 1 20))
      (is (evl '(let ((a 1) (b 20)) (+ a b) (- a b)) #'env) (- 1 20))

      ; this should not work
      (is-error (evl '(let ((a 1) (b (1+ a)))
                     (list a b)) #'env)
                error)

      (is (evl '(let ((fx (lambda (x) (+ 1 x))))
                     (fx (fx 1)))
                  #'env)
          3)

      (is (evl '(evl:label fact (x) (if (= x 0)
                                        1
                                        (* x (fact (1- x))))
                  (fact 7))
               #'env)
          5040))))

(unless (finalize) (error "error in test-evl"))
