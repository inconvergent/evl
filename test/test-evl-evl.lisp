(in-package #:evl-tests)

(plan 1)

(let ((kv `((mfx . ,(lambda (x) (* 1000 x)))
            (xx . 12) (yy . 13)
            ,@+std-env+)))

(subtest "evl-evl"
  (labels ((env (x) (let ((res (assoc x kv)))
                      (if res (cdr res)
                              (error "[EVL]: undefined variable: ~a" x)))))
    (is (evl
          `(labels
             ((env. (x) (let ((res (assoc x '((yy . 13000) ,@kv))))
                          (if res (cdr res)
                                  (error "[*EVL*]: undefined variable: ~a" x))))
              (evl. (expr env.)
                (cond ((null expr) expr)
                      ((stringp expr) expr)
                      ((numberp expr) expr)
                      ((functionp expr) expr)
                      ((keywordp expr) expr)
                      ((symbolp expr) (env. expr))

                      ((car-is expr 'quote) (cadr expr))

                      ((car-is expr 'progn)
                       (first (last (mapcar (lambda (e) (evl. e env.))
                                            (cdr expr)))))

                      ((car-is expr 'dsb*)
                       (dsb (vars in &rest rest) (cdr expr)
                         (evl/eval-dsb vars in rest evl. env.)))

                      ((car-is expr 'if)
                       (dsb (test then &optional else) (cdr expr)
                         (if (evl. test env.) (evl. then env.) (evl. else env.))))

                      ((car-is expr 'cond)
                       (dsb ((cnd x) &rest rest) (cdr expr)
                         (evl/do-cond cnd x rest evl. env.)))

                      ((car-is expr 'lambda)
                       (dsb (kk &rest rest) (cdr expr)
                         (evl/eval-lambda kk rest evl. env.)))

                      ((car-is expr 'labels)
                       (dsb (pairs &rest body) (cdr expr)
                         (evl/do-labels pairs body evl. env.)))

                      ((car-is expr 'let)
                       (dsb (vars &rest body) (cdr expr)
                         (evl/do-let vars body evl. env.)))

                      ((consp expr)
                       (apply (evl. (car expr) env.)
                              (mapcar (lambda (x) (evl. x env.))
                                      (cdr expr)))))))
             (list (evl. 'xx env.)
                   (evl. 'yy env.)
                   (evl. '(progn xx) env.)
                   (evl. '(progn yy) env.)
                   (evl. '(let ((xx -11) (yy :hello)) (list xx yy)) env.)
                   (evl. '(mfx xx) env.)
                   (evl. '(mfx yy) env.)
                   (evl. '((lambda (x) x) :val) env.)
                   (evl. '(labels ((add0 (x) (add1 (sub1 x)))
                                   (add1 (x) (1+ x))
                                   (sub1 (x) (1- x)))
                            (add0 7)) env.)
                   (evl. '(cond ((< 1 2) :yes)) env.)
                   (evl. '(cond ((< 2 1) :yes) ((> 2 1) :no)) env.)
                   (evl. '(dsb* (x) '(11) x) env.)
                   (evl. '(dsb* (&rest rest) '(1 2 3) rest) env.)))
          #'env)
        '(12 13000 12 13000 (-11 :hello) 12000 13000000 :val 7
          :yes :no 11 (1 2 3))))))

(unless (finalize) (error "error in test-evl-evl"))



