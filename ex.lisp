#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :evl) ; must be available locally. eg. in ~/common-lisp/evl
(in-package :evl)

; xprt prints the expression and the output

(defun main ()
  (let ((kv `((s . 104) (k . 107)                            ; custom var
              (myfx . ,(lambda (k) (+ (sin k) (cos (- k))))) ; custom fx
              ,@+std-env+)))
    (labels ((env (x &aux (res (assoc x kv)))
               (if res (cdr res) (error "EVL: undefined variable: ~a" x)))
             (evl. (a) (evl a #'env))) ; always use env

      (xprt (evl. '1))
      (xprt (evl. 's))
      (xprt (evl. '(+ s k)))
      (xprt (evl. '(myfx 1)))

      (xprt (evl. '(quote (+ 1 2))))
      (xprt (evl. '(quote 1)))
      (xprt (evl. '(quote s)))

      (xprt (evl. '(list s k)))
      (xprt (evl. '(progn s k :progn)))

      (xprt (evl. '(if (< 4 1) 2 3)))
      (xprt (evl. '(if (< 4 1) 2)))

      (xprt (funcall (evl. '(lambda (x) x)) 99))

      (xprt (evl. '((lambda (x) (+ s x -10000)) 888)))
      (xprt (evl. '((lambda (x y) (+ s x y)) 888 999)))
      (xprt (evl. '((lambda () 77))))

      (xprt (evl. '(let ((a 1) (b 20)) (+ a b))))
      (xprt (evl. '(let ((a 1) (b 20)) (+ a b) (- a b))))

      (xprt (evl. '(let ((fx (lambda (x) (+ 1 x))))
                      (fx (fx 1)))))

      (xprt (evl. '(labels ((fact (x) (if (= x 0)
                                          1
                                          (* x (fact (1- x))))))
                     (fact 7))))

      (xprt (evl. '(labels ((add0 (x) (add1 (sub1 x)))
                           (add1 (x) (1+ x))
                           (sub1 (x) (1- x)))
                    (add0 7))))

      (xprt (evl. '(cond ((< 2 1) 7)
                         ((< 1 2) 8)
                         (t :aa))))

      (xprt (evl. '((lambda (&rest rest) rest) 999 333))))))

(main)

