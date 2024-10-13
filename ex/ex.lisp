#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :evl)
(in-package :evl)

(defun main ()
  (let ((kv '((+ . +) (- . -) (/ . /) (* . *)
              (= . equal) (< . <) (> . >)
              (1+ . 1+) (1- . 1-)
              (t . t)
              (print . print) (list . list)
              (s . 104) (k . 107))))
    (labels ((env (x &aux (res (assoc x kv)))
               (if res (cdr res) (error "EVL: undefined variable: ~a" x))))

      (print (evl '1 #'env))
      (print (evl 's #'env))
      (print (evl '(+ s k) #'env))

      (print (evl '(list s k) #'env))
      (print (evl '(progn s k :progn) #'env))

      (print (evl '(if (< 4 1) 2 3) #'env))
      (print (evl '(if (< 4 1) 2) #'env))

      (print (funcall (evl '(lambda (x) x) #'env) 99))

      (print (evl '((lambda (x) (+ s x -10000)) 888) #'env))
      (print (evl '((lambda (x y) (+ s x y)) 888 999) #'env))
      (print (evl '((lambda () 77)) #'env))

      (print (evl '(let ((a 1) (b 20)) (+ a b)) #'env))
      (print (evl '(let ((a 1) (b 20)) (+ a b) (- a b)) #'env))
      ; (print (evl '(let ((a 1) (b (1+ a))) ; this should not work
      ;                (list a b)) #'env))
      (print (evl '(let ((fx (lambda (x) (+ 1 x))))
                     (fx (fx 1)))
                  #'env))

      (print (evl `(label fact (x) (if (= x 0)
                                       1
                                       (* x (fact (1- x))))
                     (fact 7))
                  #'env)))))

(main)

