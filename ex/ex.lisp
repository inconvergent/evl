#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :evl) ; must be available locally. eg. in ~/common-lisp/evl
(in-package :evl)

; xprt prints the expression and the output

(defun main ()
  (let ((kv `((+ . +) (- . -) (/ . /) (* . *) (1+ . 1+) (1- . 1-) ; pass through fxs
              (t . t) (= . equal) (< . <) (> . >)
              (car . car) (cdr . cdr) (cons . cons) (assoc . assoc)
              (print . print) (list . list)
              (signum . signum) (floor . floor) (round . round)
              (truncate . truncate) (float . float) (ceiling . ceiling)
              (abs . abs) (min . min) (max . max)
              (sqrt . sqrt) (exp . exp) (expt . expt) (log . log)
              (mod . mod) (rem . rem) (gcd . gcd) (lcm . lcm)
              (sin . sin) (cos . cos) (tan . tan)
              (asin . asin) (acos . acos) (atan . atan)
              (sinh . sinh) (cosh . cosh) (tanh . tanh)
              (s . 104) (k . 107)                            ; custom var
              (myfx . ,(lambda (k) (+ (sin k) (cos (- k))))) ; custom fx
              )))
    (labels ((env (x &aux (res (assoc x kv)))
               (if res (cdr res) (error "EVL: undefined variable: ~a" x))))

      (xprt (evl '1 #'env))
      (xprt (evl 's #'env))
      (xprt (evl '(+ s k) #'env))
      (xprt (evl '(myfx 1) #'env))

      (xprt (evl '(quote (+ 1 2)) #'env))
      (xprt (evl '(quote 1) #'env))
      (xprt (evl '(quote s) #'env))

      (xprt (evl '(list s k) #'env))
      (xprt (evl '(progn s k :progn) #'env))

      (xprt (evl '(if (< 4 1) 2 3) #'env))
      (xprt (evl '(if (< 4 1) 2) #'env))

      (xprt (funcall (evl '(lambda (x) x) #'env) 99))

      (xprt (evl '((lambda (x) (+ s x -10000)) 888) #'env))
      (xprt (evl '((lambda (x y) (+ s x y)) 888 999) #'env))
      (xprt (evl '((lambda () 77)) #'env))

      (xprt (evl '(let ((a 1) (b 20)) (+ a b)) #'env))
      (xprt (evl '(let ((a 1) (b 20)) (+ a b) (- a b)) #'env))

      (xprt (evl '(let ((fx (lambda (x) (+ 1 x))))
                     (fx (fx 1)))
                 #'env))

      (xprt (evl '(labels ((fact (x) (if (= x 0)
                                         1
                                         (* x (fact (1- x))))))
                    (fact 7))
                 #'env))
      )))

(main)

