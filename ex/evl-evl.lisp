#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :evl)
(in-package :evl)

(defun main ()
  (let ((kv `((prt . ,(lambda (x) (format t "~&;; ~a~&" x)))
              (err . ,(lambda (x) (error "EVL*: undefined variable ~a" x)))
              (car-is . car-is)
              ; (car-is . car-is)
              ; (destructuring-bind . destructuring-bind)
              (cadr . cadr)
              ,@+std-env+)))
    (labels ((env (x)
               (let ((res (assoc x kv)))
                 (if res (cdr res) (error "EV*: undefined variable: ~a" x)))))

      (with-evl (#'env)
        (progn
          ; (prt (+ 1 2))
          (let
            ((kv (quote ((+ . +) (t . t) (- . -) (= . =) (xx . :hi) ))))
            (labels ((env. (x) (let ((res (assoc x kv)))
                                 (if res (cdr res) (err x))))
              (evl. (expr. env.)
              (cond
                    ((null expr.) expr.)
                    ((stringp expr.) expr.)
                    ((numberp expr.) expr.)
                    ((functionp expr.) expr.)
                    ((keywordp expr.) expr.)
                    ((symbolp expr.) (env. expr.))

                    ((car-is expr. 'quote) (cadr expr.))

                    ((car-is expr. 'if)
                     (destructuring-bind (test then &optional else) (cdr expr.)
                       (if (evl. test env.) (evl. then env.) (evl. else env.))))

                    ((car-is expr. 'progn)
                     (first (last (mapcar (lambda (e) (evl. e env.))
                                          (cdr expr.)))))

                    ((car-is expr. 'cond) ; if else-if ... else
                     (destructuring-bind ((cnd x) &rest rest) (cdr expr.)
                       (evl. `(if ,cnd ,x (cond ,@rest)) env.)))
                    )))
                    ; (prt uu) ; EVL ERROR, undefined var uu
                    (prt (evl. 'xx env.))
                    (prt (evl. '(quote xx) env.))
                    (prt (evl. '(progn xx :yy) env.))
                    (prt (evl. '(if nil
                                       1 2
                                      ) env.))


                    ))))

      )))

(main)

