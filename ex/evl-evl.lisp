#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :evl)
(in-package :evl)

(defun main ()
  (let ((kv `((prt . ,(lambda (x) (format t ";; ~a~%" x)))
              (err . ,(lambda (x) (error "EVL.: undefined variable ~a" x)))
              ,@+std-env+)))
    (labels ((env (x)
               (let ((res (assoc x kv)))
                 (if res (cdr res) (error "EVL: undefined variable: ~a" x)))))

      (with-evl (#'env)
        (progn
          ; (prt (+ 1 2))
          (let ((kv (quote ((+ . +) (- . -) (= . =) (xx . :hi)))))
                  (labels ((env. (x) (let ((res (assoc x kv)))
                                      (if res (cdr res) (err x))))
                           (evl. (expr env.)
                             (cond ((null expr) nil)
                                   ((stringp expr) expr)
                                   ((numberp expr) expr)
                                   ((keywordp expr) expr)
                                   ((functionp expr) expr)
                                   ((symbolp expr) (env. expr))
                                   )
                                 ))
                    ; (prt uu) ; EVL ERROR, undefined var uu
                    (prt (evl. 'xx env.))

                    ))))
      )))

(main)

