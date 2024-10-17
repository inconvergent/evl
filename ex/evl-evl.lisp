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

      ; (with-evl (#'env)
      ;   (progn
      ;     ; (prt (+ 1 2))
      ;     (let
      ;       ((kv (quote ((+ . +) (- . -) (= . =) (xx . :hi)))))
      ;       (labels ((env. (x) (let ((res (assoc x kv)))
      ;                            (if res (cdr res) (err x))))
      ;         (evl. (expr. env.)
      ;         (cond ((null expr.) nil)
      ;               ((stringp expr.) expr.)
      ;               ((numberp expr.) expr.)
      ;               ((keywordp expr.) expr.)
      ;               ((functionp expr.) expr.)
      ;               ((symbolp expr.) (env. expr.))
      ;               ((car-is expr. 'quote) (cadr expr.))
      ;               ; ((car-is expr. 'progn) ; evaluate all exprs and return the last result
      ;               ;  (first (last (mapcar (lambda (e) (evl. e env.))
      ;               ;                       (cdr expr.)))))

      ;               ((car-is expr. 'cond) ; if else-if ... else
      ;                (destructuring-bind ((cnd xpr) &rest rest) (cdr expr.)
      ;                  (evl.  `(if ,cnd ,xpr (cond ,@rest)) env.)))
      ;               )))
      ;               ; (prt uu) ; EVL ERROR, undefined var uu
      ;               ; (prt (evl. 'xx env.))
      ;               ; (prt (evl. '(quote xx) env.))
      ;               ; (prt (evl. '(progn xx :yy) env.))
      ;               (prt (evl. '(cond ((< 1 2) t)) env.))


      ;               ))))

      (with-evl (#'env)
        (progn
          ; (prt (+ 1 2))
          (destructuring-bind
            ((xxx yyy) &rest zzz) ; vars
            (list (list :a :b) :c :d) ; in
            (prt (list :ok xxx yyy zzz))

            ; (prt (list cnd xpr rest))
            ; (evl.  `(if ,cnd ,xpr (cond ,@rest)) env.)
            )))
      )))

(main)


; (DESTRUCTURING-BIND
;   ((CND XPR) &REST REST)
;   ((:A :B) :C)
;   NIL)
