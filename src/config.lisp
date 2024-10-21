(in-package #:evl)

(defun preproc-env (a)
  (declare (list))
  (mapcar (lambda (x) (etypecase x (cons x) (symbol `(,x . ,x)))) a))

(defparameter +evl-dev-env+
  (preproc-env `(evl/car-is evl/car-is-in
                 evl/extenv new-env ; rename new-env?
                 evl/eval-dsb evl/eval-mvb evl/eval-lambda evl/eval-coerce-values
                 evl/do-labels evl/do-let evl/do-cond))
  "convenient functions when implementing evl in evl.")

(defparameter +std-env+
  `(,@+evl-dev-env+
    ,@(preproc-env
        `((pi . ,(float pi)) (pii . ,(float (* 2.0 pi))) (pi5 . ,(float (* 0.5 pi)))
          + - / * 1+ 1- t = < > equal not
          values values-list
          multiple-value-call (mvc . multiple-value-call) funcall mapcar apply
          print format
          list car cadr cdr cons cdar assoc pairlis acons first last second third nth
          intersection set-difference find member union remove-if map mapcan
          atom null evenp oddp
          stringp symbolp keywordp listp consp numberp functionp
          abs min max signum floor round truncate float ceiling
          sqrt exp expt log mod rem gcd lcm sin cos tan asin acos atan sinh cosh tanh)))
  "convenient standard environment (CL) functions and constant for evl.
none of them are required.")

(defparameter +full-env+
  `(,@+std-env+
    ,@(preproc-env
        `((atom? . atom) (null? . null) (even? . evenp) (odd? . oddp)
          (str? . stringp) (symbol? . symbolp) (keyword? . keywordp)
          (list? . listp) (cons? . consp)
          (num? . numberp) (function? . functionp)
          (member? . member))))
  "+std-env+ extended with some more Scheme-like predicates.")
