(in-package :evl)

; TODO: argument count guards

(defparameter +std-env+
  `((+ . +) (- . -) (/ . /) (* . *) (1+ . 1+) (1- . 1-)
    (t . t) (= . =) (< . <) (> . >) (equal . equal)
    (evenp . evenp) (oddp . oddp) (null . null)
    (first . first) (second . second) (third . third) (nth . nth)
    (find . find) (member . member)
    (car . car) (cdr . cdr) (cons . cons)
    (assoc . assoc) (pairlis . pairlis) (acons . acons)
    (print . print) (list . list)
    (signum . signum) (floor . floor) (round . round)
    (truncate . truncate) (float . float) (ceiling . ceiling)
    (abs . abs) (min . min) (max . max)
    (sqrt . sqrt) (exp . exp) (expt . expt) (log . log)
    (mod . mod) (rem . rem) (gcd . gcd) (lcm . lcm)
    (sin . sin) (cos . cos) (tan . tan)
    (asin . asin) (acos . acos) (atan . atan)
    (sinh . sinh) (cosh . cosh) (tanh . tanh))
  "standard environment functions in evl")

(defun car-is (l s) (and (consp l) (equal (car l) s)))
(defun extenv (env kk vv)
  "new env function with these names (kk) and values (vv)"
  (lambda (y) (let ((res (find y (mapcar #'list kk vv) :key #'car)))
                (if res (second res)
                        (funcall env y)))))

(defun evl (expr env)
  "evaluate an EVL expression in env."
  (cond ((null expr) nil)       ; explicitly eval these atoms to themselves
        ((stringp expr) expr)
        ((numberp expr) expr)
        ((keywordp expr) expr)
        ((functionp expr) expr)
        ((symbolp expr) (funcall env expr)) ; get symbol from env

        ((car-is expr 'quote) (cadr expr)) ; don't evaluate

        ((car-is expr 'progn) ; evaluate all exprs and return the last result
         (first (last (mapcar (lambda (e) (evl e env)) (cdr expr)))))

        ((car-is expr 'if)
         (destructuring-bind (test then &optional else) (cdr expr)
           (if (evl test env) (evl then env) (evl else env))))

        ((car-is expr 'lambda)
         (destructuring-bind (kk body) (cdr expr)
           (lambda (&rest vv) (evl body (extenv env kk vv)))))

        ((car-is expr 'labels) ; define local functions
         (destructuring-bind (pairs &rest body) (cdr expr)
           (labels ((wrp (k) (let ((res (find k pairs :key #'car)))
                               (if res (evl `(lambda ,@(cdr res)) #'wrp)
                                       (funcall env k)))))
             (evl `(progn ,@body) #'wrp))))

        ((car-is expr 'let) ; define local vars
         (destructuring-bind (vars &rest body) (cdr expr)
           (evl `((lambda ,(mapcar #'car vars)
                          (progn ,@body))
                  ,@(mapcar #'second vars))
                env)))

        ((consp expr) ; (apply fx/lambda ...)
         (handler-case
           (apply (evl (car expr) env)
                  (mapcar (lambda (x) (evl x env))
                          (cdr expr)))
           (error (e) (error "EVL: error at ~a: ~a" expr e))))
        (t (error "EVL: invalid expression: ~a" expr))))


(defmacro evl* (body env)
  `(evl ',body ,env))
