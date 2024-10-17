(in-package :evl)

; TODO: argument count guards

(defparameter +std-env+
  `((+ . +) (- . -) (/ . /) (* . *) (1+ . 1+) (1- . 1-)
    (t . t) (= . =) (< . <) (> . >) (equal . equal)
    (atom . atom) (null . null) (evenp . evenp) (oddp . oddp)
    (stringp . stringp) (symbolp . symbolp) (keywordp . keywordp)
    (numberp . numberp) (functionp . functionp)
    (first . first) (last . last) (second . second) (third . third) (nth . nth)
    (mapcar . mapcar)
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
  "convenient standard environment functions in evl.
none of them are required.")

(defun eval-dsb (args in expr evl* &aux (args* (flat-dsb-args args)))
   "get dsb argument values of (evl* in) as a list (l) of quoted values. then do:
(evl* '((lambda (,@args*) expr) ,@lst))
requires that evl* handles (quote ...) and ((lambda ...) ...)."
  (funcall evl* `((lambda ,args* ,@expr)
                  ; quote the elements in the list. so they will not be  evaluated by evl*
                  ,@(mapcar (lambda (x) `(quote ,x))
                      ; use CL dsb to get variables as a list
                      (eval `(destructuring-bind ,args ',(funcall evl* in)
                               (list ,@args*)))))))

(defun evl (expr env)
  "evaluate an EVL expression in env."
  (cond
        ((null expr) expr)       ; explicitly eval atoms to themselves
        ((stringp expr) expr)
        ((numberp expr) expr)
        ((functionp expr) expr)
        ((keywordp expr) expr)
        ((symbolp expr) (funcall env expr)) ; get symbol from env

        ((car-is expr 'quote) (cadr expr)) ; don't evaluate

        ((car-is expr 'progn) ; evaluate all exprs and return the last result
         (first (last (mapcar (lambda (e) (evl e env)) (cdr expr)))))

        ; TODO: &optional defaults does not work
        ((car-is expr 'destructuring-bind)
         (destructuring-bind (vars in &rest rest) (cdr expr)
           (eval-dsb vars in rest
             (lambda (x) (evl x env)))))

        ((car-is expr 'if)
         (destructuring-bind (test then &optional else) (cdr expr)
           (if (evl test env) (evl then env) (evl else env))))

        ((car-is expr 'cond) ; if else-if ... else
         (destructuring-bind ((cnd x) &rest rest) (cdr expr)
           (evl `(if ,cnd ,x (cond ,@rest)) env)))

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
           (error (e) (error "-->>~%EVL: error at:~%  ~a~%  err:~%    ~a <<--"
                             expr e))))
        (t (error "-->>~%EVL: invalid expression:~%  ~a <<--"
                  expr))))


(defmacro with-evl ((env) body)
  `(evl ',body ,env))

