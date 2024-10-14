(in-package :evl)

; TODO: argument count guards

(defparameter +evl-env+
  '((car-is . car-is)
    (evl/extenv . evl/extenv)
    (evl/eval-dsb . evl/eval-dsb)
    (evl/eval-lambda . evl/eval-lambda)
    (evl/do-labels . evl/do-labels)
    (evl/do-let . evl/do-let)
    (evl/do-cond . evl/do-cond))
  "convenient functions when implementing evl in evl.")

(defparameter +std-env+
  `(,@+evl-env+
    (+ . +) (- . -) (/ . /) (* . *) (1+ . 1+) (1- . 1-)
    (t . t) (= . =) (< . <) (> . >) (equal . equal)
    (atom . atom) (null . null) (evenp . evenp) (oddp . oddp)
    (stringp . stringp) (symbolp . symbolp) (keywordp . keywordp)
    (numberp . numberp) (functionp . functionp)
    (first . first) (last . last) (second . second) (third . third) (nth . nth)
    (mapcar . mapcar) (apply . apply)
    (find . find) (member . member)
    (car . car) (cadr . cadr) (cdr . cdr) (cons . cons)
    (assoc . assoc) (pairlis . pairlis) (acons . acons)
    (print . print) (list . list) (listp . listp) (consp . consp)
    (signum . signum) (floor . floor) (round . round)
    (truncate . truncate) (float . float) (ceiling . ceiling)
    (abs . abs) (min . min) (max . max)
    (sqrt . sqrt) (exp . exp) (expt . expt) (log . log)
    (mod . mod) (rem . rem) (gcd . gcd) (lcm . lcm)
    (sin . sin) (cos . cos) (tan . tan)
    (asin . asin) (acos . acos) (atan . atan)
    (sinh . sinh) (cosh . cosh) (tanh . tanh))
  "convenient standard environment functions for evl.
none of them are required.")


(defun car-is (l s)
  "t if consp and car is s"
  (and (consp l) (equal (car l) s)))
(defun evl/extenv (env kk vv)
  "new env function with these names (kk) and values (vv)"
  (let ((kv (mapcar #'list kk vv)))
    (lambda (y) (let ((res (find y kv :key #'car)))
                (if res (second res)
                        (funcall env y))))))

(defun flat-arg-list (args)
  "flatten and strip &-prefixed symbols."
  (remove-if (lambda (s) (match-pref (mkstr s) "&"))
             (flatten args)))

(defun evl/eval-dsb (args in expr evl* env*)
   "get dsb argument values of (evl* in) as a list (l) of quoted values. then do:
(evl* '((lambda (,@args*) expr) ,@lst))
requires that evl* implements (quote ...) and ((lambda ...) ...)."
  (funcall evl* `((lambda ,#1=(flat-arg-list args) ,@expr)
                  ; quote the elements in the list. so they will not be  evaluated by evl*
                  ,@(mapcar (lambda (x) `(quote ,x))
                      ; use CL dsb to get variables as a list
                      (eval `(destructuring-bind ,args ',(funcall evl* in env*)
                               (list ,@#1#)))))
           env*))

(defun evl/eval-lambda (kk body evl* env*)
  "use CL eval to build a function with these arguments and body
requires that evl implements (progn ...)"
  (eval `(lambda (,@kk)
           (funcall ,evl* '(progn ,@body)
             (evl/extenv ,env* ',(flat-arg-list kk)
                                 (list ,@(flat-arg-list kk)))))))

(defun evl/do-labels (pairs body evl* env*)
  (labels ((wrp (k) (let ((res (find k pairs :key #'car)))
                               (if res (funcall evl* `(lambda ,@(cdr res)) #'wrp)
                                       (funcall env* k)))))
    (funcall evl* `(progn ,@body) #'wrp)))

(defun evl/do-let (vars body evl* env*)
  (funcall evl* `((lambda ,(mapcar #'car vars) (progn ,@body))
                  ,@(mapcar #'second vars))
                env*))

(defun evl/do-cond (cnd x rest evl* env*)
  (funcall evl* `(if ,cnd ,x (cond ,@rest)) env*))

; TODO: optional symbol pass through
; TODO: &optional defaults does not work. see flat-arg-list
(defun evl (expr env)
  "evaluate an EVL expression in env."
  (cond ((null expr) expr)       ; explicitly eval atoms to themselves
        ((stringp expr) expr)
        ((numberp expr) expr)
        ((functionp expr) expr)
        ((keywordp expr) expr)
        ((symbolp expr) (funcall env expr)) ; get symbol from env

        ((car-is expr 'quote) (cadr expr)) ; don't evaluate

        ((car-is expr 'progn) ; evaluate all exprs and return the last result
         (first (last (mapcar (lambda (e) (evl e env)) (cdr expr)))))

        ((car-is expr 'destructuring-bind)
         (destructuring-bind (vars in &rest rest) (cdr expr)
           (evl/eval-dsb vars in rest #'evl env)))

        ((car-is expr 'if)
         (destructuring-bind (test then &optional else) (cdr expr)
           (if (evl test env) (evl then env) (evl else env))))

        ((car-is expr 'cond) ; if else-if ... else
         (destructuring-bind ((cnd x) &rest rest) (cdr expr)
           (evl/do-cond cnd x rest #'evl env)))

        ((car-is expr 'lambda)
         (destructuring-bind (kk &rest rest) (cdr expr)
           (evl/eval-lambda kk rest #'evl env)))

        ((car-is expr 'labels) ; define local functions
         (destructuring-bind (pairs &rest body) (cdr expr)
           (evl/do-labels pairs body #'evl env)))

        ((car-is expr 'let) ; define local vars
         (destructuring-bind (vars &rest body) (cdr expr)
           (evl/do-let vars body #'evl env)))

        ((consp expr) ; (apply fx/lambda ...)
           (apply (evl (car expr) env)
                  (mapcar (lambda (x) (evl x env))
                          (cdr expr))))
        (t (error "~&-->>~%[EVL]: invalid expression:~%  ~a <<--~&"
                  expr))))

