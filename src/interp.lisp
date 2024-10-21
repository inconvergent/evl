(in-package :evl)

; TODO: argument count guards

(defun preproc-env (a)
  (declare (list))
  (mapcar (lambda (x) (etypecase x (cons x) (symbol `(,x . ,x)))) a))

(defparameter +evl-env+
  (preproc-env `(evl/car-is evl/car-is-in
                 evl/extenv new-env ; rename new-env?
                 evl/eval-dsb evl/eval-mvb evl/eval-lambda evl/eval-coerce-values
                 evl/do-labels evl/do-let evl/do-cond))
  "convenient functions when implementing evl in evl.")

(defparameter +std-env+
  `(,@+evl-env+
    ,@(preproc-env
        `((pi . ,(float pi)) (pii . ,(float (* 2.0 pi))) (pi5 . ,(float (* 0.5 pi)))
          + - / * 1+ 1- t = < > equal not
          values values-list
          multiple-value-call (mvc . multiple-value-call) funcall mapcar apply
          print format
          list car cadr cdr cons cdar assoc pairlis acons first last second third nth
          intersection set-difference find member union
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

(defun new-env (&optional (kv +std-env+))
  (declare (speed 3))
  (lambda (k &aux (res (assoc k kv)))
    (declare (symbol k))
    (if res (cdr res) (error "[EVL] undefined variable: ~a" k))))

(defun evl/car-is-in (l ss)
  (declare (optimize (speed 3)) (list ss))
  (and (consp l) (member (car l) ss :test #'eq)))
(defun evl/car-is (l s)
  (declare (optimize (speed 3)) (symbol s))
  "t if consp and car is s"
  (and (consp l) (equal (car l) s)))
(defun evl/extenv (env kk vv)
  (declare (optimize (speed 3)) (function env) (list kk vv))
  "new env function extended with these names (kk) and values (vv)"
  (let ((kv (mapcar #'list kk vv)))
    (lambda (y) (let ((res (find y kv :key #'car)))
                (if res (second res)
                        (funcall env y))))))

(defun flat-arg-list (args)
  (declare (optimize (speed 3)) (list args))
  "flatten, and strip &-prefixed symbols."
  (remove-if (lambda (s) (match-pref (mkstr s) "&"))
             (flatten args)))

(defun evl/eval-dsb (args in expr evl* env*)
  (declare (list args) (function evl* env*))
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

(defun evl/eval-mvb (args in expr evl* env*)
  (declare (list args) (function evl* env*))
   "get dsb argument values of (evl* in) as a list (l) of quoted values. then do:
(evl* '((lambda (,@args*) expr) ,@lst))
requires that evl* implements (quote ...) and ((lambda ...) ...)."
  (funcall evl* `((lambda ,(flat-arg-list args) ,@expr)
                  ,@(mapcar (lambda (x) `(quote ,x))
                            (multiple-value-list (funcall evl* in env*))))
           env*))

(defun evl/eval-lambda (args body evl* env*)
  (declare (function evl* env*))
  "use CL eval to build a function with these args and body.
requires that evl* implements (progn ...)"
  (eval `(lambda (,@args)
           (funcall ,evl* '(progn ,@body)
             (evl/extenv ,env* ',#1=(flat-arg-list args) (list ,@#1#))))))

(defun evl/do-labels (pairs body evl* env*)
  (declare (list body) (function evl* env*))
  "evaluate this body in an env with these labels (functions)."
  (labels ((wrp (k) (let ((res (find k pairs :key #'car)))
                               (if res (funcall evl* `(lambda ,@(cdr res)) #'wrp)
                                       (funcall env* k)))))
    (funcall evl* `(progn ,@body) #'wrp)))

(defun evl/do-let (vars body evl* env*)
  (declare (list body) (function evl* env*))
  "evaluate body in an env with these named variables"
  (funcall evl* `((lambda ,(mapcar #'car vars) (progn ,@body))
                  ,@(mapcar #'second vars))
                env*))

(defun evl/do-cond (cnd x body evl* env*)
  (declare (list body) (function evl* env*))
  "recursively evaluate these conds."
  (funcall evl* `(if ,cnd ,x (cond ,@body)) env*))

(defun evl/eval-coerce-values (expr evl* env*)
  (eval `(values-list
           (concatenate 'list
            ,@(mapcar (lambda (x) `(list ,@(multiple-value-list
                                             (funcall evl* x env*))))
                      expr)))))

; TODO: optional symbol pass through
; TODO: &optional defaults does not work. see flat-arg-list
(defun evl (expr env)
  (declare (function env))
  "evaluate an EVL expression in env.

expr is the quoted expression that should be evaluated.
env is a funcion used to lookup a variable in the local scope.

supports CL syntax:
  - progn, if, cond,
  - lambda (lmb), labels (lbl),
  - let, destructuring-bind (dsb),
  - quote, values;
non CL syntax:
  - (~ ...) coerce value packs to a single pack

deviations from regular CL syntax:
  - there is no function name space; variables and functions in environment are
    indistinguishable.
  -
  - &optional, &key and &rest are supported as arguments in lambda, labels,
    destructuring-bind. but default values in optional/key are not supported (yet),
    so all defaults are nil.
  - &aux is not supported
  "
  (cond ((null expr) expr)                                       ; eval atoms to themselves
        ((stringp expr) expr)
        ((numberp expr) expr)
        ((functionp expr) expr)
        ((keywordp expr) expr)
        ((symbolp expr) (funcall env expr))                      ; get var from env

        ((evl/car-is-in expr '(declare)) nil)                    ; ignore declares

        ((evl/car-is expr 'quote) (cadr expr))                   ; quote; don't evaluate

        ; ((evl/car-is expr 'evl) (evl (print (cadr expr)) env)) ; evl?

        ((evl/car-is-in expr '(cl-user::~ evl:~ veq:~))          ; coerce value packs
         (evl/eval-coerce-values (cdr expr) #'evl env))

        ((evl/car-is expr 'values)                               ; values
         (apply #'values (mapcar (lambda (x) (evl x env))
                                 (cdr expr))))

        ((evl/car-is expr 'progn)                                ; progn; evaluate all, return last
         (destructuring-bind (a &rest rest) (cdr expr)
           (if rest (progn (evl a env) (evl (cons 'progn rest) env))
                    (evl a env))))

        ((evl/car-is-in expr '(lambda lmb))                      ; lambda
         (destructuring-bind (kk &rest rest) (cdr expr)
           (evl/eval-lambda kk rest #'evl env)))

        ((evl/car-is expr 'let)                                  ; let; define local vars
         (destructuring-bind (vars &rest body) (cdr expr)
           (evl/do-let vars body #'evl env)))

        ((evl/car-is expr 'if)                                   ; if
         (destructuring-bind (test then &optional else) (cdr expr)
           (if (evl test env) (evl then env) (evl else env))))

        ((evl/car-is expr 'cond)                                 ; if else-if ... else
         (destructuring-bind ((cnd x) &rest rest) (cdr expr)
           (evl/do-cond cnd x rest #'evl env)))

        ((evl/car-is-in expr '(destructuring-bind dsb))          ; dsb
         (destructuring-bind (vars in &rest rest) (cdr expr)
           (evl/eval-dsb vars in rest #'evl env)))
        ((evl/car-is-in expr '(multiple-value-bind mvb veq:mvb)) ; mvb
         (destructuring-bind (vars in &rest rest) (cdr expr)
           (evl/eval-mvb vars in rest #'evl env)))
        ((evl/car-is-in expr '(multiple-value-list mvl))         ; mvl
         (multiple-value-list (evl (cadr expr) env)))

        ((evl/car-is-in expr '(labels lbl))                      ; labels; local functions
         (destructuring-bind (pairs &rest body) (cdr expr)
           (evl/do-labels pairs body #'evl env)))

        ((consp expr)                                            ; (apply (fx/lambda) ...)
         (apply (evl (car expr) env)
                (mapcar (lambda (x) (evl x env))
                        (cdr expr))))
        (t (error "~&-->>~%[EVL]: invalid expression:~%  ~a <<--~&"
                  expr))))

(defun evl* (expr &optional (env (new-env)))
  (evl expr env))

