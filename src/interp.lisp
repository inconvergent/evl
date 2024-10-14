(in-package :evl)

; TODO: dont use loop
; TODO: argument count guards

(defun evl (expr env)
  "evaluate an EVL expression in env."
  (labels ((car-is (l s) (and (consp l) (equal (car l) s)))
           (extend- (env kk vv)
             (lambda (y) (loop for k in kk and v in vv
                               if (equal k y) do (return v)
                               finally (return (funcall env y))))))
    (cond ((null expr) nil) ; explicitly eval these atoms to themselves
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
             (lambda (&rest vv) (evl body (extend- env kk vv)))))

          ((car-is expr 'labels) ; define local functions
           (destructuring-bind (pairs &rest body) (cdr expr)
             (labels ((wrp (k)
                        (loop for (name arg xpr) in pairs
                              if (equal k name)
                              do (return (evl `(lambda ,arg ,xpr) #'wrp))
                              finally (return (funcall env k)))))
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
          (t (error "EVL: invalid expression: ~a" expr)))))


(defmacro evl* (body env)
  `(evl ',body ,env))
