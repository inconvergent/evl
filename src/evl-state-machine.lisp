(in-package #:evl)


(defun st/make-rule-label (rule-name arg rule-expr)
  "create rule label with name, argument and rule/condition."
  (declare (symbol rule-name arg))
  `((,rule-name (val-fx)
       (declare (function val-fx))
       (lambda (&optional act)
         (mvb (val nxt) (funcall (lambda (,arg) ,rule-expr) (funcall val-fx))
           (cond ((and nxt (functionp nxt))
                  (values nxt #1=(funcall (the function (or act *act*)) val)))
                 (nxt (values nil #1#))
                 (t (values nil nil))))))))

(defmacro st/with-rules (rules &body body)
  (declare (list rules)) "state machine context with these rules."
  `(labels (,@(mapcan (lambda (o) (apply #'st/make-rule-label o)) rules))
     ,@body))

(defmacro st/new (rule-name val-expr)
  (declare (symbol rule-name))
  "new state with this rule and expression."
  `(,rule-name (later ,val-expr)))


(defun st/acc/all (stx &optional (acc #'cons) act res)
  (declare (optimize speed) (function acc)) "accumulate all."
  (if stx (mvb (nxt val) (funcall (the function stx) act)
               (st/acc/all nxt acc act (funcall acc val res)))
          (values nil res)))

(defun st/itr/all (stx &optional act res)
  (declare (optimize speed)) "iterate all."
  (st/acc/all stx #'values act res))


(defun st/acc/n (stx &optional (n 1) (acc #'cons) act res)
  (declare (optimize speed) (fixnum n) (function acc)) "accumulate at most n times."
  (if (and stx (> n 0))
      (mvb (nxt val) (funcall (the function stx) act)
           (st/acc/n nxt (1- n) acc act (funcall acc val res)))
      (values stx res)))

(defun st/itr/n (stx &optional (n 1) act res)
  (declare (optimize speed) (fixnum n)) "iterate at most n times."
  (st/acc/n stx n #'values act res))


(defun st/acc/until (stx &optional (until #'identity) (acc #'cons) act res)
  (declare (optimize speed) (function until acc)) "accumulate until."
  (if stx (mvb (nxt val) (funcall (the function stx) act)
               (if (not (funcall until val))
                   (st/acc/until nxt until acc act (funcall acc val res))
                   (values stx (funcall acc val res))))
          (values nil res)))

(defun st/itr/until (stx &optional (until #'identity) act res)
  (declare (optimize speed) (function until)) "iterate until."
  (st/acc/until stx until #'values act res))

