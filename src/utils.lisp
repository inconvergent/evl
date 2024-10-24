(in-package :evl)

(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'evl) 'asdf:version)))
  "return/print evl version." (unless silent (format t "~&EVL version: ~a~%." v)) v)


(defun flatten (x)
  (declare (optimize (speed 3)) (list x))
  (labels ((rec (x acc) (cond ((null x) acc)
                              ((atom x) (cons x acc))
                              (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun mkstr (&rest args)
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s)))) syms) ,@body))

(defun match-pref (s pref &optional d)
  (declare (optimize (speed 3)) (string s pref)) "s if s starts with pref; or d"
  (if (and (<= (length pref) (length s))
           (string= pref s :end2 (length pref)))
       s d))

