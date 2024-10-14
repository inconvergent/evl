(in-package :evl)

(defmacro xprt (a &aux (a* (gensym "A")))
  "print expression (a) and the corresponding output. returns the result"
  `(let ((,a* ,a))
     (format t "~&>> ~a~%;; ~a~%" ',a ,a*)
     ,a*))

(defun flatten (x)
  (labels ((rec (x acc) (cond ((null x) acc)
                              ((atom x) (cons x acc))
                              (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun mkstr (&rest args)
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun match-pref (s pref &optional d)
  (declare (string s pref)) "s if s starts with pref; or d"
  (if (and (<= (length pref) (length s))
           (string= pref s :end2 (length pref)))
       s d))

