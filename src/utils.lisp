(in-package :evl)

(defmacro xprt (a &aux (a* (gensym "A")))
  "print expression (a) and the corresponding output. returns the result"
  `(let ((,a* ,a))
     (format t ">> ~a~%;; ~a~%" ',a ,a*)
     ,a*))

(defun flatten (x)
  (labels ((rec (x acc) (cond ((null x) acc)
                              ((atom x) (cons x acc))
                              (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

(defun mkstr (&rest args)
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun match-substr (sub s) (declare (string sub s))
  "returns index where substring matches s from left to right. otherwise nil."
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from match-substr i)))

(defun match-pref (s pref &optional d)
  (declare (string s pref)) "s if s starts with pref; or d"
  (if (and (<= (length pref) (length s))
           (string= pref s :end2 (length pref)))
       s d))

(defun symb (&rest args)
  (let ((*print-case* :upcase))
    (values (intern (apply #'mkstr args)))))
(defun psymb (pkg &rest args) ; https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  (let ((*print-case* :upcase))
    (values (intern (apply #'mkstr args) (if pkg pkg :veq)))))

(defun car-is (l s) (and (consp l) (equal (car l) s)))
(defun extenv (env kk vv)
  "new env function with these names (kk) and values (vv)"
  (lambda (y) (let ((res (find y (mapcar #'list kk vv) :key #'car)))
                (if res (second res)
                        (funcall env y)))))

(defun flat-dsb-args (args)
  (remove-if (lambda (s) (match-pref (mkstr s) "&"))
             (flatten args)))
