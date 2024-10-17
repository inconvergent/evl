(in-package :evl)

(defmacro xprt (a &aux (a* (gensym "A")))
  "print expression (a) and the corresponding output. returns the result"
  `(let ((,a* ,a))
     (format t ">> ~a~%;; ~a~%" ',a ,a*)
     ,a*))

; (defmacro abbrev (short long) `(defmacro ,short (&rest args) `(,',long ,@args)))

(defun flatten (x)
  (labels ((rec (x acc) (cond ((null x) acc)
                              ((atom x) (cons x acc))
                              (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

; (defun group (l n) (declare (list l) (fixnum n))
;   "group l into lists of n elements. see ungroup."
;   (if (< n 1) (error "group error: group size is smaller than 1"))
;   (labels ((rec (l acc &aux (rest (nthcdr n l)))
;              (if (consp rest)
;                  (rec rest (cons (subseq l 0 n) acc))
;                  (nreverse (cons l acc)))))
;     (if l (rec l nil) nil)))
; (defun ungroup (l &aux (res (list))) (declare (list l res)) "inverse of group."
;   (loop for s in l do (loop for k in s do (push k res)))
;   (reverse res))

(declaim (inline mkstr))
(defun mkstr (&rest args) ;(declare (optimize speed (safety 2)))
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun match-substr (sub s) ;(declare (optimize speed (safety 2)) (string sub s))
  "returns index where substring matches s from left to right. otherwise nil."
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from match-substr i)))

; (declaim (inline last*))
; (defun nth* (l i &optional d &aux (v (nth i l))) (declare (list l) (fixnum i))
;   (if v v d))
; (defun last* (l) (declare (list l)) (first (last l)))

(defun symb (&rest args) ;(declare (optimize speed (safety 1)))
  (let ((*print-case* :upcase))
    (values (intern (apply #'mkstr args)))))
(defun psymb (pkg &rest args) ; https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  ;(declare (optimize speed (safety 1)))
  (let ((*print-case* :upcase))
    (values (intern (apply #'mkstr args) (if pkg pkg :veq)))))

; (defun reread (&rest args) (values (read-from-string (apply #'mkstr args))))
; (defun undup (e &optional (flatten t)) (declare (optimize speed))
;   (remove-duplicates (if flatten (flatten e) e)))

; (abbrev mvc multiple-value-call) (abbrev mvb multiple-value-bind)
; (abbrev dsb destructuring-bind) (abbrev awg with-gensyms)
; (abbrev awf flatten)

; (defun dotted-listp (l) ; TODO: rewrite with rec to require first call to be cons
;   (cond ((null l) nil) ((atom l) t) (t (dotted-listp (cdr l)))))

(defun car-is (l s) (and (consp l) (equal (car l) s)))
(defun extenv (env kk vv)
  "new env function with these names (kk) and values (vv)"
  (lambda (y) (let ((res (find y (mapcar #'list kk vv) :key #'car)))
                (if res (second res)
                        (funcall env y)))))

(defun flat-dsb-args (args)
  (remove-if (lambda (s) (match-substr "&" (mkstr s)))
             (flatten args)))
