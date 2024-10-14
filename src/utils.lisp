(in-package :evl)

(defmacro xprt (a &aux (a* (gensym "A")))
  "print expression (a) and the corresponding output. returns the result"
  `(let ((,a* ,a))
     (format t ">> ~a~%;; ~a~%" ',a ,a*)
     ,a*))

