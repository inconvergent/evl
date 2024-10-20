
(setf prove:*enable-colors* nil)
(defpackage #:evl-tests
  (:use #:cl #:prove)
  (:import-from #:evl
    #:+std-env+
    #:evl #:evl* #:car-is #:evl/extenv #:evl/eval-dsb #:evl/eval-lambda
    #:dsb #:lmb #:lbl
    #:evl/do-labels #:evl/do-let #:evl/do-cond
    )
  (:export #:run-tests))
(in-package #:evl-tests)

(defun -run-tests (files)
  (labels ((rel (f) (mapcar (lambda (p) (asdf:system-relative-pathname
                                          "evl/tests" p))
                            f)))
    (loop with fails = 0
          for f in (rel files)
          do ;(format t "~&~%starting tests in: ~a~%" (evl:str! f))
             (unless (prove:run f :reporter :fiveam)
                     (incf fails))
             ;(format t "~&done: ~a~%" (evl:str! f))
          finally (return (unless (< fails 1) (uiop:quit 7))))))

(defun run-tests ()
  (-run-tests '(#P"test/test-evl.lisp"
                #P"test/test-evl-evl.lisp")))
