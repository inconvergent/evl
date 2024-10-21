
(setf prove:*enable-colors* nil)
(defpackage #:evl-tests
  (:use #:cl #:prove)
  (:import-from #:evl
    #:+std-env+ #:+full-env+
    #:evl #:evl*
    ; language
    #:str? #:num? #:function? #:keyword? #:symbol? #:null? #:list? #:cons?
    #:~ #:dsb #:lbl #:lmb #:mvb #:mvl
    ; utils
    #:evl #:evl*
    #:evl/env #:evl/extenv #:new-env
    #:evl/do-cond #:evl/do-labels #:evl/do-let
    #:evl/eval-dsb #:evl/eval-mvb #:evl/eval-lambda #:evl/eval-coerce-values
    #:evl/car-is #:evl/car-is-in)
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
  (-run-tests '(#P"test/evl.lisp"
                #P"test/evl-values.lisp"
                #P"test/evl-self.lisp")))
