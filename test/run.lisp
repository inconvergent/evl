
(setf prove:*enable-colors* nil)
(defpackage #:evl-tests
  (:use #:cl #:prove)
  (:import-from #:evl
    #:+std-env+
    #:evl #:evl* #:with-env
    ; language
    #:str? #:num? #:function? #:keyword? #:symbol? #:null? #:list? #:cons?
    #:even? #:odd? #:some? #:every? #:zero? #:member? #:car?
    #:~ #:~~ #:dsb #:lbl #:lmb #:mvb #:mvl
    ; utils
    #:env/extend-pair #:env/extend-alist #:env/empty #:env/new
    #:evl/do-cond #:evl/do-labels #:evl/do-let
    #:evl/eval-dsb #:evl/eval-mvb #:evl/eval-lambda #:evl/eval-coerce-values
    )
  (:export #:run-tests))
(in-package #:evl-tests)

(defun -run-tests (files)
  (labels ((rel (f) (mapcar (lambda (p) (asdf:system-relative-pathname "evl/tests" p))
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
                #P"test/evl-2.lisp"
                #P"test/evl-values.lisp"
                #P"test/evl-self.lisp")))
