(defpackage #:evl
  (:use #:common-lisp)
  (:nicknames #:cl-evl)
  (:export #:+std-env+ #:evl #:xprt
    #:evl #:dsb #:car-is #:evl/extenv #:evl/eval-dsb #:evl/eval-lambda
    #:evl/do-labels #:evl/do-let #:evl/do-cond))

