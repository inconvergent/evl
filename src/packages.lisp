(defpackage #:evl
  (:use #:common-lisp)
  (:nicknames #:cl-evl)
  (:export #:+std-env+ #:evl #:xprt
    #:evl #:evl* #:evl/env #:evl/extenv
    #:evl/do-cond
    #:evl/do-labels
    #:evl/do-let
    #:evl/eval-dsb #:evl/eval-lambda
    #:car-is #:car-is-in
    #:dsb #:lbl #:lmb))

