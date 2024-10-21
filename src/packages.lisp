(defpackage #:evl
  (:use #:common-lisp)
  (:nicknames #:cl-evl)
  (:export #:+std-env+ #:+std-env+ +full-env+ #:v?
    #:evl #:evl*
    #:evl/env #:evl/extenv #:new-env
    #:evl/do-cond #:evl/do-labels #:evl/do-let
    #:evl/eval-dsb #:evl/eval-mvb #:evl/eval-lambda #:evl/eval-coerce-values
    #:evl/car-is #:evl/car-is-in
    #:~ #:dsb #:lbl #:lmb #:mvb #:mvl))

