(defpackage #:evl
  (:use #:common-lisp)
  (:nicknames #:cl-evl)
  ; (:import-from #:veq #:dsb #:mvc #:mvb #:awf #:pn #:in)
  (:export #:+std-env+ #:evl #:xprt
    #:evl #:evl* #:evl/env #:evl/extenv #:new-env
    #:~
    #:evl/do-cond
    #:evl/do-labels
    #:evl/do-let
    #:evl/eval-dsb
    #:evl/eval-mvb
    #:evl/eval-lambda
    #:evl/eval-coerce-values
    #:car-is #:car-is-in
    #:dsb #:lbl #:lmb #:mvb #:mvl))

