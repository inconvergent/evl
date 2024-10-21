(defpackage #:evl
  (:use #:common-lisp)
  (:nicknames #:cl-evl)
  (:export #:v? #:+std-env+ #:+full-env+
    #:evl #:evl*
    ; language
    #:str? #:num? #:function? #:keyword? #:symbol? #:null? #:list? #:cons?
    #:~ #:dsb #:lbl #:lmb #:mvb #:mvl
    ; utils
    #:evl/env #:evl/extenv #:new-env
    #:evl/do-cond #:evl/do-labels #:evl/do-let
    #:evl/eval-dsb #:evl/eval-mvb #:evl/eval-lambda #:evl/eval-coerce-values
    #:evl/car-is #:evl/car-is-in
    ))

