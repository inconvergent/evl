(defpackage #:evl
  (:use #:common-lisp)
  (:nicknames #:cl-evl)
  (:export #:v? #:+std-env+
    #:evl #:evl* #:with-env #:evl-error
    ; language
    #:str? #:num? #:function? #:keyword? #:symbol? #:null? #:list? #:cons?
    #:even? #:odd? #:some? #:every? #:zero? #:member? #:car?
    #:~ #:~~ #:dsb #:lbl #:lmb #:mvb #:mvl
    ; utils
    #:env/extend-pair #:env/extend-alist #:env/empty #:env/new
    #:evl/do-cond #:evl/do-labels #:evl/do-let
    #:evl/eval-dsb #:evl/eval-mvb #:evl/eval-lambda #:evl/eval-coerce-values
    ))


(defpackage #:evl/gen
  (:use #:common-lisp)
  (:export #:gen #:signatures))

