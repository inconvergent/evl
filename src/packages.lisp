
(defpackage #:evl
  (:use #:common-lisp)
  (:nicknames #:cl-evl)
  (:export
    #:v? #:+std-env+ #:evl #:evl* #:evl-error #:with-env
    ; language
    #:str? #:num? #:function? #:keyword? #:symbol? #:null? #:list? #:cons?
    #:even? #:odd? #:some? #:every? #:zero? #:member? #:car?
    #:~ #:~~ #:dsb #:lbl #:lmb #:mvb #:mvl #:lst
    ; utils
    #:env/extend-pair #:env/extend-alist #:env/empty #:env/new #:env/merge
    #:dev/do-or #:dev/do-and
    #:dev/do-cond #:dev/do-labels #:dev/do-let
    #:dev/eval-dsb #:dev/eval-mvb #:dev/eval-lambda #:dev/eval-coerce-values)
  (:documentation "evaluator for a CL-like lisp."))

(defpackage #:evl/code
  (:use #:common-lisp)
  (:export #:gen #:signatures))

