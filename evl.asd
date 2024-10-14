(asdf:defsystem #:evl
  :description "Meta-circular Lisp Evaluator"
  :version "0.0.1"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :in-order-to ((asdf:test-op (asdf:test-op #:evl/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :components ((:file "packages")
               (:file "utils")
               (:file "interp")))

(asdf:defsystem #:evl/tests
  :depends-on (#:evl #:prove #:uiop #:asdf)
  :version "0.0.1"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':evl-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))
