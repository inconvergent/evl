(asdf:defsystem #:evl
  :description "Meta-circular Lisp Evaluator"
  :version "0.0.2"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :in-order-to ((asdf:test-op (asdf:test-op #:evl/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:lqn #:veq)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "config" :depends-on ("utils"))
               (:file "interp" :depends-on ("config"))))

(asdf:defsystem #:evl/tests
  :depends-on (#:prove #:uiop #:asdf #:evl #:lqn #:veq)
  :version "0.0.2"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':evl-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))
