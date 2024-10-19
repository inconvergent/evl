(asdf:defsystem #:evl
  :description "Meta-circular Lisp Evaluator"
  :version "0.7.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :in-order-to ((asdf:test-op (asdf:test-op #:evl/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:lqn #:veq)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "docs" :depends-on ("utils"))
               (:file "config" :depends-on ("docs"))
               (:file "evl-state-machine" :depends-on ("config"))
               (:file "interp" :depends-on ("evl-state-machine"))
               (:file "code-factory" :depends-on ("interp"))
               ))

(asdf:defsystem #:evl/tests
  :depends-on (#:prove #:uiop #:asdf #:evl #:lqn #:veq)
  :version "0.7.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':evl-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))
