# EVL - Meta-circular Evaluator

```lisp
(in-package :evl)

(let ((kv `((+ . +) (* . *) (1- . 1-) (t . t) (= . equal))))
  (labels ((env (x &aux (res (assoc x kv)))
             (if res (cdr res) (error "EVL: undefined variable: ~a" x))))

    (print (evl '(labels ((fact (x) (if (= x 0)
                                        1
                                        (* x (fact (1- x))))))
                   (fact 7))
                #'env))))
```

more examples in [ex/ex.lisp](ex/ex.lisp).

