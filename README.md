# EVL - Meta-circular Evaluator

```lisp
(in-package :evl)
(evl '(label fact (x) (if (= x 0)
                          1
                          (* x (fact (1- x))))
        (fact 7))
     #'env)
```

examples in [ex/ex.lisp](ex/ex.lisp).

