(load "~/quicklisp/setup.lisp")

(ql:quickload :evl)
(in-package :evl)

; (veq:fvdef main ()
;   (st/with-rules
;     ((rule-a i (cond ((< i 5)  (values i (st/g rule-a (1+ i))))
;                      ((= i 5)  (values i (st/g rule-b (1+ i))))))
;      (rule-b i (cond ((< i 10) (values i (st/g rule-b (+ i 0.5)) ))
;                      ((= i 10) (values i (st/g rule-a 0))))))

;   (let* ((g  (st/g rule-a 0)) ; init rule-a with value 0
;          (ga (st/itr/n 13 g  (lambda (s) (lqn:out "hi ~a~%" s))))) ; iterate 13 times

;     ; resume at ga, 3 times
;     (st/itr/n 3  ga (lambda (s) (lqn:out "hi again ~a.~%" s)))
;     ; resume at ga again, 7 times
;     (st/itr/n 7  ga (lambda (s) (lqn:out "hi ABC ~a.~%" s))))))



(veq:fvdef main ()
           ; select fizz/buzz
  (st/with-rules
    ((gx i (values i (if (< i 20) (st/g gx (1+ i)) t))))
    (let* ((gg (st/g gx 0)))
      ; (st/itr/all gg #'princ)

     ; (time (print (mvl (st/acc/n 10 gg))))
     ; (time (print (mvl (st/acc/until gg
     ;                    (lambda (i) (= i 10))
     ;                    #'cons
     ;                    ))))

     (mvb (g v) (st/itr/until gg (lambda (i) (= i 10)))
          (list (functionp g) v))
     (mvb (g v) (st/itr/until gg (lambda (i) (= i 100)))
          (list (functionp g) v)
          )
     (mvb (g v) (st/itr/n gg 10)
          (list (functionp g) v)
          )
     (mvb (g v) (st/itr/n gg 100)
          (list (functionp g) v)
          )

     )))

(time (main))

