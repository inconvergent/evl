
(in-package #:evl-tests)

(plan 3)

(subtest "test state machine"
   (st/with-rules ((gx i (cond ((< i 4)  (values i (st/new gx (1+ i)))))))
     (let ((evl:*act* (lambda (s) (lqn:out "hi~a |>" s))))
       (is (lqn:stdstr (let ((gg (st/new gx 0)))
                         (st/itr/all gg #'princ)
                         (st/itr/all gg)))
           "0123hi0 |>hi1 |>hi2 |>hi3 |>")))

  (st/with-rules
    ((gx i (values i (if (< i 20) (st/new gx (1+ i)) t))))
      (let* ((gg (st/new gx 0)))
        (mvb (g v) (st/itr/until gg (lambda (i) (= i 10)))  (is (list (functionp g) v) '(t 10)))
        (mvb (g v) (st/itr/until gg (lambda (i) (= i 100))) (is (list (functionp g) v) '(nil 20)))

        (mvb (g v) (st/itr/n gg 10)  (is (list (functionp g) v) '(t 9)))
        (mvb (g v) (st/itr/n gg 100) (is (list (functionp g) v) '(nil 20)))
        (mvb (g v) (st/itr/all gg)   (is (list (functionp g) v) '(nil 20)))

        (mvb (g v) (st/itr/n (st/itr/n gg 10) 3)
             (is (list (functionp g) v) '(t 12)))

        (mvb (g v) (st/itr/n (st/itr/until gg (lambda (i) (= i 10))) 2)
             (is (list (functionp g) v) '(t 11)))))

   (st/with-rules
    ((gx i (cond ((< i 10) (values i (st/new gx (progn (princ :/exec)
                                                    (1+ i))))))))
    (is (lqn:stdstr (let ((gg (st/new gx 0)))
                      (st/itr/n gg 3 #'princ)))
        "0/EXEC1/EXEC2")))

(subtest "test state machine 2"
  (st/with-rules
    ((gen-a i (cond ((< i 4)  (values i (st/new gen-a (1+ i))))
                    ((< i 14) (values i (st/new gen-a (+ 3 i))))))
     (gen-b l (cond ((and l (cdr l)) (values (car l) (st/new gen-b (cdr l))))
                    ((car l)         (values (car l) t)))))
    (is (lqn:stdstr
      (let* ((gint (st/new gen-a 0))
             (gintb1 (st/itr/n gint   3 (lambda (s) (lqn:out "hi ~a" s))))
             (gintb2 (st/itr/n gint   5 (lambda (s) (lqn:out "hi again ~a." s))))
             (gintc  (st/itr/n gintb1 2 (lambda (s) (lqn:out "hello ~a." s)))))
        (declare (ignorable gintb2))
        (print :--)
        (st/itr/n gintc 100 (lambda (s) (lqn:out "oh no ~a." s)))))
      "hi 0hi 1hi 2hi again 0.hi again 1.hi again 2.hi again 3.hi again 4.hello 3.hello 4.
:-- oh no 7.oh no 10.oh no 13."))

  (st/with-rules
    ((gx i (values i (if (< i 4) (st/new gx (1+ i)) t))))
    (let* ((gg (st/new gx 0)))
      ; (st/itr/all gg #'princ)

     (is (mvl (st/acc/all gg)) '(nil (4 3 2 1 0)) )
     (is (mvb (g* val) (st/acc/n gg 2) (list val (functionp g*))) '((1 0) t))

     (is (mvl (st/acc/all gg #'cons (lambda (s) (lqn:fmt "~a/" s))))
         '(nil ("4/" "3/" "2/" "1/" "0/")))
     (is (mvb (g* val) (st/acc/until gg (lambda (o) (> o 3)))
              (list val (functionp g*)))
         '((4 3 2 1 0) T)))))

(subtest "test state machine fizzbuzz"
  (labels ((which? (i) (cond ((and #1=(zerop (mod i 3))
                                   #2=(zerop (mod i 5))) :fizzbuzz)
                             (#1# :fizz)
                             (#2# :buzz)
                             (t i))))

    (st/with-rules ((gx l (let ((i (car l)))
                           (values l (st/new gx (list (1+ i)
                                                   (which? (1+ i))))))))
      (is (lqn:stdstr
            (let ((gg (st/new gx `(1 ,(which? 1)))))
              (st/itr/n gg 20 #3=(lambda (s)
                                  (typecase (second s)
                                            (number (lqn:out " ~a" (second s)))
                                            (t (lqn:out "~%~a" (second s))))))))
          " 1 2
FIZZ 4
BUZZ
FIZZ 7 8
FIZZ
BUZZ 11
FIZZ 13 14
FIZZBUZZ 16 17
FIZZ 19
BUZZ"))))

(unless (finalize) (error "error in test state machine"))

