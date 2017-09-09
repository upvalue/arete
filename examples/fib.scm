;(define fib
;  (lambda (n)
;    (if (= n 0)
;      0
;      (if (= n 1)
;        1
;        (+ (fib (- n 1)) (fib (- n 2)))))))

;(display (fib 4))
;(newline)

 (define fib 
   (lambda (n)
     (if (fx= n 0)
       0
       (if (fx= n 1)
         1
         (fx+ (fib (fx- n 1)) (fib (fx- n 2)))))))

(print (fib 20))
;(print (fib 10))

;(print (fib 30))



