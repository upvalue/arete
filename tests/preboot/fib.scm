
 (define fib 
   (lambda (n)
     (if (fx= n 0)
       0
       (if (fx= n 1)
         1
         (fx+ (fib (fx- n 1)) (fib (fx- n 2)))))))

(print (fib 20))
