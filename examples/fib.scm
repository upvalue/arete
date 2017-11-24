;; fib.scm - Dumbest possible way to compute anything

(letrec ((fib (lambda (n)
          (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))))
  (display (fib 40))
  (newline))



#|
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(display (fib 40))
(newline)

|#
