;; fib.scm - Dumbest possible way to compute anything

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(vmfunction->native! fib)

(display (fib 36)) (newline)

