;; fib.scm - Dumbest possible way to compute anything

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(cond-expand
  ((and arete)
   (vmfunction->native! fib))
  (else #t))

(display (fib 36)) (newline)

