;; fib.scm - Dumbest possible way to compute anything

#|
(letrec ((fib (lambda (n)
          (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))))
  (display (fib 40))
  (newline))
|#

;; control destination would allow us to return immediately rather than jumping

(define (fib n)
  (if (< n 2)
    ;; jump-if-false still necessary
    n
    ;; no jump necessary
    (+ (fib (- n 1)) (fib (- n 2)))))

(display (fib 36))
(newline)

