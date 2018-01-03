;; fib-fast.scm - uses 2-argument fixnum builtins

(define (fib-fast n)
  (if (fx< n 2) n (fx+ (fib-fast (fx- n 1)) (fib-fast (fx- n 2)))))

;(display (fib-fast 36)) (newline)
    
