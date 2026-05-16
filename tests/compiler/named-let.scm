(define sum-down
  (((top-level-value 'compiler)
     ((top-level-value 'expander)
       '(lambda (n)
          (let loop ((i n) (acc 0))
            (if (fx= i 0)
              acc
              (loop (fx- i 1) (fx+ acc i)))))
       (top-level-value '*core-module*)))))

(define non-tail-count
  (((top-level-value 'compiler)
     ((top-level-value 'expander)
       '(lambda (n)
          (let loop ((i n))
            (if (fx= i 0)
              0
              (fx+ 1 (loop (fx- i 1))))))
       (top-level-value '*core-module*)))))

(list (sum-down 1000) (non-tail-count 12))
