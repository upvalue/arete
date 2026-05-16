(define bad-loop
  (((top-level-value 'compiler)
     ((top-level-value 'expander)
       '(lambda (n)
          (let loop ((i n) (acc 0))
            (if (fx= i 0)
              acc
              (loop (fx- i 1)))))
       (top-level-value '*core-module*)))))

(bad-loop 3)
