

(define (reduce fn lst)
  (if (null? lst)
    '()
    (let ((len (length lst)))
      (let loop ((i 0)
                 (result (car lst)))
        (if (fx= i (fx- len 1))
          result
          (begin
            (loop (fx+ i 1) (fn result (list-ref lst (fx+ i 1))))))))))

