
(define (list-ref-cell lst i)
  (if (or (null? lst) (not (pair? lst)))
    (raise 'type "list-ref-cell expects a list with at least one element as its argument" (list lst)))
  (if (fx= i 0)
    lst
    (let loop ((rest lst)
               (ii 0))
      (if (null? rest)
        (raise 'type "list-ref-cell bounds error" (list lst (length lst) i)))
      (if (fx= ii i)
        rest
        (loop (cdr rest) (fx+ ii 1))))))

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

