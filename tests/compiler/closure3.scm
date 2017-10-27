(define make-grid
  (lambda (n)
    (define vec (make-vector n))
    (define loop
      (lambda (i)
        (if (= i n)
          vec
          (begin
            (vector-set! vec i (make-vector n))
            (loop (+ i 1))))))
    (loop 1)))

(make-grid 1)
