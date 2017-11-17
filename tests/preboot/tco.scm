(define (loop i)
  (if (= i 1000000)
    #t
    (loop (+ i 1))))

(print (loop 0))
