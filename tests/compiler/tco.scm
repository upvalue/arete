;; tco.scm - tail call optimization test

(define loop 
  (lambda (i limit)
    (if (= i limit)
      i
      (loop (+ i 1) limit))))

(loop 0 10000000)
