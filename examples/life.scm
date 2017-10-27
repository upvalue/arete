;; life.scm - Conway's Game of Life

(define (make-grid n)
  (define vec (make-vector n #f))
  (print n vec)
  (let loop ((i 0))
    ;(print "i: " i "vec: " vec "n: " n)
    (print "i:" i "n: " n "vec:"  vec)
    (if (= i n)
      vec
      (begin
        (vector-set! vec i (make-vector n #f))
        (loop (+ i 1))))))

#|
(define (make-grid n)
  (define vec (make-vector n #f))
  (print n vec)
  (define loop
    (lambda (i)
      ;(print "i: " i "vec: " vec "n: " n loop)
      ;(print i n vec loop)
      (print n vec loop)
      ;(print vec n loop)
      ;(print "i:" i "n: " n "vec:"  vec)
      ))

  (loop 0))
|#

(print (make-grid 3))

#;(define (print-grid g)
  (let loop ((i 0))
    (unless (fx= i (vector-length g))
      (print (vector-ref g i))
      (loop (fx+ i 1)))))
      
