;; life.scm - Conway's Game of Life

(define (make-grid x y)
  (define vec (make-vector y #f))
  (let loop ((i 0))
    (if (= i y)
      vec
      (begin
        (vector-set! vec i (make-vector x 0))
        (loop (+ i 1))))))

(define (print-grid g)
  (let loop ((i 0))
    (when (eq? (fx= i (vector-length g)) #f)
      (print (vector-ref g i))
      (loop (fx+ i 1)))))

(define (get-cell g x y)
  (vector-ref (vector-ref g y) x))

(define (set-cell! g x y n)
  (vector-set! (vector-ref g y) x n))
      

(define (next-cell g value x y)
  (let ((n (+ (get-cell grid (- x 1) (- y 1))
              (get-cell grid (- x 1) y)
              (get-cell grid (- x 1) (- y 1))
              (get-cell grid x (- y 1))
              (get-cell grid x (+ y 1))
              (get-cell grid x (+ x 1) (- y 1))
              (get-cell grid (+ x 1) y)
              (get-cell grid (+ x 1) (+ y 1)))))
    (if (and (= cell 1) (or (= n 2) (= n 3)))
      1
      (if (and (= cell 0) (= n 3))
        1
        0))))


(define (next-grid g)
  (let ((y (vector-length g))
        (x (vector-length (vector-ref g 0))))
    #t))

(define (simulate grid turns)
  (let loop ((i 0))
    (if (fx= i turns)
      #t
      (begin
        (print "turn:" i)
        (loop (fx+ i 1))))))

#|
(define initial-grid
  (vector 
    (vector 0 0 1 0 0 0 0 0)
    (vector 0 0 0 1 0 0 0 0)
    (vector 0 1 1 1 0 0 0 0)
    (vector 0 0 0 0 0 0 0 0)
  ))
|#

(define grid (make-grid 8 8))
(set-cell! grid 3 0 1)
(set-cell! grid 4 1 1)
(set-cell! grid 2 2 1)
(set-cell! grid 3 2 1)
(set-cell! grid 4 2 1)


(print-grid grid)

;(simulate grid 5)
;(set-cell! grid 2 2 1)
;(print-grid grid)
;(print (get-cell grid 2 2))
