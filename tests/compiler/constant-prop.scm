(define const-fold
  (lambda ()
    ((lambda (x) (fx+ x 1)) 41)))

(define partial
  (lambda (value)
    ((lambda (x y) (fx+ x y)) 1 value)))

(define assigned
  (lambda ()
    ((lambda (x) (set! x 2) x) 1)))

(define captured-set
  (lambda ()
    ((lambda (x)
       ((lambda () (set! x 2)))
       x)
     1)))

(list
  (const-fold)
  (partial 5)
  (assigned)
  (captured-set))
