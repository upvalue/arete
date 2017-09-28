;; closures.scm

;; closure without set
(define make-adder
  (lambda (x)
    (lambda (y) (fx+ x y))))

(print ((make-adder 5) 5))

;; closure with set
(define counter
  (lambda (x)
    (lambda ()
      (set! x (fx+ x 1))
      x)))

(define my-counter (counter 0))

(print (my-counter))
(print (my-counter))
(print (my-counter))
(print (my-counter))
(print (my-counter))
