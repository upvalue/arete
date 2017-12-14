(define one (make-parameter 1))
(define two (make-parameter "2" (lambda (value) (string->number value))))
(define three (make-top-level-parameter 'three 3))

(print (one) (two) (three))

(parameterize ((one 2)
               (two "3")
               (three 4))
  (print (one) (two) (three)))

(print (one) (two) (three))

