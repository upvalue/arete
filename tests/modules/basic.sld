(define-library (basic)
  (import (arete))
  (export return-true return-true-stx)

  (begin
    (define (return-true) #t)

    (define (secret-return-true) #t)

    (define-syntax return-true-stx
      (lambda (x)
        (list #'secret-return-true)))))
