(define-library (basic1)
  (import (arete core))
  (export method1 macro1)

  (begin
    (define-syntax macro1
      (lambda (x r c)
        ''success))

    (define (method1)
      'success)))
