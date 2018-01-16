(define-syntax true
  (identifier-transformer
    (lambda (x) #t)))

(print true)

(define-syntax function-id
  (identifier-transformer
    (lambda (x) 'print)))

(function-id #t)

