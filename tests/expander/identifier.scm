(define-syntax true
  (identifier 
    (lambda (x) #t)))

(print true)

(define-syntax function-id
  (identifier
    (lambda (x) 'print)))

(function-id #t)

