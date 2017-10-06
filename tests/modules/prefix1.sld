(define-library (prefix1)
  (export var func macro)

  (begin
    (define var 'success)
    (define (func) 'success)

    (define-syntax macro 
      (lambda (x r c)
        ''success))))
