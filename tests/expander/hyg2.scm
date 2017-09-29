(print 
  (let ((let 'success)) let))

(print
  (let ((begin 'success)) begin))

#|
(define-syntax hello
  (lambda (x r c)
    `(,(r 'let) ((begin 'failure))
      (,(r 'begin)

        ,(cadr x)
        (print begin)))))

(hello
  (define begin 'success))

This doesn't work because it prints a variable shadow warning (as it should) but otherwise functions fine
|#

(define-syntax hello
  (lambda (x r c)
    `(,(r 'let) ((begin 'success))
      (,(r 'begin)
        (print begin)))))

(hello)
