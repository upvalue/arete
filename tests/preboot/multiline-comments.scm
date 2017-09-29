
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
