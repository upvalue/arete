;; hyg.scm - basic hygiene tests

;; introduce a renamed variable

(define-syntax hello
  (lambda (x r c)
    `(let ((,(r 'var) 'success))
       (print ,(r 'var)))))

(hello)
(let ((var 'failure))
  (hello))

