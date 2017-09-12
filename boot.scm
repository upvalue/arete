;; boot.scm - Arete boot file

;; scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))

(define toplevel-env #(#f define syntax let syntax))

(define make-env
  (lambda (parent)
    (define vec (make-vector 1 parent))
    vec))

(define bad-function
  (lambda ()
    (do-a-bad-thing)))

(define bad-function2 (lambda () (bad-function)))

(bad-function2)

(print (make-env #f))


