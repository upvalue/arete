;; boot.scm - Arete boot file

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))

(define toplevel-env #(#f define syntax let syntax))

(define make-env
  (lambda (parent)
    (define vec (make-vector 1 parent))
    vec))

(define env-lookup
  (lambda (env name one-level?)
    #f))

(set! macroexpand 
  (lambda (x) ;; x, e = expression, environment


    (print "macroexpanding this value " x)))

(display "macroexpanded after this point")
(newline)
