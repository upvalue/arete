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

(define *expansion-env*
  #(#f define syntax lambda syntax quote syntax set! syntax
    define-syntax syntax if syntax begin syntax))

(set! macroexpand
  (lambda (x)
    (if (self-evaluating? x)
        x
        (error))))

(display "macroexpansion active!")
(newline)
