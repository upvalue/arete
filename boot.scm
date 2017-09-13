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
  (lambda (x) ;; x, e = expression, environment

    ;; what does this do
    ;; regular expressions are returned directly, right
    ;; then we dig in
    ;; (define-syntax my-macro ...)
    ;; (define x (lambda () (my-macro)))
    ;; so we have a toplevel env that contains all this info, like
    ;; #(#f my-macro #<function>)
    ;; so in a define, the body must be checked
    ;; then the lambda, the body is also checked for macros
    ;; 

    ;; so we have a global expansion environment.
    ;; macroexpander modifies this
    ;; checks if something is self-evaluating (this can occur before macroexpand is called probably as it's easy to do)
    ;; 
    ;; so basically
    ;; encounter an expression, parse parts of that expression that may have valid things in them etc...
    ;; and so on.
    ;; define-syntax
    ;; define
    ;; lambda
    ;; ?

    (print "macroexpanding this value " x)))

(display "macroexpanded after this point")
(newline)
