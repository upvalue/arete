;; boot.scm - arete boot file

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define not (lambda (x) (eq? x #f)))

(define unspecified (if #f #f))

(define _macroexpand
  (lambda (x env) 
    (if (self-evaluating? x)
        x
        (if (or (symbol? x) (box? x))
          ;; check syntax as value usage
          x
          (begin
            (define kar (car x))
            (define len (length x))
            (cond 
              ((eq? kar 'if)
                x)
              ;; LAMBDA
              ((eq? kar 'lambda)
               (if (fx< (length x) 3)
                   (raise 'expand "lambda has no body" x))
               ;; (lambda (a b c) #t)
               (define bindings #f)
               #t)
              ;; DEFINE-SYNTAX
              ((eq? kar 'define-syntax)
               (begin
                 (define name (cadr x))
                 ;; if not a symbol, throw a syntax error
                 (if (not (symbol? name))
                     (raise 'expand "define-syntax first argument should be a symbol" x))

                 (define body (caddr x))

                 ;; TODO: Check for existing definition
                 (define fn (eval-lambda body env))

                 (set-function-name! fn name)
                 (set-function-macro-bit! fn)

                 (env-define env name fn)

                 ;; macro now exists in environment
                 unspecified))

              ;; define, lambda, cond, begin, let
              ((eq? kar 'define)
                (begin
                  (print "Found a define")
                  (print x)
                  ))
              (else
                ;; go through function and args
                (begin
                  (if (symbol? kar)
                      (begin
                        (define lookup (env-lookup env kar))
                        (if (macro? lookup)
                            (lookup x
                              ;; renaming procedure
                              (lambda (name) (make-rename name (function-env lookup)))
                              ;; comparison procedure
                              (lambda (a b) #f))
                            ;; not a macro application, members must be expanded
                            (map (lambda (sub-x) (_macroexpand sub-x env)) x)))))))
                            ;; arguments must be expanded
                            ;x))))))
              )) ;; (if (symbol? x)
            ))) ;; end _macroexpand

;; This function is special-cased; its arguments will not be evaluated before it is applied in the interpreter
(define macroexpand
  (lambda (x env)
    (_macroexpand x env)))

(define-syntax let
  (lambda (x r c)
    (if (fx< (length x) 3)
      (raise 'expand "let has no body" x))
    (define let-fn-name #f)
    (define bindings #f)
    (define body #f)

    (if (symbol? (list-ref x 1))
      (begin
        (set! let-fn-name (list-ref x 1))
        (set! bindings (list-ref x 2))
        (set! body (cdddr x)))
      (begin
        (set! bindings (list-ref x 1))
        (set! body (cddr x))))

    (define names #f)
    (define vals #f)

    (set! names
      (map (lambda (binding)
             (print binding)
             (if (not (list? binding))
                 (raise 'expand "let binding should be a list with a name and a value" x))
             (define name (car binding))
             (if (not (fx= (length binding) 2))
                 (raise 'expand "let binding should have only 2 elements (name and value)" x))
             (if (not (symbol? name))
                 (raise 'expand "let binding name should be a symbol" x))
             name)
           bindings))

    (set! vals 
      (map (lambda (binding)
             ;; TODO macroexpand.
             (cadr binding)
             ) bindings))

    (define result 
       (cons-source x (r 'lambda)
         (cons-source x names body)) vals)

    (set! result
      (if let-fn-name
        ;; named function application
        (cons-source x (list-source x (r 'lambda) '()
          (list-source x (r 'define) let-fn-name result)
          (cons-source x let-fn-name vals)) '())
        ;; anonymous function application
        (cons-source x result vals)))

   ;; let return
    result))

(print
  (let loop ((x 0))
    (if (fx= x 5)
        (print x)
        (loop (fx+ x 1)))))


