;; boot.scm - Arete boot file

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))

(define macroexpand
  (lambda (x e) 
    (if (self-evaluating? x)
        x
        (begin
          (define kar (car x))
          (cond 
            ((eq? kar 'define-syntax)
             ;; Under define-syntax, what we do is
             ;; evaluate the lambda
             ;; then put it in the environment (where does the environment come from)
             ;; it could be called from toplevel, or recursively right...
             ;; so 
             ;; (macroexpand (cadr x) env) is a possibility...right?
             (begin
               (define name (cadr x))
               ;; if not a symbol, throw a syntax error
               (if (not (symbol? name))
                   (raise 'expand "define-syntax first argument should be a symbol" x))

               ;(print x)
               (define body (caddr x))
               (define fn (eval-lambda body env))


               ;; 
               ;; (define macro (eval-lambda body env))

               ;; (print body)
               
               ;; check for existing definition of thing

               ;; (define fn (eval-lambda body))
               ;; (env-define env name fn)



               ;(if (not (symbol? name))
               ;    (syntax-error 

               ;; eval the lambda body, creating a function,
               ;; then define it in the environment

               ;; if eval-args is set, using it as a value is an error


               ;; eval lambda
               ;; env-define thing as macro...
               (print "Found a define-syntax" name)))
            ((eq? kar 'define)
              (begin
                (print "Found a define")))
            (else
              ;; go through function and args

              ;; if (car x) refers to macro
              ;; env-lookup env name
              ;; if macro? 
              ;; apply macro and replace with result of macroexpansion.



              (print "application" x)
              x)))
          )))

#;(macroexpand
  (define-syntax x
    (lambda () #t)
    )
  )

(macroexpand
  (define-syntax hello
    (lambda () 
      #t))
  #f)

#;(macroexpand (x))
