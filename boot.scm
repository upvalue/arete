;; boot.scm - arete boot file

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define unspecified (if #f #f))

(define _macroexpand
  (lambda (x mac-env use-env) 
    (print "macroexpand" x)
    (if (self-evaluating? x)
        x
        (begin
          (define kar (car x))
          (define len (length x))
          (cond 
            ((eq? kar 'if)
              x)
            ;; LET
            ((eq? kar 'let)
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
              (define new-env (env-make use-env))

              (if let-fn-name
                (env-define new-env let-fn-name 'variable))

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
                       (env-define new-env name 'variable)
                       name)
                     bindings))
 
              (set! vals 
                (map (lambda (binding)
                       ;; TODO macroexpand.
                       (_macroexpand (cadr binding) mac-env new-env)
                       ) bindings))
 
              ;(print "names and values" names vals)
              ;(print "parsed let" let-fn-name bindings body)
              ;(print "let introduces env" new-env)

              (set! body 
                (map (lambda (x)
                        ; (print "sub-macroexpand" x)
                        (_macroexpand x mac-env new-env))
                  body))

              (define result 
                 (cons-source x 'lambda
                   (cons-source x names body)) vals)

              (set! result
                (if let-fn-name
                  (cons-source x (list-source x 'lambda '()
                    (list-source x 'define let-fn-name result)
                    (cons-source x let-fn-name vals)) '())
                  (cons-source x result vals)))

             ;; let return
              result)
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

               (define body (caddr x))

               ;; TODO: Check for existing definition
               (define fn (eval-lambda body mac-env))

               (set-function-name! fn name)
               (set-function-macro-bit! fn)

               (env-define mac-env name fn)

               ;; macro now exists in environment
               unspecified))

            ;; define, lambda, cond, begin, let
            ((eq? kar 'define)
              (begin
                (print "Found a define")))
            (else
              ;; go through function and args
              (begin
                (if (symbol? kar)
                    ;; if (macro? x) 
                    ;;   (apply (car x) (cdr x) env)
                    ;; also, the arguments themselves must be expanded. obviously. map should exist.
                    ;; Right? 
                    (begin
                      (define lookup (env-lookup use-env kar))
                      (if (macro? lookup)
                          (apply lookup '())
                          x)))))
            ))
          )))

;; This function is special-cased; its arguments will not be evaluated before it is applied.
(define macroexpand
  (lambda (x)
    (_macroexpand x mac-env use-env)))
