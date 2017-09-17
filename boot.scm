;; boot.scm - Arete boot file

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))

(define unspecified (if #f #f))

(define macroexpand
  (lambda (x mac-env use-env) 
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
               ;; TODO: Check for existing definition
               (define fn (eval-lambda body mac-env))
               (env-define mac-env name fn)

               ;; macro now exists in environment
               unspecified))

            ((eq? kar 'define)
              (begin
                (print "Found a define")))
            (else
              ;; go through function and args
              (begin
                (if (symbol? (car x))
                    ;; if (macro? x) 
                    ;;   (apply (car x) (cdr x) env)
                    ;; also, the arguments themselves must be expanded. obviously. map should exist.
                    ;; Right? 
                    (begin
                      (print "got a potential macro application" x))
                    x)))

                ;; if (car x) is a symbol
                ;; env-lookup env name
                ;; if (macro? x) (apply x args)
            ))
          )))

#;(macroexpand
  (define-syntax x
    (lambda () #t)
    )
  )

(macroexpand
  (define-syntax hello
    (lambda () 
      "expansion successful"))
  #f #f)

(print "macroexpansion result" (macroexpand
  (hello)
  #f #f))

#;(macroexpand (x))
