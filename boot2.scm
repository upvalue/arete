;; boot2.scm - expander, compiler, standard library

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define not (lambda (x) (eq? x #f)))

(define unspecified (if #f #f))

;;;;; EXPAND! Expansion pass

;; Shorthand for mapping expand because it's so common
(define expand-map
  (lambda (x env)
    (map (lambda (sub-x) (expand sub-x env)) x)))

(define expand-define
  (lambda (x env)
    (define name #f)
    (define value #f)
    (define kar #f)
    (define len (length x))
    (define result #f)

    (if (fx< len 2)
      (raise 'expand "define is missing first argument (name)" (list x)))

    (if (fx< len 3)
      (raise 'expand "define is missing second argument (value)" (list x)))

    (set! kar (cadr x))

    (if (not (or (identifier? kar) (list? kar)))
      (raise 'expand "define first argument must be a name or a list" (list x (cdr x))))

    (if (identifier? kar)
      (begin
        (set! name kar)
        (set! value (caddr x)))
      (begin
        (set! name (car kar))
        (set! value
          (list-source x (make-rename #f 'lambda) (cdr kar)
            (cons-source x (make-rename #f 'begin) (cddr x))))))

    (set! result (list-source x (car x) name (expand value env)))

    result))

;; Expand an identifier
(define expand-identifier
  (lambda (x env)
    x))

;; Expand an application
(define expand-apply
  (lambda (x env)
    (define kar (car x))
    (define len (length x))
    (define syntax? (and (identifier? kar) (env-syntax? env kar)))

    (if (and syntax? (rename? kar))
      (set! kar (rename-expr kar)))

    ;; Check for special syntactic forms
    (if syntax?
      (cond
        ((eq? kar 'define) (expand-define x env))
        ((eq? kar 'define-syntax) (expand-define-syntax x env))
        ((eq? kar 'lambda) (expand-lambda x env))
        (else (expand-macro x env)))
      ;; Normal function application
      x)))

(define expand-lambda
  (lambda (x env)
    x))

(define expand
  (lambda (x env)
    (cond
      ((self-evaluating? x) x)
      ((identifier? x) (expand-identifier x env))
      (else (expand-apply x env)))))

;; Install expander
(set-top-level-value! 'expander expand)

;; Is this a hack or what? We use the expander even as we're implementing it. 

(define (expand-define-syntax x env)
  (define len (length x))
  (define name #f)
  (define body #f)

  (if (fx< len 3)
    (raise 'expand "define-syntax expects at least two arguments: a name and a body" (list x)))

  (set! name (cadr x))

  (if (not (symbol? name))
    (raise 'expand "define-syntax first argument (macro name) must be a symbol" (list x (cdr x))))

  (set! body (caddr x))

  (define fn (eval-lambda (expand body env) env))

  (set-function-name! fn name)
  (set-function-macro-bit! fn)

  (env-define env name fn)

  unspecified)

;; Expand a macro appplication
(define (expand-macro x env)
  (define lookup (env-lookup env (car x)))

  (define result
    (lookup x
      ;; rename procedure
      (lambda (name) (make-rename (function-env lookup) name))
      ;; compare procedure
      (lambda (a b) (env-compare env a b))))

  (set! result (expand result env))

  result)

