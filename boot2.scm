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
        ;; Take apart the more complex (define (function) ...) case
        (set! name (car kar))
        (set! value
          (list-source x (make-rename #f 'lambda) (cdr kar)
            (cons-source x (make-rename #f 'begin) (cddr x))))))

    (set! result (list-source x (car x) name (expand value env)))

    ;; Handle module stuff
    (if (table? env)
      (begin
        ;; Table-level renames
        (if (rename? name)
          (begin (rename-gensym! name) (vector-append! (table-ref env "module-renames" name)))
          ;; If this is not a rename, it may need to be exported
          (begin
            (if (table-ref env "module-export-all")
              (table-set! (table-ref env "module-exports") name #t))))
        (env-define env name (env-resolve env name) #f)
        (set! name (env-resolve env name)))
      (env-define env name 'variable))

    result))

;; Expand an identifier
(define expand-identifier
  (lambda (x env)
    (if (env-syntax? env x)
      (raise 'expand (print-string "used syntax" x "as value") (list x)))
    (env-resolve env x)))


;; Expand an application. Could be a special form, a macro, or a normal function application
(define expand-apply
  (lambda (x env)
    (define kar (car x))
    (define len (length x))
    (define syntax? (and (identifier? kar) (env-syntax? env kar)))

    ;; Extract names from renames
    ;; TODO: (let ((begin (lambda () (print "hello")))) (begin)) ?
    (if (and syntax? (rename? kar))
      (set! kar (rename-expr kar)))

    ;; Check for special syntactic forms
    (if syntax?
      (cond
        ((eq? kar 'define) (expand-define x env))
        ((eq? kar 'define-syntax) (expand-define-syntax x env))
        ((eq? kar 'lambda) (expand-lambda x env))
        ((eq? kar 'begin) (cons-source x (car x) (expand-map (cdr x) env)))
        ((eq? kar 'if) (expand-if x env))
        ((eq? kar 'set!) (expand-set x env))
        ((eq? kar 'cond) (expand-cond x env))
        ((eq? kar 'quote) x)
        (else (begin (print x)
                (expand-macro x env))))
      ;; Normal function application
      ;; Needs to be annotated with src info, right?
      ;; map-source?
      (expand-map x env))))

(define expand-lambda
  (lambda (x env)
    (define new-env (env-make env))
    (define bindings #f)

    (if (fx< (length x) 3)
      (raise 'expand "lambda has no body" (list x)))

    (set! bindings (cadr x))

    (if (or (null? bindings) (pair? bindings))
      (for-each-improper
        (lambda (arg)
          (if (not (identifier? arg))
            (raise 'expand "non-identifier in lambda argument list" (list bindings)))
          ;; If a rename is encountered here, env-define will add a gensym
          ;; then env-resolve will search for that gensym with env-lookup
          ;; then bindings are map'd to remove renames
          (if (rename? arg)
            (rename-gensym! arg))

          (env-define new-env arg 'variable))
        bindings)
      ;; Argument is a single symbol or rename (e.g. rest arguments only)
      (if (not (identifier? bindings))
        (raise 'expand "non-identifier as lambda rest argument" (list x (cdr x)))
        (env-define new-env bindings 'variable)))

    ;; If bindings are renames, they should be gensym'd 
    (set! bindings
      (if (identifier? bindings)
        (if (and (rename? bindings) (rename-gensym bindings)) (rename-gensym bindings) x)
        (map-improper (lambda (x) (if (and (rename? x) (rename-gensym x)) (rename-gensym x) x)) bindings)))

    (cons-source x (car x) (cons-source x bindings (expand-map (cddr x) new-env)))))

(define expand-if
  (lambda (x env)
    (define len (length x))
    (define else-branch unspecified)

    (if (fx< len 3)
      (raise 'expand "if expression needs at least two arguments" (list x))
      (if (fx= len 4)
        (set! else-branch (list-ref x 3))))

    (list-source x (car x) (expand (list-ref x 1) env) (expand (list-ref x 2) env) (expand else-branch env))))

(define expand-set
  (lambda (x env)
    (define len (length x))
    (define name #f)

    (if (not (fx= len 3))
      (raise 'expand "set! expression needs exactly two arguments" (list x)))

    (set! name (cadr x))

    (if (not (identifier? name))
      (raise 'expand "set! expects an identifier as its first argument" (list x (cdr x))))

    (list-source x (car x) (expand (list-ref x 1) env) (expand (list-ref x 2) env))))

(define expand
  (lambda (x env)
    (cond
      ((self-evaluating? x) x)
      ((identifier? x) (expand-identifier x env))
      (else (expand-apply x env)))))

(define expand-toplevel
  (lambda (x env)
    (expand x env)))

;; Install expander
(set-top-level-value! 'expander expand-toplevel)

;; Is this a hack or what? We use the expander even as we're implementing it. 

(define (expand-define-transformer! x env name body)
  (if (not (symbol? name))
    (raise 'expand "define-syntax first argument (macro name) must be a symbol" (list x (cdr x))))

  (set! body (caddr x))

  (define fn (eval-lambda (expand body env) env))

  (set-function-name! fn name)
  (set-function-macro-bit! fn))


(define (expand-define-syntax x env)
  (define len (length x))
  (define name #f)
  (define body #f)

  (if (fx< len 3)
    (raise 'expand "define-syntax expects at least two arguments: a name and a body" (list x)))

  (set! name (cadr x))

  (expand-define-transformer! x env name body))

(define (expand-let-syntax x env)
  (define new-env (env-make env))
  (define bindings #f)
  (define body #f)

  (if (fx< (length x) 3)
    (raise 'expand "let-syntax expands at least three arguments" (list x)))

  (set! bindings (cadr x))
  (set! body (cddr x))

  (if (not (list? bindings))
    (raise 'expand "let-syntax bindings list must be a valid list" (list x (cadr x))))

  (for-each
    (lambda (x)
      (define name #f)
      (define body #f)

      (if (not (fx= (length x) 2))
        (raise 'expand "let-syntax bindings should have two values: a name and a transformer" (list x)))

      (set! name (car x))
      (set! body (cadr x))

      (expand-define-syntax x new-env name body))
    bindings)

  (cons-source x (make-rename #f 'begin) (expand body new-env)))


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

;; cond expander
(define (expand-cond-full-clause x env clause rest)
  (define len #f)
  (define condition #f)
  (define body #f)

  (if (not (list? clause))
    (raise 'expand "cond clause should be a list" (list clause)))

  (set! len (length clause))

  (if (not (fx= len 2))
    (raise 'expand "cond clause should have two members: condition and body" (list x clause)))

  (set! condition (car clause))
  (set! body (cadr clause))

  ;; Handle else clause
  (if (env-compare env condition (make-rename env 'else))
    (set! condition #t))

  (list-source x
    (make-rename #f 'if)
    (expand condition env)
    (expand-map (list-source x (make-rename #f 'begin) body) env)
    (if (null? rest)
      unspecified
      (expand-cond-full-clause x env (car rest) (cdr rest)))))


(define (expand-cond-clause x env clause rest)
  (if (null? clause)
    unspecified
    (expand-cond-full-clause x env clause rest)))

(define (expand-cond x env)
  (if (fx= (length x) 1)
    unspecified
    (expand-cond-clause x env (cadr x) (cddr x))))
