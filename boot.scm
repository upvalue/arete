;; boot.scm - expander,  basic syntax

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

;; TODO Cannot use a macro defined in advance. Toplevel needs to behave like letrec-syntax, I think.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define not (lambda (x) (eq? x #f)))

(define boolean? (lambda (v) (or (eq? v #t) (eq? v #f))))

(define fixnum? (lambda (v) (eq? (value-type v) 1)))
(define string? (lambda (v) (eq? (value-type v) 5)))
(define symbol? (lambda (v) (eq? (value-type v) 8)))
(define vector? (lambda (v) (eq? (value-type v) 9)))
(define table? (lambda (v) (eq? (value-type v) 15)))
(define rename? (lambda (v) (eq? (value-type v) 16)))
(define identifier? (lambda (v) (or (rename? v) (symbol? v))))
(define null? (lambda (v) (eq? (value-bits v) 10)))

;(define interpreted-function? (lambda (v) (eq? (value-type v)


(define unspecified (if #f #f))

(define rename
  (lambda (name)
    (define env (top-level-value '*current-rename-env*))

    (if (not (symbol? name))
      (raise 'expand "attempt to rename non-symbol" (list x)))

    (if (eq? (top-level-value '*current-rename-env*) unspecified)
      (raise 'expand "(rename) called without environment (did you forget to unquote a rename?)" (list name)))
    
    (make-rename env name)))

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

    (if (not (or (identifier? kar) (pair? kar)))
      (raise 'expand "define first argument must be a name or a list" (list x (cdr x))))

    (if (identifier? kar)
      (if (not (fx= len 3))
        (raise 'expand "define expects exactly two arguments (name and value)" (list x))
        (begin
          (set! name kar)
          (set! value (caddr x))))
      (begin
        ;; Take apart the more complex (define (function) ...) case
        (set! name (car kar))
        (set! value
          (list-source x (make-rename #f 'lambda) (cdr kar)
            (cons-source x (make-rename #f 'begin) (cddr x))))))

    (if (env-syntax? env name)
      (raise 'expand (print-string "definition of" name "shadows syntax") (list x)))

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
        ;; TODO. Could probably use string-append here instead of relying on env-resolve to fail.
        (env-define env name (env-resolve env name) #f)
        (set! name (env-resolve env name)))
      (env-define env name 'variable))

    (set! result (list-source x (car x) name (expand value env)))

    result))

;; Expand an identifier
(define expand-identifier
  (lambda (x env)
    (if (env-syntax? env x)
      (raise 'expand (print-string "used syntax" x "as value") (list x)))
    (env-resolve env x)))

;; Expand and/or
(define expand-and-or
  (lambda (x env)
    (cons-source x (car x) (expand-map (cdr x) env))))

(define expand-cond-expand
  (lambda (x env)
    unspecified))

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
        ((or (eq? kar 'letrec-syntax) (eq? kar 'let-syntax)) (expand-let-syntax x env))
        ((or (eq? kar 'and) (eq? kar 'or)) (expand-and-or x env))
        ((eq? kar 'lambda) (expand-lambda x env))
        ((eq? kar 'begin) (cons-source x (car x) (expand-map (cdr x) env)))
        ((eq? kar 'if) (expand-if x env))
        ((eq? kar 'set!) (expand-set x env))
        ((eq? kar 'cond) (expand-cond x env))
        ((eq? kar 'quote) x)
        ((eq? kar 'cond-expand) (expand-cond-expand x env))
        (else (begin 
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
        (if (and (rename? bindings) (rename-gensym bindings)) (rename-gensym bindings) bindings)
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

(define expand-define-transformer!
  (lambda (x env name body)
    (define expanded-body #f)
    (define fn #f)
    (define fn-arity #f)

    (if (not (symbol? name))
      (raise 'expand "define-syntax first argument (macro name) must be a symbol" (list x (cdr x))))

    (if (table? env)
      (if (table-ref env "module-export-all")
        (table-set! (table-ref env "module-exports") name #t)))

    ;(set! body (caddr x))
    (set! expanded-body (expand body env))
    (set! fn (eval expanded-body env))

    (if (not (procedure? fn))
      (raise 'expand "define-syntax body did not result in a function" (list x)))

    (set! fn-arity (function-min-arity fn))

    (if (fx< fn-arity 1)
      (raise 'expand "transformer must take at least one argument" (list x (cdr x))))

    (set-function-name! fn name)
    (set-function-macro-bit! fn)

    (env-define env name fn #t)

    unspecified))

(define expand-define-syntax
  (lambda (x env)
    (define len (length x))
    (define name #f)
    (define body #f)

    (if (fx< len 3)
      (raise 'expand "define-syntax expects at least two arguments: a name and a body" (list x)))

    (set! name (cadr x))
    (set! body (caddr x))

    (expand-define-transformer! x env name body)))

;; TODO (let-syntax () (define x #t))

(define expand-let-syntax
  (lambda (x env)
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

        (expand-define-transformer! x new-env name body))
      bindings)

    (cons-source x (make-rename #f 'begin) (expand body new-env))))

;; Expand a macro appplication
(define expand-macro 
  (lambda (x env)
    (define lookup (env-lookup env (car x)))
    (define arity (function-min-arity lookup))
    (define result #f)
    (define saved-rename-env (top-level-value '*current-rename-env*))

    (set-top-level-value! '*current-rename-env* (function-env lookup))

    (set! result
      (if (eq? arity 1)
        ;; Only pass form
        (lookup x)
        (if (eq? arity 2)
          ;; Only pass form and comparison procedure
          (lookup x (lambda (a b) (env-compare env a b)))

          (lookup x
            ;; rename procedure
            (lambda (name) (make-rename (function-env lookup) name))
            ;; compare procedure
            (lambda (a b) (env-compare env a b))))))

    (set-top-level-value! '*current-rename-env* saved-rename-env)

    (set! result (expand result env))

    result))

;; cond expander
(define expand-cond-full-clause
  (lambda (x env clause rest)
    (define len #f)
    (define condition #f)
    (define body #f)

    (if (not (list? clause))
      (raise 'expand "cond clause should be a list" (list clause)))

    (set! len (length clause))

    (if (not (fx> len 1))
      (raise 'expand "cond clause should have two members: condition and body" (list x clause)))

    (set! condition (car clause))
    (set! body (cdr clause))

    ;; Handle else clause
    (if (env-compare env condition (make-rename env 'else))
      (set! condition #t))

    (define result

    (list-source x
      (make-rename #f 'if)
      (expand condition env)
      (expand (cons-source x (make-rename #f 'begin) body) env)
      ;(list-source x (make-rename #f 'begin) (expand-map body env))
      (if (null? rest)
        unspecified
        (expand-cond-full-clause x env (car rest) (cdr rest))))

    )

    result))


(define expand-cond-clause 
  (lambda (x env clause rest)
    (if (null? clause)
      unspecified
      (expand-cond-full-clause x env clause rest))))

(define expand-cond
  (lambda (x env)
    (if (fx= (length x) 1)
      unspecified
      (expand-cond-clause x env (cadr x) (cddr x)))))

;; Install expander
(set-top-level-value! 'expander expand-toplevel)

;;;;; SYNTAX! Basic syntax: let, quasiquote, etc

(define-syntax let
  (lambda (x r c)
    (define let-fn-name #f)
    (define bindings #f)
    (define body #f)
    (define names #f)
    (define vals #f)
    (define result #f)

    (if (fx< (length x) 3)
      (raise 'expand "let has no body" x))

    (if (symbol? (list-ref x 1))
      (begin
        (set! let-fn-name (list-ref x 1))
        (set! bindings (list-ref x 2))
        (set! body (cdddr x)))
      (begin
        (set! bindings (list-ref x 1))
        (set! body (cddr x))))

    (set! names
      (map (lambda (binding)
             (define name #f)
             (if (not (list? binding))
               (raise 'syntax "let binding should be a list with a name and a value" (list x)))

             (if (not (fx= (length binding) 2))
               (raise 'syntax "let binding should have exactly 2 elements (name and value)" (list binding)))

             (set! name (car binding))

             (if (not (identifier? name))
               (raise 'syntax "let binding name should be an identifier" (list binding)))

             name)
           bindings))

    (set! vals 
      (map (lambda (binding)
             ;; TODO expand.
             (cadr binding)
             ) bindings))

    (set! result 
       (cons-source x #'lambda
         (cons-source x names body)))

    (set! result
      (if let-fn-name
        ;; named function application
        (cons-source x (list-source x #'lambda '()
          (list-source x #'define let-fn-name result)
          (cons-source x let-fn-name vals)) '())
        ;; anonymous function application
        (cons-source x result vals)))

    ;; let return
    result))

(define (concat-list x y)
  (if (pair? x)
      (cons (car x) (concat-list (cdr x) y))
      y))

(define (qq-list c lst)
  (if (pair? lst)
    (let ((obj (car lst)))
      (if (and (pair? obj) (c #'unquote-splicing (car obj)))
        (if (cdr lst)
          (list #'concat-list (cadr obj) (qq-list c (cdr lst)))
          (cadr obj))
        ;; TODO: This could be replaced with cons* for less calls and less confusing output
        (list #'cons (qq-object c obj) (qq-list c (cdr lst)))))
    (list #'quote lst)))

(define (qq-element c lst)
  (if (c #'unquote (car lst))
      (cadr lst)
      (qq-list c lst)))
         
(define (qq-object c object)
  (if (pair? object)
      (qq-element c object)
      (list #'quote object)))

(define-syntax quasiquote
  (lambda (x c)
    (qq-object c (cadr x))
    ))

(define-syntax when
  (lambda (x)
    (if (fx< (length x) 3)
      (raise 'syntax "when expects a condition and a body" (list x)))

    `(,#'if ,(list-ref x 1)
        (,#'begin ,@(cddr x)))))

(define-syntax unless
  (lambda (x)
    (if (fx< (length x) 3)
      (raise 'syntax "when expects a condition and a body" (list x)))

    `(,#'if (,#'not ,(list-ref x 1))
        (,#'begin ,@(cddr x)))))

;; case
;; TODO =>
(define-syntax case
  (lambda (x c)
    (define key #f)
    (define clauses #f)

    (if (fx< (length x) 3)
      (raise 'syntax "case expects at least three arguments (a key and a clause)" x))

    (set! key (cadr x))
    (set! clauses (cddr x))

    (if (not (list? clauses))
      (raise 'syntax "case expects a list of clauses" x))

    (define code (let loop ((clause (car clauses))
               (clauses clauses))
      (unless (pair? clause)
        (raise 'syntax "case expects clause to be a datum" x))

      ;; TODO: Most Schemes seem to support a direct comparison as well e.g.
      ;; (case 5 (5 #t))
      (unless (or (symbol? (car clause)) (list? (car clause)) (self-evaluating? (car clause)))
        (raise 'syntax "case expects an else clause, a literal, or a list of literals as its datum"))
        #;(if (eq? (car clause) 'else)
          (raise 'syntax "case expected an else clause, but it seems else has been redefined" x)
          (raise 'syntax "case expected a list or else clause as its datum" x))

      (define condition
        (if (c (car clause) #'else)
          #t 
          (if (or (symbol? (car clause)) (self-evaluating? (car clause)))
            ;; Immediate values result in an eq? call
            `(,#'eq? ,#'result (,#'quote ,(car clause)))            
            ;; Lists will be memv'd
            `(,#'memv ,#'result (,#'quote ,(car clause))))))

      (if (null? (cdr clauses))
        `(,#'if ,condition
          (,#'begin ,(cadr clause))
          unspecified)
        `(,#'if ,condition
          (,#'begin ,(cadr clause))
          ,(loop (cadr clauses) (cdr clauses))))
      ))

    `(,#'let ((,#'result ,key))
      ,code)))

(define-syntax aif
  (lambda (x)
    (unless (or (fx= (length x) 4) (fx= (length x) 3))
      (raise 'syntax "aif expects three or four arguments" (list x)))

    `(,#'let ((it ,(list-ref x 1)))
        (,#'if it ,(list-ref x 2) ,(if (fx= (length x) 4) (list-ref x 3) unspecified)))))

(define-syntax let*
  (lambda (x)
    (unless (fx> (length x) 2)
      (raise 'syntax "let* expects at least two arguments (bindings and body)" (list x)))

    (let ((bindings (list-ref x 1))
          (body (cddr x)))
      (for-each
        (lambda (binding)
          (unless (and (list? binding) (fx= (length binding) 2))
            (raise 'syntax "let* binding must be a list with two elements" (list x binding))))
        bindings)

    `(,#'let (,@(map (lambda (b) (list-source b (car b) #'unspecified)) bindings))
      ,@(map (lambda (b) (list-source b #'set! (car b) (cadr b))) bindings)
      ,@body))))

(define (max a b) (if (< a b) b a))
(define (min a b) (if (> a b) b a))

(define (values . rest)
  (cons (list 'values) rest))

#;(define (vector . rest)
  (let ((vec (make-vector)))
    (if (null? rest)
      vec
      (let loop ((rest (cdr rest))
                 (this (car rest)))
        (vector-append! vec this)
        (unless (null? rest)
          (loop (cdr rest))
          )))))
