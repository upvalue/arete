;; boot.scm - Macroexpansion, various key functions

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define not (lambda (x) (eq? x #f)))

(define unspecified (if #f #f))

;;;;; MACROEXPANSION

;; Shortcut for applying expand with an environment, since this is used pretty often
(define expand-map
  (lambda (x env)
    (map (lambda (sub-x) (expand sub-x env)) x)))

(define expand-define-syntax
  (lambda (x env name body)
    (if (not (symbol? name))
        (raise 'expand "macro name must be a symbol" x))

    (define expanded-body (expand body env))

    ;; (print expanded-body)
    (define fn (eval-lambda expanded-body env))

    (set-function-name! fn name)
    (set-function-macro-bit! fn)

    (env-define env name fn)

    unspecified))

(define expand-let-syntax
  (lambda (x env)
     (begin
       (if (fx< (length x) 3)
           (raise 'expand "let-syntax expects at least three arguments" x))

       (define new-env (env-make env))
       (define bindings (cadr x))
       (define body (cddr x))

       (if (not (list? bindings))
           (raise 'expand "let-syntax bindings list must be a valid list" x))

       (map 
          (lambda (x)
            (if (not (fx= (length x) 2))
                (raise 'expand "let-syntax bindings should have two values: a name and a macro" x)) 
            (define name (car x))
            (define body (cadr x))

            (expand-define-syntax x new-env name body))
          bindings)

       (cons-source x (make-rename #f 'begin) (expand body new-env)))))

;; define expander; handles creating a note in the env as well as 
;; extended syntax: (define (fn) #t) becomes (define fn (lambda () #t))
(define expand-define
  (lambda (x env)
    (if (fx< (length x) 2)
        (raise 'expand "define is missing name" x))
    (if (fx< (length x) 3)
        (raise 'expand "define is missing value" x))

    (define name #f)
    (define value #f)
    (define kar (cadr x))

    (if (identifier? kar)
      (begin
        (if (not (fx= (length x) 3))
            (raise 'expand "define must have exactly two arguments (name and value)" x))

        (set! name kar)
        (set! value (caddr x)))
      (if (not (pair? kar))
          (raise 'expand "define first argument must be a name or a list" x)
          (begin
            (set! name (car kar))
            (set! value (list-source x (make-rename #f 'lambda) (cdr kar)
              (cons-source x (make-rename #f 'begin) (cddr x))))
            )))

    (if (env-syntax? env name)
      (raise 'expand "definition shadows syntax" (list x name)))

    ;; If this isn't a module, we have to note it as a 'variable so env-resolve will treat it as a local variable
    (if (not (table? env))
      (env-define env name 'variable)
      (begin
        ;; Otherwise, we have to note its qualified name in the environment
        (env-define env name (env-resolve env name) #f)
        (set! name (env-resolve env name))))

    ;; (print name)
    ;; (env-define env name qualified-name)
    
    (define result (list-source x (car x) name (expand value env)))
    ;(print result)
    result))

(define expand
  (lambda (x env) 
    (if (self-evaluating? x)
        x
        (if (identifier? x)
          (if (env-syntax? env x)
              (raise 'expand (print-string "used syntax" x "as value") x)
              ;; TODO... could this annotate with module info?
              (begin
                ;(print "expander encountered variable" x)
                (env-resolve env x)
                #;x))
          (begin
            (define kar (car x))
            (define len (length x))
            (define syntax? (and (or (symbol? kar) (rename? kar)) (env-syntax? env kar)))
            ;; Extract renamed syntax
            (if (and syntax? (rename? kar))
              (begin
                (set! kar (rename-expr kar))))
            (if syntax?
              (cond 
                ;; TODO this should strip renames I think
                ((eq? kar 'quote)
;                 x)
;                 (print x)
;                 (print (list-source x (make-rename #f 'quote) (cadr x)))
                 (list-source x (make-rename #f 'quote) (cadr x)))
                ((eq? kar 'or)
                 (cons-source x (car x) (expand-map (cdr x) env)))
                ((eq? kar 'and)
                 (cons-source x (car x) (expand-map (cdr x) env)))
                ((eq? kar 'if)
                 (define if-length (length x))

                 (if (not (or (fx= if-length 3) (fx= if-length 4)))
                     (raise 'expand "if must have two or three arguments (condition, then, else)" x))

                 (define condition (list-ref x 1))
                 (define then (list-ref x 2))
                 (define else-clause unspecified)

                 (if (fx= if-length 4)
                     (set! else-clause (list-ref x 3)))

                 (list-source x (car x) (expand condition env) (expand then env) (expand else-clause env)))
                ;; COND
                ((eq? kar 'cond)
                 ;; (if clause-condition clause-body else rest-of-clauses...right?
                 x)
                ;; LAMBDA
                ((eq? kar 'lambda)
                 (if (fx< (length x) 3)
                     (raise 'expand "lambda has no body" x))

                 ;; (lambda (a b c) #t)
                 (define new-env (env-make env))
                 (define bindings (cadr x))

                 (if (or (null? bindings) (pair? bindings))
                   (for-each. (lambda (arg)
                      (if (not (identifier? arg))
                        (raise 'expand "non-identifier in lambda argument list" (list x arg)))
                      ;; If a rename is encountered here, env-define will add a gensym
                      ;; then env-resolve will search for that gensym with env-lookup
                      ;; then bindings are map'd to remove renames
                      (if (rename? arg)
                        (rename-gensym! arg))
                      (env-define new-env arg 'variable)) bindings)
                   (if (not (identifier? bindings))
                        (raise 'expand "non-identifier as lambda rest argument" (list x bindings))))

                 (set! bindings (map (lambda (x) (if (rename? x) (rename-gensym x) x)) bindings))

                 (cons-source x (car x) (cons-source x bindings (expand-map (cddr x) new-env)))
                 )
                ;; LET-SYNTAX
                ((eq? kar 'let-syntax) (expand-let-syntax x env))
                ((eq? kar 'letrec-syntax) (expand-let-syntax x env))
                ;; DEFINE-SYNTAX
                ((eq? kar 'define-syntax)
                 (begin
                   (define name (cadr x))
                   (define body (caddr x))

                   (expand-define-syntax x env name body)

                   ))
                ((eq? kar 'begin)
                 (cons-source x (car x) (expand-map (cdr x) env)))
                ;; DEFINE
                ((eq? kar 'set!)
                 (if (not (fx= (length x) 3))
                   (raise 'expand "set! must be a list with exactly three elements" (list x)))
                 (if (not (identifier? (list-ref x 1)))
                   (raise 'expand "non-identifier as set! first argument" (list x (list-ref x 1))))

                 (list-source x (car x) (expand (list-ref x 1) env) (expand (list-ref x 2) env))
                 )
                ((eq? kar 'define) (expand-define x env))
                (else
                  ;; This is a macro application and not a builtin syntax call
                  (define lookup (env-lookup env kar))
                  (if (macro? lookup)
                    (begin
                      (define result
                        (lookup x
                          ;; renaming procedure
                          (lambda (name) 
                            (make-rename (function-env lookup) name))
                          ;; comparison procedure
                          (lambda (a b)
                            (env-compare env a b))))

                      ;(print "expanding" result)
                      (set! result (expand result env))
                      
                      result
                      )
                      ;; not a macro application, elements must be expanded
                      (expand-map x env))))
                  ;; else: handle something like ((lambda () #t))
                  (begin 
                    (define result
                      (cons-source x (expand (car x) env) (map (lambda (sub-x) (expand sub-x env)) (cdr x)))
                      )
                    result
                  ))
              )) ;; (if (symbol? x))
            ))) ;; end expand

(install-expander expand)

; (set-print-expansions! #t)

#|
(define-syntax x
  (lambda (x r c)
    (list 
      (list 'lambda (list (r 'hello)) 
            (list (list 'lambda (list (r 'hello-second)) (r 'hello-second)) (r 'hello))
            ) ''success)))

(print (x))
|#

;;;;; BASIC SYNTACTIC FORMS
;; eg let & friends, quasiquote, when/unless

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
             (if (not (list? binding))
                 (raise 'expand "let binding should be a list with a name and a value" x))
             (define name (car binding))
             (if (not (fx= (length binding) 2))
                 (raise 'expand "let binding should have only 2 elements (name and value)" x))
             (if (not (identifier? name))
                 (raise 'expand "let binding name should be a symbol" x))
             name)
           bindings))

    (set! vals 
      (map (lambda (binding)
             ;; TODO expand.
             (cadr binding)
             ) bindings))

    (define result 
       (cons-source x (r 'lambda)
         (cons-source x names body)))

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

;;;;; QUASIQUOTE

(define (concat-list x y)
  (if (pair? x)
      (cons (car x) (concat-list (cdr x) y))
      y))

(define (qq-list rename c lst)
  (if (pair? lst)
    (let ((obj (car lst)))
      (if (and (pair? obj) (c #'unquote-splicing (car obj)))
        (if (cdr lst)
          (list #'concat-list (cadr obj) (qq-list rename c (cdr lst)))
          (cadr obj))
        (list #'cons (qq-object rename c obj) (qq-list rename c (cdr lst)))))
    (list #'quote lst)))

(define (qq-element rename c lst)
  (if (c #'unquote (car lst))
      (cadr lst)
      (qq-list rename c lst)))
         
(define (qq-object rename c object)
  (if (pair? object)
      (qq-element rename c object)
      (list #'quote object)))

(define-syntax quasiquote
  (lambda (x rename c)
    (qq-object rename c (cadr x))
    ))


;; er-macro-transformer. Not necessary in Arete, but here for compatibility with other Schemes which use this prefix 
;; for explicit renaming macros.
(define-syntax er-macro-transformer
  (lambda (x r c)
    (if (not (eq? (length x) 2))
        (raise 'syntax "er-macro-transformer expects one three-argument lambda as its argument" x))
    (cadr x)))

(define-syntax when
  (lambda (x rename c)
    (if (fx< (length x) 3)
      (raise 'syntax "when expects a condition and a body" x))

    (define result 
      `(#,if ,(list-ref x 1)
         (#,begin  ,@(cddr x))))

    result))

(define-syntax unless
  (lambda (x rename c)
    (if (fx< (length x) 3)
      (raise 'syntax "unless expects a condition and a body" x))

    `(#,if (#,not ,(list-ref x 1))
      (#,begin ,@(cddr x)))))

;; case
(define-syntax case
  (lambda (x rename c)
    (if (fx< (length x) 3)
      (raise 'syntax "case expects at least three arguments (a key and a clause)" x))

    (define key (cadr x))
    (define clauses (cddr x))

    (if (not (list? clauses))
      (raise 'syntax "case expects a list of clauses" x))

    (define code (let loop ((clause (car clauses))
               (clauses clauses))
      (unless (pair? clause)
        (raise 'syntax "case expects clause to be a datum" x))

      ;; TODO: Most Schemes seem to support a direct comparison as well e.g.
      ;; (case 5 (5 #t))
      (unless (or (c (car clause) #'else) (list? (car clause)))
        (raise 'syntax "case expects a list or else as its datum" x))

      (define condition
        (if (c (car clause) #'else)
          #t 
          `(#,memv #,result (#,quote ,(car clause)))))
      
      (if (null? (cdr clauses))
        `(#,if ,condition
          (#,begin ,(cadr clause))
          unspecified)
        `(#,if ,condition
          (#,begin ,(cadr clause))
          ,(loop (cadr clauses) (cdr clauses))))
      ))

    `(#,let ((#,result ,key))
      ,code)))

;; let*, letrec
;; define* and type stuff
;; syntax-rules (?)
;; structures/records
;; types, type assertions, type dispatching, methods
;; constant definitions
;; modules
;; compiler (?)

