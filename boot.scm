;; boot.scm - Expansion pass, various key functions

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define not (lambda (x) (eq? x #f)))

(define unspecified (if #f #f))

;;;;; EXPANSION

;; Shortcut for applying expand with an environment, since this is used pretty often in the code.
(define expand-map
  (lambda (x env)
    (map (lambda (sub-x) (expand sub-x env)) x)))

(define expand-define-syntax
  (lambda (x env name body)
    (if (not (symbol? name))
        (raise 'expand "macro name must be a symbol" x))

    (if (table? env)
      (if (table-ref env "module-export-all")
        (table-set! (table-ref env "module-exports") name #t)))

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

    (if (table? env)
      (begin
        ;; If this is a module, we have to note its qualified name in the environment
        ;; If this is a module, we have to note its qualified name in the environment
        (if (rename? name)
          ;; TODO: Should these renames be annotated with the module name?
          (begin
            (rename-gensym! name)
            (vector-append! (table-ref env "module-renames") name))
          (begin
            (begin 
              (if (table-ref env "module-export-all")
                (table-set! (table-ref env "module-exports") name #t)))
            ;; If this is on the list of exports
            ;; Note down the qualified name of this variable
            ;(table-set! env name (string->symbol (string-append "##" (table-ref env "module-name") "#" (symbol->string name))))
            ))
        (env-define env name (env-resolve env name) #f)

        (set! name (env-resolve env name)))
      ;; If this isn't a module, we have to note it as a 'variable so env-resolve will treat it as a local variable
      (env-define env name 'variable))

    (define result (list-source x (car x) name (expand value env)))
    ;(print result)
    result))

;; import a module
(define expand-module-import!
  (lambda (module spec)

    (define imported-module-name #f)
    (define module-import-rule 'unqualified)
    (define rule-arguments '())
    (define import-module #f)

    (if (or (not (list? spec)) (not (pair? spec)))
      (raise 'expand "imports must be a list of symbols" i))

    (if (memq (car spec) '(only rename prefix except))
      (begin
        (if (not (list? (cadr spec)))
          (raise 'expand "bad module spec" spec))
        (set! imported-module-name (list-join (cadr spec) "#"))
        (set! module-import-rule (car spec)))
      (set! imported-module-name (list-join spec "#")))

    ;; If module is not loaded at all, load it.
    (if (not (table-ref (top-level-value 'module-table) imported-module-name))
      (module-load! imported-module-name))

    ;; Retrieve the module
    (set! import-module (table-ref (top-level-value 'module-table) imported-module-name))

    ;; If it's in the process of being expanded, this is a cyclic import error
    (if (fx= (table-ref import-module "module-stage") 1)
      (raise 'expand "cyclic import" spec))

    ;; Now append this rule to the list of imports
    ;;(define module-imports (table-ref module "module-imports"))

    (cond
      ((eq? module-import-rule 'prefix)
       (define pfx (list-ref spec 2))
       (if (not (symbol? pfx))
         (raise 'expand "bad prefix" pfx))
       (set! rule-arguments pfx)))

    (module-import! module import-module module-import-rule rule-arguments)

    unspecified))

    

#|
    (define vec #f)
    (if (not (eq? module-import-rule 'all))
      (begin
        (set! vec (make-vector))
        (vector-append! vec import-module)
        (vector-append! vec module-import-rule)))

    (cond
      ((eq? module-import-rule 'all)
       (vector-append! module-imports import-module))
      ((eq? module-import-rule 'prefix)
       (vector-append! module-imports (list-ref spec 2)))
      ((or (eq? module-import-rule 'only) (eq? module-import-rule 'except))
       (for-each
         (lambda (item)
           (if (not (symbol? item))
             (raise 'expand "import only/except arguments must be symbols" item))

           (vector-append! module-imports item))
         (cddr spec))))

    ;; If following a rule, we'll build a vector describing that rule
    (print import-module)

    ;; TODO Duplicate imports.
    (vector-append! module-imports import-module)))
|#

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
            ;(print (and (identifier? kar) (env-resolve env kar)))
            (define syntax?
              (and (identifier? kar)
                   (env-syntax? env kar)))
            ;; Extract renamed syntax
            (if (and syntax? (rename? kar))
              (begin
                (set! kar (rename-expr kar))))
            (if syntax?
              (cond 
                ;; TODO this should strip renames I think
                ((eq? kar 'quote)
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
                ((eq? kar 'define-library)
                 (if (or (fx< (length x) 2) (not (list (cadr x))))
                   (raise 'expand "define-library expects a list as its first argument" x))

                 (define name-string (list-join (cadr x) "#"))
                 (define existing-module?
                   (table-ref (top-level-value 'module-table) name-string))

                 (if existing-module?
                   (raise 'expand (string-append "redefinition of module " name-string) x))

                 (define module (module-instantiate! name-string))
                 (define module-imports (table-ref module "module-imports"))
                 (define clauses (cddr x))

                 (table-set! module "module-stage" 1)

                 (if (not (list? clauses))
                   (raise 'expand "module declaration must be a valid list" x))

                 (define module-body 
                   (map
                     (lambda (x)
                       (if (not (list? x))
                         (raise 'expand "module clause must be a valid list" x))

                       (define type (car x))

                       (cond 
                         ((eq? type 'import)
                         (for-each
                           (lambda (i)
                             (expand-module-import! module i))
                           (cdr x)))
                         ((eq? type 'export-all)
                          (table-set! module "module-export-all" #t))
                         ((eq? type 'export)
                          ;(if (table-ref module "module-exports")
                          ;  (raise 'expand "multiple module export statements" x))
                          (if (table-ref module "module-export-all")
                            (raise 'expand "export-all and exports cannot coexist" x))
                          (if (not (table-ref module "module-expots"))
                            (table-set! module "module-exports" (make-table)))

                          ;(table-set! module "module-exports" (make-table))
                           (for-each
                             (lambda (i)
                               (if (not (symbol? i))
                                 (raise 'expand "exports must be symbols" i))

                               (table-set! (table-ref module "module-exports") i #t))

                             (cdr x)))
                         ((eq? type 'begin)
                           (cons (make-rename #f 'begin) (expand-map (cdr x) module))))
                       )
                     clauses))

                 ;(print module-body)

                 ;(set! module-body (cons (make-rename #f 'begin) module-body))

                 ;; Body is expanded, this is now a stage 2 module
                 (table-set! module "module-stage" 2)

                 (cons (make-rename #f 'begin) module-body))
                ((eq? kar 'import)
                 (for-each
                   (lambda (i)
                     (expand-module-import! (top-level-value 'current-module) i))
                   (cdr x)))
               ;; LAMBDA
                ((eq? kar 'lambda)
                 (if (fx< (length x) 3)
                     (raise 'expand "lambda has no body" x))

                 ;; (lambda (a b c) #t)
                 (define new-env (env-make env))
                 (define bindings (cadr x))

                 (if (or (null? bindings) (pair? bindings))
                   (for-each-improper (lambda (arg)
                      (if (not (identifier? arg))
                        (raise 'expand "non-identifier in lambda argument list" (list x arg)))
                      ;; If a rename is encountered here, env-define will add a gensym
                      ;; then env-resolve will search for that gensym with env-lookup
                      ;; then bindings are map'd to remove renames
                      (if (rename? arg)
                        (rename-gensym! arg))
                      (env-define new-env arg 'variable)) bindings)
                   (if (not (identifier? bindings))
                        (raise 'expand "non-identifier as lambda rest argument" (list x bindings))
                        (env-define new-env bindings 'variable)))

                 (if (identifier? bindings)
                   (if (and (rename? bindings) (rename-gensym bindings)) (rename-gensym bindings) x)
                   (set! bindings (map-improper (lambda (x) (if (and (rename? x) (rename-gensym x)) (rename-gensym x) x)) bindings)))

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
                      ;; TODO: If a C_SYNTAX value somehow falls through to here, it'll cause an interpreter error
                      ;; A more descriptive error message is possible but this also shouldn't happen if the expander
                      ;; is properly coded
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

(set-top-level-value! 'expander expand)

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
;; TODO =>
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
        (if (eq? (car clause) 'else)
          (raise 'syntax "case expected an else clause, but it seems else has been redefined" x)
          (raise 'syntax "case expected a list or else clause as its datum" x)))

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

(define-syntax er-macro-transformer
  (lambda (x r c)
    (cadr x)))

