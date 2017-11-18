;; expand.scm - expander rewrite

;; syntax.scm - expander, basic syntax (eg let, let*, structs)

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

;; TODO Cannot use a macro defined in advance. Toplevel needs to behave like letrec-syntax, I think.

;; TODO: Problem
;; Something like this
;; (let loop ()
;;   (error))

;; Does not result in super-descriptive error messages because the name is gensym'd by the compiler
;; How can we propagate information about where a lambda was introduced through the expander to the compiler?

;; TODO:
;;  fix else
;;  module syntax and functionality.

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define not (lambda (x) (eq? x #f)))

(define fixnum? (lambda (v) (eq? (value-type v) 1)))
(define flonum? (lambda (v) (eq? (value-type v) 4)))
(define constant? (lambda (v) (eq? (value-type v) 2)))
(define boolean? (lambda (v) (or (eq? v #t) (eq? v #f))))
(define char? (lambda (v) (eq? (value-type v) 6)))
(define pair? (lambda (v) (eq? (value-type v) 11)))
(define table? (lambda (v) (eq? (value-type v) 15)))
(define string? (lambda (v) (eq? (value-type v) 5)))
(define symbol? (lambda (v) (eq? (value-type v) 8)))
(define vector? (lambda (v) (eq? (value-type v) 9)))
(define rename? (lambda (v) (eq? (value-type v) 16)))
(define identifier? (lambda (v) (or (rename? v) (symbol? v))))
(define (function? v) (eq? (value-type v) 13))

(define list (lambda lst lst))

;; Constants
(define null? (lambda (v) (eq? (value-bits v) 10)))
(define eof-object? (lambda (v) (eq? (value-bits v) 18)))

(define input-port? (lambda (v) (and (eq? (value-type v) 23) (value-header-bit? v 9))))
(define output-port? (lambda (v) (and (eq? (value-type v) 23) (value-header-bit? v 10))))

(define current-input-port (lambda () *current-input-port*))
(define current-output-port (lambda () *current-output-port*))

(define self-evaluating?
  (lambda (v)
    (or (char? v) (fixnum? v) (constant? v) (string? v) (vector? v) (flonum? v) (table? v))))

(define unspecified (if #f #f))

(define list-tail
  (lambda (lst i)
    (if (or (null? lst) (not (pair? lst)))
      (raise 'type "list-tail expects a list with at least one element as its argument" (list lst)))

    (if (= i 0)
      lst
      ((lambda (loop)
        (loop loop (cdr lst) 1))
       (lambda (loop rest ii)
         (if (null? rest)
           (raise 'type "list-tail bounds error" (list lst (length lst) i)))
         (if (= ii i)
           rest
           (loop loop (cdr rest) (+ ii 1))))))))

;;;;; EXPAND! Expansion pass

;; NOTE: If any function here uses COND, it has to be added to a whitelist in compiler.scm to be re-expanded, as 
;; the compiler does not support COND directly but relies on the expander's if-based implementation of it.

;; TODO: Somewhat confusingly, we store transformers directly in modules but check for syntax at the toplevel and 
;; in all other cases, the "value" of a name in the module is a symbol that it's supposed to resolve to.
;; This isn't super confusing at this point but it is a little odd.

;; Rename convenience function that uses the dynamically scoped *current-rename-env* variable to make renames
(define (rename name)
  (define env (top-level-value '*current-rename-env*))

  (if (not (symbol? name))
    (raise 'expand "attempt to rename non-symbol" (list name)))

  (if (eq? (top-level-value '*current-rename-env*) unspecified)
    (raise 'expand "(rename) called without environment (did you forget to unquote a rename?)" (list name)))
  
  (make-rename env name))

;; Environment machinery

(define (env-make parent)
  (define vec (make-vector))
  (vector-append! vec parent)
  vec)

(define (env-define env name value . toplevel?)
  (cond
    ((and (eq? env #f) (not (null? toplevel?))) (set-top-level-value! name value))
    ((eq? env #f) unspecified)
    ((table? env)
     (begin
       (table-set! env name (if (eq? value 'variable) (module-qualify env name) value))))
    ((vector? env) (begin
                     (vector-append! env name)
                     (vector-append! env value)))
    (else (begin
            (raise 'expand "env-define failed" (list name value))))))

(define (identifier=? a b)
  (or (eq? a b)
      (and (rename? a) (rename? b) (eq? (rename-env a) (rename-env b)) (eq? (rename-expr a) (rename-expr b)))))

(define (env-vec-lookup env len i id)
  (if (fx= i len)
    #f
    (if (identifier=? (vector-ref env i) id)
      (list env (vector-ref env i) (vector-ref env (fx+ i 1)) #t)
      (env-vec-lookup env len (+ i 2) id))))

;; Env lookup

;; This has to be done like this, because we cannot store #<syntax> or #<undefined> in named variables.
(define (env-check-syntax name)
  (if (fx= (value-bits (top-level-value name)) 34) 'syntax (top-level-value name)))

;; Add a module's name to a symbol
(define (module-qualify mod name)
  (string->symbol (string-append "##" (table-ref mod "module-name") "#" (symbol->string name))))

(define (rename-strip id)
  (if (rename? id) (rename-expr id) id))

;; Returns environment where key was resolved, the key in the environment considered equivalent to the given name
;; (necessary for rename gensyms) the value of the key in the environment, and whether the variable was "found"
;; (variables may be referenced before definition)
(define (env-lookup env name)
  (cond
    ;; toplevel
    ((eq? env #f)
     (begin
       (list #f (rename-strip name) (env-check-syntax (rename-strip name)) #t)))
       #;(if (rename? name)
         ;; strip renames at toplevel
         (list #f (rename-expr name) (env-check-syntax (rename-expr name)))
         (list #f name (env-check-syntax name)));))
    ((table? env)
     (begin
       (define strip (rename-strip name))
       (if (table-ref env strip)
         ;; this variable is defined, can either be a transformer which is 
         ;; returned directly for the use of the expander,
         ;; or is a reference to a variable
         (if (procedure? (table-ref env strip))
           (list env strip (table-ref env strip) #t)
           (list env (table-ref env strip) 'variable #t))
         ;; problem

         ;; if a variable is not defined, sometimes we want to treat it basically as a toplevel value.
         ;; for example, (env-compare #f 'else (make-rename <some table> 'else)) => #t
         ;; we also want to check for built-in syntax
         ;; but for most code we want to return a qualified name so that something in a module can reference
         ;; something defined later on in a module.
         (if (eq? (env-check-syntax strip) 'syntax)
           (list #f strip 'syntax #t)
           (list env (module-qualify env strip) unspecified #f)))))
    ;; local environment
    ((vector? env)
     (begin
       (define len (vector-length env))
       (define res (env-vec-lookup env len 1 name))
       (if res 
         res
         (env-lookup (vector-ref env 0) name))))))

;; Compare two identifiers to see if they "mean" the same thing
;; For example
;; 'else and (make-rename #f 'else) => #t
;; 'else and (make-rename <environment where else is defined by the user> 'else) => #f
(define (env-compare env a b)
  (cond
    ((eq? a b) #t)
    ((and (symbol? a) (rename? b)) (env-compare env b a))
    ((and (rename? a) (symbol? b))

     ;; here's the problem
     ;; unquote resolves to ##arete#core#unquote
     ;; #'unquote resolves to unquote.
     (and (eq? (rename-expr a) b)
      (apply 
         (lambda (renv rkey rvalue rfound)
           (apply
             (lambda (env key value found)
               ;; env-lookup will still return a table and qualified name
               ;; in the case that something cannot be found
               (define senv (if (and (not found) (table? env)) #f env))
               (define rsenv (if (and (not rfound) (table? renv)) #f renv))
               (eq? rsenv senv))

             (env-lookup env b)))
         (env-lookup (rename-env a) (rename-expr a)))))

    (else #f)))

;; Name resolution

;; Local renames become gensyms, e.g.
;; (lambda (#R:name) #R:name) => (lambda (#:name0) #:name0)

;; Global renames are stripped

;; Symbols in modules become qualified, e.g. (define x #t) in module (arete) becomes 
;; (define ##arete#x #t)

(define (env-resolve env name)
  (apply
    (lambda (env key value found)
      (cond
        ((and (rename? key) (rename-gensym key)) (rename-gensym key))
        ((table? env) key)
        (else name)))
    (env-lookup env name)))

(define (env-syntax? env name)
  (apply 
    (lambda (env name value found)
      (or (eq? value 'syntax) (function-macro? value)))
    (env-lookup env name)))

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
      (raise-source x 'expand "define is missing first argument (name)" (list x)))

    (if (fx< len 3)
      (raise-source x 'expand "define is missing second argument (value)" (list x)))

    (set! kar (cadr x))

    (if (not (or (identifier? kar) (pair? kar)))
      (raise-source (cdr x) 'expand "define first argument must be a name or a list" (list x)))

    (if (identifier? kar)
      (if (not (fx= len 3))
        (raise-source x 'expand "define expects exactly two arguments (name and value)" (list x))
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
      (raise-source x 'expand (print-string "definition of" name "shadows syntax") (list x)))

    (if (rename? name)
      (rename-gensym! name))

    ;; Handle module stuff
    (if (and (table? env) (table-ref env "module-export-all"))
      (table-set! (table-ref env "module-exports" name #t)))

    (env-define env name 'variable)

    (set! result (list-source x (make-rename #f 'define)
                              ;; We'll replace name with the actual gensym here if there is one
                              (env-resolve env name)#;(list-ref (env-lookup env name) 1)
                              #;(if (and (rename? name) (rename-gensym name)) (rename-gensym name) name)
                              (expand value env)))

    result))

;; Expand an identifier
(define expand-identifier
  (lambda (x env)
    (if (env-syntax? env x)
      (raise-source x 'expand (print-string "used syntax" x "as value") (list x)))
    (env-resolve env x)))

;; Expand and/or
(define expand-and-or
  (lambda (x env)
    (cons-source x (car x) (expand-map (cdr x) env))))

;;;;; Module machinery
(define (module-spec->string src lst . str)
  (if (null? lst)
    (car str)
    (if (or (not (pair? lst)) (not (symbol? (car lst))))
      (raise-source src 'expand "module specification must be a proper list of symbols" (list lst))
      (module-spec->string
        src
        (cdr lst)
        (if (not (null? str)) (string-append (car str) "#" (symbol->string (car lst))) (symbol->string (car lst)))
        ))))


(define (module-make name)
  (define mod (make-table))

  (table-set! mod "module-stage" 1)
  (table-set! mod "module-name" name)
  (table-set! mod "module-exports" (make-table))

  mod)

;; Check a list of specifiers
(define (module-check-spec src name syms)
  (for-each
    (lambda (x)
      (if (not (symbol? x))
        (raise-source src 'expand (print-string "arguments to import specifier" name " must all be symbols") (list syms))))
    syms))

;; How this works.
;; Basically, when it encounters the actual module specifier like say (sdl)
;; It will tell the runtime to load the module
;; Then it converts its exported bindings into a list of pairs correlating names with values

;; Then the modifiers only, prefix, rename and except are applied to this list with map & filter
(define (assq obj alist)
  (if (null? alist)
    #f
    (if (eq? (caar alist) obj)
      (car alist)
      (assq obj (cdr alist)))))

(define (module-import-eval spec)
  (if (not (list? spec))
    (raise-source spec 'expand "module specifier must be a list" (list spec)))

  (define kar (car spec))

  (cond
    ((eq? kar 'prefix)
     (begin
       (if (not (symbol? (caddr spec)))
         (raise-source (cddr spec) 'expand "module prefix must be a symbol" (list spec)))
       (if (not (fx= (length spec) 3))
         (raise-source (cdddr spec) 'expand "prefix specifier must have exactly three elements" (list spec)))
       (define pfx-str (symbol->string (caddr spec)))
       (map
         (lambda (cell)
           (if (pair? cell)
             (cons (string->symbol (string-append pfx-str (symbol->string (car cell)))) (cdr cell))
             #f))
         (module-import-eval (cadr spec))))
    )
    ((eq? kar 'only)
     (begin
       (define allowed (cddr spec))
       (module-check-spec spec 'only allowed)
       (filter
         (lambda (cell)
           (and cell (memq (car cell) allowed)))
         (module-import-eval (cadr spec)))
     )
    )
    ((eq? kar 'except)
     (begin
       (define forbidden (cddr spec))
       (module-check-spec spec 'except forbidden)
       (filter
         (lambda (cell) (and cell (not (memq (car cell) forbidden))))
         (module-import-eval (cadr spec)))))

    ((eq? kar 'rename)
     (begin
       (define renames (cddr spec))
       (for-each
         (lambda (x)
           (if (or (not (list? x)) (not (fx= (length x) 2)) (not (symbol? (car x))) (not (symbol? (cadr x))))
             (raise-source spec 'expand (print-string "arguments to rename specifier must all be lists with two symbols") (list spec))))
         renames)
       (map
         (lambda (cell)
           (if cell
             (begin
               (define check (assq (car cell) renames))
               (if check
                 (cons (cadr check) (cdr cell))
                 cell))
             cell))
         (module-import-eval (cadr spec)))))

    (else
      ;; This is an actual module, load it if necessary and import it.
      (begin
        (define module-name (module-spec->string spec spec))
        (define module (table-ref module-table module-name))
        (table-map
          (lambda (k v)
            (if (symbol? k)
              (cons k v)
              #f))
          module)))))

(define (module-import! mod1 mod2-spec)
  (define mod2-name (module-spec->string mod2-spec mod2-spec))
  (define mod (table-ref module-table mod2-name))

  (print "importing" mod2-name "to" (table-ref mod1 "module-name"))

  #t)

(define (expand-import mod spec)
  (define imports (filter (lambda (v) v) (module-import-eval spec)))

  (print imports)
  #t)

(define (expand-module-decl mod x env)
  (define kar (car x))

  (cond
    ((eq? kar 'import)

     (for-each
       (lambda (x)
         (expand-import mod x))
       (cdr x)))
  ) ;cond
)

(define (expand-module x env)
  (define len (length x))
  (if (not (fx> len 1))
    (raise-source x 'expand "module requires at least one argument (module name specifier)" (list x)))

  ;; module fart...
  (define name (module-spec->string (cdr x) (cadr x)))
  (define module (make-table))

  (if (table-ref module-table name)
    (begin
      (print "raising exception")
      (raise-source x 'expand (print-string "module" (cadr x) "encountered twice") (list x))))

  (table-set! module-table name module)
  (table-set! module "module-name" name)
  (table-set! module "module-stage" 1)

  (set-top-level-value! '*current-module* module)

  (for-each
    (lambda (x)
      (expand-module-decl module x env))
    (cddr x))

  unspecified
)

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
        ((eq? kar 'module) (expand-module x env))
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
        (else (begin 
                (expand-macro x env))))
      ;; Normal function application
      ;; Needs to be annotated with src info, right?
      ;; map-source?
      (expand-map x env))))

(define expand-lambda
  (lambda (x env)
    (if (fx< (length x) 3)
      (raise-source x 'expand "lambda has no body" (list x)))

    (define bindings (cadr x))
    (define new-env (env-make env))

    (if (or (null? bindings) (pair? bindings))
      (for-each-improper-i
        (lambda (i arg)
          (if (not (identifier? arg))
            (raise-source (list-tail bindings i) 'expand "non-identifier in lambda argument list" (list x)))
          ;; If a rename is encountered here, env-define will add a gensym
          ;; then env-resolve will search for that gensym with env-lookup
          ;; then bindings are map'd to remove renames
          (if (rename? arg)
            (rename-gensym! arg))

          (env-define new-env arg 'variable))
        bindings)
      ;; Argument is a single symbol or rename (e.g. rest arguments only)
      (if (not (identifier? bindings))
        (raise-source (cdr x) 'expand "non-identifier as lambda rest argument" (list x))
        (env-define new-env bindings 'variable)))

    ;; If bindings are renames, they should be gensym'd 
    (set! bindings
      (if (identifier? bindings)
        (if (and (rename? bindings) (rename-gensym bindings)) (rename-gensym bindings) bindings)
        (map-improper (lambda (x) (if (and (rename? x) (rename-gensym x)) (rename-gensym x) x)) bindings)))

    (cons-source x (make-rename #f 'lambda) (cons-source x bindings (expand-map (cddr x) new-env)))))

(define expand-if
  (lambda (x env)
    (define len (length x))
    (define else-branch unspecified)

    (if (fx< len 3)
      (raise-source x 'expand "if expression needs at least two arguments" (list x))
      (if (fx= len 4)
        (set! else-branch (list-ref x 3))))

    (list-source x (car x) (expand (list-ref x 1) env) (expand (list-ref x 2) env) (expand else-branch env))))

(define expand-set
  (lambda (x env)
    (define len (length x))
    (define name #f)

    (if (not (fx= len 3))
      (raise-source x 'expand "set! expression needs exactly two arguments" (list x)))

    (set! name (cadr x))

    (if (not (identifier? name))
      (raise-source (cdr x) 'expand "set! expects an identifier as its first arguments" (list x)))

    (list-source x (car x) (expand (list-ref x 1) env) (expand (list-ref x 2) env))))

(define expand
  (lambda (x env)
    (cond
      ((self-evaluating? x) x)
      ((identifier? x) (expand-identifier x env))
      (else (begin
              (expand-apply x env))))))

(define expand-toplevel
  (lambda (x env)
    (expand x env)))

(define expand-define-transformer!
  (lambda (x env name body)
    (define expanded-body #f)
    (define fn #f)
    (define fn-arity #f)

    (if (not (symbol? name))
      (raise-source (cdr x) 'expand "define-syntax first argument (macro name) must be a symbol" (list x (cdr x))))

    (if (table? env)
      (if (table-ref env "module-export-all")
        (table-set! (table-ref env "module-exports") name #t)))

    (set! expanded-body (expand body env))
    (set! fn (eval expanded-body #f))

    (if (not (procedure? fn))
      (raise-source (cddr x) 'expand "define-syntax body did not evaluate to a function" (list x)))

    (set! fn-arity (function-min-arity fn))

    (if (fx< fn-arity 1)
      (raise-source (caddr x) 'expand "define-syntax body must evaluate to a function that takes one argument" (list x)))

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
      (raise-source x 'expand "define-syntax expects at least two arguments: a name and a body" (list x)))

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
      (raise-source x 'expand "let-syntax expands at least three arguments" (list x)))

    (set! bindings (cadr x))
    (set! body (cddr x))

    (if (not (list? bindings))
      (raise-source (cadr x) 'expand "let-syntax bindings list must be a valid list" (list x (cadr x))))

    (for-each
      (lambda (x)
        (define name #f)
        (define body #f)

        (if (not (fx= (length x) 2))
          (raise-source x 'expand "let-syntax bindings should have two values: a name and a transform" (list x)))

        (set! name (car x))
        (set! body (cadr x))

        (expand-define-transformer! x new-env name body))
      bindings)

    (cons-source x (make-rename #f 'begin) (expand body new-env))))

;; Expand a macro appplication
(define expand-macro 
  (lambda (x env)
    (define lookup (list-ref (env-lookup env (car x)) 2))
    (define arity (function-min-arity lookup))
    (define saved-rename-env (top-level-value '*current-rename-env*))

    (set-top-level-value! '*current-rename-env* (function-env lookup))

    (define result
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

    (expand result env)))

;; cond expander
(define expand-cond-full-clause
  (lambda (x env clause rest)
    (define len #f)
    (define condition #f)
    (define body #f)

    (if (not (list? clause))
      (raise-source clause 'expand "cond clause should be a list" (list clause)))

    (set! len (length clause))

    (if (not (> len 1))
      (raise-source clause 'expand "cond clause should have two members: condition and body" (list x clause)))

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
(define module-table (make-table))

(define *core-module* (module-make "arete"))

(table-set! module-table "arete" *core-module*)

(define test (module-make "test"))

(table-set! module-table "test" test)

(table-set! test 'car 'car)
(table-set! test 'cdr 'cdr)

(define *current-module* *core-module*)

;; populate core module
(top-level-for-each
  (lambda (k v)
    (if (or (procedure? v) (memq k '(unspecified)))
      ((lambda (name)
         (set-top-level-value! name v)
         (table-set! (table-ref *core-module* "module-exports") k #t)
         (table-set! *core-module* k k))
       (module-qualify *core-module* k)))))

(set-top-level-value! 'expander expand-toplevel)

;; Basic module support

;; (module (module1))
;; (define hello-world)

#;(module (module1)
        (import (test)))


