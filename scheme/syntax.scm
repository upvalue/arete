;; syntax - expander, basic syntax (eg let, let*, structs)

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

;; TODO Cannot use a macro defined in advance. Toplevel needs to behave like letrec-syntax, I think.

;; TODO: Problem
;; Something like this
;; (let loop ()
;;   (error))

;; Does not result in super-descriptive error messages because the name is gensym'd by the compiler
;; How can we propagate information about where a lambda was introduced through the expander to the compiler?

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

(define rename
  (lambda (name)
    (define env (top-level-value '*current-rename-env*))

    (if (not (symbol? name))
      (raise 'expand "attempt to rename non-symbol" (list name)))

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
      (raise-source x 'expand "define is missing first argument (name)" (list x)))
;      (raise 'expand "define is missing first argument (name)" (list x)))

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
    (env-define env name 'variable)
    #|
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
    |#

    (set! result (list-source x (car x)
                              ;; We'll replace name with the actual gensym here if there is one
                              (if (and (rename? name) (rename-gensym name)) (rename-gensym name) name)
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
        #;((eq? kar 'cond-expand) (expand-cond-expand x env))
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

    ;(set! body (caddr x))
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

    (if (< (length x) 3)
      (raise-source x 'syntax "let must have at least two arguments (bindings and body)" (list x)))

    (if (identifier? (list-ref x 1))
      (begin
        (set! let-fn-name (list-ref x 1))
        (set! bindings (list-ref x 2))
        (set! body (cdddr x)))
      (begin
        (set! bindings (list-ref x 1))
        (set! body (cddr x))))

    ;(print let-fn-name)
    ;(print "crashy crashy: " let-fn-name)

    (set! names
      (map-i (lambda (i binding)
             (define name #f)
             (if (not (list? binding))
               (raise-source (list-tail bindings i) 'syntax "let binding should be a list with a name and a value" (list x)))

             (if (not (fx= (length binding) 2))
               (raise-source binding 'syntax "let binding should have exactly 2 elements (name and value)" (list binding)))

             (set! name (car binding))

             (if (not (identifier? name))
               (raise-source binding 'syntax "let binding name should be an identifier" (list binding)))
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

(define (append x y)
  (if (pair? x)
      (cons (car x) (append (cdr x) y))
      y))

(define (qq-list c lst)
  (if (pair? lst)
    (let ((obj (car lst)))
      (if (and (pair? obj) (c #'unquote-splicing (car obj)))
        (if (cdr lst)
          (list #'append (cadr obj) (qq-list c (cdr lst)))
          (cadr obj))
        ;; TODO: This could be replaced with cons* for less calls and less confusing output
        (list #'cons (qq-object c obj) (qq-list c (cdr lst)))
      ))
    (begin
      (if (null? lst) 
        '()
        (list #'quote lst)))))

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
      (raise-source x 'syntax "when expects a condition and a body" (list x)))

    `(,#'if ,(list-ref x 1)
        (,#'begin ,@(cddr x)))))

(define-syntax unless
  (lambda (x)
    (if (fx< (length x) 3)
      (raise-source x 'syntax "unless expects a condition and a body" (list x)))

    `(,#'if (,#'not ,(list-ref x 1))
        (,#'begin ,@(cddr x)))))

;; This actually seriously slows down the compiler (which uses memv via case) vs using a C++ function.
;;Need some basic inlining for sure.

#|
(define (%member-impl cmp? obj lst)
  (let loop ((lst lst))
    (if (null? lst)
      #f
      (if (cmp? (car lst) obj)
        lst
        (loop (cdr lst))))))

(define (memq obj lst) (%member-impl eq? obj lst))
(define (memv obj lst) (%member-impl eqv? obj lst))
(define (member obj lst) (%member-impl equal? obj lst))
|#

#;(define (memv obj lst)
  (if (null? lst) 
    #f
    (if (eqv? (car lst) obj)
      lst
      (memv obj (cdr lst)))))

;; case
(define-syntax case
  (lambda (x c)
    (define key #f)
    (define clauses #f)

    (if (fx< (length x) 3)
      (raise-source x 'syntax "case expects at least three arguments (a key and a clause)" x))

    (set! key (cadr x))
    (set! clauses (cddr x))

    (if (not (list? clauses))
      (raise-source x 'syntax "case expects a list of clauses" x))

    (define code (let loop ((clause (car clauses))
               (clauses clauses))
      (unless (pair? clause)
        (raise 'syntax "case expects clause to be a datum" x))

      ;; TODO: Most Schemes seem to support a direct comparison as well e.g.
      ;; (case 5 (5 #t))
      (unless (or (symbol? (car clause)) (list? (car clause)) (self-evaluating? (car clause)))
        (raise 'syntax "case expects an else clause, a literal, or a list of literals as its datum"))

      (define condition
        (if (c (car clause) #'else)
          #t 
          (if (or (symbol? (car clause)) (self-evaluating? (car clause)))
            ;; Immediate values result in an eq? call
            `(,#'eqv? ,#'result (,#'quote ,(car clause)))            
            ;; Lists will be memv'd
            `(,#'memv ,#'result (,#'quote ,(car clause))))))

      (define branch
        (if (and (c (car clause) #'else) (c (cadr clause) #'=>))
          `(,(list-ref clause 2) ,#'result)
          `(,#'begin ,@(cdr clause))))

      (if (null? (cdr clauses))
        `(,#'if ,condition
          (,#'begin ,branch)
          unspecified)
        `(,#'if ,condition
          (,#'begin ,branch)
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
    (unless (> (length x) 2)
      (raise-source x 'syntax "let* expects at least two arguments (bindings and body)" (list x)))

    (let ((bindings (list-ref x 1))
          (body (cddr x)))
      (for-each-i
        (lambda (i binding)
          (unless (and (list? binding) (fx= (length binding) 2) (identifier? (car binding)))
            (raise-source (list-tail (cadr x) i) 'syntax "let* binding must be a list with two elements" (list binding))))
        bindings)

    `(,#'let (,@(map (lambda (b) (list-source b (car b) #'unspecified)) bindings))
      ,@(map (lambda (b) (list-source b #'set! (car b) (cadr b))) bindings)
      ,@body))))

;; TODO: letrec restrictions.
(define-syntax letrec
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "let* expects at least two arguments (bindings and body)" (list x)))

    (let ((bindings (list-ref x 1))
          (body (cddr x)))
      (for-each-i
        (lambda (i binding)
          (unless (and (list? binding) (fx= (length binding) 2) (identifier? (car binding)))
            (raise-source (list-tail (cadr x) i) 'syntax "let* binding must be a list with two elements" (list binding))))
        bindings)

    `(,#'let (,@(map (lambda (b) (list-source b (car b) #'unspecified)) bindings))
      ,@(map (lambda (b) (list-source b #'set! (car b) (cadr b))) bindings)
      ,@body))))

(define (max a b) (if (< a b) b a))
(define (min a b) (if (> a b) b a))

(define (values . rest)
  (cons (list 'values) rest))

;; Records.

(define-syntax define-record
  (lambda (x)
    (define name #f)
    (define field-count 0)
    (define parent #f)
    (define fields #f)
    (define accessors #f)
    (define name-string #f)
    (define constructor #f)
    (define predicate #f)

    (unless (fx> (length x) 1)
      (raise-source x 'syntax "define-record expects at least one argument: a record name" (list x)))

    (set! name (cadr x))

    (unless (symbol? name)
      (raise-source (cdr x) 'syntax (print-string "define-record name must be a symbol but got" name) (list x)))

    (set! name-string (symbol->string name))

    (when (fx> (length x) 2)
      (if (list? (list-ref x 2))
        (begin
          (when (null? (list-ref x 2))
            (raise-source (caddr x) 'syntax "define-record inheritance argument must be a list with exactly one symbol but got none" (list x (cddr x))))
          (when (> (length (list-ref x 2)) 1)
            (raise-source (caddr x) 'syntax "define-record inheritance argument must be a list with exactly one symbol but more than one" (list x (cddr x))))
          (set! parent (car (list-ref x 2)))
          (set! fields (cdddr x)))
        (set! fields (cddr x))))

    (unless (or (symbol? parent) (not parent))
      (raise-source (caddr x) 'syntax "define-record inheritance argument should be a list with exactly one symbol" (list x (caddr x))))

    (set! accessors
      (map-i
        (lambda (i field-name)
          (define getter #f)
          (define setter #f)

          (unless (symbol? field-name)
            (raise-source (list-tail fields i) 'syntax "define-record fields must be symbols" (list x name)))

          (set! getter (string->symbol (string-append name-string "/" (symbol->string field-name))))
          (set! getter `(,#'define (,getter ,#'rec) (,#'record-ref ,name ,#'rec ,field-count)))

          (set! setter (string->symbol (string-append name-string "/" (symbol->string field-name) "!")))
          (set! setter `(,#'define (,setter ,#'rec ,#'value) (,#'record-set! ,name ,#'rec ,field-count ,#'value)))

          (set! field-count (fx+ field-count 1))

          `(,#'begin ,getter ,setter))
      fields))

    (set! constructor
      `(,#'define (,(string->symbol (string-append name-string "/make")) ,@(map rename fields))
        (,#'let ((,#'instance (,#'make-record ,name)))
        ,@(map-i
          (lambda (i x)
            `(,#'record-set! ,name ,#'instance ,i ,(rename (list-ref fields i))))
          fields)
        ,#'instance)))

    (set! predicate
      `(,#'define ,(list (string->symbol (string-append name-string "?")) #'instance)
        (,#'record-isa? ,#'instance ,name)))

    `(,#'begin
       (,#'define ,name
        (,#'let ((,#'fields (list ,@(map (lambda (x) `(,#'quote ,x)) fields))))
          (begin
            ;(print ,#'fields)
            (,#'register-record-type ,name-string ,field-count 0 ,#'fields ,parent))))
       ,predicate
       ,constructor
       ,@accessors
      )
    
  )) ;; lambda (x)

;; PORTS

(define (call-with-input-file path thunk)
  (let* ((file (open-input-file path))
         (result (thunk file)))
    (close-input-port file)
    result))

(define (call-with-output-file path thunk)
  (let* ((file (open-output-file path))
         (result (thunk file)))
    (close-output-port file)
    file))

;; LISTS

(define (assoc-impl cmp? obj alist)
  (if (null? alist)
    #f
    (let loop ((elt (car alist))
               (rest (cdr alist)))
      (if (and (pair? elt) (cmp? (car elt) obj))
        elt
        (if (null? rest)
          #f
          (loop (car rest) (cdr rest)))))))

(define (assq obj alist) (assoc-impl eq? obj alist))
(define (assv obj alist) (assoc-impl eqv? obj alist))
(define (assoc obj alist) (assoc-impl equal? obj alist))

(define (reduce fn lst)
  (if (null? lst)
    '()
    (let ((len (length lst)))
      (let loop ((i 0)
                 (result (car lst)))
        (if (fx= i (fx- len 1))
          result
          (begin
            (loop (fx+ i 1) (fn result (list-ref lst (fx+ i 1))))))))))

(define (filter fn lst)
  (if (null? lst)
    '()
    (if (fn (car lst))
      (cons (car lst) (filter fn (cdr lst)))
      (filter fn (cdr lst)))))

;; SRFI-0

(define (cond-expand-check-feature x form)
  (unless (and (list? form) (not (null? form)))
    (raise-source form 'syntax "cond-expand feature requirement must be a list with at least one element" (list form)))

  (case (car form)
    ((and)
     (reduce (lambda (a b) (and a b)) (map (lambda (x) (if (memq x *features*) #t #f)) (cdr form))))
    ((or)
     (reduce (lambda (a b) (or a b)) (map (lambda (x) (if (memq x *features*) #t #f)) (cdr form))))
    ((else) #t)
    ((library) (raise 'syntax "cond-expand does not support library yet" (list form))))
)

(define (pi x)
  (print x)
  x)

(define-syntax cond-expand
  (lambda (x)

    (if (null? (cdr x))
      (if #f #f)

      (cons-source x #'begin
            (let loop ((clause (cadr x))
                 (clauses (cddr x))
                 (results '()))

              (unless (= (length clause) 2)
                (begin
                  (raise 'syntax "cond-expand clause length must be exactly 2 (feature list and command or definition)"))
              )

              (let* ((feature-list (car clause))
                     (commands (cadr clause))
                     (new-results
                      (if (cond-expand-check-feature x feature-list)
                        (cons-source x commands results)
                        results)))
                (if (null? clauses)
                  new-results
                  (loop (cadr clauses) (cddr clauses) new-results)
                  )))))))
;; While loop

(define-syntax while
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "while loop expected at least two arguments: condition and body" (list x)))
    `(,#'let ,#'loop ((,#'condition ,(cadr x)))
        (,#'when ,#'condition
          ,@(cddr x)
          (,#'loop ,(cadr x))))))

;; compatibility

(define-syntax er-macro-transformer
  (lambda (x)
    (cadr x)))
