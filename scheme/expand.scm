;; expand.scm - most basic boot functionality, macroexpansion & modules

;; This file loads and installs the expander and module system. It sets the default module to (arete), after which
;; point the other core files should be loaded, and then the default module should be set to the (user) module.

;; Scheme is so great, you can't program in it!
;; - A comment in the TinyCLOS source.

;; TODO Toplevel renames

;; TODO Make modules compilable as individual units

;; TODO Cannot use a macro defined in advance. Toplevel needs to behave like letrec-syntax, I think.

;; TODO: Fix redundant compilation, VM primitive compilation.
;; TODO disallow inter-module set!

;; TODO Disallow (if #t (define x #t) (define x #f)) with a helpful error message
;; This will probably require adding some parameters to the expander. 

;; TODO could avoid allocations by renaming special forms up front

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cdddr x)  (cdr (cdr (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

(define (not x) (eq? x #f))

(define (fixnum? v) (eq? (value-type v) 1))
(define (flonum? v) (eq? (value-type v) 4))
(define (number? x) (or (fixnum? x) (flonum? x)))
(define (constant? v) (eq? (value-type v) 2))
(define (boolean? v) (or (eq? v #t) (eq? v #f)))
(define (char? v) (eq? (value-type v) 6))
(define (pair? v) (eq? (value-type v) 11))
(define (table? v) (eq? (value-type v) 15))
(define (string? v) (eq? (value-type v) 5))
(define (symbol? v) (eq? (value-type v) 8))
(define (vector? v) (eq? (value-type v) 9))
(define (rename? v) (eq? (value-type v) 16))
(define (identifier? v) (or (rename? v) (symbol? v)))
(define (function? v) (eq? (value-type v) 13))
(define (vmfunction? v) (eq? (value-type v) 19))

(define list (lambda lst lst))

;; Constants
(define (null? v) (eq? (value-bits v) 10))
(define (eof-object? v) (eq? (value-bits v) 18))

(define input-port? (lambda (v) (and (eq? (value-type v) 23) (value-header-bit? v 9))))
(define output-port? (lambda (v) (and (eq? (value-type v) 23) (value-header-bit? v 10))))

;; @returns whether a symbol is already qualified 
(define (symbol-qualified? v)
  (and (symbol? v) (value-header-bit? v 12)))

(define current-input-port (lambda () (top-level-value'*current-input-port*)))
(define current-output-port (lambda () (top-level-value '*current-output-port*)))

(define (self-evaluating? v)
  (or (char? v) (fixnum? v) (constant? v) (string? v) (vector? v) (flonum? v) (table? v)))

(define unspecified (if #f #f))

(define (expander-trace . rest)
  (if (eq? (top-level-value 'xtrace) #t)
    (apply print (cons "expander:" rest))))

(define (list-tail lst i)
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
         (loop loop (cdr rest) (+ ii 1)))))))

(define (assq obj alist)
  (if (null? alist)
    #f
    (if (eq? (caar alist) obj)
      (car alist)
      (assq obj (cdr alist)))))

(define (string-map-i i limit src dst fn)
  (string-set! dst i (fn (string-ref src i)))
  (if (not (fx= (fx+ i 1) limit))
    (string-map-i (fx+ i 1) limit src dst fn)))

(define (string-map fn str)
  (define ret (make-string (string-length str)))
  (define len (string-length str))
  (if (fx= len 0)
    ret
    (string-map-i 0 len str ret fn))
  ret)

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

(define (env-make parent . syntactic?)
  (define vec (make-vector))
  (vector-append! vec parent)
  ;; Env first field is #t if an environment is purely syntactic (eg. introduced by let-syntax), #f otherwise
  (vector-append! vec (not (null? syntactic?)))
  vec)

(define (closest-non-syntactic-env env)
  (if (vector? env)
    (if (eq? (vector-ref env 1) #f)
      env
      (closest-non-syntactic-env (vector-ref env 0)))

    env))

(define (env-define env name value . toplevel?)
  (cond
    ((and (eq? env #f) (not (null? toplevel?))) (set-top-level-value! name value))
    ((eq? env #f) unspecified)
    ((table? env)
     (begin
       (table-set! env name (if (symbol? value) (module-qualify env name) value))
       (if (or (eq? (table-ref env "module-export-all") #t) (table-ref (table-ref env "module-exports") name))
         (table-set! (table-ref env "module-exports") name name))
    ))
    ((vector? env)
     ((lambda (closest)
        ;; Here we do some stuff to support the splicing of definitions from let-syntax
        ;; Consider the expression: (let-syntax () (define x #t))
        ;; If this happens at the toplevel of a module, x must become qualified normally, so we define it to
        ;; ##module#x as though it were a toplevel variable. As the let-syntax exits, the definitiosn will be folded
        ;; into the higher level
       (vector-append! env name)
       (vector-append! env
        (if (table? closest)
          (if (symbol? value) (module-qualify closest name) value)
          ;; Normal variables used to be env-defined to 'variable, but now resolve to gensyms
          ;; However, there is no need to gensym names
          (if (and (symbol? name) (not (gensym? name)) (eq? value 'variable)) (gensym name) (if (gensym? name) name value))))

       ;(vector-append! env (if (and (symbol? name) (not (gensym? name)) (eq? value 'variable)) (gensym name) (if (gensym? name) name value)))
       (if (and (eq? (top-level-value 'EXPANDER-PRINT) #t) (table? closest))
         (pretty-print env))
       (vector-ref env (fx- (vector-length env) 1)))
      (closest-non-syntactic-env env)))
    (else (begin (raise 'expand "env-define failed" (list name value))))))

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

;; This has to be done like this, because we cannot store #<syntax> or #<undefined> in named variables as the 
;; interpreter will error out when this happens.
(define (env-check-syntax name)
  (if (fx= (value-bits (top-level-value name)) 34) 'syntax (top-level-value name)))

;; Add a module's name to a symbol
(define (module-qualify mod name)
  (string->symbol (string-append "##" (table-ref mod "module-name") "#" (symbol->string name))))

;; Retrieve a rename-expr from a rename, leave other expressions alone
(define (rename-strip id)
  (if (rename? id) (rename-expr id) id))

;; Returns environment where key was resolved, the key in the environment considered equivalent to the given name
;; (necessary for rename gensyms) the value of the key in the environment, and whether the variable was "found"
;; (variables may be referenced before definition)
(define (env-lookup env name)
  (define strip (rename-strip name))
  (cond
    ;; toplevel
    ((eq? env #f)
     (begin
       (list #f strip (env-check-syntax strip) #t)))
    ((table? env)
     (if (rename? name)
       (begin
         (if (table? (rename-env name))
           #t#;(print (cdr (env-lookup (rename-env name) (rename-expr name)))))
         ;(print (env-lookup (rename-env name) (rename-expr name)))
         (env-lookup (rename-env name) (rename-expr name)))
       (begin
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
             (list env (module-qualify env strip) unspecified #f))))))
    ;; local environment
    ((vector? env)
     ((lambda (res)
       (if res 
         res
         (env-lookup (vector-ref env 0) name)))
      (env-vec-lookup env (vector-length env) 2 name)))))

;; For env-compare. This will lookup NAME with rename-env and rename-expr if it is a rename, if not, it will look
;; in ENV
(define (env-lookup-maybe-rename env name)
  (if (rename? name)
    (begin
      (env-lookup (rename-env name) (rename-expr name))
    )
    (env-lookup env name)))

;; Compare two identifiers to see if they "mean" the same thing
;; For example
;; 'else and (make-rename #f 'else) => #t
;; 'else and (make-rename <environment where else is defined by the user> 'else) => #f
(define (env-compare env a b)
  (cond
    ((eq? a b) #t)
    ((and (symbol? a) (rename? b)) (env-compare env b a))
    ((and (rename? a) (identifier? b))

     (and (eq? (rename-expr a) (rename-strip b))
      (apply 
         (lambda (renv rkey rvalue rfound)
           (apply
             (lambda (env key value found)
               ;; env-lookup will still return a table and qualified name
               ;; in the case that something cannot be found
               (define senv (if (and (not found) (table? env)) #f env))
               (define rsenv (if (and (not rfound) (table? renv)) #f renv))
               (eq? rsenv senv))

             (env-lookup-maybe-rename env b)))
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
        (else
          (if (or (gensym? value) (symbol-qualified? value))
            value
            name))))
    (env-lookup env name)))

(define (env-syntax? env name)
  (apply 
    (lambda (env name value found)
      (if (eq? value 'syntax)
        name
        (if (function-macro? value)
          'macro
          #f))
    )
    (env-lookup env name)))

;; Shorthand for mapping expand because it's so common
(define expand-map
  (lambda (x env)
    (map1 (lambda (sub-x) (expand sub-x env)) x)))

(define expand-define
  (lambda (x env)
    (define name #f)
    (define value #f)
    (define kar #f)
    (define len (length x))

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
    (env-define env name 'variable)

    (define result (list-source x (make-rename #f 'define)
                              ;; We'll replace name with the actual gensym here if there is one
                              (env-resolve env name)
                              (expand value env)))

    result))

;; Expand an identifier
(define (expand-identifier x env)
  (if (env-syntax? env x)
    (apply
      (lambda (env name value found)
        (if (or (eq? value 'syntax) (not (function-identifier-macro? x)))
          (expand-macro x env value)
          (raise-source x 'expand (print-string "used syntax" x "as value") (list x))))
      (env-lookup env x))
    (if (symbol-qualified? x)
      x
      (begin
        (apply
          (lambda (env name value found)
            (env-resolve env x)
            )
        (env-lookup env x))))))

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
  (for-each1
    (lambda (x)
      (if (not (symbol? x))
        (raise-source src 'expand (print-string "arguments to import specifier" name " must all be symbols") (list syms))))
    syms))

(define *module-paths* '("."))
(define *module-extensions* '(".scm" ".sld"))

(define (module-try-load name paths exts)
  (if (null? exts)
    #f
    (if (null? paths)
      (module-try-load name *module-paths* (cdr exts))
      (begin
        ;(print exts paths)
        (define path (string-append (car paths) (make-string 1 (path-separator)) name (car exts)))
        ;(print ";; checking" path)
        (if (file-exists? path)
          path
          (module-try-load name (cdr paths) exts))))))

(define (module-load src name)
  (define mod (table-ref (top-level-value 'module-table) name))
  (if mod
    (begin
      (if (eq? (table-ref mod "module-stage") 1)
        (raise-source src 'expand "cyclic import" (list name)))
      mod)
    (begin
      ;; thing#asdf 
      ;; becomes thing/asdf
      ;; then we try load path + extension combos
      ;; ./thing/asdf.sld
      ;; ./thing/asdf.scm
      ;; etc
      (define path (module-try-load (string-map (lambda (c) (if (eqv? c #\#) (path-separator) c)) name) *module-paths* *module-extensions*))
      (if (not path)
        (raise-source src 'expand (print-string "Could not find a file for module" name "in paths" *module-paths* "with extensions" *module-extensions*) (list name)))
      (load path)
      (if (not (table-ref (top-level-value 'module-table) name))
        (raise-source src 'expand (print-string "expected loading file" path "to provide module" name "but it did not") (list name)))
      (table-ref (top-level-value 'module-table) name))
  ) ;if
)

;; Despite being long, this is fairly simple.

;; Basically, when it encounters the actual module specifier like say (sdl)
;; It will load the module and return all its bindings in the form of a list correlating names with values

;; The modifiers only, prefix, rename and except then modify that list with map & filter

;; Filter a list of imports. For only/except
(define (module-import-filter spec name check)
  (define syms (cddr spec))
  (module-check-spec spec name syms)
  (filter (lambda (cell) (check cell syms)) (module-import-eval (cadr spec))))

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
       (map1
         (lambda (cell)
           (if (pair? cell)
             (cons (string->symbol (string-append pfx-str (symbol->string (car cell)))) (cdr cell))
             #f))
         (module-import-eval (cadr spec))))
    )
    ((eq? kar 'only)
     (module-import-filter spec 'only (lambda (cell allowed) (and cell (memq (car cell) allowed)))))
    ((eq? kar 'except)
     (module-import-filter spec 'except (lambda (cell forbidden) (and cell (not (memq (car cell) forbidden))))))
    ((eq? kar 'rename)
     ((lambda (renames)
       (for-each1
         (lambda (x)
           (if (or (not (list? x)) (not (fx= (length x) 2)) (not (symbol? (car x))) (not (symbol? (cadr x))))
             (raise-source spec 'expand (print-string "arguments to rename specifier must all be lists with two symbols") (list spec))))
         renames)
       (map1
         (lambda (cell)
           (define check (and cell (assq (car cell) renames)))
           (if check
             (cons (cadr check) (cdr cell))
             cell))
         (module-import-eval (cadr spec))))
      (cddr spec)))

    (else
      ((lambda (mod)
        (table-map
          (lambda (k v)
            (cons k (table-ref mod v)))
          (table-ref mod "module-exports")))

       (module-load spec (module-spec->string spec spec))))

      ))

(define (expand-import mod spec)
  (define imports (filter (lambda (v) v) (module-import-eval spec)))

  (for-each1
    (lambda (k)
      (if (table-ref mod (car k))
        (print "warning: duplicate import" k))
      (table-set! mod (car k) (cdr k)))
    imports)

  unspecified)

(define (expand-toplevel-import x env)
  (if (not (list? x))
    (raise-source x 'expand "import spec must be a list" (list x)))
  (for-each1
    (lambda (x) (expand-import env x))
    (cdr x)))

(define (expand-module-decl mod x env)
  (define kar (car x))

  (cond
    ((eq? kar 'import)
     (for-each1
       (lambda (x)
         (expand-import mod x))
       (cdr x)))

    ((eq? kar 'begin)
     (cons-source x (make-rename #f 'begin) (expand-map (cdr x) mod)))

    ((eq? kar 'export)
     (for-each-i
       (lambda (i cell)
         (if (not (or (symbol? cell) (and (list? cell) (fx= (length cell) 3) (symbol? (car cell)) (symbol? (cadr cell)) (symbol? (caddr cell )))))
           (raise-source (list-tail x (fx+ i 1)) 'expand "export arguments must be either a symbol or (rename from to)" (list x)))

         (define name (if (symbol? cell) cell (caddr cell)))
         (define value (if (symbol? cell) cell (cadr cell)))

         ;; this is already defined and may be imported from somewhere else
         (set! value
           (if (table-ref mod name)
             (table-ref mod name)
             (module-qualify mod value)))

         (table-set! (table-ref mod "module-exports") name value))
     (cdr x)))

    (else
      (raise-source x 'expand "only import, export and begin are currently supported as part of module declarations" (list x)))

  ) ;cond
)

(define (expand-module x env)
  (define len (length x))
  (if (not (fx> len 1))
    (raise-source x 'expand "module requires at least one argument (module name specifier)" (list x)))

  (define name (module-spec->string (cdr x) (cadr x)))

  (if (table-ref (top-level-value 'module-table) name)
    (begin
      (raise-source x 'expand (print-string "module" (cadr x) "encountered twice") (list x))))

  (define mod (module-make name))
  (table-set! (top-level-value 'module-table) name mod)
  (table-set! mod "module-name" name)
  (table-set! mod "module-stage" 1)

  (if (eq? (car x) 'module)
    (set-top-level-value! '*current-module* mod))

  (define result
    (cons-source x (make-rename #f 'begin)
      (map1
        (lambda (x)
          (expand-module-decl mod x env))
        (cddr x))))

  (table-set! mod "module-stage" 2)

  result
)

;; Strip an expression of all renames
(define (strip-renames x)
  (if (rename? x)
    (rename-expr x)
    (if (pair? x)
      (map-improper strip-renames x)
      x)))

(define (expand-quote x env)
  (if (fx> (length x) 3)
    (raise-source x 'expand "quote only takes one argument" (list x)))

  #|
  (if (eq? (cadr x) (string->symbol "##no-strip"))
    (begin
      (print x)
      (list-source x (make-rename #f 'quote) (list-ref x 2)))
    (if (fx> (length x) 2)
      (raise-source x 'expand "quote only takes one argument" (list x))
      (list-source x (make-rename #f 'quote) (strip-renames (cadr x))))))
|#
  (list-source x (make-rename #f 'quote) (strip-renames (cadr x))))
  ;(list-source x 

  ;(cons-source x (make-rename #f 'quote) (cdr x)))

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
        ((eq? syntax? 'macro)
                (expand-macro x env (list-ref (env-lookup env (car x)) 2)))
        ((or (eq? kar 'define-library) (eq? kar 'module)) (expand-module x env))
        ((eq? kar 'import)
         (if (not (table? env))
           (raise-source x "import must be part of module statement or be at toplevel"))
         (expand-toplevel-import x env))
        ((eq? kar 'define) (expand-define x env))
        ((eq? kar 'define-syntax) (expand-define-syntax x env))
        ((eq? kar 'let-syntax) (expand-let-syntax 'let-syntax x env))
        ((eq? kar 'letrec-syntax) (expand-let-syntax 'letrec-syntax x env))
        ((or (eq? kar 'and) (eq? kar 'or)) (expand-and-or x env))
        ((eq? kar 'lambda) (lambda () (expand-lambda x env)))
        ((eq? kar 'begin) (cons-source x (make-rename #f 'begin) (expand-map (cdr x) env)))
        ((eq? kar 'if) (expand-if x env))
        ((eq? kar 'set!) (expand-set x env))
        ((eq? kar 'cond) (expand-cond x env))
        #;((eq? kar 'quote)  (cons-source x (make-rename #f 'quote) (cdr x)))
        ((eq? kar 'quote) (expand-quote x env))
        (else (raise-source x 'expand (print-string "unknown special form" (car x)) (list x))))
      ;; Normal function application
      ;; Needs to be annotated with src info, right?
      ;; map-source?
      (expand-map x env))))

(define (expand-lambda x env)
  (if (fx< (length x) 3)
    (raise-source x 'expand "lambda has no body" (list x)))

  (define new-env (env-make env))
  (define args (cadr x))

  (define bindings
    (if (or (null? args) (pair? args))
      (map-improper-i
        (lambda (i arg)
          (if (not (identifier? arg))
            (raise-source (list-tail bindings i) 'expand "non-identifier in lambda argument list" (list x)))

          (if (rename? arg)
            (begin
              (rename-gensym! arg)
              (env-define new-env arg 'variable)
              (rename-gensym arg))
            (env-define new-env arg 'variable)))
        args)
      (if (not (identifier? args))
        (raise-source (cdr x) 'expand "non-identifier as lambda rest argument" (list x))
        (if (rename? args)
          (begin (rename-gensym! args) (rename-gensym args))
          (env-define new-env args 'variable)))))

  (cons-source x (make-rename #f 'lambda) (cons-source x bindings (expand-map (cddr x) new-env))))

(define (expand-if x env)
  (define len (length x))
  (define else-branch unspecified)

  (if (fx< len 3)
    (raise-source x 'expand "if expression needs at least two arguments" (list x))
    (if (fx= len 4)
      (set! else-branch (list-ref x 3))))

  (list-source x (make-rename #f 'if) (expand (list-ref x 1) env) (expand (list-ref x 2) env) (expand else-branch env)))

(define (expand-set x env)
  (define len (length x))
  (define name #f)

  (if (not (fx= len 3))
    (raise-source x 'expand "set! expression needs exactly two arguments" (list x)))

  (set! name (cadr x))

  (if (not (identifier? name))
    (raise-source (cdr x) 'expand "set! expects an identifier as its first arguments" (list x)))

  (list-source x (make-rename #f 'set!) (expand (list-ref x 1) env) (expand (list-ref x 2) env)))

(define (expand x env)
  (cond
    ((self-evaluating? x) x)
    ((identifier? x) (expand-identifier x env))
    (else (expand-apply x env))))

(define (expand-delayed x)
  (if (pair? x)
    (map-improper expand-delayed x)
    (if (procedure? x)
      (expand-delayed (x))
      x)))

;; Expander entry point
(define (expand-toplevel x env)
  (expand-delayed (expand x env)))

(define define-transformer!
  (lambda (x env trans-env name body)
    (define expanded-body #f)
    (define fn #f)
    (define fn-arity #f)
    (define identifier-transformer #f)

    (if (not (identifier? name))
      (raise-source (cdr x) 'expand "define-syntax first argument (macro name) must be an identifier" (list x (cdr x))))

    (if (table? env)
      (if (table-ref env "module-export-all")
        (table-set! (table-ref env "module-exports") name #t)))

    (if (eq? (car body) 'identifier)
      (begin
        (set! body (cadr body))
        (set! identifier-transformer #t)
        #;(print "found identifier transformer" body)))

    (set! expanded-body (expand-delayed (expand body env)))

    (set! fn (eval expanded-body env))

    (if (not (procedure? fn))
      (raise-source (cddr x) 'expand "define-syntax body did not evaluate to a function" (list x)))

    (set-function-macro-env! fn trans-env)

    (set! fn-arity (function-min-arity fn))

    (if (fx< fn-arity 1)
      (raise-source (caddr x) 'expand "define-syntax body must evaluate to a function that takes one argument" (list x)))

    (set-function-name! fn (rename-strip name))
    (set-function-macro-bit! fn)
    (if identifier-transformer
      (set-function-identifier-macro-bit! fn))

    (env-define env name fn #t)

    unspecified))

(define (expand-define-syntax x env)
  (define len (length x))
  (define name #f)
  (define body #f)

  (if (fx< len 3)
    (raise-source x 'expand "define-syntax expects at least two arguments: a name and a body" (list x)))

  (set! name (cadr x))
  (set! body (caddr x))

  (define-transformer! x env env name body))

;; Fold an environment into another environment
;; Used for let-syntax and letrec-syntax definition splicing
(define (env-vec-fold parent env len i)
  (if (fx= i len)
    #f
    (begin
      (env-define parent (vector-ref env i) (vector-ref env (fx+ i 1)))
      (env-vec-fold parent env len (fx+ i 2)))))

(define (env-fold env)
  (env-vec-fold (vector-ref env 0) env (vector-length env) 2))

;; Expander for let-syntax and letrec-syntax
(define (expand-let-syntax type x env)
  (define new-env (env-make env #t))
  (define bindings #f)

  (if (fx< (length x) 3)
    (raise-source x 'expand "let(rec)-syntax expands at least three arguments" (list x)))

  (define bindings (cadr x))
  (define body (cddr x))

  (if (not (list? bindings))
    (raise-source (cadr x) 'expand "let(rec)-syntax bindings list must be a valid list" (list x (cadr x))))

  (for-each1
    (lambda (x)
      (if (not (fx= (length x) 2))
        (raise-source x 'expand "let(rec)-syntax bindings should have two values: a name and a transformer" (list x)))

      (define name (car x))
      (define body (cadr x))

      (define-transformer! x new-env (if (eq? type 'let-syntax) env new-env) name body))
    bindings)

  (define result 
    (cons-source x (make-rename #f 'begin) (expand-map body new-env)))

  ;; fold new-env into old-env
  ;(pretty-print new-env)
  (env-fold new-env)

  #;(expand result env)
  result)

;; Expand a macro application
(define (expand-macro x env transformer)
  ;(define lookup (list-ref (env-lookup env (car x)) 2))
  (define arity (function-min-arity transformer))
  (define saved-rename-env (top-level-value '*current-rename-env*))
  (define identifier-application? (and (not (identifier? x)) (function-identifier-macro? transformer)))
  (define form (if identifier-application? (car x) x))

  (set-top-level-value! '*current-rename-env* (function-env transformer))

  (if identifier-application?
    (set! form (car x)))

  (if (and (identifier? x) (not (function-identifier-macro? transformer)))
    (raise-source x 'expand (print-string "use of syntax" x "as value") (list x)))

  (define result
    (unwind-protect
      (lambda ()
        (if (eq? arity 1)
          ;; Only pass form
          (transformer form)
          (if (eq? arity 2)
            ;; Only pass form and comparison procedure
            (transformer form (lambda (a b) (env-compare env a b)))

            (transformer form
              ;; rename procedure
              (lambda (name) (make-rename (function-env transformer) name))
              ;; compare procedure
              (lambda (a b) (env-compare env a b))))))
      (lambda ()
        (set-top-level-value! '*current-rename-env* saved-rename-env))))

  (if identifier-application?
    (cons-source x result (expand-map (cdr x) env)) 
    (expand result env)))

;; cond expander
(define (expand-cond-full-clause x env clause rest)
  (if (or (null? clause) (not (list? clause)))
    (raise-source clause 'expand "cond clause should be a list" (list clause)))

  (define len (length clause))

  (define condition (car clause))
  (define body (cdr clause))

  ;; Handle else clause
  (if (env-compare env condition (make-rename env 'else))
    (set! condition #t)
    (set! condition (expand condition env)))

  (define result
    (if (null? body)
      ;; We may need to return this value
      ((lambda (name)
         (list-source x 
           (list-source x 
              (make-rename #f 'lambda)
              (list-source x name)
              (list-source x
                (make-rename #f 'if)
                name
                name
                (if (null? rest) unspecified (expand-cond-full-clause x env (car rest) (cdr rest)))))
           condition))
       (gensym))

      (if (env-compare env (car body) (make-rename #f '=>))
        (begin
          (if (null? (cdr body))
            (raise-source body 'expand "cond expects function after =>" (list x))
            (if (not (null? (cddr body)))
              (raise-source body 'expand "cond expects only two elements in => clause" (list x))
              ((lambda (name)
                 ;; Generate a function in the CAR position so as to not double-evaluate
                 (list-source x
                   (list-source x
                                (make-rename #f 'lambda)
                                (list-source x name)
                                (list-source x
                                             (make-rename #f 'if)
                                             name
                                            (list-source x (cadr body) name)
                                            (if (null? rest)
                                              unspecified
                                              (expand-cond-full-clause x env (car rest) (cdr rest))))
                                )
                   condition))
               (gensym 'result)))))
        ;; Otherwise it's a simple if
        (list-source x
          (make-rename #f 'if)
          condition
          (expand (cons-source x (make-rename #f 'begin) body) env)
          (if (null? rest)
            unspecified
            (expand-cond-full-clause x env (car rest) (cdr rest))))))
  )

  result)

(define (expand-cond-clause x env clause rest)
  (if (null? clause)
    unspecified
    (expand-cond-full-clause x env clause rest)))

(define (expand-cond x env)
  (if (fx= (length x) 1)
    unspecified
    (expand-cond-clause x env (cadr x) (cddr x))))

;; Set up module system

;; This is finished at the end of compiler.scm

(define *core-module* (module-make "arete"))

(table-set! (top-level-value 'module-table) "arete" *core-module*)
(table-set! *core-module* "module-export-all" #t)
(table-set! *core-module* "module-stage" 2)

(define *current-module* *core-module*)

;; populate core module with already-defined things
(top-level-for-each
  (lambda (k v)
    (if (or (procedure? v) (memq k '(unspecified)))
      ((lambda (name)
         (set-top-level-value! name v)
         (table-set! (table-ref *core-module* "module-exports") k k)
         (table-set! *core-module* k k))
       (module-qualify *core-module* k)))))

(define (make-empty-module name)
  (define mod (module-make name))
  (table-set! (top-level-value 'module-table) name mod)
  (table-set! mod "module-export-all" #t)
  (table-set! mod "module-stage" 2)
  mod)

(for-each1 make-empty-module '("scheme#base" "scheme#cxr" "scheme#file" "scheme#inexact" "scheme#write"
                               "scheme#time" "scheme#read" "scheme#char" "scheme#complex"))

(define *user-module* (module-make "user"))

(table-set! (top-level-value 'module-table) "user" *user-module*)
(table-set! *user-module* "module-export-all" #t)
(table-set! *user-module* "module-stage" 2)

(set-top-level-value! '*push-module* *user-module*)
(set-top-level-value! '*current-module* *core-module*)

; Install expander
(set-top-level-value! 'expander expand-toplevel)
