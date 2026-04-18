;; expand.scm - most basic boot functionality, macroexpansion & modules

;; This file loads and installs the expander and module system. It sets the default module to (arete), after which
;; point the other core files should be loaded, and then the default module should be set to the (user) module.

;; Scheme is so great, you can't program in it! - A comment in the TinyCLOS source.

;; TODO Toplevel renames

;; TODO Make modules compilable as individual units

;; TODO Cannot use a macro defined in advance. Toplevel needs to behave like letrec-syntax, I think.

;; TODO: Fix redundant compilation, VM primitive compilation.
;; TODO disallow inter-module set!

;; TODO Disallow (if #t (define x #t) (define x #f)) with a helpful error message
;; This will probably require adding some parameters to the expander. 

;; TODO could avoid allocations by renaming special forms up front

;; TODO: Have to create something with a rename to represent delayed expansion rather than just straight functions.

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
(define (caddar x) (car (cdr (cdr (car x)))))
(define (caaar x) (car (car (car x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))

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
(define (record? v) (eq? (value-type v) 17))
(define (record-type? v) (eq? (value-type v) 18))

(define (symbol? v)
  (and (eq? (value-type v) 8) (not (value-header-bit? v 15))))

(define (keyword? v)
  (and (eq? (value-type v) 8) (value-header-bit? v 15)))

(define (vector? v) (eq? (value-type v) 9))
(define (rename? v) (eq? (value-type v) 16))
(define (identifier? v) (or (rename? v) (symbol? v)))
(define (function? v) (eq? (value-type v) 13))
(define (vmfunction? v) (eq? (value-type v) 19))
(define (cfunction? v) (eq? (value-type v) 14))

(define (list . lst) lst)

;; Constants
(define (null? v) (eq? (value-bits v) 10))
(define (eof-object? v) (eq? (value-bits v) 18))

(define (input-port? v) (and (eq? (value-type v) 23) (value-header-bit? v 9)))
(define (output-port? v) (and (eq? (value-type v) 23) (value-header-bit? v 10)))

;; @returns whether a symbol is already qualified 
(define (symbol-qualified? v)
  (and (symbol? v) (value-header-bit? v 13)))

(define (self-evaluating? v)
  (or (keyword? v) (char? v) (fixnum? v) (constant? v) (string? v) (vector? v) (flonum? v) (table? v)
      (record? v) (record-type? v) (cfunction? v)
      
      ))

(define unspecified (if #f #f))

(define (expander-trace . rest)
  (if (eq? (top-level-value 'xtrace) #t)
    (apply print (cons "expander:" rest))))

(define (list-tail lst i)
  (if (or (null? lst) (not (pair? lst)))
    (if (and (null? lst) (fx= i 0))
      '()
      (raise 'type "list-tail expects a list with at least one element as its argument" (list lst))))

  (if (= i 0)
    lst
    ((lambda (loop)
      (loop loop (cdr lst) 1))
     (lambda (loop rest ii)
       (if (null? rest)
         (if (fx= ii i)
           '()
           (raise 'type "list-tail bounds error" (list lst (length lst) i))))
       (if (= ii i)
         rest
         (loop loop (cdr rest) (+ ii 1)))))))

(define (assq obj alist)
  (if (null? alist)
    #f
    (if (eq? (caar alist) obj)
      (car alist)
      (assq obj (cdr alist)))))

(define (identifier->keyword x) (string->symbol (string-append (symbol->string (rename-strip x)) ":")))

;; ---------------------------------------------------------------------------
;; Helpers for optional/keyword argument destructuring
;; ---------------------------------------------------------------------------
;; The expander rewrites lambdas with #!optional/#!key into plain variadic
;; lambdas with inline destructuring code.  These helpers are called at
;; runtime by the generated code.  They are referenced via qualified symbols
;; (##arete#...) so they cannot be shadowed by user bindings.

;; Create a qualified ##arete# symbol reference for use in generated code.
(define (core-ref name)
  (string->symbol (string-append "##arete#" (symbol->string name))))

;; (%check-arity rest max-optional fn-name)
;; Called after all optionals have been consumed.  Raises if rest is non-empty.
(define (%check-arity rest max-optional fn-name)
  (if (pair? rest)
    (raise 'arity
           (string-append fn-name ": too many arguments; expected at most "
                          (number->string max-optional) " optional argument"
                          (if (fx= max-optional 1) "" "s"))
           (list rest))))

;; (check-keywords rest valid-keywords fn-name)
;; Validates that rest is a well-formed keyword argument list.
(define (%keywords-string kws)
  ((lambda (loop) (loop loop kws ""))
   (lambda (loop ks acc)
     (if (null? ks)
       acc
       (loop loop (cdr ks)
             (string-append acc (if (string=? acc "") "" ", ")
                            (symbol->string (car ks))))))))

(define (%check-keywords rest valid-keywords fn-name)
  ((lambda (check) (check check rest))
   (lambda (check r)
     (if (null? r) #t
       (if (null? (cdr r))
         (raise 'arity
                (string-append fn-name ": keyword argument list has odd length; "
                               "missing value after " (symbol->string (car r)))
                (list rest))
         (if (not (keyword? (car r)))
           (raise 'type
                  (string-append fn-name ": expected a keyword argument but got "
                                 (symbol->string (car r))
                                 "; valid keywords are "
                                 (%keywords-string valid-keywords))
                  (list (car r) rest))
           (if (not (memq (car r) valid-keywords))
             (raise 'arity
                    (string-append fn-name ": unknown keyword " (symbol->string (car r))
                                   "; valid keywords are "
                                   (%keywords-string valid-keywords))
                    (list (car r) rest))
             (check check (cddr r)))))))))

;; (%keyword-lookup rest keyword default-thunk) -> value
;; Returns the value stored after `keyword` in the plist `rest`, or the
;; result of calling `default-thunk` if not present. A thunk is used
;; (rather than an "absent" sentinel) so the default expression is evaluated
;; lazily only when the keyword is missing.
(define (%keyword-lookup rest keyword default-thunk)
  (if (null? rest) (default-thunk)
    (if (eq? (car rest) keyword) (cadr rest)
      (%keyword-lookup (cddr rest) keyword default-thunk))))

(define (string-map-i i limit src dst fn)
  (string-set! dst i (fn (string-ref src i)))
  (if (not (fx= (fx+ i 1) limit))
    (string-map-i (fx+ i 1) limit src dst fn)))

(define (string-map fn str)
  (define len (string-length str))
  (define ret (make-string len))
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

(define (closest-module env)
  (if (vector? env)
    (closest-module (vector-ref env 0))
    env))

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
  (if (symbol-qualified? name)
    name
    (string->symbol (string-append "##" (table-ref mod "module-name") "#" (symbol->string name)))))

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
         (if (symbol-qualified? strip)
           (list #f strip (env-check-syntax strip) (top-level-bound? strip))
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
             (list env (module-qualify env strip) unspecified #f)))))))
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

(define (env-lookup-destructure lookup fn)
  (if (not (and (pair? lookup) (pair? (cdr lookup)) (pair? (cddr lookup)) (pair? (cdddr lookup))))
    (raise 'expand (print-string "env-lookup returned malformed result:" lookup) (list lookup)))
  (fn (car lookup) (cadr lookup) (caddr lookup) (cadddr lookup)))

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
      (env-lookup-destructure
        (env-lookup (rename-env a) (rename-expr a))
        (lambda (renv rkey rvalue rfound)
          (env-lookup-destructure
            (env-lookup-maybe-rename env b)
            (lambda (env key value found)
              ;; env-lookup will still return a table and qualified name
              ;; in the case that something cannot be found
              (define senv (if (and (not found) (table? env)) #f env))
              (define rsenv (if (and (not rfound) (table? renv)) #f renv))
              (eq? rsenv senv)))))))

    (else #f)))

;; Name resolution

;; Local renames become gensyms, e.g.
;; (lambda (#R:name) #R:name) => (lambda (#:name0) #:name0)

;; Global renames are stripped

;; Symbols in modules become qualified, e.g. (define x #t) in module (arete) becomes 
;; (define ##arete#x #t)

(define (env-resolve env name)
  (env-lookup-destructure
    (env-lookup env name)
    (lambda (env key value found)
      (cond
        ((and (rename? key) (rename-gensym key)) (rename-gensym key))
        ((table? env) key)
        (else
          (if (or (gensym? value) (symbol-qualified? value))
            value
            name))))))

(define (env-syntax? env name)
  (env-lookup-destructure
    (env-lookup env name)
    (lambda (env name value found)
      (if (eq? value 'syntax)
        name
        (if (function-macro? value)
          'macro
          #f)))))

;; Shorthand for mapping expand because it's so common
(define (expand-map x env params)
  (map1 (lambda (sub-x) (expand sub-x env params)) x))

;; Expander parameters

;; (disallow-defines #t)
;; If #t, defines are an expansion-time error (for example, (if #t (define x #t) #t #f)

;; (procedure-name <symbol>)
;; The name of the currently expanding procedure (if any)

;; Params are cleared upon entering a module or function

(define (expand-define x env params)
  (define name #f)
  (define value #f)
  (define kar #f)
  (define len (length x))

  (if (assq 'disallow-defines params)
    (raise-source x 'expand "define is not allowed here" (list x)))

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
      (if (not (identifier? name))
        (raise-source kar 'expand "lambda name must be an identifier" (list kar)))
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
                            (expand value env params)))

  result)

;; Expand an identifier
(define (expand-identifier x env params)
  ;; Check for identifier-transformers or invalid use of 
  (if (env-syntax? env x)
    (env-lookup-destructure
      (env-lookup env x)
      (lambda (env name value found)
        (if (or (eq? value 'syntax) (not (function-identifier-macro? value)))
          (raise-source x 'expand (print-string "used syntax" x "as identifier") (list x))
          (expand-macro x env value params))))
    (if (symbol-qualified? x)
      x
      (begin
        (env-lookup-destructure
          (env-lookup env x)
          (lambda (env name value found)
            (env-resolve env x)
            ))))))

;; Expand and/or
(define (expand-and-or x env params)
  (cons-source x (if (symbol? (car x)) (make-rename #f (car x)) (car x)) (expand-map (cdr x) env params)))

;;;;; Module machinery
(define (module-spec->string src lst . str)
  (if (null? lst)
    (car str)
    (if (or (not (pair? lst)) (not (symbol? (car lst))))
      (raise-source src 'expand "module specification must be a proper list of symbols or fixnums" (list lst))
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
       (if (and (not (keyword? (caddr spec))) (not (symbol? (caddr spec))))
         (raise-source (cddr spec) 'expand "module prefix must be a symbol" (list spec)))
       (if (not (fx= (length spec) 3))
         (raise-source (cdddr spec) 'expand "prefix specifier must have exactly three elements" (list spec)))
       ((lambda (pfx-str)
         (map1
           (lambda (cell)
             (if (pair? cell)
               (cons (string->symbol (string-append pfx-str (symbol->string (car cell)))) (cdr cell))
               #f))
           (module-import-eval (cadr spec))))
        (symbol->string (caddr spec))))
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
     (cons-source x (make-rename #f 'begin) (expand-map (cdr x) mod '())))

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

(define (expand-quote x env params)
  (if (fx> (length x) 3)
    (raise-source x 'expand "quote only takes one argument" (list x)))

  (list-source x (make-rename #f 'quote) (strip-renames (cadr x))))

;; Expand an application. Could be a special form, a macro, or a normal function application
(define (expand-apply x env params)
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
      ((eq? syntax? 'macro) (expand-macro x env (list-ref (env-lookup env (car x)) 2) params))
      ((or (eq? kar 'define-library) (eq? kar 'module)) (expand-module x env))
      ((eq? kar 'import)
       (if (not (table? env))
         (raise-source x "import must be part of module statement or be at toplevel"))
       (expand-toplevel-import x env))
      ((eq? kar 'define) (expand-define x env params))
      ((eq? kar 'define-syntax) (expand-define-syntax x env params))
      ((eq? kar 'let-syntax) (expand-let-syntax 'let-syntax x env params))
      ((eq? kar 'letrec-syntax) (expand-let-syntax 'letrec-syntax x env params))
      ((or (eq? kar 'and) (eq? kar 'or)) (expand-and-or x env params))
      ((eq? kar 'lambda) (lambda () (expand-lambda x env params)))
      ((eq? kar 'begin) (cons-source x (make-rename #f 'begin) (expand-map (cdr x) env params)))
      ((eq? kar 'if) (expand-if x env params))
      ((eq? kar 'set!) (expand-set x env params))
      ((eq? kar 'cond) (expand-cond x env params))
      ((eq? kar 'quote) (expand-quote x env params))
      (else (raise-source x 'expand (print-string "unknown special form" (car x)) (list x))))
    ;; Normal function application
    ;; Needs to be annotated with src info, right?
    ;; map-source?
    (expand-map x env params)))


;; Argument list parsing

(define (define-argument! env x)
  ;; Check for existing argument only in this environment, which is guaranteed to be a vector as this is a function
  (if (env-vec-lookup env (vector-length env) 2 x)
    (raise-source x 'expand (print-string "duplicate argument name" x) (list x)))

  (if (rename? x)
    (begin
      (rename-gensym! x)
      (env-define env x 'variable)
      (rename-gensym x))
     (env-define env x 'variable)))

;; This function takes care of several things:

;; Defining all arguments in the function's new environment
;; Expanding the default values of optional and keyword arguments
;; Prepending expressions to the body of the lambda in question to execute said defaults

;; Fast-path predicate: does `x` have no #!optional/#!key/#!rest/#!keys markers
;; and no default-value pairs? Covers the common cases `(lambda (a b c) ...)`,
;; `(lambda (a b . rest) ...)` and `(lambda () ...)` — the vast majority of
;; lambdas in practice — so we can skip the destructuring machinery below.
;;
;; NOTE: uses only `if` (not `cond`/`and`/`or`) because this function is defined
;; in expand.scm, which loads before the expander is installed. pull-up-bootstraps
;; compiles it directly from raw source, so macros aren't available.
(define (plain-arg-list? x)
  (if (null? x) #t
    (if (identifier? x) #t
      (if (pair? x)
        (if (eq? (car x) #!optional) #f
          (if (eq? (car x) #!key) #f
            (if (eq? (car x) #!rest) #f
              (if (eq? (car x) #!keys) #f
                (if (pair? (car x)) #f
                  (plain-arg-list? (cdr x)))))))
        #f))))

(define (expand-argument-list x env new-env)
  (if (plain-arg-list? x)
    ;; Fast path: no markers, no default-valued args. Single-pass walk via the
    ;; C builtin `map-improper`, same shape as before the optional/keyword
    ;; migration. Bootstrap takes this path for ~all of its lambdas.
    (cons
      (if (identifier? x)
        (define-argument! new-env x)
        (map-improper (lambda (a) (define-argument! new-env a)) x))
      '())
    (expand-argument-list-slow x env new-env)))

;; Slow path: handles #!optional/#!key/#!rest/#!keys and default-valued args.
;; Must be in the memq list in `recompile-function` (compiler.scm) because it
;; uses `cond`/macros.
(define (expand-argument-list-slow x env new-env)
  (define seen-optional #f)
  (define seen-rest #f) ;; #f = not seen, 1 = seen and expecting symbol, 2 = not expecting symbol
  (define seen-keys #f) ;; same as above
  (define seen-key #f)
  (define argi 0)
  (define body '())            ;; prepended body forms (in reverse)
  (define required-args '())   ;; required arg names (in reverse)
  (define rest-gensym #f)      ;; gensym for the rest parameter (created on demand)
  (define optional-count 0)    ;; count of optional args (for arity check)
  (define keyword-names '())   ;; list of keyword symbols (for check-keywords)
  (define user-rest-name #f)   ;; user's #!rest binding name (if any)

  ;; Lazily create the rest parameter gensym
  (define (ensure-rest!)
    (if (not rest-gensym)
      (begin
        (set! rest-gensym (gensym '%rest))
        (define-argument! new-env rest-gensym))))

  ;; Emit: (define <name> (if (##arete#pair? %rest) (##arete#car %rest) <default>))
  ;;       (set! %rest (if (##arete#pair? %rest) (##arete#cdr %rest) '()))
  (define (emit-optional-pop! name default)
    (set! body (cons
      (list (make-rename #f 'set!) rest-gensym
            (list (make-rename #f 'if) (list (core-ref 'pair?) rest-gensym)
                  (list (core-ref 'cdr) rest-gensym)
                  (list (make-rename #f 'quote) '())))
      (cons
        (list (make-rename #f 'define) name
              (list (make-rename #f 'if) (list (core-ref 'pair?) rest-gensym)
                    (list (core-ref 'car) rest-gensym)
                    default))
        body))))

  ;; Emit: (define <name>
  ;;         (##arete#%keyword-lookup %rest '<keyword> (lambda () <default>)))
  ;; The thunk keeps the default lazy — it is only evaluated when the keyword
  ;; is not supplied, so side effects in defaults fire at most once per call.
  (define (emit-keyword-lookup! name keyword default)
    (set! body (cons
      (list (make-rename #f 'define) name
            (list (core-ref '%keyword-lookup) rest-gensym
                  (list (make-rename #f 'quote) keyword)
                  (list (make-rename #f 'lambda) '() default)))
      body)))

  (define simple-rest-arg #f)

  (define (process-arg! a)
    (cond
      ((eq? a #!optional)
       (if seen-optional (raise-source x 'expand "multiple #!optional in lambda arguments list" (list x)))
       (if seen-rest (raise-source x 'expand "#!optional in lambda arguments list not allowed after #!rest" (list x)))
       (if seen-keys (raise-source x 'expand "#!optional and #!keys cannot be mixed" (list x)))
       (if seen-key (raise-source x 'expand "#!optional and #!key cannot be mixed" (list x)))
       (set! seen-optional #t)
       (ensure-rest!))

      ((eq? a #!rest)
       (if seen-rest (raise-source x 'expand "multiple #!rest in lambda arguments list" (list x)))
       (if seen-keys (raise-source x 'expand "#!rest and #!keys cannot be mixed" (list x)))
       (if seen-key (raise-source x 'expand "#!rest and #!key cannot be mixed" (list x)))
       (set! seen-rest 1)
       (ensure-rest!))

      ((eq? a #!key)
       (if seen-rest (raise-source x 'expand "#!rest and #!key cannot be mixed" (list x)))
       (if seen-keys (raise-source x 'expand "#!key and #!keys cannot be mixed" (list x)))
       (if seen-optional (raise-source x 'expand "#!key and #!optional cannot be mixed" (list x)))
       (if seen-key (raise-source x 'expand "duplicate #!key in lambda arguments list" (list x)))
       (set! seen-key #t)
       (ensure-rest!))

      ((eq? a #!keys)
       (if seen-keys (raise-source x 'expand "multiple #!keys in lambda arguments list" (list x)))
       (if seen-key (raise-source x 'expand "#!key and #!keys cannot be mixed" (list x)))
       (if seen-rest (raise-source x 'expand "#!rest and #!keys cannot be mixed" (list x)))
       (set! seen-keys 1)
       (ensure-rest!))

      ((identifier? a)
       (if (eq? seen-rest 2)
         (raise-source x 'expand "identifier in lambda arguments list after #!rest argument" (list x)))
       (if (eq? seen-keys 2)
         (raise-source x 'expand "identifier in lambda arguments list after #!keys argument" (list x)))

       (cond
         (seen-optional
          (if (eq? seen-rest 1)
            ;; #!rest name after optionals
            (begin
              (set! user-rest-name (define-argument! new-env a))
              (set! seen-rest 2))
            (begin
              (set! optional-count (fx+ optional-count 1))
              (emit-optional-pop! (define-argument! new-env a) #f))))
         (seen-key
          ((lambda (name)
             (set! keyword-names (cons (identifier->keyword a) keyword-names))
             (emit-keyword-lookup! name (identifier->keyword a) #f))
           (define-argument! new-env a)))
         ((or (eq? seen-rest 1) (eq? seen-keys 1))
          ;; This is the #!rest or #!keys binding name
          (set! user-rest-name (define-argument! new-env a))
          (if seen-rest (set! seen-rest 2))
          (if seen-keys (set! seen-keys 2)))
         (else
          ;; Required argument
          (set! required-args (cons (define-argument! new-env a) required-args)))))

      ((pair? a)
       (if seen-rest
         (raise-source x 'expand "pair item in lambda arguments list after #!rest argument" (list x)))
       (if seen-keys
         (raise-source x 'expand "pair item in lambda arguments list after #!keys argument" (list x)))
       (if (not (or seen-optional seen-key))
         (raise-source a 'expand "required argument cannot have default value" (list a)))
       (if (not (identifier? (car a)))
         (raise-source a 'expand "argument name must be a valid identifier" (list a)))

       ((lambda (default)
          (cond
            (seen-key
             ((lambda (name)
                (set! keyword-names (cons (identifier->keyword (car a)) keyword-names))
                (emit-keyword-lookup! name (identifier->keyword (car a)) default))
              (define-argument! new-env (car a))))
            (seen-optional
             (set! optional-count (fx+ optional-count 1))
             (emit-optional-pop! (define-argument! new-env (car a)) default))))
        (expand (cdr a) new-env '((disallow-defines . #t)))))

      (else
        (raise-source x 'expand (print-string "invalid element" a "in arguments list: must be an identifier, a #! specifier like #!optional, or a pair of an identifier and default argument") (list x)))))

  (if (identifier? x)
    ;; Simple variadic: (lambda rest ...)
    (set! simple-rest-arg (define-argument! new-env x))
    (begin
      ;; Walk the proper pairs of the list, processing each element.
      ;; Do NOT use map-improper -- we need to handle the dotted tail separately.
      ((lambda (walk) (walk walk x))
       (lambda (walk lst)
         (if (pair? lst)
           (begin
             (process-arg! (car lst))
             (walk walk (cdr lst))))))

      ;; Handle improper list tail: (lambda (a b . rest) ...)
      (if (not (list? x))
        ((lambda (tail-id)
           (if rest-gensym
             ;; Had #!optional/#!key — rest gensym already created; the dotted
             ;; tail is the user's rest binding name (like #!rest <name>)
             (if (not user-rest-name)
               (set! user-rest-name (define-argument! new-env tail-id)))
             ;; Plain variadic (no #!optional/#!key): define the tail directly
             (set! rest-gensym (define-argument! new-env tail-id))))
         ((lambda (find-tail) (find-tail find-tail x))
          (lambda (find-tail lst)
            (if (pair? (cdr lst)) (find-tail find-tail (cdr lst)) (cdr lst))))))))

  ;; Assemble the final prepend-body in correct order:
  ;;   1. keyword validation (if #!key)
  ;;   2. optional/keyword destructuring (from body, reversed)
  ;;   3. arity check (if #!optional without #!rest)
  ;;   4. user rest binding (if #!rest after optionals)
  (define prepend-body
    (if rest-gensym
      ((lambda (result)
         ;; Append arity check (wrapped in set! for zero stack effect)
         (if (and seen-optional (not user-rest-name) (not seen-rest))
           (set! result (append result
             (list (list (make-rename #f 'set!) rest-gensym
                         (list (make-rename #f 'begin)
                               (list (core-ref '%check-arity) rest-gensym
                                     optional-count
                                     (list (make-rename #f 'quote) "lambda"))
                               rest-gensym))))))
         ;; Append user rest binding
         (if user-rest-name
           (set! result (append result
             (list (list (make-rename #f 'define) user-rest-name rest-gensym)))))
         result)
       ;; Start with keyword validation (if needed) + destructuring
       ;; Note: validation calls are wrapped in (set! %rest (begin (check ...) %rest))
       ;; so they have zero net stack effect in the lambda body.
       (if seen-key
         (cons
           (list (make-rename #f 'set!) rest-gensym
                 (list (make-rename #f 'begin)
                       (list (core-ref '%check-keywords) rest-gensym
                             (list (make-rename #f 'quote) (reverse keyword-names))
                             (list (make-rename #f 'quote) "lambda"))
                       rest-gensym))
           (reverse body))
         (reverse body)))
      '()))

  ;; Build the output argument list
  (define result-args
    (if simple-rest-arg
      simple-rest-arg
      (if rest-gensym
        ;; Build (required-arg1 required-arg2 ... . rest-gensym)
        ((lambda (build) (build build (reverse required-args)))
         (lambda (build args)
           (if (null? args)
             rest-gensym
             (cons (car args) (build build (cdr args))))))
        ;; No optionals/keys — reconstruct proper list from required-args
        (reverse required-args))))

  (cons result-args prepend-body))

(define (expand-lambda x env params)
  (if (fx< (length x) 3)
    (raise-source x 'expand "lambda has no body" (list x)))

  (define new-env (env-make env))

  #|
  (define args-ret (make-argument-list-info))
  (define args (cadr x))

  (define bindings
    (parse-next-argument args args-ret env new-env '()))
  |#

  (define bindings-and-body (expand-argument-list (cadr x) env new-env))
  (define bindings (car bindings-and-body))
  (define prepend-body (cdr bindings-and-body))
  (define body (expand-map (cddr x) new-env '()))

  (cons-source x (make-rename #f 'lambda) (cons-source x bindings
                                                       (if (null? prepend-body)
                                                         body
                                                         (append prepend-body body)))))

(define (expand-if x env params)
  (define len (length x))
  (define else-branch unspecified)

  (if (fx< len 3)
    (raise-source x 'expand "if expression needs at least two arguments" (list x))
    (if (fx= len 4)
      (set! else-branch (list-ref x 3))))

  (define new-params (cons '(disallow-defines #t) params))

  (list-source x
    (make-rename #f 'if)
    (expand (list-ref x 1) env new-params)
    (expand (list-ref x 2) env new-params)
    (expand else-branch env params)))

(define (expand-set x env params)
  (define len (length x))

  (if (not (fx= len 3))
    (raise-source x 'expand "set! expression needs exactly two arguments" (list x)))

  (define name (cadr x))

  (if (not (identifier? name))
    (raise-source (cdr x) 'expand "set! expects an identifier as its first arguments" (list x)))

   ;; TODO: Check for inter-module sets, which are disallowed
  ((lambda (location)
     ;; This is corny, but we check inter-module sets by seeing if the set! is on a module-level variable,
     ;; then qualifying the name and seeing if the names are similar. This is the easiest way to do it as is
     ;; because env-lookup does not return the module a variable was defined in, just where the definition was found
     ;; (and imported variables are defined "in" a module by importing them)
     #t
     #;(if (and (table? (car location)) (not (eq? (module-qualify (car location) (cadr x)) (cadr location))))
       (raise-source x 'expand "Implementation restriction: set!ing an imported variable not allowed" (list x))))
   (env-lookup env (cadr x)))
  (list-source x (make-rename #f 'set!) (expand (list-ref x 1) env params) (expand (list-ref x 2) env params)))

(define (expand x env params)
  (cond
    ((self-evaluating? x) x)
    ((identifier? x) (expand-identifier x env params))
    (else (expand-apply x env params))))

(define (expand-delayed x)
  (if (pair? x)
    (map-improper expand-delayed x)
    (if (and (procedure? x) (not (cfunction? x)))
      (expand-delayed (x))
      x)))

;; Expander entry point
(define (expand-toplevel x env)
  (expand-delayed (expand x env '())))

(define (define-transformer! x env trans-env params name body)
  (define expanded-body #f)
  (define fn #f)
  (define fn-arity #f)
  (define procedural-transformer #t)
  (define id-transformer #f)

  (if (not (identifier? name))
    (raise-source (cdr x) 'expand "define-syntax first argument (macro name) must be an identifier" (list x (cdr x))))

  (if (table? env)
    (if (table-ref env "module-export-all")
      (table-set! (table-ref env "module-exports") name #t)))

  (if (eq? (rename-strip (car body)) 'identifier-transformer)
    (begin
      (set! body (cadr body))
      (set! procedural-transformer #f)
      (set! id-transformer #t)))

  (if (eq? (rename-strip (car body)) 'combined-transformer)
    (begin
      (set! body (cadr body))
      (set! procedural-transformer #t)
      (set! id-transformer #t)))

  (set! expanded-body (expand-delayed (expand body env params)))

  (set! fn (eval expanded-body env))

  (if (not (procedure? fn))
    (raise-source (cddr x) 'expand "define-syntax body did not evaluate to a function" (list x)))

  (set-function-macro-env! fn trans-env)

  (set! fn-arity (function-min-arity fn))

  (if (fx< fn-arity 1)
    (raise-source (caddr x) 'expand "define-syntax body must evaluate to a function that takes at least one argument" (list x)))

  (set-function-name! fn (rename-strip name))

  (if procedural-transformer
    (set-function-macro-bit! fn))

  (if id-transformer
    (set-function-identifier-macro-bit! fn))

  (env-define env name fn #t)

  (if (top-level-value 'EXPANDER-PRINT #f)
    (print expanded-body))

  unspecified)

(define (expand-define-syntax x env params)
  (define len (length x))
  (define name #f)
  (define body #f)

  (if (fx< len 3)
    (raise-source x 'expand "define-syntax expects at least two arguments: a name and a body" (list x)))

  (set! name (cadr x))
  (set! body (caddr x))

  (define-transformer! x env env params name body))

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
(define (expand-let-syntax type x env params)
  (define new-env (env-make env #t))

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

      (define-transformer! x new-env (if (eq? type 'let-syntax) env new-env) params name body));
    bindings)

  (define result 
    (cons-source x (make-rename #f 'begin) (expand-map body new-env params)))

  ;; fold new-env into old-env
  ;(pretty-print new-env)
  (env-fold new-env)

  #;(expand result env)
  result)

;; Expand a macro application
(define (expand-macro x env transformer params)
  ;(define lookup (list-ref (env-lookup env (car x)) 2))
  (define arity (function-min-arity transformer))
  (define saved-rename-env (top-level-value '*current-rename-env*))
  (define saved-fn-name (top-level-value '*current-macro-name*))
  ;; #t if this is an application of an identifier-only macro...
  (define identifier-application? (and (not (identifier? x)) (function-identifier-macro? transformer) (not (function-procedural-macro? transformer))))
  (define form (if identifier-application? (car x) x))

  (set-top-level-value! '*current-rename-env* (function-env transformer))
  (set-top-level-value! '*current-macro-name* (function-name transformer))

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
        (set-top-level-value! '*current-rename-env* saved-rename-env)
        (set-top-level-value! '*current-macro-name* saved-fn-name)
        
        )))

  (if identifier-application?
    (cons-source x result (expand-map (cdr x) env params)) 
    (expand result env params)))

;; cond expander
(define (expand-cond-full-clause x env params clause rest)
  (if (or (null? clause) (not (list? clause)))
    (raise-source clause 'expand "cond clause should be a list" (list clause)))

  (define len (length clause))

  (define condition (car clause))
  (define body (cdr clause))

  (define new-params (cons '(disallow-defines #t) params))

  ;; Handle else clause
  (if (env-compare env condition (make-rename env 'else))
    (set! condition #t)
    (set! condition (expand condition env params)))

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
                (if (null? rest) unspecified (expand-cond-full-clause x env new-params (car rest) (cdr rest)))))
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
                                            (list-source x (expand (cadr body) env new-params) name)
                                            (if (null? rest)
                                              unspecified
                                              (expand-cond-full-clause x env new-params (car rest) (cdr rest))))
                                )
                   condition))
               (gensym 'result)))))
        ;; Otherwise it's a simple if
        (list-source x
          (make-rename #f 'if)
          condition
          (expand (cons-source x (make-rename #f 'begin) body) env new-params)
          (if (null? rest)
            unspecified
            (expand-cond-full-clause x env new-params (car rest) (cdr rest))))))
  )

  result)

(define (expand-cond-clause x env params clause rest)
  (if (null? clause)
    unspecified
    (expand-cond-full-clause x env params clause rest)))

(define (expand-cond x env params)
  (if (fx= (length x) 1)
    unspecified
    (expand-cond-clause x env params (cadr x) (cddr x))))
