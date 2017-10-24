;; compiler.scm - Arete bytecode compiler

;; DONE Closures (via upvalues)
;; TODO define
;; TODO set!
;; TODO if
;; TODO and/or
;; TODO Check lambda body
;; TODO Error reporting

;; Note: Internal structure is manipulated by openfn_to_procedure in builtins.cpp and that must be updated
;; if anything is rearranged here
(define-record OpenFn
  ;; A name or description for a function
  name ;; 0
  ;; A vector of instructions (symbols and fixnums)
  insns ;; 1
  ;; A vector of constants
  constants ;; 2
  ;; A vector of source-locations,  
  sources  ;; 3
  ;; Current stack size of the function
  stack-size ;; 4
  ;; Number of local variables in the function
  local-count ;; 5
  ;; Maximum stack size
  stack-max ;; 6
  ;; The function this function was defined in, or #f for toplevel functions
  parent ;; 7
  ;; Environment, table mapping symbols to Var structures
  env ;; 8
  ;; Minimum arguments to function
  min-arity ;; 9
  ;; Maximum arguments to function; ignored if var-arity is #t
  max-arity ;; 10
  ;; #t if function is variable arity
  var-arity ;; 11
  ;; Depth of the function
  depth ;; 12
  ;; If this is a closure, this will be a vector of upvalue locations e.g.
  ;; #(#t 0 #f 0) = capture local variable 0 from the calling function, capture enclosed value 0 from the calling
  ;; functions closure
  closure ;; 13
  ;; Free variable count
  free-variable-count ;; 14
  ;; Vector of free variables
  free-variables ;; 15
  )

(set! OpenFn/make
  (let ((make OpenFn/make))
    (lambda (name)
      (make name (make-vector) (make-vector) (make-vector) 0 0 0 #f (make-table) 0 0 0 0 #f 0 #f))))

(define-record Var
  idx
  name
  ;; If #t, this is a reference to a free variable
  upvalue?
  ;; If not #f, index in f.fn->upvalues. Mutually exclusive with upvalue?
  free-variable-id)

(set! Var/make
  (let ((make Var/make))
    (lambda (id name)
      (make id name #f #f))))

(define (compiler-log fn . rest)
  (when (eq? (top-level-value '*compiler-log*) #t)
    (display "arete:cc: ")
    (let loop ((i 0))
      (unless (fx= i (OpenFn/depth fn))
        (display ">")
        (loop (fx+ i 1))))

    (apply pretty-print rest)))

(define (register-constant fn const)
  ;; todo duplicates
  (vector-append! (OpenFn/constants fn) const)
  (fx- (vector-length (OpenFn/constants fn)) 1))

;; bad = 0
;; push-constant = 1
;; local-get = 2
;; local-set = 3
;; return = 4

(define (insn->byte insn)
  (if (fixnum? insn)
    insn
    (case insn
      (push-constant 1)
      (global-get 2)
      (global-set 3)
      (return 4)
      (apply 5)
      (apply-tail 6)
      (local-get 7)
      (local-set 8)
      (upvalue-get 9)
      (upvalue-set 10)
      (close-over 11)
      (else (raise 'compile "unknown named instruction" (list insn))))))

(define (max a b) (if (< a b) b a))

(define (fn-adjust-stack fn size)
  (define new-size (fx+ (OpenFn/stack-size fn) size))
  (compiler-log fn "stack +=" size "=" new-size)
  (OpenFn/stack-size! fn new-size)
  (OpenFn/stack-max! fn (max new-size (OpenFn/stack-max fn)))
)

(define (emit fn . insns)
  (fn-adjust-stack fn 
    (case (car insns)
      ((push-constant global-get local-get upvalue-get) 1)
      ((upvalue-set local-set global-set) 2)
      ;; Remove arguments from stack, but push a single result
      ((apply apply-tail) (fx- (cadr insns)))
      ((words close-over return) 0)
      (else (raise 'compile (print-string "unknown instruction" (car insns)) (list fn (car insns) insns)))
    ))

  ;; Stack size sanity check
  (when (fx< (OpenFn/stack-size fn) 0)
    (raise 'compile "stack size went below zero" (list fn (OpenFn/stack-size fn))))

  ;(print insns)
  (for-each
    (lambda (insn) 
      ;; Words isn't a real instruction, just an argument to emit that means we don't need to check
      ;; stack invariants
      (unless (eq? insn 'words)
        (vector-append! (OpenFn/insns fn) insn)))
    insns))

(define (compile-constant fn x)
  (emit fn 'push-constant (register-constant fn x)))

(define (fn-push-source fn x)
  (aif (list-get-source x)
    (vector-append! (OpenFn/sources fn) it)))

(define (compile-apply fn x tail?)
  (define stack-check #f)
  (define argc (length (cdr x)))
  ;; (print) => OP_GLOBAL_GET 'print OP_APPLY 0
  (compile-expr fn (car x) (and (eq? argc 1) tail?))

  (set! stack-check (OpenFn/stack-size fn))

  (for-each-i
    (lambda (i x)
      (compile-expr fn x (and (eq? argc (fx- i 1)) tail?)))
    (cdr x))

  (unless (eq? (fx- (OpenFn/stack-size fn) argc) stack-check)
    (raise 'compile "stack size does not reflect function arguments" (list fn x)))
  
  (emit fn (if tail? 'apply-tail 'apply) (length (cdr x)))
  )

;; TODO> Cannot use a macro defined in advance. Toplevel needs to behave like letrec-syntax, I think.

(define-syntax aif
  (lambda (x)
    (unless (or (fx= (length x) 4) (fx= (length x) 3))
      (raise 'syntax "aif expects three or four arguments" (list x)))

    `(,#'let ((it ,(list-ref x 1)))
        (,#'if it ,(list-ref x 2) ,(if (fx= (length x) 4) (list-ref x 3) unspecified)))))

(define (register-free-variable fn x)
  ;; Go up through function stack,
  ;; capturing X as necessary
  (unless (OpenFn/closure fn)
    (OpenFn/closure! fn (make-vector)))

  (let ((parent-fn (OpenFn/parent fn))
        (closure (OpenFn/closure fn)))

    (aif (table-ref (OpenFn/env parent-fn) x)
      ;; Found it
      (let ((var (Var/make 0 x)))
        (compiler-log fn (OpenFn/name fn) "registered free variable" x)
        ;; Calculate index in closure from closure length
        (Var/idx! var (fx/ (vector-length closure) 2))
        ;; This is an upvalue
        (Var/upvalue?! var #t)
        ;; Add to function environment
        (table-set! (OpenFn/env fn) x var)
        ;; Append to closure
        (unless (Var/upvalue? it)
          (OpenFn/free-variable-count! parent-fn (fx+ (OpenFn/free-variable-count parent-fn) 1))
          (Var/free-variable-id! it (fx- (OpenFn/free-variable-count parent-fn) 1)))
        (vector-append! closure it)
        #;(vector-append! closure (Var/idx it)))
      (register-free-variable parent-fn x))))


(define (fn-lookup fn x)
  (let loop ((search-fn fn))
    (if (eq? search-fn #f)
      (cons 'global x)
      (aif (table-ref (OpenFn/env search-fn) x)
        (if (and (eq? fn search-fn) (not (Var/upvalue? it)))
          (cons 'local (Var/idx it))
          ;; This is an upvalue
          (if (eq? fn search-fn)
            ;; This upvalue has already been added to the closure
            (begin
              (cons 'upvalue (Var/idx it)))
            (begin
              (register-free-variable fn x)
              (fn-lookup fn x))))
        (loop (OpenFn/parent search-fn))))))

(define (compile-identifier fn x)
  (define result (fn-lookup fn x))

  (compiler-log fn "compiling identifier" result)

  (case (car result)
    (local (emit fn 'local-get (cdr result)))
    (global (emit fn 'global-get (register-constant fn x)))
    (upvalue (emit fn 'upvalue-get (cdr result)))
    (else (raise 'compiler ":(" (list x))))
)

;; This gives us the number of proper arguments to a function
(define (args-length x)
  (if (or (identifier? x) (null? x))
    0
    (let loop ((rest (cdr x))
               (i 1))
      (if (or (null? rest) (not (pair? rest)))
        i
        (loop (cdr rest) (fx+ i 1))))))

(define (compile-lambda fn x)
  ;; So compile lambda has to recurse into the body of the lambda;
  ;; it creates an additional OpenFN with this as a parent
  (define sub-fn (OpenFn/make (gensym 'lambda)))
  (define args (cadr x))
  (define arg-len (args-length args))
  (define varargs (or (not (list? args)) (identifier? args)))

  (OpenFn/depth! sub-fn (fx+ (OpenFn/depth fn) 1))
  (compiler-log sub-fn (OpenFn/name sub-fn))
  (OpenFn/parent! sub-fn fn)
  (compiler-log fn "parent of " (OpenFn/name sub-fn) " is " (OpenFn/name fn))

  (OpenFn/min-arity! sub-fn arg-len)
  ;; TODO optional arguments
  (OpenFn/max-arity! sub-fn arg-len)
  (OpenFn/var-arity! sub-fn (or (not (list? args)) (identifier? args)))
  ;; Calculate local count
  (OpenFn/local-count! sub-fn (if (identifier? args) 1 (fx+ arg-len (if (OpenFn/var-arity sub-fn) 1 0))))

  (if (identifier? args)
    (raise 'compile "can't handle varargs" (list x)))

  (for-each-i
    (lambda (i x)
      (table-set! (OpenFn/env sub-fn) x (Var/make i x))
      #;(OpenFn/local-count! sub-fn (fx+ i 1)))
    args)

  (compile sub-fn (cddr x))
  ;(pretty-print sub-fn)
  (compile-finish sub-fn)

  (emit fn 'push-constant (register-constant fn (OpenFn->procedure sub-fn)))

  ;; If this is a closure, we emit an instruction
  ;; That will create a closure at runtime out of the compiled function
  (aif (OpenFn/closure sub-fn)
    (begin
      (emit fn 'close-over (vector-length it))
      (let loop ((i 0))
        (unless (eq? i (vector-length it))
          (let ((var (vector-ref it i)))
            (if (Var/upvalue? var)
              (emit fn 'words 1 (Var/idx var))
              (emit fn 'words 0 (Var/free-variable-id var))))
          (loop (fx+ i 1))))))


  ;(pretty-print fn)
)

(define (compile-define fn x)
  (define name (cadr x))
  (define result (fn-lookup fn name))

  (compiler-log fn "compiling define" result)

  (compile (list-ref x 2))

  (case (car result)
    (global (emit fn 'global-set (register-constant fn x))))

)

(define (compile-special-form fn x type tail?)
  (compiler-log fn "compiling special form" type x)
  (case type
    (lambda (compile-lambda fn x))
    (define (compile-define fn x))))

(define (special-form x)
  (when (rename? x)
    (if (rename-env x) 
      (raise 'compile "compiler encountered non-toplevel rename" (list x)))

    (set! x (rename-expr x)))

  (if (memq x '(lambda)) x #f))

(define (compile-expr fn x tail?)
  (compiler-log fn "compiling expr" x)
  (cond
    ((self-evaluating? x) (compile-constant fn  x))
    ((identifier? x) (compile-identifier fn  x))
    ((list? x)
     (aif (special-form (car x))
       (compile-special-form fn x it tail?)
       (compile-apply fn x tail?)))
    (else (raise 'compile "don't know how to compile expression" (list x))))
  fn)

(define (compile fn body)
  (define end (fx- (length body) 1))
  (compiler-log fn "compiling body" body)
  (for-each-i
    (lambda (i x)
      (compile-expr fn x (fx= i end)))
    body)

  (emit fn 'return)

  fn)

(define (compile-finish fn)
  (table-for-each 
    (lambda (_ var)
      (when (Var/free-variable-id var)
        (unless (OpenFn/free-variables fn)
          (OpenFn/free-variables! fn (make-vector)))

        (compiler-log fn "has free variable" (Var/name var) " at local idx " (Var/idx var))

        (vector-append! (OpenFn/free-variables fn) (Var/idx var))))
    (OpenFn/env fn))

  (let loop ((i 0))
    (unless (fx= i (vector-length (OpenFn/insns fn)))
      (vector-set! (OpenFn/insns fn) i (insn->byte (vector-ref (OpenFn/insns fn) i)))
      (loop (fx+ i 1)))))

;; Do the thing.
(define fn (OpenFn/make 'vm-toplevel))

    ; this should not result in a tail-call twice (((lambda (a) (lambda () a)) #t))
#;(define fn-body
  '(
    ((lambda (a e g y)
      (lambda (d c) (lambda () (fx+ a c y)))) 2 0 0 2)
    #;(lambda (a) (lambda (b) (lambda () (fx+ a b))))
  )
)

(define fn-body
  '(
    (((lambda (a) (lambda () a)) #t))
  ))


;(print fn-body)
(compile fn fn-body)

;(pretty-print fn)
(compile-finish fn)

(define compiled-proc (OpenFn->procedure fn))

