;; compiler.scm - Arete bytecode compiler

;; Note: Internal structure is manipulated by openfn_to_procedure in builtins.cpp and that must be updated
;; if anything is rearranged here
(define-record OpenFn
  name ;; 0
  insns ;; 1
  constants ;; 2
  sources  ;; 3
  stack-size ;; 4
  local-count ;; 5
  stack-max ;; 6
  parent ;; 7
  env ;; 8
  min-arity ;; 9
  max-arity ;; 10
  var-arity ;; 11
  )

(set! OpenFn/make
  (let ((make OpenFn/make))
    (lambda (name)
      (make name (make-vector) (make-vector) (make-vector) 0 0 0 #f (make-table) 0 0 0))))

(define-record Var
  id
  name
  upvalue?)

(set! Var/make
  (let ((make Var/make))
    (lambda (id name)
      (make id name #f))))

(define (compiler-log . rest)
  (display "arete:cc: ")
  (if (or #t (top-level-value '*compiler-log*))
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
      (else (raise 'compile "unknown named instruction" (list insn))))))

(define (max a b) (if (< a b) b a))

(define (fn-adjust-stack fn size)
  (define new-size (fx+ (OpenFn/stack-size fn) size))
  (compiler-log "stack +=" size "=" new-size)
  (OpenFn/stack-size! fn new-size)
  (OpenFn/stack-max! fn (max new-size (OpenFn/stack-max fn)))
)

(define (emit fn . insns)
  (fn-adjust-stack fn 
    (case (car insns)
      ((push-constant global-get local-get) 1)
      ;; Remove arguments from stack, but push a single result
      ((apply apply-tail) (fx- (cadr insns)))
      (return 0)
      (else (raise 'compile "unknown instruction" (list fn insns)))
    ))

  ;; Stack size sanity check
  (when (fx< (OpenFn/stack-size fn) 0)
    (raise 'compile "stack size went below zero" (list fn (OpenFn/stack-size fn))))

  ;(print insns)
  (for-each
    (lambda (insn) (vector-append! (OpenFn/insns fn) insn))
    insns))

(define (compile-constant fn x)
  (emit fn 'push-constant (register-constant fn x)))

(define (compile-apply parent fn x tail?)
  (define stack-check #f)
  (define argc (length (cdr x)))
  ;; (print) => OP_GLOBAL_GET 'print OP_APPLY 0
  (compile-expr parent fn (car x) tail?)

  (set! stack-check (OpenFn/stack-size fn))

  (for-each
    (lambda (x)
      (compile-expr parent fn  x #f))
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

(aif #f #t (print "AIF"))

(define (fn-lookup fn x)
  (let loop ((search-fn fn))
    (if (eq? search-fn #f)
      (cons 'global x)
      (aif (table-ref (OpenFn/env fn) x)
        (cons
          (if (eq? fn search-fn) 'local 'upvalue)
          (Var/id it))))))

(define (compile-identifier parent fn x)
  (define result (fn-lookup fn x))

  (compiler-log "compile ye identifier" result)

  (case (car result)
    (local (emit fn 'local-get (cdr result)))
    (global (emit fn 'global-get (register-constant fn x)))
    (else (raise 'compiler ":(" (list x))))
)

(define (special-form x)
  (when (rename? x)
    (if (rename-env x) 
      (raise 'compile "compiler encountered non-toplevel rename" (list x)))

    (set! x (rename-expr x)))

  (if (memq x '(lambda)) x #f))


;; This gives us the number of proper arguments to a function
(define (args-length x)
  (if (or (identifier? x) (null? x))
    0
    (let loop ((rest (cdr x))
               (i 1))
      (if (or (null? rest) (not (pair? rest)))
        i
        (loop (cdr rest) (fx+ i 1))))))

(define (compile-lambda parent fn x)
  ;; So compile lambda has to recurse into the body of the lambda;
  ;; it creates an additional OpenFN with this as a parent
  (define sub-fn (OpenFn/make (gensym 'lambda)))
  (define args (cadr x))
  (define arg-len (args-length args))
  (define varargs (or (not (list? args)) (identifier? args)))

  (OpenFn/parent! sub-fn fn)
  (OpenFn/min-arity! sub-fn arg-len)
  ;; TODO optional arguments
  (OpenFn/max-arity! sub-fn arg-len)
  (OpenFn/var-arity! sub-fn (or (not (list? args)) (identifier? args)))
  ;; (lambda rest 3)
  ;; Calculate local count
  (OpenFn/local-count! sub-fn (if (identifier? args) 1 (fx+ arg-len (if (OpenFn/var-arity sub-fn) 1 0))))
  (print "hello")

  (if (identifier? args)
    (raise 'compile "can't handle varargs" (list x)))

  (for-each-i
    (lambda (i x)
      (table-set! (OpenFn/env sub-fn) x (Var/make i x))
      (OpenFn/local-count! sub-fn i))
    args)

  ;(pretty-print "subfun" sub-fn)

  (compile fn sub-fn (cddr x))
  ;(compiler-log "subfunction" sub-fn)
  (compile-finish sub-fn)

  (emit fn 'push-constant (register-constant fn (OpenFn->procedure sub-fn))))

(define (compile-special-form parent fn x type tail?)
  (compiler-log "compiling special form" type x)
  (case type
    (lambda (compile-lambda parent fn x))))

(define (compile-expr parent fn x tail?)
  (compiler-log "compiling expr" x)
  (cond
    ((self-evaluating? x) (compile-constant fn  x))
    ((identifier? x) (compile-identifier parent fn  x))
    ((list? x)
     (aif (special-form (car x))
       (compile-special-form parent fn x it tail?)
       (compile-apply parent fn x tail?)))
    (else (raise 'compile "don't know how to compile expression" (list x))))

  fn)

(define (compile parent fn body)
  (define end (fx- (length body) 1))
  (compiler-log "compiling body" body)
  (for-each-i
    (lambda (i x)
      (compile-expr parent fn x (fx= i end)))
    body)

  (emit fn 'return)

  fn)

(define (compile-finish fn)
  (let loop ((i 0))
    (unless (fx= i (vector-length (OpenFn/insns fn)))
      (vector-set! (OpenFn/insns fn) i (insn->byte (vector-ref (OpenFn/insns fn) i)))
      (loop (fx+ i 1)))))

;; Do the thing.
(define fn (OpenFn/make 'vm-toplevel))

(compile #f fn '(
  ((lambda (a) a) 12345))
)

(pretty-print fn)
;(print fn)
(compile-finish fn)

;(pretty-print fn)

;(print fn)

(define compiled-proc (OpenFn->procedure fn))
(pretty-print fn)
(print (vector-ref (OpenFn/constants fn) 0))
(print (compiled-proc))
;(define subproc (compiled-proc))
;(print subproc)
;(print "result of execution" ((compiled-proc) 1))

