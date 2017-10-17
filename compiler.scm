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
  )

(set! OpenFn/make
  (let ((make OpenFn/make))
    (lambda (name)
      (make name #() #() #() 0 0))))

(define-record Var
  name
  upvalue?)

(set! Var/make
  (let ((make Var/make))
    (lambda (name)
      (make name #f #f))))

(define (compiler-log . rest)
  (display "arete:cc: ")
  (if (or #t (top-level-value '*compiler-log*))
    (apply print rest)))

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
      (else (raise 'compile "unknown named instruction" (list insn))))))

(define (fn-adjust-stack fn size)
  (compiler-log "stack +=" size)
  (OpenFn/stack-size! fn (fx+ (OpenFn/stack-size fn) size)))

;(set-top-level-value! '*expander-print* #t)

(define (emit fn . insns)
  (fn-adjust-stack fn 
    (case (car insns)
      ((push-constant global-get) 1)
      ;; Remove arguments from stack, but push a single result
      ((apply apply-tail) (fx+ (fx- (cadr insns)) 1))
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

(define (compile-constant fn env x)
  (emit fn 'push-constant (register-constant fn x)))

(define (compile-apply fn env x tail?)
  (define stack-check #f)
  (define argc (length (cdr x)))
  ;; (print) => OP_GLOBAL_GET 'print OP_APPLY 0
  (compile-expr fn env (car x) tail?)

  (set! stack-check (OpenFn/stack-size fn))

  (for-each
    (lambda (x)
      (compile-expr fn env x #f))
    (cdr x))

  (unless (eq? (fx- (OpenFn/stack-size fn) argc) stack-check)
    (raise 'compile "stack size does not reflect function arguments" (list fn x)))
  
  (emit fn (if tail? 'apply-tail 'apply) (length (cdr x)))
  )

(define (compile-identifier fn env x)
  (emit fn 'global-get (register-constant fn x))
)

(define (compile-expr fn env x tail?)
  (print tail?)
  (cond
    ((self-evaluating? x) (compile-constant fn env x))
    ((identifier? x) (compile-identifier fn env x))
    ((list? x)
     (compile-apply fn env x tail?))
    (else (raise 'compile "don't know how to compile expression" (list x))))

  fn)

; (print "123")
;; constants: #(print "123")
;; push-constant 0
;; get-global
;; push-constant 1
;; apply 2

(define (compile fn env body)
  (define end (fx- (length body) 1))
  (compiler-log "compiling body" body)
  (for-each-i
    (lambda (i x)
      (compile-expr fn env x (fx= i end)))
    body)

  (emit fn 'return)

  fn)

(define (compile-finish fn)
  (let loop ((i 0))
    (unless (fx= i (vector-length (OpenFn/insns fn)))
      (vector-set! (OpenFn/insns fn) i (insn->byte (vector-ref (OpenFn/insns fn) i)))
      (loop (fx+ i 1)))))

(define fn (OpenFn/make "vm-function"))

(compile fn #f '((print "do re mi" "fa so la" "ti do")))
(compile-finish fn)
;(compile fn #f '(print "Hello world"))

(print fn)

(print fn)

(define compiled-proc (OpenFn->procedure fn))
(print compiled-proc)
(print "result of execution" (compiled-proc))

