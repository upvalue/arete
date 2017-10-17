;; compiler.scm - Arete bytecode compiler

;; Note: Internal structure is manipulated by C++ code and must be changed if anything is rearranged here
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
      ((push-constant) 1)
      ((return) 4)
      (else (raise 'compile "unknown named instruction" (list insn))))))

(define (fn-adjust-stack fn size)
  (compiler-log "stack +=" size)
  (OpenFn/stack-size! fn (fx+ (OpenFn/stack-size fn) size)))

(define (emit fn . insns)
  (case (car insns)
    ('push-constant (fn-adjust-stack fn 1)))

  ;(print insns)
  (for-each
    (lambda (insn) (vector-append! (OpenFn/insns fn) insn))
    insns))

(define (compile-constant fn env x)
  (emit fn 'push-constant (register-constant fn x)))

(define (compile-apply fn env x)
  #t)

(define (compile-identifier fn env x)
  #t)

(define (compile-expr fn env x)
  (cond
    ((self-evaluating? x) (compile-constant fn env x))
    ((eq? x 'abuja) (compile-identifier fn env x))
    ((list? x)
     (compile-apply fn env x))
    (else (raise 'compile "don't know how to compile expression" (list x))))

  fn)

; (print "123")
;; constants: #(print "123")
;; push-constant 0
;; get-global
;; push-constant 1
;; apply 2

(define (compile fn env body)
  (compiler-log "compiling body" body)
  (for-each 
    (lambda (x) (compile-expr fn env x))
    body)

  (emit fn 'return)

  fn)

(define (compile-finish fn)
  (let loop ((i 0))
    (unless (fx= i (vector-length (OpenFn/insns fn)))
      (vector-set! (OpenFn/insns fn) i (insn->byte (vector-ref (OpenFn/insns fn) i)))
      (loop (fx+ i 1)))))

(define fn (OpenFn/make "vm-function"))

(compile fn #f '(123))
;(compile fn #f '(print "Hello world"))

(print fn)

(compile-finish fn)

(print fn)

(define compiled-proc (OpenFn->procedure fn))
(print compiled-proc)
(print "result of execution" (compiled-proc))
