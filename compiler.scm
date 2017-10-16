;; compiler.scm - Arete bytecode compiler

;; Note: Internal structure is manipulated by C++ code and must be changed if anything is rearranged
(define-record OpenFn
  name
  insns
  constants
  sources
  stack-size
  local-count)

;; bad = 0
;; push-constant = 1
;; local-get = 2
;; local-set = 3
;; return = 4

(define (compiler-log . rest)
  (if (or #t (top-level-value '*compiler-log*))
    (apply print rest)))

(set! OpenFn/make
  (let ((make OpenFn/make))
    (lambda (name)
      (make name #() #() #() 0 0))))

(define (register-constant fn const)
  ;; todo duplicates
  (vector-append! (OpenFn/constants fn) const)
  (fx- (vector-length (OpenFn/constants fn)) 1))

(define (insn->byte insn)
  (if (fixnum? insn)
    insn
    (case insn
      ((push-constant) 1)
      ((return) 4)
      (else (raise 'compile "unknown named instruction" (list insn))))))

(define (emit fn . insns)
  ;(print insns)
  (for-each
    (lambda (insn) (vector-append! (OpenFn/insns fn) insn))
    insns))

(define (compile-constant fn env x)
  (emit fn 'push-constant (register-constant fn x)))

(define (compile-expr fn env x)
  (cond
    ((self-evaluating? x) (compile-constant fn env x))
    (else (raise 'compile "don't know how to compile expression" (list x))))

  fn)

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

(compile fn #f '(#t))

(print fn)

(compile-finish fn)

(print fn)


;; So this openFN has to be passed to some kind of function so that it can then be executed

;; (OpenFn->real-function)

;; 


(print fn)

;(print (compile (OpenFn/make "vm-function") #f #t))

;(compile #f #f #t)
