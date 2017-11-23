;; Analysis pass

;; What this does:

;; ((lambda (a)
;;   ((lambda (b)
;;     (set! a 5)
;;     (+ a b)) 10)) 5)

;; Becomes

;; (#<OpenFn env: (#<Binding name: #:a47 mutated: #t>)
;;   (#<OpenFn env: (#<Binding name: #:b47 mutated: #f>) body:
;;      (set! #<Binding name: #:a47 mutated: #t>))
;;      etc...

;; This allows the compiler to (a) inline these functions without causing a name conflict and (b) generate cheaper
;; OP_CLOSURE_REF instructions in the case that a variable is never mutated.

#|
(define (special-form=? x type)
  (and (pair? x) (rename? (car x)) (eq? (rename-env (car x)) #f) (eq? (rename-expr (car x)) type)))

;; (define a 5)
;; ((lambda (b) (+ a b))
;; Analysis pass does this
;; (#<OpenFn body: (+ #<Binding name: a depth: 0> #<Binding name: b depth: 1>)
;; When this is encountered during the code generation pass in the CAR position
;; We add the bindings from the sub-function to the function, and generate the code directly in that function.

;; A binding of depth 0 is a global, a binding of depth equal to current function is a local...

;; When inlining, we have to adjust all Vars which are shared in the source code, right?

;; Increase local-count by arguments of inlined function.
;; Adjust all variables > current location

;; This is the free-variable handling, it's necessarily somewhat complex

;; ((lambda (a) a) 5)

;; #<Binding name: a idx: 0>

;; (can we just replace function env entirely while compiling the body?)
;; ((lambda (a) a) 5)
;; ((lambda (a) (lambda (b) (+ a b)) 5) 5)

;; ((lambda (a) 5) 5)
;; Inline a function in the CAR. We compile it backwards: evaluate and local-set the arguments, then compile the
;; function body as though it were there.

;; What makes this hard: name conflicts. Our current upvalue handling is also built into the name lookup process.

;; Considering a name itself equivalent to a location is the source of some confusion

(define (analyze-lookup fn x src)
  (let loop ((search-fn fn))
    (if (eq? search-fn #f)
      x
      (if (OpenFn/toplevel? search-fn)
        (begin
          (unless (or (top-level-bound? x) (table-ref (OpenFn/env search-fn) x))
            (print-source src "Warning: reference to undefined global variable" x))
          x)
        (aif (table-ref (OpenFn/env search-fn) x)
          it
          (loop (OpenFn/parent search-fn)))))))

;; By the time compiler reaches VARs, the inlining decision is already made.
;; how does this effect upvalues and upvalue generation?

(define (analyze-expr fn x)
  (cond
    ((self-evaluating? x) x)
    ((identifier? x)
     (analyze-lookup fn x #f))

    ((special-form=? x 'define)
     (if (OpenFn/toplevel? fn)
       x
       (let* ((name (cadr x)) (var (AVar/make (OpenFn/local-count fn) name fn)))
         (table-set! (OpenFn/env fn) name var)
         (OpenFn/local-count! fn (fx+ (OpenFn/local-count fn) 1))
         (list-source x (car x) var (cddr x)))))

    ((special-form=? x 'set!)
     (let ((var (analyze-lookup fn (cadr x) #f)))
       (Var/mutated?! var #t)
       (cons-source x (car x) (cons-source x var (cddr x)))))
     
    ((special-form=? x 'lambda)
     (let* ((sub-fn (OpenFn/make (gensym 'lambda)))
            (args (cadr x))
            (arg-len (args-length args))
            (varargs (or (not (list? args)) (identifier? args))))
        (OpenFn/parent! sub-fn fn)
        (OpenFn/depth! sub-fn (if fn (fx+ (OpenFn/depth fn) 1) 0))
        (OpenFn/min-arity! sub-fn arg-len)
        (OpenFn/max-arity! sub-fn arg-len)
        (OpenFn/var-arity! sub-fn varargs)

        ;; Calculate argument count
        (OpenFn/local-count! sub-fn (if (identifier? args) 1 (fx+ arg-len (if (OpenFn/var-arity sub-fn) 1 0))))

        (unless (null? args)
          (if (identifier? args)
            ;; (lambda args args)
            (table-set! (OpenFn/env sub-fn) args (Var/make 0 args))

            ;; Handle arguments, including varargs
            (let loop ((rest (cdr args))
                       (item (car args))
                       (i 0))
              (table-set! (OpenFn/env sub-fn) item (AVar/make i item sub-fn ))
              (unless (null? rest)

                (if (pair? rest)
                  (loop (cdr rest) (car rest) (+ i 1))
                  ;; We've hit the varargs
                  (begin
                    (table-set! (OpenFn/env sub-fn) rest (AVar/make (+ i 1) rest sub-fn))))))))

        (OpenFn/body! sub-fn (analyze-body sub-fn (cddr x)))

        sub-fn))
    (else 
      (map (lambda (sub-x) (analyze-expr fn sub-x)) x)
      )))

(define (analyze-body fn body)
  (map (lambda (sub-x) (analyze-expr fn sub-x)) body))

(define (analyze-toplevel body)
  (define fn (OpenFn/make 'vm-toplevel))
  (OpenFn/toplevel?! fn #t)

  (scan-defines fn body)

  (OpenFn/body! fn (analyze-body fn body))

  fn)
|#

(define-library (arete analysis)
  (import (arete))
  (begin

(define-record Location name idx mutated?)
(define-record Env parent bindings)

(define %Env/make Env/make)
(define (Env/make parent)
  (%Env/make parent (make-table)))

(define (Env/add-location! env name)
  (define location (Location/make name (table-entries (Env/bindings env)) #f))
  (table-set! (Env/bindings env) name location)
  location)

(define (Env/lookup env name)
  (if (eq? env #f)
    name
    (aif (table-ref (Env/bindings env) name)
      it
      (Env/lookup (Env/parent env) name))))

(define (analyze-lambda env x)
  (define arguments (cadr x))

  (define sub-env (Env/make env))

  (define new-arguments
    (if (pair? arguments)
      (map-improper 
        (lambda (x)
          (Env/add-location! sub-env x))
        arguments)
      (Env/add-location! sub-env x)))

  (print (cddr x))

  (cons-source x (car x) (cons-source x new-arguments (map (lambda (x) (analyze-expr sub-env x)) (cddr x)))))

(define (analyze-set env x)
  (let ((location (Env/lookup env (cadr x))))
    (when (Location? location)
      (Location/mutated?! location #t))

    (cons-source x (car x) (cons-source x location (map (lambda (x) (analyze-expr env x)) (cddr x))))))


;; Most expressions just need their CDR analyzed for occurences of identifiers
(define (analyze-recons env x)
  (cons-source x (car x) (map analyze-expr (cdr x))))

(define (analyze-expr env x)
  (cond
    ((pair? x)
     (print (car x))
     (case (rename-strip (car x))
       (lambda (analyze-lambda env x))
       ((and or) (analyze-recons env x))
       ((set!) (analyze-set env x))
       (else (map (lambda (x) (analyze-expr env x)) x))))
    ((identifier? x) 
     (Env/lookup env x))

    (else x)))

(define (analyze-toplevel code)
  (analyze-expr #f code))

(begin
  (print (analyze-toplevel '(lambda (a b) a b (set! a #t) b))))

))
