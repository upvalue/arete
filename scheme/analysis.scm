;; analyze.scm - Analysis pass

;; simple table-based analysis is possible now that we perform alpha-conversion in the expander

;; for example
#;(define (thing)
  (define closure-var-1 #t)
  (define closure-var-2 #t)
  (list
    (lambda ()
      closure-var-1)
    (lambda (n)
      (set! closure-var-2 n)

      (lambda () 
        closure-var-2))))

;; okay, say we go through this
;; we can have a table that contains something like

;; closure-var-1-depth 0
;; closure-var-1-modified #f
;; closure-var-2-depth 0
;; closure-var-2-modified #t

;; closure conversion

#;(define (thing)
  (define closure-var-1 #t)
  (list
    ($make-closure
      (lambda () closure-var-1)
      ($local closure-var-1))
    ($make-closure
      (lambda ()

        
        closure-var-2)
      ($local-upvalue closure-var-2))))

;; TODO Can we do closure generation here without too much trouble?

;; closure sharing
;; (define (make-counter)
;;   (define count 0)
;;   (list
;;      (lambda () count)
;;      (lambda () (set! count (+ count 1))))

;; There is no need to create a separate closure for these lambdas. What we can do is create a closure using some
;; method that indicates where the closure variables come from (the same stuff we currently emit for OP_CLOSE_OVER)
;; and then compare them with equal? and member. We'll store that closure in a local variable after creating it,
;; and pass it to the second lambda on creation. Less expensive OP_CLOSE_OVER operations, less memory usage.

;; A quick & dirty analysis of the source code indicates that this situation only occurs once, so it may not be 
;; as worthwhile as thought. And in any case, this doesn't handle the case where two sub-functions capture
;; an equivalent closure:

;; (let ((a 5))
;;   (let () a)
;;   (let () (let () a)))

;; Anyway, the goal of this is to (1) Replace OP_UPVALUE_GET and OP_UPVALUE_SET with a new OP_CLOSURE_GET
;; and OP_CLOSURE_SET in the case of variables which are not mutated and 
;; (2) allow the inlining of functions in the application position i.e.

;; ((lambda (a) a) 5)
;; becomes
;; 5 OP-LOCAL-SET a
;; OP-LOCAL-GET a
;; So the evaluation of arguments needs to be placed first and converted into local variable usages
;; How to handle rest arguments :/
;; For now we could just not inline those functions. Let & friends won't introduce them in any case, right? That
;; sort of function is much rarer

;; So, when we find an application in the CAR position of another application, we will inline it by (1) subsuming
;; its argument Locations into the current OpenFn (2) compiling the arguments FIRST and generating OP_LOCAL_SETs as
;; necessary

(define-record Location name unique idx mutated?)
(define-record Env parent bindings)
(define-record AnalyzedFn env arity var-arity body)

(define %Env/make Env/make)
(define (Env/make parent)
  (%Env/make parent (make-table)))

(define (Env/add-location! env name)
  (if (eq? env #f)
    name
    (begin
      (define location (Location/make name name (table-entries (Env/bindings env)) #f))
      (table-set! (Env/bindings env) name location)
      location)))

(define (Env/lookup env name)
  (if (eq? env #f)
    name
    (aif (table-ref (Env/bindings env) name)
      it
      (Env/lookup (Env/parent env) name))))

;; This function gives us the number of proper arguments to a function
(define (args-length x)
  (if (or (identifier? x) (null? x))
    0
    (let loop ((rest (cdr x))
               (i 1))
      (if (or (null? rest) (not (pair? rest)))
        i
        (loop (cdr rest) (fx+ i 1))))))

;; subsume-locals.


(define (analyze-lambda env x)
  (define arguments (cadr x))
  (define sub-env (Env/make env))
  (define fn (AnalyzedFn/make sub-env 0 #f #f))

  (unless (null? arguments)
    (if (identifier? arguments)
      (begin
        (Env/add-location! sub-env arguments)
        (AnalyzedFn/arity! fn 0)
        (AnalyzedFn/var-arity! fn #t))
      (let loop ((rest (cdr arguments))
                 (item (car arguments))
                 (i 0))
        (Env/add-location! sub-env item)
        (if (null? rest)
          (AnalyzedFn/arity! fn i)
          (if (pair? rest)
            (loop (cdr rest) (car rest) (fx+ i 1))
            (begin
              (AnalyzedFn/arity! fn (fx+ i 1))
              ;; Improper list variable arguments
              (Env/add-location! sub-env rest)
              (AnalyzedFn/var-arity! fn #t)))))))

  (AnalyzedFn/body! fn (analyze-expr sub-env (cddr x)))

  fn
  )

(define (analyze-set env x)
  (let ((location (Env/lookup env (cadr x))))
    (when (Location? location)
      (Location/mutated?! location #t))

    (cons-source x (car x) (cons-source x location (map (lambda (x) (analyze-expr env x)) (cddr x))))))

(define (analyze-define env x)
  (define loc (Env/add-location! env (cadr x)))
  (cons-source x (car x) (cons-source x loc (cddr x))))

;; Most expressions just need their CDR analyzed for occurences of identifiers
(define (analyze-recons env x)
  (cons-source x (car x) (map1 (lambda (sub-x) (analyze-expr env sub-x)) (cdr x))))

(define (analyze-expr env x)
  (cond
    ((pair? x)
     (case (rename-strip (car x))
       (lambda (analyze-lambda env x))
       (define (analyze-define env x))
       (quote x)
       ((and or if begin) (analyze-recons env x))
       ((set!) (analyze-set env x))
       (else (map1 (lambda (x) (analyze-expr env x)) x))))
    ((identifier? x) 
     (Env/lookup env x))

    (else x)))

;; Problem with communication here. It'd be much better if we could avoid doing some of these things repeatedly
;; e.g. scanning defines. Perhaps this could return an AnalyzedFn in place of lambdas with that already taken care of.

(define (analyze-toplevel code)
  (analyze-expr #f code))

;; (analyze-toplevel '(lambda () (define (internal2) (internal1)) (define (internal1) (internal2))))

(define qq-list
  '(lambda (c lst) 
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
        (set! c #t)
        (if (null? lst) 
          '()
          (list #'quote lst))))))

;; TODO: If we want to compile stuff as blocks, how do we inline stuff at the module level?

(begin
  (pretty-print (expand-toplevel qq-list (top-level-value '*core-module*)))
  (pretty-print (analyze-toplevel (expand-toplevel qq-list (top-level-value '*core-module*))))
  )
