;; optimizer.scm - optional source-to-source optimizer for expanded forms

;; Global kill switch for the optimizer. Individual rewrites have their own
;; flags so a suspect optimization can be disabled without losing the whole pass.
(define (set-top-level-default! name value)
  (when (eq? (top-level-value name unspecified) unspecified)
    (set-top-level-value! name value)))

(set-top-level-default! 'COMPILER-OPTIMIZE #t)
(set-top-level-default! 'OPTIMIZER-INLINE-CAR #t)
(set-top-level-default! 'COMPILER-OPTIMIZER-LOG #f)

(define-record OptimizerConfig
  optimize?
  inline-car?
  log?)

(define (optimizer-config-from-toplevel)
  (OptimizerConfig/make
    (eq? (top-level-value 'COMPILER-OPTIMIZE #t) #t)
    (and (eq? (top-level-value 'OPTIMIZER-INLINE-CAR #t) #t)
         ;; Compatibility with the old compiler-side direct-lambda inline flag.
         (eq? (top-level-value 'COMPILER-INLINE-CAR #t) #t))
    (eq? (top-level-value 'COMPILER-OPTIMIZER-LOG #f) #t)))

(define (optimizer-log config . rest)
  (when (OptimizerConfig/log? config)
    (display "arete:opt: ")
    (apply pretty-print rest)))

(define (optimizer-head-name x)
  (if (and (pair? x) (identifier? (car x)))
    (rename-strip (car x))
    #f))

(define (optimizer-lambda-form? x)
  (and (pair? x)
       (eq? (optimizer-head-name x) 'lambda)
       (pair? (cdr x))
       (list? (cadr x))))

;; Preserve the named-let expansion pattern for the compiler's existing
;; labeled-loop lowering. This should move into the optimizer later as its own
;; rewrite, but this first slice only extracts direct lambda-call inlining.
(define (optimizer-named-loop-form? x)
  (and (list? x)
       (pair? x)
       (optimizer-lambda-form? (car x))
       (null? (cadar x))
       (null? (cdr x))
       (let ((body (cddar x)))
         (and (list? body)
              (fx= (length body) 2)
              (let ((def (car body))
                    (call (cadr body)))
                (and (pair? def)
                     (eq? (optimizer-head-name def) 'define)
                     (fx= (length def) 3)
                     (identifier? (cadr def))
                     (optimizer-lambda-form? (caddr def))
                     (pair? call)
                     (identifier? (car call))
                     (identifier=? (cadr def) (car call))))))))

(define (optimizer-make-inline-let src args vals body)
  (cons-source src '##arete#inline-let
    (cons-source src args
      (cons-source src vals body))))

(define (optimizer-inline-lambda-call config x in-lambda?)
  (let* ((lambda-form (car x))
         (args (cadr lambda-form))
         (body (cddr lambda-form))
         (vals (cdr x)))
    (optimizer-log config "inline direct lambda call" x)
    (optimizer-make-inline-let
      x
      args
      (map1 (lambda (v) (optimizer-expr config v in-lambda?)) vals)
      (map1 (lambda (b) (optimizer-expr config b #t)) body))))

(define (optimizer-lambda config x)
  (cons-source x (car x)
    (cons-source (cdr x) (cadr x)
      (map1 (lambda (b) (optimizer-expr config b #t)) (cddr x)))))

(define (optimizer-define config x in-lambda?)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) in-lambda?))
    x))

(define (optimizer-set! config x in-lambda?)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) in-lambda?))
    x))

(define (optimizer-generic-list config x in-lambda?)
  (cons-source x
    (optimizer-expr config (car x) in-lambda?)
    (map1 (lambda (sub-x) (optimizer-expr config sub-x in-lambda?)) (cdr x))))

(define (optimizer-expr config x in-lambda?)
  (cond
    ((not (pair? x)) x)
    ((not (list? x)) x)
    ((and in-lambda?
          (OptimizerConfig/inline-car? config)
          (optimizer-lambda-form? (car x))
          (not (optimizer-named-loop-form? x)))
     (optimizer-inline-lambda-call config x in-lambda?))
    (else
      (case (optimizer-head-name x)
        ((quote) x)
        ((lambda) (optimizer-lambda config x))
        ((define) (optimizer-define config x in-lambda?))
        ((set!) (optimizer-set! config x in-lambda?))
        (else (optimizer-generic-list config x in-lambda?))))))

(define (optimize-toplevel body . maybe-config)
  (let ((config (if (null? maybe-config)
                  (optimizer-config-from-toplevel)
                  (car maybe-config))))
    (if (OptimizerConfig/optimize? config)
      (optimizer-expr config body #f)
      body)))
