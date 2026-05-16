;; optimizer.scm - optional source-to-source optimizer for expanded forms

;; Global kill switch for the optimizer. Individual rewrites have their own
;; flags so a suspect optimization can be disabled without losing the whole pass.
(define (set-top-level-default! name value)
  (when (eq? (top-level-value name unspecified) unspecified)
    (set-top-level-value! name value)))

(set-top-level-default! 'COMPILER-OPTIMIZE #t)
(set-top-level-default! 'OPTIMIZER-INLINE-CAR #t)
(set-top-level-default! 'OPTIMIZER-NAMED-LET #t)
(set-top-level-default! 'COMPILER-OPTIMIZER-LOG #f)

(define-record OptimizerConfig
  optimize?
  inline-car?
  named-let?
  log?)

(define (optimizer-config-from-toplevel)
  (OptimizerConfig/make
    (eq? (top-level-value 'COMPILER-OPTIMIZE #t) #t)
    (and (eq? (top-level-value 'OPTIMIZER-INLINE-CAR #t) #t)
         ;; Compatibility with the old compiler-side direct-lambda inline flag.
         (eq? (top-level-value 'COMPILER-INLINE-CAR #t) #t))
    (eq? (top-level-value 'OPTIMIZER-NAMED-LET #t) #t)
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

;; Detect the named-let expansion pattern:
;;   ((lambda () (define NAME (lambda ARGS BODY...)) (NAME INITS...)))
;; Returns (NAME ARGS BODY INITS) on match, #f otherwise. This is specifically
;; the shape produced by the (let loop ...) macro in syntax.scm.
(define (optimizer-named-loop-match x)
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
                     (fx>= (length (caddr def)) 3)
                     (list? call)
                     (pair? call)
                     (identifier? (car call))
                     (identifier=? (cadr def) (car call))
                     (let* ((inner-lambda (caddr def))
                            (args (cadr inner-lambda))
                            (body (cddr inner-lambda)))
                       (and (list? args)
                            (every identifier? args)
                            (fx= (length args) (length (cdr call)))
                            (list (cadr def) args body (cdr call))))))))))

;; Verify that NAME is used only as the head of tail applications, and that it
;; is never set! or captured. This mirrors the old compiler-side refusal logic.
(define (optimizer-named-loop-usage-ok? name body tail?)
  (let ((ok #t))
    (define (ident=? x) (and (identifier? x) (identifier=? x name)))
    (define (args-shadow? args)
      (cond
        ((null? args) #f)
        ((identifier? args) (ident=? args))
        ((pair? args) (or (ident=? (car args)) (args-shadow? (cdr args))))
        (else #f)))
    (define (scan-expr x tail?)
      (when ok
        (cond
          ((identifier? x)
           (when (ident=? x) (set! ok #f)))
          ((pair? x)
           (let ((head (car x)))
             (cond
               ((and (identifier? head) (eq? (rename-strip head) 'quote)) #f)
               ((and (identifier? head) (eq? (rename-strip head) 'set!))
                (when (and (pair? (cdr x)) (ident=? (cadr x)))
                  (set! ok #f))
                (when (and ok (pair? (cdr x)) (pair? (cddr x)))
                  (scan-expr (caddr x) #f)))
               ((and (identifier? head) (eq? (rename-strip head) 'lambda))
                ;; Capturing the loop procedure would defeat jump lowering.
                (unless (and (pair? (cdr x)) (args-shadow? (cadr x)))
                  (scan-body-no-ref (cddr x))))
               ((and (identifier? head) (eq? (rename-strip head) 'if))
                (when (pair? (cdr x)) (scan-expr (cadr x) #f))
                (when (and (pair? (cdr x)) (pair? (cddr x)))
                  (scan-expr (caddr x) tail?))
                (when (and (pair? (cdr x)) (pair? (cddr x)) (pair? (cdddr x)))
                  (scan-expr (cadddr x) tail?)))
               ((and (identifier? head) (eq? (rename-strip head) 'begin))
                (scan-body (cdr x) tail?))
               ((and (identifier? head) (eq? (rename-strip head) 'define))
                (when (and (pair? (cdr x)) (ident=? (cadr x)))
                  (set! ok #f))
                (when (and ok (pair? (cdr x)) (pair? (cddr x)))
                  (scan-expr (caddr x) #f)))
               ((and (identifier? head) (or (eq? (rename-strip head) 'and)
                                            (eq? (rename-strip head) 'or)))
                (scan-body (cdr x) tail?))
               (else
                 (cond
                   ((ident=? head)
                    (unless tail? (set! ok #f))
                    (when ok (scan-args (cdr x))))
                   ((and (pair? head)
                         (identifier? (car head))
                         (eq? (rename-strip (car head)) 'lambda)
                         (pair? (cdr head))
                         (list? (cadr head))
                         (every identifier? (cadr head)))
                    (scan-args (cdr x))
                    (when ok
                      (unless (args-shadow? (cadr head))
                        (scan-body (cddr head) tail?))))
                   (else
                     (scan-expr head #f)
                     (when ok (scan-args (cdr x))))))))))))
    (define (scan-args xs)
      (when (and ok (pair? xs))
        (scan-expr (car xs) #f)
        (scan-args (cdr xs))))
    (define (scan-body xs tail?)
      (when (and ok (pair? xs))
        (if (null? (cdr xs))
          (scan-expr (car xs) tail?)
          (begin
            (scan-expr (car xs) #f)
            (scan-body (cdr xs) tail?)))))
    (define (scan-body-no-ref xs)
      (when (and ok (pair? xs))
        (scan-expr-no-ref (car xs))
        (scan-body-no-ref (cdr xs))))
    (define (scan-expr-no-ref x)
      (when ok
        (cond
          ((identifier? x) (when (ident=? x) (set! ok #f)))
          ((pair? x)
           (let ((head (car x)))
             (cond
               ((and (identifier? head) (eq? (rename-strip head) 'quote)) #f)
               ((and (identifier? head) (eq? (rename-strip head) 'lambda))
                (unless (and (pair? (cdr x)) (args-shadow? (cadr x)))
                  (scan-body-no-ref (cddr x))))
               (else (for-each-improper scan-expr-no-ref x))))))))
    (scan-body body tail?)
    ok))

(define (optimizer-make-named-loop src name args inits body)
  (cons-source src '##arete#named-loop
    (cons-source src name
      (cons-source src args
        (cons-source src inits body)))))

(define (optimizer-make-inline-let src args vals body)
  (cons-source src '##arete#inline-let
    (cons-source src args
      (cons-source src vals body))))

(define (optimizer-body config body in-lambda? tail?)
  (let loop ((xs body))
    (cond
      ((null? xs) '())
      ((null? (cdr xs))
       (cons-source xs
         (optimizer-expr config (car xs) in-lambda? tail?)
         '()))
      (else
        (cons-source xs
          (optimizer-expr config (car xs) in-lambda? #f)
          (loop (cdr xs)))))))

(define (optimizer-inline-lambda-call config x in-lambda? tail?)
  (let* ((lambda-form (car x))
         (args (cadr lambda-form))
         (body (cddr lambda-form))
         (vals (cdr x)))
    (optimizer-log config "inline direct lambda call" x)
    (optimizer-make-inline-let
      x
      args
      (map1 (lambda (v) (optimizer-expr config v in-lambda? #f)) vals)
      (optimizer-body config body #t tail?))))

(define (optimizer-named-loop config x tail? pattern)
  (let ((name (car pattern))
        (args (cadr pattern))
        (body (caddr pattern))
        (inits (cadddr pattern)))
    (optimizer-log config "lower named let" name)
    (optimizer-make-named-loop
      x
      name
      args
      (map1 (lambda (v) (optimizer-expr config v #t #f)) inits)
      (optimizer-body config body #t tail?))))

(define (optimizer-lambda config x)
  (cons-source x (car x)
    (cons-source (cdr x) (cadr x)
      (optimizer-body config (cddr x) #t #t))))

(define (optimizer-define config x in-lambda?)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) in-lambda? #f))
    x))

(define (optimizer-set! config x in-lambda?)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) in-lambda? #f))
    x))

(define (optimizer-generic-list config x in-lambda?)
  (cons-source x
    (optimizer-expr config (car x) in-lambda? #f)
    (map1 (lambda (sub-x) (optimizer-expr config sub-x in-lambda? #f)) (cdr x))))

(define (optimizer-begin config x in-lambda? tail?)
  (cons-source x (car x) (optimizer-body config (cdr x) in-lambda? tail?)))

(define (optimizer-if config x in-lambda? tail?)
  (let ((len (length x)))
    (cond
      ((fx= len 4)
       (list-source x (car x)
         (optimizer-expr config (cadr x) in-lambda? #f)
         (optimizer-expr config (caddr x) in-lambda? tail?)
         (optimizer-expr config (cadddr x) in-lambda? tail?)))
      ((fx= len 3)
       (list-source x (car x)
         (optimizer-expr config (cadr x) in-lambda? #f)
         (optimizer-expr config (caddr x) in-lambda? tail?)))
      (else (optimizer-generic-list config x in-lambda?)))))

(define (optimizer-expr config x in-lambda? tail?)
  (cond
    ((not (pair? x)) x)
    ((not (list? x)) x)
    (else
      (let ((named-loop-pattern (and in-lambda? (optimizer-named-loop-match x))))
        (cond
          ((and named-loop-pattern
                (OptimizerConfig/named-let? config)
                (optimizer-named-loop-usage-ok? (car named-loop-pattern)
                                                (caddr named-loop-pattern)
                                                tail?))
           (optimizer-named-loop config x tail? named-loop-pattern))
          (named-loop-pattern
           (optimizer-generic-list config x in-lambda?))
          ((and in-lambda?
                (OptimizerConfig/inline-car? config)
                (optimizer-lambda-form? (car x)))
           (optimizer-inline-lambda-call config x in-lambda? tail?))
          (else
            (case (optimizer-head-name x)
              ((quote) x)
              ((lambda) (optimizer-lambda config x))
              ((define) (optimizer-define config x in-lambda?))
              ((set!) (optimizer-set! config x in-lambda?))
              ((begin) (optimizer-begin config x in-lambda? tail?))
              ((if) (optimizer-if config x in-lambda? tail?))
              ((and or) (optimizer-begin config x in-lambda? tail?))
              (else (optimizer-generic-list config x in-lambda?)))))))))

(define (optimize-toplevel body . maybe-config)
  (let ((config (if (null? maybe-config)
                  (optimizer-config-from-toplevel)
                  (car maybe-config))))
    (if (OptimizerConfig/optimize? config)
      (optimizer-expr config body #f #f)
      body)))
