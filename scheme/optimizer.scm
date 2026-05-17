;; optimizer.scm - optional source-to-source optimizer for expanded forms

;; Global kill switch for the optimizer. Individual rewrites have their own
;; flags so a suspect optimization can be disabled without losing the whole pass.
(define (set-top-level-default! name value)
  (when (eq? (top-level-value name unspecified) unspecified)
    (set-top-level-value! name value)))

(set-top-level-default! 'COMPILER-OPTIMIZE #t)
(set-top-level-default! 'OPTIMIZER-INLINE-CAR #t)
(set-top-level-default! 'OPTIMIZER-NAMED-LET #t)
(set-top-level-default! 'OPTIMIZER-PRIMITIVE-FOLD #t)
(set-top-level-default! 'OPTIMIZER-CONSTANT-IF #f)
(set-top-level-default! 'COMPILER-OPTIMIZER-LOG #f)
(set-top-level-default! 'COMPILER-OPTIMIZER-COUNTERS #f)

(define-record OptimizerConfig
  optimize?
  inline-car?
  named-let?
  primitive-fold?
  constant-if?
  vm-primitives?
  log?
  counters?)

(define (optimizer-config-from-toplevel)
  (OptimizerConfig/make
    (eq? (top-level-value 'COMPILER-OPTIMIZE #t) #t)
    (and (eq? (top-level-value 'OPTIMIZER-INLINE-CAR #t) #t)
         ;; Compatibility with the old compiler-side direct-lambda inline flag.
         (eq? (top-level-value 'COMPILER-INLINE-CAR #t) #t))
    (eq? (top-level-value 'OPTIMIZER-NAMED-LET #t) #t)
    (eq? (top-level-value 'OPTIMIZER-PRIMITIVE-FOLD #t) #t)
    (eq? (top-level-value 'OPTIMIZER-CONSTANT-IF #t) #t)
    (eq? (top-level-value 'COMPILER-VM-PRIMITIVES #t) #t)
    (eq? (top-level-value 'COMPILER-OPTIMIZER-LOG #f) #t)
    (eq? (top-level-value 'COMPILER-OPTIMIZER-COUNTERS #f) #t)))

(define optimizer-counter-names
  '(inline-let-applied
    named-loop-applied
    primitive-fold-applied
    primitive-fold-refused-unknown
    constant-if-applied
    constant-if-refused-unspecified
    named-loop-refused-disabled
    named-loop-refused-usage))

(define optimizer-counter-table (make-table))

(define (optimizer-counter-value name)
  (table-ref optimizer-counter-table name 0))

(define (optimizer-reset-counters!)
  (for-each1
    (lambda (name)
      (table-set! optimizer-counter-table name 0))
    optimizer-counter-names))

(define (optimizer-counter-snapshot)
  (map1
    (lambda (name)
      (list name (optimizer-counter-value name)))
    optimizer-counter-names))

(define (optimizer-count! config name)
  (when (OptimizerConfig/counters? config)
    (table-set! optimizer-counter-table name
      (fx+ (optimizer-counter-value name) 1))))

(define-record OptimizerPrimitive
  name
  min-argc
  max-argc
  foldable?
  effect-free?
  allocation-free?
  vm-primitive?)

(define optimizer-primitive-table
  (let ((tbl (make-table)))
    (define (add name min-argc max-argc foldable? effect-free? allocation-free? vm-primitive?)
      (table-set! tbl name
        (OptimizerPrimitive/make
          name min-argc max-argc foldable? effect-free? allocation-free? vm-primitive?)))
    (add 'not 1 1 #t #t #t #t)
    (add 'eq? 2 2 #t #t #t #t)
    (add 'null? 1 1 #t #t #t #t)
    ;; The current compiler-side pair? primitive does not return the same true
    ;; value as the Scheme predicate in every compiled path, so keep metadata
    ;; descriptive until that behavior is unified.
    (add 'pair? 1 1 #f #t #t #t)
    (add 'fixnum? 1 1 #t #t #t #t)
    ;; Descriptive entry only for now: the compiler does not currently treat
    ;; symbol? as a VM primitive under primitive-table.
    (add 'symbol? 1 1 #f #t #t #f)
    (add 'fx+ 2 2 #t #t #t #t)
    (add 'fx- 2 2 #t #t #t #t)
    (add 'fx< 2 2 #t #t #t #t)
    tbl))

(define (optimizer-primitive-metadata name)
  (table-ref optimizer-primitive-table name))

(define (optimizer-primitive-simple? name)
  (aif (optimizer-primitive-metadata name)
    (and (OptimizerPrimitive/effect-free? it)
         (OptimizerPrimitive/allocation-free? it))
    #f))

(define (optimizer-primitive-foldable? name)
  (aif (optimizer-primitive-metadata name)
    (OptimizerPrimitive/foldable? it)
    #f))

(define (optimizer-shadow-name name shadowed)
  (let ((stripped (if (identifier? name) (rename-strip name) name)))
    (if (and (symbol? stripped)
             (optimizer-primitive-metadata stripped)
             (not (memq stripped shadowed)))
      (cons stripped shadowed)
      shadowed)))

(define (optimizer-shadow-args args shadowed)
  (cond
    ((null? args) shadowed)
    ((identifier? args) (optimizer-shadow-name args shadowed))
    ((pair? args)
     (optimizer-shadow-args (cdr args)
       (optimizer-shadow-name (car args) shadowed)))
    (else shadowed)))

(define (optimizer-shadow-define x shadowed)
  (if (and (pair? (cdr x)) (identifier? (cadr x)))
    (optimizer-shadow-name (cadr x) shadowed)
    shadowed))

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

(define (optimizer-quote-form? x)
  (and (list? x)
       (fx= (length x) 2)
       (eq? (optimizer-head-name x) 'quote)))

(define (optimizer-known value)
  (cons 'known value))

(define (optimizer-known-value known)
  (cdr known))

(define (optimizer-constant-value x)
  (cond
    ((self-evaluating? x) (optimizer-known x))
    ((optimizer-quote-form? x) (optimizer-known (cadr x)))
    (else #f)))

;; Keep this in sync with the compiler's safe-eq-immediate? predicate. These
;; are the values where eq? identity is stable enough to decide at source level.
(define (optimizer-safe-eq-immediate? v)
  (or (symbol? v) (fixnum? v) (boolean? v) (null? v)))

(define (optimizer-small-fixnum? v)
  (and (fixnum? v)
       (fx< -1000000 v)
       (fx< v 1000000)))

(define (optimizer-known-small-fixnum-value x)
  (aif (optimizer-constant-value x)
    (let ((value (optimizer-known-value it)))
      (and (optimizer-small-fixnum? value) value))
    #f))

(define (optimizer-primitive-arity-ok? meta argc)
  (and (fx= argc (OptimizerPrimitive/min-argc meta))
       (fx= argc (OptimizerPrimitive/max-argc meta))))

(define (optimizer-constant->expr src value)
  (if (self-evaluating? value)
    value
    (list-source src (make-rename #f 'quote) value)))

(define (optimizer-false? value)
  (eq? value #f))

(define (optimizer-unspecified? value)
  (eq? value unspecified))

(define (optimizer-fold-primitive name args)
  (case name
    ((not)
     (aif (optimizer-constant-value (car args))
       (optimizer-known (if (eq? (optimizer-known-value it) #f) #t #f))
       #f))
    ((eq?)
     (let ((a (optimizer-constant-value (car args)))
           (b (optimizer-constant-value (cadr args))))
       (and a b
            (let ((av (optimizer-known-value a))
                  (bv (optimizer-known-value b)))
              (and (optimizer-safe-eq-immediate? av)
                   (optimizer-safe-eq-immediate? bv)
                   (optimizer-known (eq? av bv)))))))
    ((null?)
     (aif (optimizer-constant-value (car args))
       (optimizer-known (null? (optimizer-known-value it)))
       #f))
    ((pair?)
     (aif (optimizer-constant-value (car args))
       (optimizer-known (pair? (optimizer-known-value it)))
       #f))
    ((fixnum?)
     (aif (optimizer-constant-value (car args))
       (optimizer-known (fixnum? (optimizer-known-value it)))
       #f))
    ((fx+)
     (let ((a (optimizer-known-small-fixnum-value (car args)))
           (b (optimizer-known-small-fixnum-value (cadr args))))
       (and a b
            (let ((result (fx+ a b)))
              (and (optimizer-small-fixnum? result)
                   (optimizer-known result))))))
    ((fx-)
     (let ((a (optimizer-known-small-fixnum-value (car args)))
           (b (optimizer-known-small-fixnum-value (cadr args))))
       (and a b
            (let ((result (fx- a b)))
              (and (optimizer-small-fixnum? result)
                   (optimizer-known result))))))
    ((fx<)
     (let ((a (optimizer-known-small-fixnum-value (car args)))
           (b (optimizer-known-small-fixnum-value (cadr args))))
       (and a b (optimizer-known (fx< a b)))))
    (else #f)))

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

(define (optimizer-body config body in-lambda? tail? shadowed)
  (let loop ((xs body) (shadowed shadowed))
    (cond
      ((null? xs) '())
      ((null? (cdr xs))
       (let ((expr-shadowed (if (eq? (optimizer-head-name (car xs)) 'define)
                              (optimizer-shadow-define (car xs) shadowed)
                              shadowed)))
         (cons-source xs
           (optimizer-expr config (car xs) in-lambda? tail? expr-shadowed)
           '())))
      (else
        (let ((expr-shadowed (if (eq? (optimizer-head-name (car xs)) 'define)
                               (optimizer-shadow-define (car xs) shadowed)
                               shadowed)))
          (cons-source xs
            (optimizer-expr config (car xs) in-lambda? #f expr-shadowed)
            (loop (cdr xs) expr-shadowed)))))))

(define (optimizer-inline-lambda-call config x in-lambda? tail? shadowed)
  (let* ((lambda-form (car x))
         (args (cadr lambda-form))
         (body (cddr lambda-form))
         (vals (cdr x))
         (body-shadowed (optimizer-shadow-args args shadowed)))
    (optimizer-log config "inline direct lambda call" x)
    (optimizer-count! config 'inline-let-applied)
    (optimizer-make-inline-let
      x
      args
      (map1 (lambda (v) (optimizer-expr config v in-lambda? #f shadowed)) vals)
      (optimizer-body config body #t tail? body-shadowed))))

(define (optimizer-named-loop config x tail? pattern shadowed)
  (let ((name (car pattern))
        (args (cadr pattern))
        (body (caddr pattern))
        (inits (cadddr pattern)))
    (optimizer-log config "lower named let" name)
    (optimizer-count! config 'named-loop-applied)
    (optimizer-make-named-loop
      x
      name
      args
      (map1 (lambda (v) (optimizer-expr config v #t #f shadowed)) inits)
      (optimizer-body config body #t tail?
        (optimizer-shadow-args args (optimizer-shadow-name name shadowed))))))

(define (optimizer-lambda config x shadowed)
  (cons-source x (car x)
    (cons-source (cdr x) (cadr x)
      (optimizer-body config (cddr x) #t #t
        (optimizer-shadow-args (cadr x) shadowed)))))

(define (optimizer-define config x in-lambda? shadowed)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) in-lambda? #f shadowed))
    x))

(define (optimizer-set! config x in-lambda? shadowed)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) in-lambda? #f shadowed))
    x))

(define (optimizer-generic-list config x in-lambda? shadowed)
  (cons-source x
    (optimizer-expr config (car x) in-lambda? #f shadowed)
    (map1 (lambda (sub-x) (optimizer-expr config sub-x in-lambda? #f shadowed)) (cdr x))))

(define (optimizer-primitive-application config x in-lambda? tail? meta shadowed)
  (let* ((args (map1 (lambda (sub-x) (optimizer-expr config sub-x in-lambda? #f shadowed)) (cdr x)))
         (fold-candidate?
           (and (OptimizerConfig/primitive-fold? config)
                (OptimizerConfig/vm-primitives? config)
                (not (memq (OptimizerPrimitive/name meta) shadowed))
                (OptimizerPrimitive/foldable? meta)
                (OptimizerPrimitive/vm-primitive? meta)
                (optimizer-primitive-arity-ok? meta (length args)))))
    (aif (and fold-candidate?
              (optimizer-fold-primitive (OptimizerPrimitive/name meta) args))
      (let ((result (optimizer-known-value it)))
        (optimizer-log config "fold primitive" (OptimizerPrimitive/name meta) x "=>" result)
        (optimizer-count! config 'primitive-fold-applied)
        (optimizer-constant->expr x result))
      (begin
        (when fold-candidate?
          (optimizer-count! config 'primitive-fold-refused-unknown))
        (cons-source x (car x) args)))))

(define (optimizer-application config x in-lambda? tail? shadowed)
  (let ((meta (optimizer-primitive-metadata (optimizer-head-name x))))
    (if meta
      (optimizer-primitive-application config x in-lambda? tail? meta shadowed)
      (optimizer-generic-list config x in-lambda? shadowed))))

(define (optimizer-begin config x in-lambda? tail? shadowed)
  (cons-source x (car x) (optimizer-body config (cdr x) in-lambda? tail? shadowed)))

(define (optimizer-if config x in-lambda? tail? shadowed)
  (let ((len (length x)))
    (cond
      ((fx= len 4)
       (let ((condition (optimizer-expr config (cadr x) in-lambda? #f shadowed)))
         (aif (and (OptimizerConfig/constant-if? config)
                   (optimizer-constant-value condition))
           (let ((test-value (optimizer-known-value it)))
             (if (and (optimizer-false? test-value)
                      (optimizer-unspecified? (cadddr x)))
               (begin
                 (optimizer-count! config 'constant-if-refused-unspecified)
                 (list-source x (car x)
                   condition
                   (optimizer-expr config (caddr x) in-lambda? tail? shadowed)
                   (cadddr x)))
               (begin
                 (optimizer-log config "fold constant if" test-value x)
                 (optimizer-count! config 'constant-if-applied)
                 (optimizer-expr config
                   (if (optimizer-false? test-value) (cadddr x) (caddr x))
                   in-lambda?
                   tail?
                   shadowed))))
           (list-source x (car x)
             condition
             (optimizer-expr config (caddr x) in-lambda? tail? shadowed)
             (optimizer-expr config (cadddr x) in-lambda? tail? shadowed)))))
      ((fx= len 3)
       (let ((condition (optimizer-expr config (cadr x) in-lambda? #f shadowed)))
         (aif (and (OptimizerConfig/constant-if? config)
                   (optimizer-constant-value condition)
                   (not (optimizer-false? (optimizer-known-value it))))
           (begin
             (optimizer-log config "fold constant if" (optimizer-known-value it) x)
             (optimizer-count! config 'constant-if-applied)
             (optimizer-expr config (caddr x) in-lambda? tail? shadowed))
           (list-source x (car x)
             condition
             (optimizer-expr config (caddr x) in-lambda? tail? shadowed)))))
      (else (optimizer-generic-list config x in-lambda? shadowed)))))

(define (optimizer-expr config x in-lambda? tail? shadowed)
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
           (optimizer-named-loop config x tail? named-loop-pattern shadowed))
          (named-loop-pattern
           (optimizer-count! config
             (if (OptimizerConfig/named-let? config)
               'named-loop-refused-usage
               'named-loop-refused-disabled))
           (optimizer-generic-list config x in-lambda? shadowed))
          ((and in-lambda?
                (OptimizerConfig/inline-car? config)
                (optimizer-lambda-form? (car x)))
           (optimizer-inline-lambda-call config x in-lambda? tail? shadowed))
          (else
            (case (optimizer-head-name x)
              ((quote) x)
              ((lambda) (optimizer-lambda config x shadowed))
              ((define) (optimizer-define config x in-lambda? shadowed))
              ((set!) (optimizer-set! config x in-lambda? shadowed))
              ((begin) (optimizer-begin config x in-lambda? tail? shadowed))
              ((if) (optimizer-if config x in-lambda? tail? shadowed))
              ((and or) (optimizer-begin config x in-lambda? tail? shadowed))
              (else (optimizer-application config x in-lambda? tail? shadowed)))))))))

(define (optimize-toplevel body . maybe-config)
  (let ((config (if (null? maybe-config)
                  (optimizer-config-from-toplevel)
                  (car maybe-config))))
    (if (OptimizerConfig/optimize? config)
      (optimizer-expr config body #f #f '())
      body)))
