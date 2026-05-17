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
(set-top-level-default! 'OPTIMIZER-CONSTANT-PROP #t)
(set-top-level-default! 'OPTIMIZER-TEST-IF #t)
(set-top-level-default! 'COMPILER-OPTIMIZER-LOG #f)
(set-top-level-default! 'COMPILER-OPTIMIZER-COUNTERS #f)

(define-record OptimizerConfig
  optimize?
  inline-car?
  named-let?
  primitive-fold?
  constant-if?
  constant-prop?
  test-if?
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
    (eq? (top-level-value 'OPTIMIZER-CONSTANT-PROP #t) #t)
    (eq? (top-level-value 'OPTIMIZER-TEST-IF #t) #t)
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
    constant-prop-applied
    test-if-applied
    test-if-inverted-applied
    boolean-if-applied
    test-not-inverted-applied
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

(define (optimizer-true-constant-expr? x)
  (aif (optimizer-constant-value x)
    (eq? (optimizer-known-value it) #t)
    #f))

(define (optimizer-false-constant-expr? x)
  (aif (optimizer-constant-value x)
    (eq? (optimizer-known-value it) #f)
    #f))

(define (optimizer-negated-boolean-if-condition? x)
  (and (list? x)
       (fx= (length x) 4)
       (eq? (optimizer-head-name x) 'if)
       (optimizer-false-constant-expr? (caddr x))
       (optimizer-true-constant-expr? (cadddr x))))

(define optimizer-boolean-primitive-names '(not eq? null? fixnum? fx<))

(define (optimizer-boolean-result-expr? config x shadowed)
  (or (optimizer-true-constant-expr? x)
      (optimizer-false-constant-expr? x)
      (and (OptimizerConfig/vm-primitives? config)
           (list? x)
           (let ((name (optimizer-head-name x)))
             (and (memq name optimizer-boolean-primitive-names)
                  (not (memq name shadowed))
                  (aif (optimizer-primitive-metadata name)
                    (and (OptimizerPrimitive/vm-primitive? it)
                         (optimizer-primitive-arity-ok? it (fx- (length x) 1)))
                    #f))))))

(define (optimizer-not-condition-arg config x shadowed)
  (and (OptimizerConfig/vm-primitives? config)
       (list? x)
       (fx= (length x) 2)
       (eq? (optimizer-head-name x) 'not)
       (not (memq 'not shadowed))
       (aif (optimizer-primitive-metadata 'not)
         (and (OptimizerPrimitive/vm-primitive? it)
              (optimizer-primitive-arity-ok? it 1)
              (cadr x))
         #f)))

;; Cp0-style result contexts.  For now the optimizer uses them only to make
;; traversal intent explicit; do not use 'effect to drop expressions without
;; separate effect facts.
(define (optimizer-tail-context? ctxt)
  (eq? ctxt 'tail))

(define (optimizer-immediate-constant-expr src x)
  (aif (optimizer-constant-value x)
    (let ((value (optimizer-known-value it)))
      (and (optimizer-safe-eq-immediate? value)
           (optimizer-constant->expr src value)))
    #f))

(define (optimizer-arg-shadows-name? args name)
  (cond
    ((null? args) #f)
    ((identifier? args) (identifier=? args name))
    ((pair? args)
     (or (and (identifier? (car args))
              (identifier=? (car args) name))
         (optimizer-arg-shadows-name? (cdr args) name)))
    (else #f)))

(define (optimizer-unique-identifiers? args)
  (define (seen? arg seen)
    (and (pair? seen)
         (or (identifier=? arg (car seen))
             (seen? arg (cdr seen)))))
  (let loop ((rest args) (seen '()))
    (cond
      ((null? rest) #t)
      ((not (identifier? (car rest))) #f)
      ((seen? (car rest) seen) #f)
      (else (loop (cdr rest) (cons (car rest) seen))))))

(define (optimizer-name-mutated-in-body? name body)
  (let ((mutated? #f))
    (define (scan-body xs)
      (when (and (not mutated?) (pair? xs))
        (scan-expr (car xs))
        (scan-body (cdr xs))))
    (define (scan-expr x)
      (when (not mutated?)
        (cond
          ((not (pair? x)) #f)
          ((not (list? x)) #f)
          (else
            (case (optimizer-head-name x)
              ((quote) #f)
              ((lambda)
               (when (and (pair? (cdr x))
                          (not (optimizer-arg-shadows-name? (cadr x) name)))
                 (scan-body (cddr x))))
              ((set!)
               (when (and (pair? (cdr x))
                          (identifier? (cadr x))
                          (identifier=? (cadr x) name))
                 (set! mutated? #t))
               (when (and (not mutated?) (pair? (cdr x)) (pair? (cddr x)))
                 (scan-expr (caddr x))))
              ((define)
               (when (and (pair? (cdr x))
                          (identifier? (cadr x))
                          (identifier=? (cadr x) name))
                 (set! mutated? #t))
               (when (and (not mutated?) (pair? (cdr x)) (pair? (cddr x)))
                 (scan-expr (caddr x))))
              (else
                (for-each1 scan-expr x)))))))
    (scan-body body)
    mutated?))

(define (optimizer-substitute-constant-in-body name replacement body)
  (map1 (lambda (x) (optimizer-substitute-constant name replacement x)) body))

(define (optimizer-substitute-constant name replacement x)
  (cond
    ((identifier? x)
     (if (identifier=? x name) replacement x))
    ((not (pair? x)) x)
    ((not (list? x)) x)
    (else
      (case (optimizer-head-name x)
        ((quote) x)
        ((lambda)
         (if (and (pair? (cdr x))
                  (optimizer-arg-shadows-name? (cadr x) name))
           x
           (cons-source x (car x)
             (cons-source (cdr x) (cadr x)
               (optimizer-substitute-constant-in-body name replacement (cddr x))))))
        ((set!)
         (if (and (pair? (cdr x)) (pair? (cddr x)))
           (list-source x (car x) (cadr x)
             (optimizer-substitute-constant name replacement (caddr x)))
           x))
        ((define)
         (if (and (pair? (cdr x)) (pair? (cddr x)))
           (list-source x (car x) (cadr x)
             (optimizer-substitute-constant name replacement (caddr x)))
           x))
        (else
          (let loop ((xs x))
            (cond
              ((null? xs) '())
              ((pair? xs)
               (cons-source xs
                 (optimizer-substitute-constant name replacement (car xs))
                 (loop (cdr xs))))
              (else (optimizer-substitute-constant name replacement xs)))))))))

(define (optimizer-constant-prop-plan config src args vals body)
  (if (and (OptimizerConfig/constant-prop? config)
           (fx= (length args) (length vals))
           (optimizer-unique-identifiers? args))
    (let loop ((as args) (vs vals) (body body))
      (if (null? as)
        (list '() '() body 0)
        (let* ((rest (loop (cdr as) (cdr vs) body))
               (rest-args (car rest))
               (rest-vals (cadr rest))
               (rest-body (caddr rest))
               (rest-count (cadddr rest))
               (arg (car as))
               (val (car vs))
               (replacement (optimizer-immediate-constant-expr src val)))
          (if (and replacement
                   (not (optimizer-name-mutated-in-body? arg body)))
            (begin
              (optimizer-log config "propagate constant argument" arg replacement)
              (list rest-args
                    rest-vals
                    (optimizer-substitute-constant-in-body arg replacement rest-body)
                    (fx+ rest-count 1)))
            (list (cons arg rest-args)
                  (cons val rest-vals)
                  rest-body
                  rest-count)))))
    (list args vals body 0)))

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

(define (optimizer-body config body ctxt in-lambda? shadowed)
  (let loop ((xs body) (shadowed shadowed))
    (cond
      ((null? xs) '())
      ((null? (cdr xs))
       (let ((expr-shadowed (if (eq? (optimizer-head-name (car xs)) 'define)
                              (optimizer-shadow-define (car xs) shadowed)
                              shadowed)))
         (cons-source xs
           (optimizer-expr config (car xs) ctxt in-lambda? expr-shadowed)
           '())))
      (else
        (let ((expr-shadowed (if (eq? (optimizer-head-name (car xs)) 'define)
                               (optimizer-shadow-define (car xs) shadowed)
                               shadowed)))
          (cons-source xs
            (optimizer-expr config (car xs) 'effect in-lambda? expr-shadowed)
            (loop (cdr xs) expr-shadowed)))))))

(define (optimizer-inline-lambda-call config x ctxt in-lambda? shadowed)
  (let* ((lambda-form (car x))
         (args (cadr lambda-form))
         (body (cddr lambda-form))
         (vals (map1 (lambda (v) (optimizer-expr config v 'value in-lambda? shadowed)) (cdr x)))
         (prop-plan (optimizer-constant-prop-plan config x args vals body))
         (inline-args (car prop-plan))
         (inline-vals (cadr prop-plan))
         (inline-body (caddr prop-plan))
         (prop-count (cadddr prop-plan))
         (body-shadowed (optimizer-shadow-args inline-args shadowed)))
    (optimizer-log config "inline direct lambda call" x)
    (optimizer-count! config 'inline-let-applied)
    (when (fx> prop-count 0)
      (optimizer-count! config 'constant-prop-applied))
    (optimizer-make-inline-let
      x
      inline-args
      inline-vals
      (optimizer-body config inline-body ctxt #t body-shadowed))))

(define (optimizer-named-loop config x ctxt pattern shadowed)
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
      (map1 (lambda (v) (optimizer-expr config v 'value #t shadowed)) inits)
      (optimizer-body config body ctxt #t
        (optimizer-shadow-args args (optimizer-shadow-name name shadowed))))))

(define (optimizer-lambda config x shadowed)
  (cons-source x (car x)
    (cons-source (cdr x) (cadr x)
      (optimizer-body config (cddr x) 'tail #t
        (optimizer-shadow-args (cadr x) shadowed)))))

(define (optimizer-define config x in-lambda? shadowed)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) 'value in-lambda? shadowed))
    x))

(define (optimizer-set! config x in-lambda? shadowed)
  (if (and (pair? (cdr x)) (pair? (cddr x)))
    (list-source x (car x) (cadr x)
      (optimizer-expr config (caddr x) 'value in-lambda? shadowed))
    x))

(define (optimizer-generic-list config x in-lambda? shadowed)
  (cons-source x
    (optimizer-expr config (car x) 'application in-lambda? shadowed)
    (map1 (lambda (sub-x) (optimizer-expr config sub-x 'value in-lambda? shadowed)) (cdr x))))

(define (optimizer-primitive-application config x in-lambda? meta shadowed)
  (let* ((args (map1 (lambda (sub-x) (optimizer-expr config sub-x 'value in-lambda? shadowed)) (cdr x)))
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

(define (optimizer-application config x in-lambda? shadowed)
  (let ((meta (optimizer-primitive-metadata (optimizer-head-name x))))
    (if meta
      (optimizer-primitive-application config x in-lambda? meta shadowed)
      (optimizer-generic-list config x in-lambda? shadowed))))

(define (optimizer-begin config x ctxt in-lambda? shadowed)
  (cons-source x (car x) (optimizer-body config (cdr x) ctxt in-lambda? shadowed)))

(define (optimizer-if config x ctxt in-lambda? shadowed)
  (let ((len (length x)))
    (cond
      ((fx= len 4)
       (let ((condition (optimizer-expr config (cadr x) 'test in-lambda? shadowed)))
         (cond
           ((and (OptimizerConfig/test-if? config)
                 (eq? ctxt 'test)
                 (optimizer-true-constant-expr? (caddr x))
                 (optimizer-false-constant-expr? (cadddr x)))
            (optimizer-log config "simplify boolean test if" x)
            (optimizer-count! config 'test-if-applied)
            condition)
           ((and (OptimizerConfig/test-if? config)
                 (optimizer-true-constant-expr? (caddr x))
                 (optimizer-false-constant-expr? (cadddr x))
                 (optimizer-boolean-result-expr? config condition shadowed))
            (optimizer-log config "simplify boolean-valued if" x)
            (optimizer-count! config 'boolean-if-applied)
            condition)
           ((and (OptimizerConfig/test-if? config)
                 (optimizer-negated-boolean-if-condition? condition))
            (optimizer-log config "invert boolean test if" x)
            (optimizer-count! config 'test-if-inverted-applied)
            (list-source x (car x)
              (cadr condition)
              (optimizer-expr config (cadddr x) ctxt in-lambda? shadowed)
              (optimizer-expr config (caddr x) ctxt in-lambda? shadowed)))
           ((and (OptimizerConfig/test-if? config)
                 (optimizer-not-condition-arg config condition shadowed))
            (optimizer-log config "invert not test if" x)
            (optimizer-count! config 'test-not-inverted-applied)
            (let ((not-arg (optimizer-not-condition-arg config condition shadowed)))
              (list-source x (car x)
                (optimizer-expr config not-arg 'test in-lambda? shadowed)
                (optimizer-expr config (cadddr x) ctxt in-lambda? shadowed)
                (optimizer-expr config (caddr x) ctxt in-lambda? shadowed))))
           ((and (OptimizerConfig/constant-if? config)
                 (optimizer-constant-value condition))
            (let ((test-value (optimizer-known-value
                                (optimizer-constant-value condition))))
              (if (and (optimizer-false? test-value)
                       (optimizer-unspecified? (cadddr x)))
                (begin
                  (optimizer-count! config 'constant-if-refused-unspecified)
                  (list-source x (car x)
                    condition
                    (optimizer-expr config (caddr x) ctxt in-lambda? shadowed)
                    (cadddr x)))
                (begin
                  (optimizer-log config "fold constant if" test-value x)
                  (optimizer-count! config 'constant-if-applied)
                  (optimizer-expr config
                    (if (optimizer-false? test-value) (cadddr x) (caddr x))
                    ctxt
                    in-lambda?
                    shadowed)))))
           (else
             (list-source x (car x)
               condition
               (optimizer-expr config (caddr x) ctxt in-lambda? shadowed)
               (optimizer-expr config (cadddr x) ctxt in-lambda? shadowed))))))
      ((fx= len 3)
       (let ((condition (optimizer-expr config (cadr x) 'test in-lambda? shadowed)))
         (aif (and (OptimizerConfig/constant-if? config)
                   (optimizer-constant-value condition)
                   (not (optimizer-false? (optimizer-known-value it))))
           (begin
             (optimizer-log config "fold constant if" (optimizer-known-value it) x)
             (optimizer-count! config 'constant-if-applied)
             (optimizer-expr config (caddr x) ctxt in-lambda? shadowed))
           (list-source x (car x)
             condition
             (optimizer-expr config (caddr x) ctxt in-lambda? shadowed)))))
      (else (optimizer-generic-list config x in-lambda? shadowed)))))

(define (optimizer-expr config x ctxt in-lambda? shadowed)
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
                                                (optimizer-tail-context? ctxt)))
           (optimizer-named-loop config x ctxt named-loop-pattern shadowed))
          (named-loop-pattern
           (optimizer-count! config
             (if (OptimizerConfig/named-let? config)
               'named-loop-refused-usage
               'named-loop-refused-disabled))
           (optimizer-generic-list config x in-lambda? shadowed))
          ((and in-lambda?
                (OptimizerConfig/inline-car? config)
                (optimizer-lambda-form? (car x)))
           (optimizer-inline-lambda-call config x ctxt in-lambda? shadowed))
          (else
            (case (optimizer-head-name x)
              ((quote) x)
              ((lambda) (optimizer-lambda config x shadowed))
              ((define) (optimizer-define config x in-lambda? shadowed))
              ((set!) (optimizer-set! config x in-lambda? shadowed))
              ((begin) (optimizer-begin config x ctxt in-lambda? shadowed))
              ((if) (optimizer-if config x ctxt in-lambda? shadowed))
              ((and or) (optimizer-begin config x ctxt in-lambda? shadowed))
              (else (optimizer-application config x in-lambda? shadowed)))))))))

(define (optimize-toplevel body . maybe-config)
  (let ((config (if (null? maybe-config)
                  (optimizer-config-from-toplevel)
                  (car maybe-config))))
    (if (OptimizerConfig/optimize? config)
      (optimizer-expr config body 'value #f '())
      body)))
