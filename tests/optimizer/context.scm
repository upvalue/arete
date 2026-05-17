(define (head-name x)
  (if (and (pair? x) (identifier? (car x)))
    (rename-strip (car x))
    #f))

(define (contains-head? name x)
  (if (pair? x)
    (if (eq? (head-name x) name)
      #t
      (let loop ((xs x))
        (cond
          ((null? xs) #f)
          ((pair? xs)
           (if (contains-head? name (car xs))
             #t
             (loop (cdr xs))))
          (else (contains-head? name xs)))))
    #f))

(define (optimized expr)
  (optimize-toplevel
    (expand-toplevel expr (top-level-value '*core-module*))))

(define (optimized/user expr)
  (optimize-toplevel
    (expand-toplevel expr (top-level-value '*user-module*))))

(set-top-level-value! 'COMPILER-OPTIMIZE #t)
(set-top-level-value! 'OPTIMIZER-INLINE-CAR #t)
(set-top-level-value! 'COMPILER-INLINE-CAR #t)
(set-top-level-value! 'OPTIMIZER-NAMED-LET #t)
(set-top-level-value! 'OPTIMIZER-PRIMITIVE-FOLD #t)
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF #f)
(set-top-level-value! 'OPTIMIZER-CONSTANT-PROP #t)
(set-top-level-value! 'COMPILER-VM-PRIMITIVES #t)

(define top-level-direct-call
  '((lambda (x) x) 1))

(define tail-direct-call
  '(lambda (z)
     ((lambda (x) x) z)))

(define effect-direct-call
  '(lambda (z)
     ((lambda (x) x) z)
     z))

(define branch-direct-call
  '(lambda (flag z)
     (if flag
       ((lambda (x) x) z)
       z)))

(define tail-branch-named-loop
  '(lambda (n flag)
     (if flag
       (let loop ((i n))
         (if (fx= i 0)
           i
           (loop (fx- i 1))))
       n)))

(define test-position-named-loop
  '(lambda (n)
     (if (let loop ((i n))
           (if (fx= i 0)
             #f
             (loop (fx- i 1))))
       1
       2)))

(define operand-position-named-loop
  '(lambda (n)
     (fx+ 1
       (let loop ((i n))
         (if (fx= i 0)
           i
           (loop (fx- i 1)))))))

(define primitive-fold-effect
  '(lambda (x)
     (not '#f)
     x))

(define primitive-fold-test
  '(lambda (x)
     (if (not '#f) x #f)))

(define primitive-fold-application
  '(lambda (x)
     ((not '#f) x)))

(define shadow-effect
  '(lambda (not x)
     (not '#f)
     x))

(define shadow-test
  '(lambda (not x)
     (if (not '#f) x #f)))

(define shadow-application
  '(lambda (not x)
     ((not '#f) x)))

(define constant-if-tail-branch
  '(lambda (n)
     (if #t
       (let loop ((i n))
         (if (fx= i 0)
           i
           (loop (fx- i 1))))
       n)))

(define constant-if-test-branch
  '(lambda (n)
     (if (if #t
           (let loop ((i n))
             (if (fx= i 0)
               #f
               (loop (fx- i 1))))
           #f)
       1
       2)))

(print (contains-head? '##arete#inline-let (optimized top-level-direct-call)))
(print (contains-head? '##arete#inline-let (optimized tail-direct-call)))
(print (contains-head? '##arete#inline-let (optimized effect-direct-call)))
(print (contains-head? '##arete#inline-let (optimized branch-direct-call)))
(print (contains-head? '##arete#named-loop (optimized tail-branch-named-loop)))
(print (contains-head? '##arete#named-loop (optimized test-position-named-loop)))
(print (contains-head? '##arete#named-loop (optimized operand-position-named-loop)))
(print (contains-head? 'not (optimized primitive-fold-effect)))
(print (contains-head? 'not (optimized primitive-fold-test)))
(print (contains-head? 'if (optimized primitive-fold-test)))
(print (contains-head? 'not (optimized primitive-fold-application)))
(print (not (self-evaluating? (caddr (optimized/user shadow-effect)))))
(print (not (self-evaluating? (cadr (caddr (optimized/user shadow-test))))))
(print (not (self-evaluating? (car (caddr (optimized/user shadow-application))))))

(set-top-level-value! 'OPTIMIZER-CONSTANT-IF #t)
(print (contains-head? '##arete#named-loop (optimized constant-if-tail-branch)))
(print (contains-head? '##arete#named-loop (optimized constant-if-test-branch)))
