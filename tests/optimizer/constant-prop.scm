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

(define (find-head name x)
  (cond
    ((not (pair? x)) #f)
    ((eq? (head-name x) name) x)
    (else
      (let loop ((xs x))
        (cond
          ((null? xs) #f)
          ((pair? xs)
           (aif (find-head name (car xs))
             it
             (loop (cdr xs))))
          (else (find-head name xs)))))))

(define (inline-let-arg-count x)
  (aif (find-head '##arete#inline-let x)
    (length (cadr it))
    #f))

(define (optimized expr)
  (optimize-toplevel
    (expand-toplevel expr (top-level-value '*user-module*))))

(define saved-constant-prop (top-level-value 'OPTIMIZER-CONSTANT-PROP))

(set-top-level-value! 'COMPILER-OPTIMIZE #t)
(set-top-level-value! 'OPTIMIZER-INLINE-CAR #t)
(set-top-level-value! 'COMPILER-INLINE-CAR #t)
(set-top-level-value! 'OPTIMIZER-PRIMITIVE-FOLD #t)
(set-top-level-value! 'OPTIMIZER-CONSTANT-PROP #t)
(set-top-level-value! 'COMPILER-VM-PRIMITIVES #t)

(define fold-through-constant
  '(lambda (n)
     ((lambda (x) (fx+ x 1)) 41)))

(define one-remaining-arg
  '(lambda (n)
     ((lambda (x y) (fx+ x y)) 1 n)))

(define assigned-arg
  '(lambda (n)
     ((lambda (x) (set! x 2) x) 1)))

(define captured-set!
  '(lambda (n)
     ((lambda (x)
        (lambda () (set! x 2))
        x)
      1)))

(define defined-arg
  '(lambda (n)
     ((lambda (x) (define x 2) x) 1)))

(define shadowed-inner-lambda
  '(lambda (n)
     ((lambda (x)
        (lambda (x) (fx+ x 1)))
      1)))

(define quoted-symbol
  '(lambda (n)
     ((lambda (x) (eq? x 'a)) 'a)))

(define quoted-pair-refused
  '(lambda (n)
     ((lambda (x) (eq? x x)) '(a))))

(print (contains-head? 'fx+ (optimized fold-through-constant)))
(print (inline-let-arg-count (optimized fold-through-constant)))
(print (inline-let-arg-count (optimized one-remaining-arg)))
(print (inline-let-arg-count (optimized assigned-arg)))
(print (inline-let-arg-count (optimized captured-set!)))
(print (inline-let-arg-count (optimized defined-arg)))
(print (contains-head? 'fx+ (optimized shadowed-inner-lambda)))
(print (contains-head? 'eq? (optimized quoted-symbol)))
(print (contains-head? 'eq? (optimized quoted-pair-refused)))

(set-top-level-value! 'OPTIMIZER-CONSTANT-PROP #f)
(print (contains-head? 'fx+ (optimized fold-through-constant)))
(print (inline-let-arg-count (optimized one-remaining-arg)))

(set-top-level-value! 'OPTIMIZER-CONSTANT-PROP saved-constant-prop)
