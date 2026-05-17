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
    (expand-toplevel expr (top-level-value '*user-module*))))

(print (optimizer-primitive-simple? 'not))
(print (optimizer-primitive-foldable? 'not))
(print (optimizer-primitive-simple? 'symbol?))
(print (optimizer-primitive-foldable? 'symbol?))
(print (optimizer-primitive-simple? 'pair?))
(print (optimizer-primitive-foldable? 'pair?))

(print (optimized '(not '#f)))
(print (optimized '(not 'value)))
(print (optimized '(null? '())))
(print (optimized '(fixnum? 12)))
(print (optimized '(eq? 'a 'a)))
(print (optimized '(eq? 'a 'b)))
(print (optimized '(fx+ 2 3)))
(print (optimized '(fx- 2 5)))
(print (optimized '(fx< 2 5)))
(print (contains-head? 'pair? (optimized '(pair? '(a b)))))

(define shadow-lambda (optimized '(lambda (not) (not '#f))))
(print (self-evaluating? (caddr shadow-lambda)))

(define saved-fold (top-level-value 'OPTIMIZER-PRIMITIVE-FOLD))
(set-top-level-value! 'OPTIMIZER-PRIMITIVE-FOLD #f)
(print (contains-head? 'not (optimized '(not '#f))))
(set-top-level-value! 'OPTIMIZER-PRIMITIVE-FOLD saved-fold)

(define saved-vm-primitives (top-level-value 'COMPILER-VM-PRIMITIVES))
(set-top-level-value! 'COMPILER-VM-PRIMITIVES #f)
(print (contains-head? 'not (optimized '(not '#f))))
(set-top-level-value! 'COMPILER-VM-PRIMITIVES saved-vm-primitives)
