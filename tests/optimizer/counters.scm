(define (optimized expr)
  (optimize-toplevel
    (expand-toplevel expr (top-level-value '*user-module*))))

(define saved-counters (top-level-value 'COMPILER-OPTIMIZER-COUNTERS))
(define saved-constant-if (top-level-value 'OPTIMIZER-CONSTANT-IF))

(set-top-level-value! 'COMPILER-OPTIMIZER-COUNTERS #t)
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF #t)
(optimizer-reset-counters!)

(optimized
  '(lambda (z)
     ((lambda (x) x) z)))

(optimized
  '(lambda (n)
     (let loop ((i n))
       (if i
         i
         (loop i)))))

(optimized '(not '#f))
(optimized '(not x))
(optimized '(if #t 1 2))
(optimized '(if #f 1))

(optimized
  '(lambda (n)
     (let loop ((i n))
       (cons (loop i) '()))))

(print (optimizer-counter-snapshot))

(set-top-level-value! 'COMPILER-OPTIMIZER-COUNTERS #f)
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF #t)
(optimizer-reset-counters!)
(optimized '(if #t (not '#f) (not '#t)))
(print (optimizer-counter-snapshot))

(set-top-level-value! 'COMPILER-OPTIMIZER-COUNTERS saved-counters)
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF saved-constant-if)
