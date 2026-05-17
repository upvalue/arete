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

(define saved-constant-if (top-level-value 'OPTIMIZER-CONSTANT-IF))
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF #t)

(print (optimized '(if #t 1 2)))
(print (optimized '(if #f 1 2)))
(print (optimized '(if '() 1 2)))
(print (optimized '(if (not '#f) (not '#f) (not '#t))))

(print (contains-head? 'if (optimized '(if #t 1 2))))
(print (contains-head? 'if (optimized '(if #f 1))))

(set-top-level-value! 'OPTIMIZER-CONSTANT-IF #f)
(print (contains-head? 'if (optimized '(if #t 1 2))))
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF saved-constant-if)
