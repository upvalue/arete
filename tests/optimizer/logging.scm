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

(define (shape x)
  (list
    (contains-head? 'if x)
    (contains-head? '##arete#inline-let x)
    (contains-head? 'fx+ x)))

(define sample
  (expand-toplevel
    '(lambda (x)
       (if (not '#f)
         ((lambda (y) (fx+ y 1)) x)
         (fx+ x 2)))
    (top-level-value '*user-module*)))

(define saved-log (top-level-value 'COMPILER-OPTIMIZER-LOG))
(define saved-constant-if (top-level-value 'OPTIMIZER-CONSTANT-IF))

(set-top-level-value! 'COMPILER-OPTIMIZER-LOG #f)
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF #t)
(define quiet-shape (shape (optimize-toplevel sample)))

(set-top-level-value! 'COMPILER-OPTIMIZER-LOG #t)
(define logged-shape (shape (optimize-toplevel sample)))

(set-top-level-value! 'COMPILER-OPTIMIZER-LOG saved-log)
(set-top-level-value! 'OPTIMIZER-CONSTANT-IF saved-constant-if)

(print quiet-shape)
(print logged-shape)
(print (equal? quiet-shape logged-shape))
