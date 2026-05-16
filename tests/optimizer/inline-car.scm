(define (head-name x)
  (if (and (pair? x) (identifier? (car x)))
    (rename-strip (car x))
    #f))

(define (contains-inline-let? x)
  (if (pair? x)
    (if (eq? (head-name x) '##arete#inline-let)
      #t
      (let loop ((xs x))
        (if (null? xs)
          #f
          (if (contains-inline-let? (car xs))
            #t
            (loop (cdr xs))))))
    #f))

(define sample
  (expand-toplevel
    '(lambda (z)
       ((lambda (x) x) z))
    (top-level-value '*core-module*)))

(print (contains-inline-let? (optimize-toplevel sample)))

(define named-loop-sample
  (expand-toplevel
    '(lambda (n)
       (let loop ((i n))
         (if (fx= i 0) i (loop (fx- i 1)))))
    (top-level-value '*core-module*)))

(print (contains-inline-let? (optimize-toplevel named-loop-sample)))

(set-top-level-value! 'OPTIMIZER-INLINE-CAR #f)
(print (contains-inline-let? (optimize-toplevel sample)))

(set-top-level-value! 'OPTIMIZER-INLINE-CAR #t)
(set-top-level-value! 'COMPILER-INLINE-CAR #f)
(print (contains-inline-let? (optimize-toplevel sample)))
