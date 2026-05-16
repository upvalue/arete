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

(define normal
  '(lambda (n)
     (let loop ((i n) (acc 0))
       (if (fx= i 0)
         acc
         (loop (fx- i 1) (fx+ acc i))))))

(define non-tail-call
  '(lambda (n)
     (let loop ((i n))
       (if (fx= i 0)
         0
         (fx+ 1 (loop (fx- i 1)))))))

(define captured-loop
  '(lambda (n)
     (let loop ((i n))
       (if (fx= i 0)
         loop
         (loop (fx- i 1))))))

(define assigned-loop
  '(lambda (n)
     (let loop ((i n))
       (set! loop (lambda (x) x))
       i)))

(print (contains-head? '##arete#named-loop (optimized normal)))
(print (contains-head? '##arete#named-loop (optimized non-tail-call)))
(print (contains-head? '##arete#named-loop (optimized captured-loop)))
(print (contains-head? '##arete#named-loop (optimized assigned-loop)))

(set-top-level-value! 'OPTIMIZER-NAMED-LET #f)
(print (contains-head? '##arete#named-loop (optimized normal)))

(set-top-level-value! 'OPTIMIZER-NAMED-LET #t)
(set-top-level-value! 'COMPILER-OPTIMIZE #f)
(print (contains-head? '##arete#named-loop (optimized normal)))
