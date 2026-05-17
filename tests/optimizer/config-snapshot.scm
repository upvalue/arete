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

(define user-mod (top-level-value '*user-module*))

(define inline-sample
  (expand-toplevel
    '(lambda (z)
       ((lambda (x) x) z))
    user-mod))

(define named-loop-sample
  (expand-toplevel
    '(lambda (n)
       (let loop ((i n))
         (if (fx< i 1)
           i
           (loop (fx- i 1)))))
    user-mod))

(define constant-if-sample
  (expand-toplevel
    '(if #t 1 2)
    user-mod))

(define primitive-fold-sample
  (expand-toplevel
    '(not '#f)
    user-mod))

(define saved-top-level-value top-level-value)
(define saved-compiler-optimize (top-level-value 'COMPILER-OPTIMIZE))
(define saved-inline-car (top-level-value 'OPTIMIZER-INLINE-CAR))
(define saved-compiler-inline-car (top-level-value 'COMPILER-INLINE-CAR))
(define saved-named-let (top-level-value 'OPTIMIZER-NAMED-LET))
(define saved-primitive-fold (top-level-value 'OPTIMIZER-PRIMITIVE-FOLD))
(define saved-constant-if (top-level-value 'OPTIMIZER-CONSTANT-IF))
(define saved-vm-primitives (top-level-value 'COMPILER-VM-PRIMITIVES))

(define optimizer-config-flags
  '(COMPILER-OPTIMIZE
    OPTIMIZER-INLINE-CAR
    COMPILER-INLINE-CAR
    OPTIMIZER-NAMED-LET
    OPTIMIZER-PRIMITIVE-FOLD
    OPTIMIZER-CONSTANT-IF
    COMPILER-VM-PRIMITIVES))

(define (set-config-flags! value)
  (set-top-level-value! 'COMPILER-OPTIMIZE value)
  (set-top-level-value! 'OPTIMIZER-INLINE-CAR value)
  (set-top-level-value! 'COMPILER-INLINE-CAR value)
  (set-top-level-value! 'OPTIMIZER-NAMED-LET value)
  (set-top-level-value! 'OPTIMIZER-PRIMITIVE-FOLD value)
  (set-top-level-value! 'OPTIMIZER-CONSTANT-IF value)
  (set-top-level-value! 'COMPILER-VM-PRIMITIVES value))

(define (restore-config-flags!)
  (set-top-level-value! 'COMPILER-OPTIMIZE saved-compiler-optimize)
  (set-top-level-value! 'OPTIMIZER-INLINE-CAR saved-inline-car)
  (set-top-level-value! 'COMPILER-INLINE-CAR saved-compiler-inline-car)
  (set-top-level-value! 'OPTIMIZER-NAMED-LET saved-named-let)
  (set-top-level-value! 'OPTIMIZER-PRIMITIVE-FOLD saved-primitive-fold)
  (set-top-level-value! 'OPTIMIZER-CONSTANT-IF saved-constant-if)
  (set-top-level-value! 'COMPILER-VM-PRIMITIVES saved-vm-primitives))

(define (optimize-while-flipping-flags expr)
  (set-config-flags! #t)
  (set! top-level-value
    (lambda (name . rest)
      (let ((value (apply saved-top-level-value (cons name rest))))
        (when (memq name optimizer-config-flags)
          (set-top-level-value! name #f))
        value)))
  (let ((result (optimize-toplevel expr)))
    (set! top-level-value saved-top-level-value)
    (restore-config-flags!)
    result))

(print (contains-head? '##arete#inline-let
         (optimize-while-flipping-flags inline-sample)))
(print (contains-head? '##arete#named-loop
         (optimize-while-flipping-flags named-loop-sample)))
(print (optimize-while-flipping-flags constant-if-sample))
(print (optimize-while-flipping-flags primitive-fold-sample))
(print (top-level-value 'OPTIMIZER-CONSTANT-IF))
