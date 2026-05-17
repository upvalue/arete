(define (head-name x)
  (if (and (pair? x) (identifier? (car x)))
    (rename-strip (car x))
    #f))

(define (count-head name x)
  (cond
    ((not (pair? x)) 0)
    ((eq? (head-name x) name)
     (fx+ 1 (count-list name x)))
    (else (count-list name x))))

(define (count-list name xs)
  (cond
    ((null? xs) 0)
    ((pair? xs)
     (fx+ (count-head name (car xs))
          (count-list name (cdr xs))))
    (else (count-head name xs))))

(define (contains-head? name x)
  (fx> (count-head name x) 0))

(define (optimized expr)
  (optimize-toplevel
    (expand-toplevel expr (top-level-value '*user-module*))))

(define saved-test-if (top-level-value 'OPTIMIZER-TEST-IF))
(define saved-vm-primitives (top-level-value 'COMPILER-VM-PRIMITIVES))

(set-top-level-value! 'COMPILER-OPTIMIZE #t)
(set-top-level-value! 'OPTIMIZER-TEST-IF #t)

(define positive
  '(lambda (x)
     (if (if x #t #f) 1 2)))

(define value-context
  '(lambda (x)
     (if x #t #f)))

(define boolean-value-context
  '(lambda (x)
     (if (eq? x 'a) #t #f)))

(define not-value-context
  '(lambda (x)
     (if (not x) #t #f)))

(define fx-value-context
  '(lambda (x y)
     (if (fx< x y) #t #f)))

(define quoted-boolean-value-context
  '(lambda (x)
     (if (null? x) '#t '#f)))

(define pair-value-context
  '(lambda (x)
     (if (pair? x) #t #f)))

(define shadowed-eq-value-context
  '(lambda (eq? x)
     (if (eq? x 'a) #t #f)))

(define non-boolean-branch
  '(lambda (x)
     (if (if x 1 #f) 1 2)))

(define negated-test
  '(lambda (x)
     (if (if x #f #t) 1 2)))

(define not-test
  '(lambda (x)
     (if (not x) 1 2)))

(define composed-not-test
  '(lambda (x)
     (if (not (if x #t #f)) 1 2)))

(define shadowed-not-test
  '(lambda (not x)
     (if (not x) 1 2)))

(define quoted-booleans
  '(lambda (x)
     (if (if x '#t '#f) 1 2)))

(define shadowed-not
  '(lambda (not x)
     (if (if x #f #t) 1 2)))

(define quoted-negated
  '(lambda (x)
     (if (if x '#f '#t) 1 2)))

(print (count-head 'if (optimized positive)))
(print (count-head 'if (optimized value-context)))
(print (count-head 'if (optimized boolean-value-context)))
(print (count-head 'if (optimized not-value-context)))
(print (count-head 'if (optimized fx-value-context)))
(print (count-head 'if (optimized quoted-boolean-value-context)))
(print (count-head 'if (optimized pair-value-context)))
(print (count-head 'if (optimized shadowed-eq-value-context)))
(print (count-head 'if (optimized non-boolean-branch)))
(print (count-head 'if (optimized negated-test)))
(print (count-head 'if (optimized not-test)))
(print (contains-head? 'not (optimized not-test)))
(print (count-head 'if (optimized composed-not-test)))
(print (count-head 'if (optimized shadowed-not-test)))
(print (count-head 'if (optimized quoted-booleans)))
(print (contains-head? 'not (optimized shadowed-not)))
(print (count-head 'if (optimized quoted-negated)))

(set-top-level-value! 'COMPILER-VM-PRIMITIVES #f)
(print (count-head 'if (optimized positive)))
(print (count-head 'if (optimized boolean-value-context)))
(print (contains-head? 'not (optimized not-test)))
(set-top-level-value! 'COMPILER-VM-PRIMITIVES saved-vm-primitives)

(set-top-level-value! 'OPTIMIZER-TEST-IF #f)
(print (count-head 'if (optimized positive)))
(print (count-head 'if (optimized negated-test)))
(print (contains-head? 'not (optimized not-test)))

(set-top-level-value! 'OPTIMIZER-TEST-IF saved-test-if)
