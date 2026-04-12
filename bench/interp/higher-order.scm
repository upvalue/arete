;; higher-order.scm - apply-heavy fold with a user lambda.
;;
;; Isolates: the higher-order call path — fold calls `f` on every pair,
;; `f` is itself a closure built at the call site. This exercises
;; repeated apply of the same closure (as opposed to closure.scm which
;; builds a fresh closure per call) plus list traversal.

(define (my-fold f init xs)
  (let loop ((xs xs) (acc init))
    (if (pair? xs)
        (loop (cdr xs) (f (car xs) acc))
        acc)))

(define (iota n)
  (let loop ((i n) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons i acc)))))

(define data (iota 500))

(define (run reps)
  (let loop ((i 0) (acc 0))
    (if (= i reps)
        acc
        (loop (+ i 1)
              (+ acc (my-fold (lambda (x a) (+ a (* x 2))) 0 data))))))

(let ((start (current-millisecond)))
  (let ((r (run 2000)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,higher-order,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
