;; closure.scm - make-adder style closure creation inside a hot loop.
;;
;; Isolates: closure allocation (fn.function_parent_env capture) and
;; apply of a freshly-built Function. The returned closure is applied
;; once per iter so closure-build cost dominates rather than repeated
;; dispatch of a shared closure.

(define (make-adder x) (lambda (y) (+ x y)))

(define (run n)
  (let loop ((i 0) (acc 0))
    (if (= i n)
        acc
        (loop (+ i 1) (+ acc ((make-adder i) 7))))))

(let ((start (current-millisecond)))
  (let ((r (run 2500000)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,closure,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
