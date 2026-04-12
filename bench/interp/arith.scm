;; arith.scm - tight tail-recursive loop of integer arithmetic.
;;
;; Isolates: arithmetic dispatch cost (+ - < =) and tail-call handling
;; in the interpreter's apply path. Env is shallow (one frame, 3 bindings)
;; so env_lookup is not a confounder.

(define (run n)
  (let loop ((i 0) (acc 0))
    (if (= i n)
        acc
        (loop (+ i 1) (+ acc (* i 3) (- i 1))))))

(let ((start (current-millisecond)))
  (let ((r (run 2000000)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,arith,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
