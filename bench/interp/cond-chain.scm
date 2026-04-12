;; cond-chain.scm - long cond chain inside a tight loop.
;;
;; Isolates: the S_COND special-form dispatcher, per-clause predicate
;; evaluation, and branch selection. Each iter touches several clauses
;; before hitting a match so we're measuring linear cond walk, not just
;; the first-clause-wins case.

(define (classify n)
  (cond ((= n 0) 'zero)
        ((= n 1) 'one)
        ((= n 2) 'two)
        ((= n 3) 'three)
        ((= n 4) 'four)
        ((= n 5) 'five)
        ((= n 6) 'six)
        ((= n 7) 'seven)
        (else    'many)))

(define (run n)
  (let loop ((i 0) (acc 0))
    (if (= i n)
        acc
        (loop (+ i 1)
              (+ acc (if (eq? (classify (modulo i 9)) 'many) 1 0))))))

(let ((start (current-millisecond)))
  (let ((r (run 500000)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,cond-chain,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
