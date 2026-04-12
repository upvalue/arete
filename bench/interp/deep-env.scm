;; deep-env.scm - reference variables bound many frames up the env chain.
;;
;; Isolates: env_lookup_impl walking the parent chain. Each `let` allocates
;; its own env frame, so `a1` lives many frames up and every iteration
;; pays the chain walk. We sum 8 outer bindings per iter to amplify the
;; per-iteration cost relative to the loop's own accounting.

(define (run n)
  (let ((a1 1) (a2 2) (a3 3) (a4 4))
    (let ((b1 5) (b2 6) (b3 7) (b4 8))
      (let ((c1 9) (c2 10) (c3 11) (c4 12))
        (let ((d1 13) (d2 14) (d3 15) (d4 16))
          (let ((e1 17) (e2 18) (e3 19) (e4 20))
            (let ((f1 21) (f2 22) (f3 23) (f4 24))
              (let loop ((i 0) (acc 0))
                (if (= i n)
                    acc
                    (loop (+ i 1)
                          (+ acc a1 a2 b1 b2 c1 c2 d1 d2 e1 e2 f1 f2)))))))))))

(let ((start (current-millisecond)))
  (let ((r (run 1500000)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,deep-env,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
