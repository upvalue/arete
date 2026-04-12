;; global-ref.scm - tight loop dominated by top-level variable references.
;;
;; Isolates: the toplevel path in env_lookup — the interpreter walks the
;; env chain until reached_toplevel and then does a global table hit on
;; each reference. Each iteration touches 8 distinct globals.

(define a1 11)
(define a2 22)
(define a3 33)
(define a4 44)
(define a5 55)
(define a6 66)
(define a7 77)
(define a8 88)

(define (run n)
  (let loop ((i 0) (acc 0))
    (if (= i n)
        acc
        (loop (+ i 1) (+ acc a1 a2 a3 a4 a5 a6 a7 a8)))))

(let ((start (current-millisecond)))
  (let ((r (run 2000000)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,global-ref,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
