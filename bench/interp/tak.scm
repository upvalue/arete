;; tak.scm - scaled-down Takeuchi function.
;;
;; Isolates: non-tail recursive calls (three child takes per parent) and
;; arithmetic on small integers. Classical interpreter stress; inputs
;; kept small because every call pays full apply-path cost under the
;; tree-walker.

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(let ((start (current-millisecond)))
  (let ((r (tak 24 16 8)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,tak,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
