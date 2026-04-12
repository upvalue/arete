;; gc-churn.scm - repeatedly allocate and drop short-lived lists.
;;
;; Isolates: nursery / minor-collection throughput. Each iter allocates
;; a fresh 40-element list, sums its head, and discards it. The list is
;; never retained so everything lives and dies inside the young space.

(define (make-list-n n)
  (let loop ((i 0) (acc '()))
    (if (= i n) acc (loop (+ i 1) (cons i acc)))))

(define (sum5 xs)
  (+ (car xs)
     (car (cdr xs))
     (car (cdr (cdr xs)))
     (car (cdr (cdr (cdr xs))))
     (car (cdr (cdr (cdr (cdr xs)))))))

(define (run reps)
  (let loop ((i 0) (acc 0))
    (if (= i reps)
        acc
        (loop (+ i 1) (+ acc (sum5 (make-list-n 40)))))))

(let ((start (current-millisecond)))
  (let ((r (run 30000)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,gc-churn,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
