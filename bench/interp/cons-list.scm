;; cons-list.scm - pair allocation plus car/cdr walking.
;;
;; Isolates: cons allocation (make_pair), car/cdr access, and the GC
;; bookkeeping of churning through pairs. Each outer iteration builds a
;; fresh length-200 list and sums it — the list is dropped after each
;; pass so nursery traffic dominates.

(define (build n)
  (let loop ((i 0) (acc '()))
    (if (= i n) acc (loop (+ i 1) (cons i acc)))))

(define (sum-list l)
  (let loop ((xs l) (acc 0))
    (if (pair? xs) (loop (cdr xs) (+ acc (car xs))) acc)))

(define (run reps len)
  (let loop ((i 0) (acc 0))
    (if (= i reps) acc (loop (+ i 1) (+ acc (sum-list (build len)))))))

(let ((start (current-millisecond)))
  (let ((r (run 4000 200)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-interp,cons-list,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
