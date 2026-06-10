;; callrate.scm - VM non-tail call rate microbenchmark.
;;
;; Isolates: the apply_vm calling convention. A 2-argument self-recursive
;; function whose body is one compare, two non-tail calls and one add, so
;; nearly all time is call/return machinery rather than opcode work.
;;
;; Call count is exact and deterministic: T(n) = 1 + T(n-1) + T(n-2) with
;; T(0) = T(1) = 1, i.e. T(n) = 2*fib(n+1) - 1. For n = 34 that is
;; 18,454,929 calls. utils/run-callrate.sh divides self-reported wall time
;; by vm.calls from a --perf-report run to get ns/call.

(define (cr n acc)
  (if (< n 2)
      acc
      (+ (cr (- n 1) acc) (cr (- n 2) acc))))

(let ((start (current-millisecond)))
  (let ((r (cr 34 1)))
    (let ((elapsed (/ (- (current-millisecond) start) 1000.0)))
      (display "+!CSVLINE!+arete-vm,callrate,")
      (display elapsed)
      (newline)
      (display ";; result: ") (display r) (newline))))
