;; r7rs-bench-prelude.scm - shim layer for running ecraven/r7rs-benchmarks under Arete.
;;
;; The benchmark harness in vendor/r7rs-benchmarks expects a few R7RS
;; procedures that Arete does not (yet) provide out of the box. We define
;; the minimum needed to get src/common.scm to load; individual benchmarks
;; that use further unsupported functions are expected to fail. That is
;; fine per the current scaffolding goals.
;;
;; This file lives outside vendor/ on purpose so that the vendored tree
;; stays untouched.

;; ---------------------------------------------------------------------------
;; Timing. Arete exposes (current-millisecond), which returns a fixnum of
;; wall-clock milliseconds. Per R7RS we convert that to jiffies (= ms here).

(define (jiffies-per-second) 1000)
(define (current-jiffy) (current-millisecond))
(define (current-second) (/ (current-millisecond) 1000.0))

;; ---------------------------------------------------------------------------
;; Number conversions. Arete ships exact->inexact / inexact->exact but not
;; the R7RS short spellings.

(define (inexact x) (exact->inexact x))
(define (exact x) (inexact->exact x))

;; ---------------------------------------------------------------------------
;; Misc R7RS procedures used by common.scm and many benchmark bodies.

(define (square x) (* x x))

(define (write-string s . rest)
  (let ((port (if (null? rest) (current-output-port) (car rest))))
    (display s port)))
