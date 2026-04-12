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

(define (exact-integer? x) (and (integer? x) (exact? x)))

;; R7RS string->number must return #f for non-numeric input. Arete's
;; builtin raises a reader error instead; wrap it so the compiler
;; benchmark's valid-module-name? (which uses string->number as a
;; numeric-string test) works.
(define %arete-raw-string->number string->number)
(define (string->number s . rest)
  (define result '(#f))
  (try
    (lambda ()
      (set! result
        (list (if (null? rest)
                  (%arete-raw-string->number s)
                  (%arete-raw-string->number s (car rest))))))
    (lambda (exc) 'string->number-non-numeric))
  (car result))

(define (exact-integer-sqrt n)
  (let ((sq (exact (floor (sqrt n)))))
    (values sq (- n (* sq sq)))))

(define rational? real?)

(define (gcd . args)
  (define (gcd2 a b)
    (if (= b 0) (abs a) (gcd2 b (remainder a b))))
  (cond ((null? args) 0)
        ((null? (cdr args)) (abs (car args)))
        (else (let loop ((acc (gcd2 (car args) (cadr args))) (rest (cddr args)))
                (if (null? rest) acc (loop (gcd2 acc (car rest)) (cdr rest)))))))

(define (lcm . args)
  (cond ((null? args) 1)
        ((null? (cdr args)) (abs (car args)))
        (else (let loop ((acc (abs (car args))) (rest (cdr args)))
                (if (null? rest) acc
                    (let ((x (abs (car rest))))
                      (loop (if (= acc 0) 0 (* (quotient acc (gcd acc x)) x))
                            (cdr rest))))))))

(define (read-line . rest)
  (let ((port (if (null? rest) (current-input-port) (car rest))))
    (let loop ((chars '()))
      (let ((c (read-char port)))
        (cond ((eof-object? c)
               (if (null? chars) c (list->string (reverse chars))))
              ((char=? c #\newline) (list->string (reverse chars)))
              (else (loop (cons c chars))))))))

;; ack needs a deeper non-tail-recursive call stack than Arete's default
;; 1500-frame cap. See src/vm.cpp:149, src/state.cpp:161. Setting too
;; high causes a C-stack overflow / SIGSEGV. ack(3,12) needs ~64k frames.
(set-top-level-value! 'RECURSION-LIMIT 75000)
