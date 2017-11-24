;; stack-trace.scm. This isn't a test that is run automatically, but it should demonstrate a variety of different
;; sorts of stack traces (e.g. frames lost due to tail call optimization, repetitive entries due to non-tail recursive
;; function calls etc

(define (do-nothing) #f)

(define (spin-out n)
  (if (= n 10)
    (dont-have-a-cow-man!)
    (begin
      (spin-out (+ n 1))
      (do-nothing))))

;; lose some frames to tail call optimization
(define (start n)
  (if (= n 5)
    (spin-out 0)
    (start (+ n 1))))

(start 0)

