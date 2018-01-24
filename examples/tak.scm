;; takeuchi function

(define (tak x y z)
  (if (not (< y x))
    z
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y))))

(cond-expand
  ((and arete) (vmfunction->native! tak))
  (else))

(display (tak 40 20 11)) (newline)

