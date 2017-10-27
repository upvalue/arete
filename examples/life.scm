;; life.scm - Conway's Game of Life

(define (make-grid x y)
  (define vec (make-vector y #f))
  (let loop ((i 0))
    (if (= i y)
      vec
      (begin
        (vector-set! vec i (make-vector x 0))
        (loop (+ i 1))))))

(define (print-grid g)
  (let loop ((i 0))
    (unless (= i (vector-length g))
      (display "(")
      (let loop2 ((sg (vector-ref g i))
                  (j 0))
        (unless (= j (vector-length sg))
          (display
            (if (eq? (vector-ref sg j) 1) "#" (if (= j (- (vector-length sg) 1)) "-" "-")))
          (unless (= j (- (vector-length sg) 1))
            (display " "))
          (loop2 sg (+ j 1))))
      (display ")")
      (newline)
      (loop (+ i 1))      
      )))

(define (get-cell g x y)
  ;(print x y)
  (cond 
    ((< x 0) 0)
    ((< y 0) 0)
    ((>= x (vector-length (vector-ref g 0))) 0)
    ((>= y (vector-length g)) 0)
    (else (vector-ref (vector-ref g y) x))))

(define (set-cell! g x y n)
  (vector-set! (vector-ref g y) x n))

(define (next-cell g value x y)
  (let ((n (+ (get-cell g (- x 1) (- y 1))
              (get-cell g (- x 1) y)
              (get-cell g (- x 1) (+ y 1))
              (get-cell g x (- y 1))
              (get-cell g x (+ y 1))
              (get-cell g (+ x 1) (- y 1))
              (get-cell g (+ x 1) y)
              (get-cell g (+ x 1) (+ y 1)))))
    (cond 
      ;; live cell with two or three living cells becomes dead 
      ((and (= value 1) (or (= n 2) (= n 3))) 1)
      ;; dead cell with three live cells becomes live
      ((and (= value 0) (= n 3)) 1)
      ;; everything else is dead
      (else 0))))

(define (next-grid g)
  (let* ((y (vector-length g))
         (x (vector-length (vector-ref g 0)))
         (new-grid (make-grid x y)))
    (let loop ((xi 0)
               (yi 0))
      (unless (= yi y)
        (set-cell! new-grid xi yi (next-cell g (get-cell g xi yi) xi yi))
        (loop
          (if (= (+ xi 1) x) 0 (+ xi 1))
          (if (= (+ xi 1) x) (+ yi 1) yi))))
    new-grid))

(define (simulate grid turns)
  (define loop
    (lambda (i grid)
      (unless (= i turns)
        (display "turn: ")
        (display (+ i 1))
        (newline)
        (print-grid grid)
        (loop (+ i 1) (next-grid grid)))))

  (loop 0 grid))

;; Glider that goes into the lower right and becomes a 2x2 box
(begin
  (define grid (make-grid 8 8))
  (set-cell! grid 2 0 1)
  (set-cell! grid 3 1 1)
  (set-cell! grid 1 2 1)
  (set-cell! grid 2 2 1)
  (set-cell! grid 3 2 1)
  (print-grid grid)

  (simulate grid 25))

#|
;; Blinker
(begin
  (define grid (make-grid 3 3))

  (set-cell! grid 0 1 1)
  (set-cell! grid 1 1 1)
  (set-cell! grid 2 1 1))

(simulate grid 20)
|#
