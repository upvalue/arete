(define vars
  (let ((count 0))
    (define (set-count! c)
      (set! count c)
      count)

    (define (get-count) count)

    (define (increment!)
      (set! count (fx+ count 1))
      count)

    (list set-count! get-count increment!)))

(define set-count! (list-ref vars 0))
(define get-count (list-ref vars 1))
(define increment! (list-ref vars 2))

(print (get-count))
(print (set-count! 5))
(print (increment!))
(print (increment!))
(print (set-count! 0))
