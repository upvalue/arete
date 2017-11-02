;; bf.scm - brainfuck interpreter

;; Need file input & output

(define code (make-vector))
(define data (make-vector))
(define data-pointer 0)

(define (data-grow i)
  (if (< (vector-length data) i)
    (let loop ((c (vector-length data)))
      (vector-append! data 0)
      (unless (= i c)
        (loop (+ c 1))))))

(define (data-set! i x)
  ;; grow vector
  ;; TODO there should be some kind of vector-grow! vec length #f  thing.
  (data-grow i)
  (vector-set! data i (+ (vector-ref data i) 1)))

(define (data-get i)
  (data-grow i)
  (vector-ref data i))


;(data-set! 5 1)
;(print data)

#|
(let loop ()
  (let ((char (read-char)))
    (unless (eof-object? char)
      (case char
        ((#\+) (set! data-pointer (+ data-pointer 1)))
        ((#\-) (set! data-pointer (- data-pointer 1)))
        ((#\<) (data-set! data-pointer -1))
        ((#\>) (data-set! data-pointer +1))
        ((#\.) (write-char (integer->char (

      (loop))))

(print data-pointer)

|#
