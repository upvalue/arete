(define x (make-table))

(define loop
  (lambda (i)
    (if (fx< i 10)
      (begin
        (table-set! x i (fx- 0 i))
        (loop (fx+ i 1))))))

(loop 0)

(define loop2
  (lambda (i)
    (if (fx< i 10)
      (begin
        (print (table-ref x i))
        (loop2 (fx+ i 1))))))

(loop2 0)

(table-set! x 'one "one")
(table-set! x 'two "two")
(table-set! x 'three "three")
(table-set! x "four" "four")

(print (table-ref x 'one))
(print (table-ref x 'two))
(print (table-ref x 'three))
(print (table-ref x "four"))

