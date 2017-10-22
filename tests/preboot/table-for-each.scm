(define x (make-table))

(table-set! x 'a 1)
(table-set! x 'b 2)
(table-set! x 'c 3)
(table-set! x 'd 4)
(table-set! x 'e 5)
(table-set! x 'g 6)
(table-set! x 'h 7)
(table-set! x 'i 8)
(table-set! x 'j 9)

(table-for-each
  (lambda (key value)
    (print key value))
  x)

