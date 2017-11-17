(define x (make-table))

;; note that this foolishly relies on table iteration order being the same each time. which it is, so long as we don't
;; change anything about the hash table implementation.

(table-set! x 'a 1)
(table-set! x 'b 2)
(table-set! x 'c 3)

(table-for-each
  (lambda (key value)
    (print key value))
  x)

