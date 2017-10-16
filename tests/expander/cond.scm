(print
  (cond
    ((eq? #t #t) 1 2 3 'success)
    (else 'failure)))

(print
  (cond
    (else 'success)))

(print
  (cond
    ((eq? #f #t) 'failure)
    ((eq? #t #t) 'success)
    (else 'failure)))
