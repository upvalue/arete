(define side 0)

(define mark
  (lambda (value)
    (set! side (fx+ side 1))
    value))

(list
  (if #t (mark 'then) (mark 'bad))
  side
  (if #f (mark 'bad) (mark 'false))
  side
  (if '() (mark 'nil-true) (mark 'bad))
  side
  (if (not '#f) (mark 'folded-test) (mark 'bad))
  side)
