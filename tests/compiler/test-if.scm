(define count 0)

(define mark
  (lambda (value)
    (set! count (fx+ count 1))
    value))

(define value-context
  (lambda (value)
    (if value #t #f)))

(define boolean-value-context
  (lambda (value)
    (if (eq? value 'truthy) #t #f)))

(define shadowed-eq-value-context
  (lambda (eq?)
    (if (eq? 'truthy) #t #f)))

(define shadowed-not-test
  (lambda (not)
    (if (not #f) 'bad 'shadowed-not-else)))

(list
  (if (if (mark 'truthy) #t #f)
    'then
    'else)
  count
  (begin
    (set! count 0)
    (if (if (mark #f) #t #f)
      'then
      'else))
  count
  (begin
    (set! count 0)
    (if (if (mark 'truthy) #f #t)
      'bad
      'inverted-else))
  count
  (begin
    (set! count 0)
    (if (if (mark #f) #f #t)
      'inverted-then
      'bad))
  count
  (value-context 'truthy)
  (value-context #f)
  (boolean-value-context 'truthy)
  (boolean-value-context 'other)
  (begin
    (set! count 0)
    (if (fx< (mark 1) (mark 2)) #t #f))
  count
  (begin
    (set! count 0)
    (if (not (mark #f)) 'not-then 'bad))
  count
  (begin
    (set! count 0)
    (if (not (mark 'truthy)) 'bad 'not-else))
  count
  (shadowed-not-test (lambda (value) #f))
  (shadowed-eq-value-context (lambda (value) 'truthy)))
