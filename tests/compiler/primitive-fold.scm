(define shadow-call
  (lambda (not)
    (not '#f)))

(define shadow-proc
  (lambda (x) 'shadowed))

(list
  (not '#f)
  (not 'value)
  (null? '())
  (pair? '(a b))
  (pair? '())
  (fixnum? 12)
  (eq? 'a 'a)
  (eq? 'a 'b)
  (fx+ 2 3)
  (fx- 2 5)
  (fx< 2 5)
  (shadow-call shadow-proc))
