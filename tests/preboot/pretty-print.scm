;; pretty-print.scm - Printing of basic shared structure
(define x '(#f))
(define y (cons x '()))

(set-car! x y)

(pretty-print x)
(pretty-print (cons x (cons y '())))

