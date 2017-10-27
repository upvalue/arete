(define lyst
  (lambda (a b . rest)
    (list a b rest)))

(print (lyst 1 2))
(lyst 1 2 3)

