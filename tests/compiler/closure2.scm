(((lambda (a b c)
    (lambda ()
      (fx+ a b))
    (set! a 10)
    ;; 2 + 10 + 2 + 2 = 16
    (lambda (d)
      (fx+ a b c d))) 2 2 2) 2)
