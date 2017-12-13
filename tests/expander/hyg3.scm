(define asdf 'incorrect1)
((lambda (asdf)
  (let-syntax
    ((foo (lambda (x) (rename 'asdf))))
    ((lambda (asdf)
       (print (foo))
      )
     'incorrect2))) 
 'correct)


