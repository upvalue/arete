(let-syntax
  ((hello (lambda (x r c) ''success)))
    (display (hello))
    (newline))

(let-syntax
  ((hello (lambda (x r c) '(hello2)))
   (hello2 (lambda (x r c) ''success)))
    (display (hello))
    (newline))

(letrec-syntax
  ((hello (lambda (x r c) '(hello2)))
   (hello2 (lambda (x r c) ''success)))
    (display (hello))
    (newline))
