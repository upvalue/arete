(let-syntax
  ((hello (er-macro-transformer (lambda (x r c) ''success))))
    (display (hello))
    (newline))

(let-syntax
  ((hello (er-macro-transformer (lambda (x r c) '(hello2))))
   (hello2 (er-macro-transformer (lambda (x r c) ''success))))
    (display (hello))
    (newline))

(letrec-syntax
  ((hello (er-macro-transformer (lambda (x r c) '(hello2))))
   (hello2 (er-macro-transformer (lambda (x r c) ''success))))
    (display (hello))
    (newline))
