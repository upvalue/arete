(let ((var 'hello))
  (print var))

(let loop ((x 0))
  (if (fx= x 5)
      (print x)
      (loop (fx+ x 1))))
