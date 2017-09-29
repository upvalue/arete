(print ((lambda () 'success)))
(print ((lambda (arg) arg) 'success))
(print ((lambda rest rest) 1 2 3))
(print ((lambda (arg . rest) rest) 1 2 3))

