(print
  (try
    (lambda () #t)
    (lambda () #t)))

(print
  (try
    (lambda () (raise 'thing "asdf"))
    (lambda (exc)
      (if (eq? (exception-tag exc) 'thing)
        #t
        #f))))

;; try re-raising an exception and then catching it

(print 
  (try
    (lambda ()
      (try
        (lambda () (raise 'thing "asdf"))
        (lambda (exc) #f)))
    (lambda (exc)
      (eq? (exception-tag exc) 'thing))))

