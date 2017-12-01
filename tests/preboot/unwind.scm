;; unwind.scm - unwind protection
;; assumes that TRY works

(define var #f)

(try
  (lambda ()
    (unwind-protect
      (lambda ()
        (raise 'thing "!!!"))
      (lambda () (set! var #t))))
  (lambda (exc) #t))

(print var)
