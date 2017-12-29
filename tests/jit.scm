;; tests/jit.scm - primitive native compilation tests

(define-syntax test-equals
  (lambda (x)
    #`(let ()
        (define fn ,(caddr x))
        (print ";; compiling function" (quote ,(caddr x)))
        (define result
          (try (lambda () (apply vmfunction->native (cons fn (quote ,(cdddr x)))))
               (lambda (exc) ''exception)))
        (set! test-results
          (append 
            test-results
            (list 
              (if (not (equal? result (quote ,(cadr x))))
                (print-string ":( expected expression" (quote ,(cons (caddr x) (cdddr x))) "to result in" (quote ,(cadr x)) "but got" result)
                (print-string ":)" (quote ,(cons (caddr x) (cdddr x))) "=>" result))))))))

(define global 123)

(define test-results '())

(test-equals #t (lambda () #t))
(test-equals "asdf" (lambda () #t "asdf"))
(test-equals 123 (lambda () global))
(test-equals 124 (lambda () (set! global 124) global))

(test-equals 4 (lambda () (fx+ 2 2)))

(test-equals 423 (lambda (a) a) 423)
(test-equals 10 (lambda (a b) (fx+ a b)) 5 5)
(test-equals 723 (lambda (a) (set! a 723) a) #f)

(test-equals 5
             (lambda ()
               (let ((a 5))
                 a)))

(test-equals #t (lambda () (if #t #t #f)))
(test-equals #t (lambda () (if (if #t #t #f) #t #f)))
(test-equals 'asdf (lambda (a) (if a a #f)) 'asdf) 

(define (return-two) 2)

(test-equals 2 (lambda () (return-two)))
(test-equals 2 (lambda () (return-two) (return-two) (return-two)))

(define (return-exc) (raise 'asdf "asdf" #t))

(test-equals 'exception (lambda () (return-exc)))

(define (return-arg a) a)

(test-equals 2 (lambda () (return-arg 2)))
(test-equals 2 (lambda () (return-arg 2) (return-arg 2) (return-arg 2)))

(test-equals 'exception (lambda () (2)))

(for-each print test-results)
