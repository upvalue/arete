;; tests/jit.scm - primitive native compilation tests

(define-syntax test-equals
  (lambda (x)
    #`(let ()
        (define fn ,(caddr x))
        (print ";; compiling function" (quote ,(caddr x)))
        (define result
          (try (lambda () (apply native-call (cons (vmfunction->native! fn) (quote ,(cdddr x)))))
               (lambda (exc) (print exc) ''exception)))
        (set! test-results
          (append 
            test-results
            (list 
              (if (not (equal? result (quote ,(cadr x))))
                (print-string ":( expected expression" (quote ,(cons (caddr x) (cdddr x))) "to result in" (quote ,(cadr x)) "but got" result)
                (print-string ":)" (quote ,(cons (caddr x) (cdddr x))) "=>" result))))))))

;; same as above, but don't compile the function as it's done by hand for more complex situations (closures)
(define-syntax test-equals-noc
  (lambda (x)
    #`(let ()
        (define fn ,(caddr x))
        (print ";; running already-compiled function" (quote ,(caddr x)))
        (define result
          (try (lambda () (apply native-call (cons fn (quote ,(cdddr x)))))
               (lambda (exc) (print exc) ''exception)))
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
;(test-equals "asdf" (lambda () #t "asdf"))
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
(define (return-true) #t)
(define (return-false) #f)

(test-equals 2 (lambda () (return-two)))
(test-equals 2 (lambda () (return-two) (return-two) (return-two)))
(test-equals #f (lambda () (return-false)))

(define (return-exc) (raise 'asdf "asdf" #t))

(test-equals 'exception (lambda () (return-exc)))

(define (return-arg a) a)

(test-equals 2 (lambda () (return-arg 2)))
(test-equals 2 (lambda () (return-arg 2) (return-arg 2) (return-arg 2)))

(test-equals #t (lambda () (not #f)))
(test-equals #f (lambda () (not #t)))

;; assumes inlining
(test-equals 2 (lambda () (let ((a 0) (b 1) (c 2)) c)))

(test-equals 'exception (lambda () (2)))

(test-equals () (lambda rest rest))

(test-equals (1 2 3) (lambda rest rest) 1 2 3)

(test-equals () (lambda (a b . c) c) 1 2)
(test-equals (3) (lambda (a b . c) c) 1 2 3)

;; calling another native function
(define (native-thing) (define a 'thing) a)

(vmfunction->native! native-thing)

(test-equals thing (lambda () (native-thing)))

;; first closure: access a native and closed variable from a VM closure

(define (make-closure a)
  (define (closure) a)
  (closure)
  closure)

(test-equals 5
             (lambda ()
               (define cl (make-closure 5))
               (cl)))

(define (make-closure2)
  (define (closure) a)
  (vmfunction->native! closure)
  (closure)
  closure)

(test-equals 6 (lambda () (define cl (make-closure 6)) (cl)))

;; closure 2: access a native and closed upvalue from a native closure

(define (make-closure3)
  (define (closure) a)
  (vmfunction->native! closure)
  (closure)
  closure)

(vmfunction->native! make-closure3)

(test-equals 7 (lambda () (define cl (make-closure 7)) (cl)))

;; closure 3: setting varibles

(define (make-closure4 a)
  (define (closure) (set! a (+ a 1)) a)
  (closure)
  closure)

(test-equals 12 (lambda () (define cl (make-closure4 8)) (cl) (cl) (cl)))

;; primitives written in direct assembly

(test-equals asdf (lambda () (car '(asdf))))
(test-equals bsdf (lambda () (cdr '(asdf . bsdf))))
(test-equals 'exception (lambda () (car #f)))
(test-equals 'exception (lambda () (cdr #f)))
(test-equals 'exception (lambda () (car 5)))
(test-equals 'exception (lambda () (car 5)))

(test-equals #t (lambda () (not #f)))
(test-equals #f (lambda () (not #t)))
(test-equals #f (lambda () (not 5)))
(test-equals #f (lambda () (not "thing")))

(for-each print test-results)
