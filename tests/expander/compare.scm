;; compare.scm

(define-syntax test-compare
  (lambda (x r c)
    (print (c 'a 'a)) ; > #t
    (print (c (r 'a) (r 'a))))) ; > #t

(test-compare)

(define-syntax test-compare2
  (lambda (x r c)
    (print (c (r 'else) 'else))))

(test-compare2) ; #t

(lambda (else) (test-compare2)) ; #f
    


