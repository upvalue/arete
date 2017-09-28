;; aif.scm - anaphoric if

;; this tests that
;; - macro bodies are expanded (using quasiquote)
;; - macro results are expanded (using let)
;; - it's possible to introduce variables into macro-expanded code

(define-syntax aif
  (lambda (x r c)
    `(let ((it ,(list-ref x 1)))
       (if it
           ,(list-ref x 2)
           ,(list-ref x 3)))))

(print (aif #t it 'failure))
(print (aif #f 'failure it))
