;; extended.scm - extended macros (which behave differently depending on whether they are used
;; as an identifier or in the CAR of an application position)

(define-syntax thing
  (combined-transformer
    (lambda (x)
      (if (identifier? x)
        "identifier" 
        "application"))))

(print thing)
(print (thing))
