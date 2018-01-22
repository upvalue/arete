(define port
  (open-input-file "tests/preboot/files.txt"))

(define loop
  (lambda (exprs)
    (define expr (read port))

    (if (eq? (value-bits expr) 18)
      exprs
      (cons expr (loop exprs)))))

(print (loop '()))

(close-input-port port)
(set! port #f)
(gc-collect) ;; trigger finalization
