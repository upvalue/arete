;; thread-macros.scm - clojure style threading macros

(define-syntax ->>
  (lambda (x)
    (syntax-assert-length<> x 1)

    (let ((var (cadr x))
          (appls (cddr x)))
      (print var appls)
      (pi#`(let ((result ,var))
        ,(fold-right
          (lambda (a b)
            (if (eq? b '())
              (append a (list var))
              (append a (list b))))
          '() appls))))))

(let ((text "HELLO WORLD"))
  (print
    (->> text
        (string-map char-downcase)
        (string-map 
          (lambda (c)
            (if (eqv? c #\h) #\H c))))))
