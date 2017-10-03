;; module-shadow

;; tests that renames are properly gensymed at the module level

(define shadow 0)

(print shadow)

(define-syntax shadow1
  (lambda (x r c)
    `(begin
       (define ,(r 'shadow) 1)
       (print shadow)
       (print ,(r 'shadow)))))

(define-syntax shadow2
  (lambda (x r c)
    `(begin
       (define ,(r 'shadow) 2)
       (print shadow)
       (set! ,(r 'shadow) 3)
       (print ,(r 'shadow))
       ((lambda (,(r 'shadow)) (print ,(r 'shadow))) 4))))

(define-syntax shadow3
  (lambda (x r c)
    `(print ,(r 'shadow))))

(shadow1)
(shadow2)

(shadow3)

(let ((shadow 'failure))
  (shadow3))
