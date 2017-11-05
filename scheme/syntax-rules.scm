;; syntax-rules.scm - syntax-rules implementation

(define (syntax-rules-match pattern form)
  (cond
    ((and (identifier? pattern) (or (identifier? form) (self-evaluating? form)))
     (list (list pattern form)))
    ;; (one ...) ()
    ((and (identifier? pattern) (null? form))
     (list (list pattern form)))
    ((and (identifier? pattern) (pair? form))
     (list (list pattern form)))
    ((and (pair? pattern) (pair? form))
     ;; try to get a ...
     ;(print pattern form)
     (if (and (null? (cdr pattern)) (null? (cdr form)))
       (syntax-rules-match (car pattern) (car form))
       ;; Handle (asdf ...) ()
       (if (null? (cdr form))
         ;; ...
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) '...))
           (list (list (car pattern) (car form)))
           (if (and (null? (cdr form)) (not (null? (cdr pattern))) (eq? (cadr pattern) '...))
             (list (list (car pattern) '()))
             (list-concat
               (syntax-rules-match (car pattern) (car form))
               (syntax-rules-match (cdr pattern) (cdr form)))))
         ;; More form

         ;; one or more elements after ...
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) '...))
           (list (list (car pattern) form))
           ;; no special cases, continue
           (list-concat
             (syntax-rules-match (car pattern) (car form))
             (syntax-rules-match (cdr pattern) (cdr form))))))
     )
    ((and (pair? pattern) (null? form))
     (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) '...))
       (list (list (car pattern) '()))
       #f))
    (else
      (print "could not match:" pattern form))
    
    ))

;; (let ((var x)) y)
;; we have to rename everything except those variables which are introduced by the user.


(print (syntax-rules-match '(hello) '(hello)))
(print (syntax-rules-match '(hello one) '(hello 1)))
(print (syntax-rules-match '(hello one two) '(hello bug 5)))
(print (syntax-rules-match '(hello one ...) '(hello bug 5)))
(print (syntax-rules-match '(hello one ...) '(hello bug)))
(print (syntax-rules-match '(hello one) '(hello (1))))
(print (syntax-rules-match '(hello (one two three ...)) '(hello (1 2 3 4))))
(print (syntax-rules-match '(herro one two three) '(herro two three)))
#|
(print (syntax-rules-match '(hello (one two three)) '(hello (four five six))))
(print (syntax-rules-match '(hello (one two three ...)) '(hello (four five six))))
(print (syntax-rules-match '(hello one two three ...) '(hello 1 2)))
|#


;; remove _ and asdf from name

;;

#;(define-syntax asdf
  (syntax-rules ()
    ((_ #t))))

;(print (syntax-rules-parser '(#t)))

