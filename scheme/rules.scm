;; rules.scm - rules implementation

;; TODO Keywords

(define ellipses '...)

(define (rules-try ellipses pattern form)
   (let ((kar (rules-match ellipses (car pattern) (car form)))
         (kdr (rules-match ellipses (cdr pattern) (cdr form))))
     (if (or (not kar) (not kdr))
       #f
       (append kar kdr))))

(define (rules-match ellipses pattern form)
  (cond
    ((and (self-evaluating? pattern) (self-evaluating? form))
     (if (equal? pattern form)
       (list '(#t #t))
             #f))
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
       (rules-match ellipses (car pattern) (car form))
       ;; Handle (asdf ...) ()
       (if (null? (cdr form))
         ;; ...
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
           (list (list (car pattern) (car form)))
           (if (and (null? (cdr form)) (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
             (list (list (car pattern) '()))
             (rules-try ellipses pattern form) ))
         ;; More form

         ;; one or more elements after ...
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
           (list (list (car pattern) form))
           ;; no special cases, continue
           (rules-try ellipses pattern form))))
     )
    ((and (pair? pattern) (null? form))
     (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
       (list (list (car pattern) '()))
       #f))
    (else
      #;(print "could not match:" pattern form) #f)
    ))

(define (fold-template matches a b)
  (cond
    ((pair? a)
     (cons (fold-right (lambda (a b) (fold-template matches a b)) '() a) b))
    ((symbol? a)
      (let* ((match (assq a matches))
             (kar (or match a)))
        (if (and (pair? b) (eq? (car b) '...))
          ;; requests a list
          (if (not match)
            (raise 'expand (print-string "... occurred in syntax-rules template after" a "which is not in the pattern") (list a))
            (append (cadr match) (cdr b)))
          (cons (if match (cadr match) (if (eq? a '...) a (make-rename #f a))) b))))
    (else (cons a b))))

(define (rules-substitute matches template)
  (fold-right (lambda (a b) (fold-template matches a b)) '() template))

;; (let ((var x)) y)
;; we have to rename everything except those variables which are introduced by the user.

#|
(print (rules-match '... '(hello) '(hello)))
(print (rules-match '... '(hello one) '(hello 1)))
(print (rules-match '... '(hello one two) '(hello bug 5)))
(print (rules-match '... '(hello one ...) '(hello bug 5)))
(print (rules-match '... '(hello one ...) '(hello bug)))
(print (rules-match '... '(hello one) '(hello (1))))
(print (rules-match '... '(hello (one two three ...)) '(hello (1 2 3 4))))
(print (rules-match '... '(hello (one two three)) '(hello (four five six))))
(print (rules-match '... '(hello (one two three ...)) '(hello (four five six))))
(print (rules-match '... '(hello one two three ...) '(hello 1 2)))
|#

;(print "matchery:" (rules-match '... '(print1 "stringlet" hello) '(print1 "stringlet" gub)))

;; bad patterns
;(print (rules-match '... '(hello one) '(hello 1 2)))
#|
(print "substitution result:" (rules-substitute (rules-match '... '(hello one) '(hello 1)) '(hello one)))
(print "substitution result:" (rules-substitute (rules-match '... '(hello (rest ...)) '(hello (2 3 4 5))) '(hello (rest ...))))
(print "substitution result:" (rules-substitute (rules-match '... '(hello rest ...) '(hello 2 3 4 5)) '(hello rest ...)))
(print "substitution result:" (rules-substitute (rules-match '... '(hello a (b ...) c) '(hello 2 (3 4) 5)) '(hello a (b ...) c)))
(print "substitution result:" (rules-substitute (rules-match '... '(print1 a) '(print1 #t)) '(print a)))
|#

(define (rules-execute name form pairs)
  (let loop ((form form)
             (rule (car pairs))
             (rules (cdr pairs)))

    (if (eq? (car form) name)
      (let ((match (rules-match '... (car rule) form )))
        (if match
          (begin
            (loop (rules-substitute match (cadr rule)) (car pairs) (cdr pairs)))

          (if (null? rules)
            (raise 'expand "failed to match pattern" (list form))
            (loop form (car rules) (cdr rules)))))
      form)))

(print "matched:" (rules-execute 'print1 '(print1 12345)
  '(
    ((print1 "string:" hello) (print hello))
    ((print1 hello) (print1 "string:" hello))
  )))

(print "matched:" (rules-execute 'let1 '(let1 name 12345 name)
  '(
    ((let1 binding value expr)
     (let ((binding value)
           (somethingelse 5))
       (print expr)
       (print somethingelse)))
  )))

(print "matched:" (rules-execute 'progn '(progn 1 2 3)
  '(
    ((progn body ...)
     (begin body ...))
  )
))

(print "matched:" (rules-execute 'nth-value '(nth-value 1 (values 1 2 3))
  '(
    ((nth-value n values-producing-form)
     (call-with-values 
       (lambda () values-producing-form)
       (lambda all-values (list-ref all-values n))))

  )
))
