;; rules.scm - rules implementation

;; TODO Literals

;; TODO Substitutions

;; TODO Dotted list
;; TODO Vectors

(define ellipses '...)

(define (rules-rename name)
  (make-rename #f name))

(define (rules-shadowed? name)
  #f)

(define (rules-try ellipses pattern form literals)
   (let ((kar (rules-match ellipses (car pattern) (car form) literals))
         (kdr (rules-match ellipses (cdr pattern) (cdr form) literals)))
     (if (or (not kar) (not kdr))
       #f
       (append kar kdr))))

(define (rules-match ellipses pattern form literals)
  (cond
    ;; Handle atomic values e.g. "string" "string"
    ((and (self-evaluating? pattern) (self-evaluating? form))
     (if (equal? pattern form)
       '()
       #f))

    ;; Check for literals
    ((and (eq? pattern form) (identifier? pattern) (memq pattern literals) (not (rules-shadowed? pattern)))
     (begin
       ;(print "found ye literal" pattern)
       (list '(#t #t))))
    ((and (identifier? pattern) (or (identifier? form) (self-evaluating? form)))
     (list (list pattern 'single form)))
    ;; Check for NULL match
    ((and (identifier? pattern) (null? form))
     (list (list pattern 'single form)))

    ((and (identifier? pattern) (pair? form))
     (list (list pattern 'single form)))
    ((and (pair? pattern) (pair? form))
     (if (and (null? (cdr pattern)) (null? (cdr form)))
       (rules-match ellipses (car pattern) (car form) literals)
       ;; Handle (asdf ...) (asdf)
       (if (null? (cdr form))
         ;; This is an ellipses
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
           (list (list (car pattern) 'splat form))
           ;; This is not an ellipses so try to match it normally
           (rules-try ellipses pattern form literals))
         ;; More form

         ;; one or more elements after ...
         (if (and (eq? (cadr pattern) ellipses))
           (list (list (car pattern) 'splat form))
           ;; no special cases, continue
           (rules-try ellipses pattern form literals))))
     )
    ;; Handle ellipses with null
    ((and (pair? pattern) (null? form))

     (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
       (list (list (car pattern) 'splat '()))
       #f))
    (else
      #;(print "could not match:" pattern form) #f)
    ))

(define (try-cons a b)
  (and a (cons a b)))

(define (fold-template matches lst)
  (fold-right
    (lambda (a b)
      (and b
        (cond 
          ((pair? a)
           (if (and (pair? b) (eq? (car b) '...))
             (let ((nmatches (map (lambda (c) (if (eq? (cadr c) 'splat) (list (car c) 'splat-consume (caddr c)) c)) matches))
                   (expected-lengths (map (lambda (c) (list (car c) (if (eq? (cadr c) 'splat) (length (caddr c)) #f))) matches)))
               (let loop
                 ((attempt (fold-template nmatches a))
                  (result '()))

                 ;; Attempt terminated, now check for errors
                 (if (not attempt)
                   (begin
                     ;; CHECKING LENGTHS.
                     (map2
                       (lambda (a b)
                         (if (and (eq? (cadr a) 'splat-consume) (eq? (cadr b) #f))
                           (raise 'expand "... consumed too many values" (list a))))
                       nmatches expected-lengths)
                     ;; (map (a b) nmatches expected-lengths). check splat-consume for #f
                     ;(print "ye result:" result)
                     ;(print matches)
                     (append result (cdr b)))
                   (begin
                     (loop (fold-template nmatches a) (cons attempt result))))))

             (try-cons (fold-template matches a) b)))
          ((symbol? a)
           (let ((match (assq a matches)))
             ;; splat requested
             (if (and (pair? b) (eq? (car b) '...))
               (if (not match)
                 (raise 'expand "... occurred in syntax-rules template after" a "which is not a pattern variable" (list a))
                 (if (eq? (cadr match) 'single)
                   (raise 'expand (print-string "... occurred in syntax-rules template after" a "which is not a pattern variable") (list a))
                   (append (caddr match) (cdr b))))
               ;; splat not requested
               (begin
                 (try-cons
                   ;; matched variable
                   (if match
                     (case (cadr match)
                       ;; if this is a '... we want to just pass it through to the next invocation of this function
                       (single a)
                       (splat
                         (raise 'expand (print-string "syntax-rules template substitution" a "must be followed by ...") (list a )))
                       (splat-consume
                         (let ((lst (caddr match)))
                           (if (null? lst)
                             (begin
                               (set-car! (cddr match) #f)
                               #f)
                             (begin
                               (if (not lst)
                                 (raise 'expand (print-string "... length mismatch") (list lst)))
                               (set-car! (cddr match) (cdr lst))
                               (car lst))))
                            ))
                       (if (eq? a '...) a (make-rename #f a)))
                   b)))))

          (else (try-cons a b)))))
    '() lst))
  #|

  #;(cond
    ((pair? a)
     (if (and (pair? b) (eq? (car b) '...))
       ;; Splat requested against more complex source
       ;; We need a way to "consume" the splat each time it is encountered
       ;; What we'll do - replace 'splat with 'splat-consume
       ;; 'splat-consume will mutate the alist given in place
       ;; if it runs into NULL it will cause an error
       (let ((matches-consume (map (lambda (c) (if (eq? (cadr c) 'splat) (list (car c) 'splat-consume (caddr c)) c)) matches)))
         (cons
           (fold-right (lambda (a b) (fold-template matches-consume a b)) '() a)
           b))
       (cons (fold-right (lambda (a b) (fold-template matches a b)) '() a) b)))
    ((symbol? a)
      (let* ((match (assq a matches))
             (kar (or match a)))

        (if (and (pair? b) (eq? (car b) '...))
          ;; A splat has been requested
          (if (not match)
            (raise 'expand (print-string "... occurred in syntax-rules template after" a "which is not in the pattern") (list a))
            (if (eq? (cadr match) 'single)
              (raise 'expand (print-string "... occurred in syntax-rules template after" a "which did not have a ... after it in the pattern (or ... was already used)") (list a))
              ;; Simple
              (append (caddr match) (cdr b))
                #;(let ((single-matches (filter (lambda (m)
                                                (if (eq? (car m) a)
                                                  (list (car m) 'single (caddr m)))) matches)))
                  (print "single-matches:" single-matches)
                  (append (fold-right (lambda (a b) (fold-template single-matches a b)) '() a) (cdr b)))))
          ;; A splat has not been requested
          (cons
            (if match
              (case (cadr match)
                (single (if (eq? a '...) a (make-rename #f a)))
                (splat 
                  (raise 'expand (print-string "syntax-rules template name" a "must be followed by a ...") (list a)))
                (splat-consume
                  (let ((lst (caddr match)))
                    (if (null? lst)
                      (raise 'expand (print-string "... given uneven lists; pattern variable" (car match) "ran out") (list a)))
                    (set-car! (cddr match) (cdr lst))
                    (car lst))))
                (if (eq? a '...) 
                  a
                  (make-rename #f a)))
            b))))

    (else (cons a b))))
|#

(print (fold-template '((hello single hello) (one single 1)) '(hello one)))
(print (fold-template '((hello single hello) (one splat (1 2 3))) '(hello one ...)))
(print (fold-template '((hello single hello) (one splat (1 3 5)) (two splat (2 4 6))) '(hello (begin (print one two) ...))))

#;(define (rules-substitute matches template)
  (fold-right (lambda (a b) (fold-template matches a b)) '() template))

;; (let ((var x)) y)
;; we have to rename everything except those variables which are introduced by the user.

;; how do we distinguish between something like
;; (hello one ...) (hello 1)
;; (hello one) (hello (1))

;(assert-equal? (rules-match '... '(hello) '(hello) '()) '((hello single hello)))
;(assert-equal? (rules-match '... '(hello one ...) '(hello 1) '()) '((hello single hello) (one splat (1))))

;(print (rules-match '... '(hello) '(hello) '()))

;(print (rules-substitute '((hello single hello)) '(hello)))
;(print (rules-substitute '((hello single hello) (one single 1)) '(hello one)))
;; error:
;; (print (rules-substitute '((hello single hello) (one splat (1))) '(hello one)))
;(print 5 (rules-substitute '((hello single hello) (one splat (1))) '(hello one ...)))
;(print 6 (rules-substitute '((hello single hello) (one splat (1 2 3))) '(hello one ...)))

;(print 7 (rules-substitute '((hello single hello) (one splat (1 2 3))) '(hello (print one) ...)))

#|

(print (rules-match '... '(hello one ...) '(hello 1) '()))
(print (rules-match '... '(hello) '(hello) '()))
;;(print (rules-match '... '(hello one) '(hello ()) '()))
(print (rules-match '... '(hello one) '(hello 1) '()))
(print (rules-match '... '(hello one two) '(hello bug 5) '()))
(print (rules-match '... '(hello one ...) '(hello bug 5) '()))
(print (rules-match '... '(hello one ...) '(hello bug) '()))
(print (rules-match '... '(hello one) '(hello (1)) '()))
(print (rules-match '... '(hello (one two three ...)) '(hello (1 2 3 4)) '()))
(print (rules-match '... '(hello (one two three)) '(hello (four five six)) '()))
(print (rules-match '... '(hello (one two three ...)) '(hello (four five six)) '()))
(print (rules-match '... '(hello one two three ...) '(hello 1 2) '()))
(print (rules-match '... '(hello "atoms") '(hello "atoms") '()))
|#
;(print (rules-match '... '(hello (keyword item)) '(hello (keyword item)) '(keyword)))

#|
(print (rules-match '... '(if #t (then then-body ...)) '(if #t (then 1)) '(then)))

(print (rules-substitute '((if if) (then-body (1 2 3))) '(if #t (begin then-body ...))))
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

#;(define (rules-execute name literals form pairs)
  (let loop ((form form)
             (rule (car pairs))
             (rules (cdr pairs)))

    (if (or (eq? (car form) '_) (eq? (car form) name))
      (let ((match (rules-match '... (car rule) form literals)))
        (if match
          (begin
            (loop (rules-substitute match (cadr rule)) (car pairs) (cdr pairs)))
          (if (null? rules)
            (raise 'expand "failed to match pattern" (list form))
            (loop form (car rules) (cdr rules)))))
      form)))

#|
(print "matched:" (rules-execute 'print1 '() '(print1 12345)
  '(
    ((print1 "string:" hello) (print hello))
    ((print1 hello) (print1 "string:" hello))
  )))

(print "matched:" (rules-execute 'let1 '() '(let1 name 12345 name)
  '(
    ((let1 binding value expr)
     (let ((binding value)
           (somethingelse 5))
       (print expr)
       (print somethingelse)))
  )))

(print "matched:" (rules-execute 'progn '() '(progn 1 2 3)
  '(
    ((progn body ...)
     (begin body ...))
  )
))

(print "matched:" (rules-execute 'nth-value '() '(nth-value 1 (values 1 2 3))
  '(
    ((nth-value n values-producing-form)
     (call-with-values 
       (lambda () values-producing-form)
       (lambda all-values (list-ref all-values n))))

  )
))

(print "matched:"
  (rules-execute 'if '(then else) '(if #t (then 12345) (else 555))
    '(
      ((_ condition (then then-body ...) (else else-body ...))
       (print conditiion (begin then-body ...) (begin else-body ...))
      )))
    )
|#


#|
Here's a Problem.

(define-syntax pattern
  (syntax-rules ()
    ((_ thing asdf ...) (begin
                     (begin
                       thing
                       (display asdf)
                       (newline)) ...
                     (newline)))))
|#
