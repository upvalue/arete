;; rules.scm - syntax-rules implementation

;; How this works: rules-match checks a syntax-rules pattern against a given form
;; It does this by diving into that form and returning an association list for the template substitution if successful,
;; and #f if it fails

;; If a pattern is successfully matched, rules-substitute does a fold-right over the template. The only tricky part
;; (which I am not happy about) is the ... handling; it copies the alist from rules-match and modifies it for each
;; iteration, then does some tricky math to make sure that all the lists used by the ... have been fully consumed

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

;; TODO We cannot use #f to terminate. Perhaps a qualified-symbol would be appropriate here, as something we can
;; rightfully say should not occur in source code, e.g. ##terminate

;; need to change try-cons and (and b)

;; The PROBLEM. We cease on consuming a list, but then have no way of "consuming" the other lists


;; (1 3 5 7) (

(define (fold-template matches lst)
  (fold-right
    (lambda (a b)
      (and b
        (cond 
          ((pair? a)
           (if (and (pair? b) (eq? (car b) '...))

             (let* ((nmatches (map (lambda (c) (if (eq? (cadr c) 'splat) (list (car c) 'splat-consume (caddr c) '()) c)) matches))
                    (expected-lengths (map (lambda (c) (list (car c) (if (eq? (cadr c) 'splat) (length (caddr c)) #f))) matches))
                    (first-attempt (fold-template nmatches a)))
               (let loop
                 ((attempt first-attempt)
                  (result (list first-attempt)))

                 ;; Attempt terminated, now check for errors
                 (if (not attempt)
                   (begin
                     ;; Checking for early termination is tricky because evaluation stops right-to-left
                     ;; We keep a list of values consumed for each name

                     ;; If any of those lists do not have the same lengths, or if there are still unconsumed values
                     ;; in any list that has been touched, it is an error
                     (let ((consumption-limit (reduce min (map1 (lambda (c) (print c)(length (cadddr c))) (filter (lambda (c) (and (eq? (cadr c) 'splat-consume) (not (null? (cadddr c))))) nmatches)))))

                       (map1 
                         (lambda (c)
                           (print c)
                           ;; Some values were consumed, meaning caddr c (values not consumed) should be NULL,
                           ;; which will fail if a value not rightmost resulted in consumption
                           (when (eq? (cadr c) 'splat-consume)
                             (when (and (not (eq? (caddr c) '())) (not (eq? (cadddr c) '())))
                               (raise 'expand (print-string "template value" (car c) "was given too many values and ... resulted in length mismatches")))

                             ;; Some values were consumed, meaning cadddr c should equal consumption-limit,
                             ;; which will fail if a value rightmost resulted in consumption
                             (when (and (not (null? (cadddr c))) (not (fx= (length (cadddr c)) consumption-limit)))
                               (raise 'expand (print-string "template value" (car c) "was given too many values and ... resulted in length mismatches")))))
                         nmatches)

                     ;; Checking proper consumption of values
                       nmatches expected-lengths)
                     (append result (cdr b)))
                   (begin
                     (let ((attempt (fold-template nmatches a)))
                       ;(print attempt)
                       ;(print result)
                       (loop attempt (if attempt (append result (list attempt)) result)))))))

             (try-cons (fold-template matches a) b)))
          ((symbol? a)
           (let ((match (assq a matches)))
             ;; splat requested
             (if (and (pair? b) (eq? (car b) '...))
               (if (not match)
                 (raise 'expand "... occurred in syntax-rules template after" a "which is not a pattern variable" (list a))
                 (if (not (eq? (cadr match) 'splat))
                   (raise 'expand (print-string "... occurred in syntax-rules template after" a "which is not a pattern variable") (list a))
                   (append (caddr match) (cdr b))))
               ;; splat not requested
               (begin
                 (let ((kar
                   ;; matched variable
                   (if match
                     (case (cadr match)
                       ;; if this is a '... we want to just pass it through to the next invocation of this function
                       (single (caddr match))
                       (splat
                         (raise 'expand (print-string "syntax-rules template substitution" a "must be followed by ...") (list a )))
                       (splat-consume
                         ;; We cannot rely on order of evaluation as it depends on how the user has entered them
                         ;; However, we also cannot rely on early termination. How do we continue "consuming" but
                         ;; also indicate finishing?
                         (let ((lst (caddr match)))
                           (if (null? lst)
                             (begin
                               #f)
                             (begin
                               (if (not lst)
                                 (raise 'expand (print-string "... length mismatch") (list lst)))
                               ;; Note consumed value
                               (set-car! (cdddr match) (cons (car lst) (cadddr match)))
                               (set-car! (cddr match) (cdr lst))
                               (car lst))))
                            ))
                       (if (eq? a '...) a (make-rename #f a)))))
                   (if (eq? kar #f)
                     #f
                     (try-cons kar b))
                   #;(try-cons kar b)
                   )))))

          (else (try-cons a b)))))
    '() lst))

;(print (fold-template '((hello single hello) (one single 1)) '(hello one)))
;(print (fold-template '((hello single hello) (one splat (1 2 3))) '(hello one ...)))
;; if we terminate on the rightmost list we have no way of checking results by golly

(print (fold-template '((hello single hello) (thing single "thing") (one splat (1 3 5)) (two splat (2 4 6)) (three splat (0 0 0 5))) '(hello (begin (print thing) (print  one two) ... (begin (print three) ... )))))
;(print (fold-template '((hello single hello) (one splat (1 3 5)) (two splat (2 4 6)) (three splat (0 0 0 5))) '(hello (begin (print three one two) ...))))
;(print (fold-template '((hello single hello) (one splat (1 3 5)) (two splat (2 4 6 8)) (three splat (0 0 0))) '(hello (begin (print three one two) ...))))
 
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
 9|#
