;; rules.scm - syntax-rules implementation

;; How this works: rules-match checks a syntax-rules pattern against a given form
;; It does this by diving into that form and returning an association list for the template substitution if successful,
;; and #f if it fails

;; If a pattern is successfully matched, rules-substitute does a fold-right over the template. The only tricky part
;; (which I am not happy about) is the ... handling; it copies the alist from rules-match and modifies it for each
;; iteration, then does some tricky math to make sure that all the lists used by the ... have been fully consumed

;; TODO Literals
;; TODO Substitutions
;; TODO Improper lists
;; TODO Vectors

;; TODO rewrite with fold instead of fold-right. 
;; TODO do not use mutation

(define ellipses '...)

(define (rules-rename name)
  (if (eq? (top-level-value '*current-rename-env*) unspecified)
    (make-rename #f name)
    (make-rename (top-level-value '*current-rename-env*) name)))

(define (rules-shadowed? name)
  #f)

(define (rules-try ellipses pattern form literals)
   (let ((kar (rules-match ellipses (car pattern) (car form) literals))
         (kdr (rules-match ellipses (cdr pattern) (cdr form) literals)))
     (if (or (not kar) (not kdr))
       #f
       (append kar kdr))))

#|
(define (rules-try-improper pattern form literals)
  (let ((kar (rules-match ellipses (car pattern) (car form) literals)))
    (if kar
      (if (identifier? (cdr pattern))
        (append kar (list (cdr pattern) 'single (cdr form)))
        (let ((kdr (rules-match el
        (if (not (equal? (cdr pattern) (cdr form)))
          #f
          kar)))))
        |#

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
         ;; Ellipses, but not more form, so we match only one element
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
           (list (list (car pattern) 'splat form))
           ;; This is not an ellipses so try to match it normally
           (rules-try ellipses pattern form literals))

         (if (null? (cdr pattern))
           ;; There's more form but not more pattern
           #f
           (if (not (pair? (cdr pattern)))
             (rules-try ellipses pattern form literals)
             ;; One or more elements after ...
             (if (and (eq? (cadr pattern) ellipses))
               ;; There may be elements in the pattern after ...
               (begin
                 (if (not (null? (cddr pattern)))
                   (let* ((expect-after (length (cddr pattern)))
                          (to-take (- (length form) expect-after)))

                     (for-each-i
                       (lambda (i x)
                         (if (eq? x ellipses)
                           (raise-source (list-tail (cddr pattern) (fx- i 1)) 'syntax "multiple ... on same level in pattern" (list pattern))))
                       (cddr pattern))
                     #;(every (lambda (i)
                              (if (eq? i ellipses) (raise-source pattern 'syntax "multiple ... on same level in pattern" (list pattern))))
                            (cddr pattern))

                     (if (fx< to-take 0)
                       #f
                       (let ((rest (rules-try ellipses (cddr pattern) (list-tail form to-take) literals)))
                         (and rest
                              (cons (list (car pattern) 'splat (take form to-take)) rest)))))

                   (list (list (car pattern) 'splat form)))
               ) 
               ;; no special cases, continue
               (rules-try ellipses pattern form literals)))))
       )
     )
    ;; Handle ellipses with null
    ((and (pair? pattern) (null? form))

     (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
       (list (list (car pattern) 'splat '()))
       #f))
    (else
      #;(print "could not match:" pattern form) #f)
    ))

(print (rules-match '... '(hello one ...) '(hello 1) '()))
(print (rules-match '... '(hello) '(hello) '()))
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
(print (rules-match '... '(hello one ... two three four) '(hello 1 2 3) '()))
(print (rules-match '... '(hello one ... two three four five) '(hello 1 2 3) '()))

;(print "ok:"(rules-match '... '(thing . 5) '(thing . 6) '()))
#|

;; something like this means we need at least three elements,
;; or rather, we need at least one element after ...

;; So, when we encounter ..., we'll (1) check that there are no ... after ...
;; and (2) how many elements are after it. Then we TAKE length of list - elements after it from the list, and continue
;; rules-match with the cddr

;; TODO We cannot use #f to terminate. Perhaps a qualified-symbol would be appropriate here, as something we can
;; rightfully say should not occur in source code, e.g. ##terminate

;; need to change try-cons and (and b)

;; The PROBLEM. We cease on consuming a list, but then have no way of "consuming" the other lists

;; (1 3 5 7) (

(define terminate '##terminate)

(define (try-cons a b)
  (and (not (eq? a terminate)) (cons a b)))

(define (fold-template matches lst)
  (fold-right
    (lambda (a b)
      (and (not (eq? b terminate))
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
                             (begin terminate)
                             (begin
                               (if (not lst)
                                 (raise 'expand (print-string "... length mismatch") (list lst)))
                               ;; Note consumed value
                               (set-car! (cdddr match) (cons (car lst) (cadddr match)))
                               (set-car! (cddr match) (cdr lst))
                               (car lst))))
                            ))
                       (if (eq? a '...) a (rules-rename a)))))
                   (if (eq? kar terminate)
                     terminate
                     (try-cons kar b))
                   #;(try-cons kar b)
                   )))))

          (else (try-cons a b)))))
    '() lst))

;; (define-syntax thing
;;   (syntax-rules (literals)
;;     ((pattern) template) ...))

(print "fold-template test" (fold-template '((hello single hello)) '(begin #t)))
(print "fold-template test" (fold-template '((hello single hello)) '(#t)))

(define (syntax-rules-matcher literals pare rest)
  (if (not (list? pare))
    (raise-source pare 'syntax "syntax-rules argument must be a list (pattern and template)" (list pare)))

  (let ((pattern (car pare)) (template (cdr pare)))
    (if (not (list? pattern))
      (raise-source pare 'syntax "syntax-rules pattern (first element) must be a list" (list pare)))

    (if (not (and (list? template) (null? (cdr template))))
      (raise-source (cdr template) 'syntax "syntax-rules template must only have one element" (list pare)))

    `(let ((maybe (rules-match '... (quote ,pattern) form ,literals)))
       (print "rules-match result" maybe form)
       (if maybe 
         (cons 'begin (fold-template maybe (quote ,template)))
         ,(if (null? rest)
            `(raise 'syntax "syntax-rules failed to find a match for form" (list form))
            (syntax-rules-matcher literals (car rest) (cdr rest)))))))

(define-syntax syntax-rules
  (lambda (x)
    (unless (fx> (length x) 2)
      (raise-source x 'syntax "syntax-rules must have at least two arguments (literals list and one pattern/template pair)" (list x)))
    (define literals (cadr x))
    (define pares (cddr x))

    (if (not (and (list? literals) (every symbol? literals)))
      (raise-source x 'syntax "syntax-rules literals list must be a list of symbols"))

    (define body (syntax-rules-matcher literals (car pares) (cdr pares)))

    (print body)
    (pi `(,#'lambda (,#'x)
       (let ((form ,#'x))
         (pi ,body))))))

(define-syntax gub
  (syntax-rules ()
    ((_) (begin 1 2 3))
    ((_ rst ...) (begin rst ...))
    ((_ asdf) asdf)
    ))

;; recursive syntax-rules invocation

(define-syntax rekur
  (syntax-rules ()
    ((_) (rekur 123))
    ((_ echo) (rekur "literal" echo))
    ((_ "notliteral" thing) "bad result")
    ((_ "literal" thing) thing)))


;Current problem. ... is not supposed to match *everything*, we need to allow for matching after ... ! 

(display
(let-syntax
  ((foo (syntax-rules ()
          ((foo args ... penultimate ultimate)
           (list ultimate penultimate args ...)))))
   (foo 1 2 3 4 5)))
(newline)

|#
