;; rules.scm - syntax-rules implementation

;; TODO

;; How this works: rules-match checks a syntax-rules pattern against a given form
;; It does this by diving into that form and returning an association list for the template substitution if successful,
;; and #f if it fails

;; If a pattern is successfully matched, fold-template does a fold-right over the template. The only tricky part
;; (which I am not happy about) is the ... handling; it copies the alist from rules-match and modifies it for each
;; iteration, then does some tricky math to make sure that all the lists used by the ... have been fully consumed

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

(define (rules-try cmp ellipses pattern form literals)
   (let ((kar (rules-match cmp ellipses (car pattern) (car form) literals))
         (kdr (rules-match cmp ellipses (cdr pattern) (cdr form) literals)))
     (if (or (not kar) (not kdr))
       #f
       (append kar kdr))))

(define (ellipses? cmp ellip item)
  (and (symbol? ellip)
    (cmp item (make-rename #f ellip))))

(define (rules-match cmp ellipses pattern form literals)
  (cond
    ;; Handle atomic values e.g. "string" "string"
    ((and (self-evaluating? pattern) (self-evaluating? form))
     (if (equal? pattern form)
       '()
       #f))
    ;; Check for literals
    ((and (eq? pattern form) (identifier? pattern) (memq pattern literals) (not (rules-shadowed? pattern)))
     '())
    ((and (not (eq? pattern form)) (identifier? pattern) (memq pattern literals) (not (rules-shadowed? pattern)))
     #f)
    ((and (identifier? pattern) (or (identifier? form) (self-evaluating? form)))
     (list (list pattern 'single form)))
    ;; Check for NULL match
    ((and (identifier? pattern) (null? form))
     (list (list pattern 'single form)))

    ((and (identifier? pattern) (pair? form))
     (list (list pattern 'single form)))
    ((and (pair? pattern) (pair? form))
     ;; No more matching required
     (if (and (null? (cdr pattern)) (null? (cdr form)))
       (rules-match cmp ellipses (car pattern) (car form) literals)
       ;; Handle (asdf ...) (asdf)
       (if (null? (cdr form))
         ;; Ellipses, but not more form, so we match only one element
         (if (and (pair? (cdr pattern)) (ellipses? cmp ellipses (cadr pattern)))
           (begin
             (when (pair? (car pattern))
               (print "pattern splat" pattern))
             (list (list (car pattern) 'splat form)))
           ;; This is not an ellipses so try to match it normally
           (rules-try cmp ellipses pattern form literals))

         (if (null? (cdr pattern))
           ;; There's more form but not more pattern
           #f
           (if (not (pair? (cdr pattern)))
             (rules-try cmp ellipses pattern form literals)
             ;; One or more elements after ...
             (if (ellipses? cmp ellipses (cadr pattern))
               ;; There may be elements in the pattern after ...
               (begin
                 (if (not (null? (cddr pattern)))
                   (let* ((expect-after (length (cddr pattern)))
                          (to-take (- (length form) expect-after)))


                     (for-each-i
                       (lambda (i x)
                         (if (ellipses? cmp ellipses x)
                           (raise-source (list-tail (cddr pattern) (fx- i 1)) 'syntax "multiple ... on same level in pattern" (list pattern))))
                       (cddr pattern))

                     (if (fx< to-take 0)
                       #f
                       (let ((rest (rules-try cmp ellipses (cddr pattern) (list-tail form to-take) literals)))
                         (and rest
                              (cons (list (car pattern) 'splat (take form to-take)) rest)))))

                   (list (list (car pattern) 'splat form)))
               ) 
               ;; no special cases, continue
               (rules-try cmp ellipses pattern form literals)))))
       )
     )
    ;; Handle ellipses with null
    ((and (pair? pattern) (null? form))

     (if (and (not (null? (cdr pattern))) (ellipses? cmp ellipses (cadr pattern)))
       (list (list (car pattern) 'splat '()))
       #f))
    (else
      #;(print "could not match:" pattern form) #f)
    ))

(define terminate '##terminate)

(define (try-cons a b)
  (and (not (eq? a terminate)) (cons a b)))

(define (fold-template ellipses matches lst)
  ;(print matches)
  (fold-right
    (lambda (a b)
      (and (not (eq? b terminate))
        (cond 
          ((pair? a)
           (if (and (pair? b) (eq? (car b) ellipses))

             (let* ((nmatches (map (lambda (c) (if (eq? (cadr c) 'splat) (list (car c) 'splat-consume (caddr c) '()) c)) matches))
                    (expected-lengths (map (lambda (c) (list (car c) (if (eq? (cadr c) 'splat) (length (caddr c)) #f))) matches))
                    (first-attempt (fold-template ellipses nmatches a)))
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
                           ;(print c)
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
                     (let ((attempt2 (fold-template ellipses nmatches a)))
                       (loop attempt2 (if attempt (append result (list attempt2)) result)))))))

             (try-cons (fold-template ellipses matches a) b)))
          ((symbol? a)
           (let ((match (assq a matches)))
             ;; splat requested
             (if (and (pair? b) (eq? (car b) ellipses))
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
                       (if (eq? a ellipses)
                         a
                        (rules-rename a)))))
                   (if (eq? kar terminate)
                     terminate
                     (try-cons kar b))
                   #;(try-cons kar b)
                   )))))

          (else (try-cons a b)))))
    '() lst))

(define (syntax-rules-matcher ellipses literals pare rest)
  (if (not (list? pare))
    (raise-source pare 'syntax "syntax-rules argument must be a list (pattern and template)" (list pare)))

  (let ((pattern (car pare)) (template (cdr pare)))
    (if (not (pair? pattern))
      (raise-source pare 'syntax "syntax-rules pattern (first element) must be a pair" (list pare)))

    (if (not (and (list? template) (null? (cdr template))))
      (raise-source (cdr template) 'syntax "syntax-rules template must only have one element" (list pare)))

    ;; Problem: Form may contain renamed variables
    ;; This is because macros may define macros

    `(let ((maybe (rules-match compare ',ellipses (quote ,pattern) form (quote ,literals))))
       (if (eq? (top-level-value 'rdbg) #t) (print "rules-match result" maybe form))
       (if maybe 
         (cons 'begin (fold-template ',ellipses maybe (quote ,template)))
         ,(if (null? rest)
            `(raise 'syntax (print-string "syntax-rules failed to find a match for form" form) (list form))
            (syntax-rules-matcher ',ellipses literals (car rest) (cdr rest)))))))

(define (syntax-rules-make-matcher ellipses literals pare rest)
  (if (not (and (list? literals) (every symbol? literals)))
    (raise-source x 'syntax "syntax-rules literals list must be a list of symbols"))

  `(lambda (expr compare)
     (let ((form expr))
      ,(syntax-rules-matcher ellipses literals pare rest))))

(define-syntax syntax-rules
  (lambda (x)
    (unless (fx> (length x) 2)
      (raise-source x 'syntax "syntax-rules must have at least two arguments (literals list and one pattern/template pair)" (list x)))

    ;; SRFI-46/R7RS: ellipses override
    (if (symbol? (cadr x))
      (let ((ellipses (cadr x))
            (literals (caddr x))
            (pares (cdddr x)))
        (syntax-rules-make-matcher ellipses literals (car pares) (cdr pares)))
      (let ((ellipses '...)
            (literals (cadr x))
            (pares (cddr x)))
        (syntax-rules-make-matcher '... literals (car pares) (cdr pares))))))


(define (fake-cmp? a b)
  (eq? a (rename-expr b)))

#|
(print (rules-match fake-cmp? '... '(hello one ...) '(hello 1) '()))
(print (rules-match fake-cmp? '... '(hello (one two three ...)) '(hello (1 2 3 4)) '()))
(print (rules-match fake-cmp? '... '(hello (one . rest) ...) '(hello (1 2 3 4)) '()))
(print (rules-match fake-cmp? '... '(hello (one . rest) ...) '(hello (1 2 3 4) (5 6 7 8)) '()))
(print (rules-match fake-cmp? '... '(hello (one two three) ...) '(hello (1 2 3)) '()))
(print (rules-match fake-cmp? '... '(hello) '(hello) '()))
(print (rules-match fake-cmp? '... '(hello one) '(hello 1) '()))
(print (rules-match fake-cmp? '... '(hello one two) '(hello bug 5) '()))
(print (rules-match fake-cmp? '... '(hello one ...) '(hello bug 5) '()))
(print (rules-match fake-cmp? '... '(hello one ...) '(hello bug) '()))
(print (rules-match fake-cmp? '... '(hello one) '(hello (1)) '()))
(print (rules-match fake-cmp? '... '(hello (one two three ...)) '(hello (1 2 3 4)) '()))
(print (rules-match fake-cmp? '... '(hello (one two three)) '(hello (four five six)) '()))
(print (rules-match fake-cmp? '... '(hello (one two three ...)) '(hello (four five six)) '()))
(print (rules-match fake-cmp? '... '(hello one two three ...) '(hello 1 2) '()))
(print (rules-match fake-cmp? '... '(hello "atoms") '(hello "atoms") '()))
(print (rules-match fake-cmp? '... '(hello one ... two three four) '(hello 1 2 3) '()))
(print (rules-match fake-cmp? '... '(hello one ... two three four five) '(hello 1 2 3 4) '()))
(print (rules-match fake-cmp? '... '(hello (a ...) ...) '(hello (1) (2) (3)) '()))
|#
