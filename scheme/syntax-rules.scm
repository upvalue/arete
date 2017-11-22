;; syntax-rules.scm - syntax-rules implementation

;; TODO Keywords

(define ellipses '...)

(define (syntax-rules-try ellipses pattern form)
   (let ((kar (syntax-rules-match ellipses (car pattern) (car form)))
         (kdr (syntax-rules-match ellipses (cdr pattern) (cdr form))))
     (if (or (not kar) (not kdr))
       #f
       (append kar kdr))))

(define (syntax-rules-match ellipses pattern form)
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
       (syntax-rules-match ellipses (car pattern) (car form))
       ;; Handle (asdf ...) ()
       (if (null? (cdr form))
         ;; ...
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
           (list (list (car pattern) (car form)))
           (if (and (null? (cdr form)) (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
             (list (list (car pattern) '()))
             (syntax-rules-try ellipses pattern form) #;(let ((kar (syntax-rules-match (car pattern) (car form)))
                   (kdr (syntax-rules-match (cdr pattern) (cdr form))))
               (if (or (not kar) (not kdr))
                 #f
                 (append kar kdr)))

             #;(append
               (syntax-rules-match (car pattern) (car form))
               (syntax-rules-match (cdr pattern) (cdr form)))))
         ;; More form

         ;; one or more elements after ...
         (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
           (list (list (car pattern) form))
           ;; no special cases, continue
           (syntax-rules-try ellipses pattern form)
           #;(let ((kar (syntax-rules-match (car pattern) (car form)))
                 (kdr (syntax-rules-match (cdr pattern) (cdr form))))
             (if (or (not kar) (not kdr))
               #f
               (append kar kdr)))
           #;(append
             (syntax-rules-match (car pattern) (car form))
             (syntax-rules-match (cdr pattern) (cdr form))))))
     )
    ((and (pair? pattern) (null? form))
     (if (and (not (null? (cdr pattern))) (eq? (cadr pattern) ellipses))
       (list (list (car pattern) '()))
       #f))
    (else
      (print "could not match:" pattern form) #f)
    ))

;; (let ((var x)) y)
;; we have to rename everything except those variables which are introduced by the user.

(print (syntax-rules-match '... '(hello) '(hello)))
(print (syntax-rules-match '... '(hello one) '(hello 1)))
(print (syntax-rules-match '... '(hello one two) '(hello bug 5)))
(print (syntax-rules-match '... '(hello one ...) '(hello bug 5)))
(print (syntax-rules-match '... '(hello one ...) '(hello bug)))
(print (syntax-rules-match '... '(hello one) '(hello (1))))
(print (syntax-rules-match '... '(hello (one two three ...)) '(hello (1 2 3 4))))
(print (syntax-rules-match '... '(hello (one two three)) '(hello (four five six))))
(print (syntax-rules-match '... '(hello (one two three ...)) '(hello (four five six))))
(print (syntax-rules-match '... '(hello one two three ...) '(hello 1 2)))

;; bad patterns
(print (syntax-rules-match '... '(hello one) '(hello 1 2)))

;; remove _ and asdf from name

#;(define-syntax asdf
  (syntax-rules ()
    ((_ #t))))

;; Now we have to go over the template
;; Something like (_ one two three)
;; We have to map over it, touch every identifier, but we also have to deal with ...; maybe map-i?

;; how do we expand the ellipses?
;; We find the ellipses

;; (define-syntax let1
;;   ((_ name value body)
;;    (let ((name value))
;;      body ...)))

;; If a substitution results in an identifier, it needs to be renamed.
;; Something like

;; (define-syntax print1
;;   ((_ value)
;;    (print value)))

;; (#'print value)

;; (define-syntax bind1
;;   ((_ name value)
;;    ((lambda (name) name) value)))

;; (bind1 hello #t)

;; ((name hello) (value #t))
;; (assq match 'hello) => name
;; wait no, we rename all symbols UNLESS they are pulled out of the syntax-rules-match result
;; so something like 
;; (lambda (name name-shadow) #t)
;; (#'lambda (name #'name-shadow) #t)

;; asdf ...


;; use FOLD-RIGHT
