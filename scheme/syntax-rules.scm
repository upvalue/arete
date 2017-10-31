;; syntax-rules.scm - syntax-rules implementation

(define (syntax-rules-parser ellipses expr)
  ;; literals, syntax rules

  ;; patterns
  ;; (pattern template)
  ;; pattern = (pattern ...)
  ;; (pattern pattern ... . pattern)
  expr)

#|
(define (syntax-rules-check-pattern form pattern template)
  ;; non-literal identifier
  ;; literal identifier and f is an identifier with the same binding
  ;; ((_ asdf) 
  ;; P is a list and F is a list of n forms that match P_1 through P_n
  ;; same but with improper list
  (_ #t)
  |#

(rules-check-pattern
  '(asdf) '(asdf) #t
  ;; should return #t
)

(rules-check-patern
  '(asdf #t) '(asdf #t) #t
  ;; should return #t
)

(rules-check-pattern
  '(asdf 1 2 3) '(asdf 1 ...) #t
)

#;(define-syntax
  (syntax-rules ()
    ((asdf rest ...)
     (print rest ...))))




;; remove _ and asdf from name

;;

#;(define-syntax asdf
  (syntax-rules ()
    ((_ #t))))

;(print (syntax-rules-parser '(#t)))

