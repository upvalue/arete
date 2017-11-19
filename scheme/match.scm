;; match.scm - pattern matching 

;; Example usage:

;; (pattern-match '(* (? symbol? x) body ...) '(define-thing name 1 2 3))
;; =>
;; '((x name) (body (1 2 3)))

;; (pattern-match (any-single-element) '(hello))
;; '((any-single-element hello))

(define (pattern-match pattern expr)
  ;(print (and (pair? pattern) (pair? expr)))
  (cond
    ((and (pair? pattern) (pair? expr))
     (if (and (pair? (car pattern)) (memq (caar pattern) '(?)))
       (case (caar pattern)
         ((?) (print "attempting to match expr on predicate") pattern)))
       (if (and (null? (cdr pattern)) (not (null? (cdr expr))))
         'match-error
         (cons
           (pattern-match (car pattern) (car expr))
           (pattern-match (cdr pattern) (cdr expr)))))
    ((and (eq? pattern '()) (eq? expr '())) '())
    ((symbol? pattern)
     (list pattern expr))))

(print (pattern-match '(any-single-element (? pair? name)) '(hello fight)))
;(print (pattern-match `((? ,symbol? x)) '(hello)))

;; (define-syntax define-record
;;    (match-syntax 
;;       ((_ (? symbol? name "Record name must be a symbol")
;;           ((? symbol? record-name "Record parent must be a list with only one symbol as its element"))
;;           (or (? symbol? field-name "Field name must be a symbol")
;;               ((? symbol? field-name) (? symbol? field-getter-name)) 
;; )))))

;; optional stuff, is it possible or desirable? like "if this rule doesn't match, just skip to the next one"
