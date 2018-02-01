;; analyze.scm - Analysis pass
(define analysis-table (make-table))

;; (let loop ((i 0))
;;   (if (= i 5) #t (loop (+ i 1)))

;; if a function never escapes (is only used in application position)
;; it can be converted into a label

;; something like this

;; ($local-set i 0)
;; ($label loop)
;; (if (= i 5) 
;;    (begin #t ($goto return-label))
;;    (begin 
;;      ($local-set 0 (+ i 1)) 
;;      ($goto loop))))
;; ($set-return-label loop-end)
;; ($label loop-end)

;; How would this work?

;; If it escape-possible, mutated? are #f and known-function? is #t

;; so we need an additional thing to detect when a function is only used recursively and immediately after it...


;; perhaps if its only used once at a particular "level", then we know we can convert it to a label?

;; (?)

(define-record Var mutated? escape-possible known-function?)

;; subsume-locals.
(define (analyze-lambda x)
  (define arguments (cadr x))
  (for-each1 analyze-expr (cddr x))
  'lambda)

(define (analyze-set x)
  (let ((var (table-ref analysis-table (cadr x))))
    (Var/mutated?! var #t)))

(define (analyze-define x)
  (define var (Var/make #f #f #f))
  (table-set! analysis-table (cadr x) var)
  (when (eq? (analyze-expr (caddr x)) 'lambda)
    (Var/known-function?! var #t))
  x)

;; Most expressions just need their CDR analyzed for occurences of identifiers
(define (analyze-recons x)
  (cons-source x (car x) (map1 (lambda (sub-x) (analyze-expr sub-x)) (cdr x))))

(define (analyze-ref x)
  (let ((var (table-ref analysis-table x)))
    (Var/escape-possible! var #t)))

(define (analyze-expr x)
  (cond
    ((pair? x)
     (case (rename-strip (car x))
       (lambda (analyze-lambda x))
       (define (analyze-define x))
       (quote x)
       ((and or if begin) (analyze-recons x))
       ((set!) (analyze-set x))
       (else (map1 (lambda (x) (analyze-expr x)) x))))
    ((identifier? x) (analyze-ref x))

    (else x)))

(define (analyze-toplevel code)
  (analyze-expr code))

;; (analyze-toplevel '(lambda () (define (internal2) (internal1)) (define (internal1) (internal2))))

(define qq-list
  '(define (c)
     (lambda ()
       (define loop 
         (lambda ()
           #t))
       loop
       (set! c #t))
     c))

;; TODO: If we want to compile stuff as blocks, how do we inline stuff at the module level?

(begin
  (pretty-print (expand-toplevel qq-list (top-level-value '*core-module*)))
  (pretty-print (analyze-toplevel (expand-toplevel qq-list (top-level-value '*core-module*))))
  (pretty-print analysis-table)
  )
