(define-library (template-a)
  (import (arete))
  (export build-id build-call build-dynamic build-binding local-id)
  (begin
    (define x 'A)
    (define (get-x) x)
    (define (build-id) #'x)
    (define (build-call) #`(get-x))
    (define (build-dynamic) (rename 'x))
    (define (build-binding value) #`(let ((temp ,value)) temp))
    (define-syntax local-id (lambda (form) (build-id)))))

(define-library (template-b)
  (import (arete) (template-a))
  (export via-id via-call via-dynamic via-explicit via-binding direct-id)
  (begin
    (define x 'B)
    (define (get-x) x)
    (define-syntax via-id (lambda (form) (build-id)))
    (define-syntax via-call (lambda (form) (build-call)))
    (define-syntax via-dynamic (lambda (form) (build-dynamic)))
    (define-syntax via-explicit (lambda (form r c) (r 'x)))
    (define-syntax via-binding (lambda (form) (build-binding (cadr form))))
    (define-syntax direct-id (lambda (form) #'x))))

(import (template-a) (template-b))
(define x 'C)
(define (get-x) x)
(print (local-id))
(print (direct-id))
(print (via-id))
(print (via-call))
(print (let ((x 'D) (get-x (lambda () 'D))) (list (via-id) (via-call))))
(print (via-dynamic))
(print (via-explicit))
(print (list (via-binding 1) (via-binding 2) (via-binding (via-binding 3))))

;; Templates also work outside a transformer and allocate fresh identifiers.
(define first-id (build-id))
(define second-id (build-id))
(print (eq? first-id second-id))
(print (env-compare #f first-id second-id))

;; Reader shorthand does not call a user binding named rename.
(print (let ((rename (lambda (name) 'wrong))) (rename? #'x)))

;; Capture the environment before a later definition fills in its binding.
(define (build-forward) #'later)
(define-syntax forward (lambda (form) (build-forward)))
(define later 'forward)
(print (forward))

;; A local syntax binding belongs to the template's definition scope.
(print
  (let-syntax ((value (lambda (form) ''outer)))
    (let-syntax ((get-value (lambda (form) #`(value))))
      (let-syntax ((value (lambda (form) ''inner)))
        (get-value)))))

(print
  (let ((value 'outer))
    (let-syntax ((get-value (lambda (form) #'value)))
      (let ((value 'inner))
        (get-value)))))
