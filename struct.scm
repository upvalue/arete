;; struct.scm - record support syntax

(define-syntax define-record
  (lambda (x)
    (define name #f)
    (define field-count 0)
    (define parent #f)
    (define fields #f)
    (define accessors #f)
    (define name-string #f)
    (define constructor #f)
    (define predicate #f)

    (unless (fx> (length x) 1)
      (raise 'syntax "define-record expects at least one argument: a record name" (list x)))

    (set! name (cadr x))

    (unless (symbol? name)
      (raise 'syntax (print-string "define-record name must be a symbol but got" name) (list x)))

    (set! name-string (symbol->string name))

    (when (fx> (length x) 2)
      (if (list? (list-ref x 2))
        (begin
          (when (null? (list-ref x 2))
            (raise 'syntax "define-record inheritance argument must be a list with exactly one symbol but got none" (list x (cddr x))))
          (set! parent (car (list-ref x 2)))
          (set! fields (cdddr x)))
        (set! fields (cddr x))))

    (unless (or (symbol? parent) (not parent))
      (raise 'syntax "define-record inheritance argument should be a list with exactly one symbol" (list x (caddr x))))

    (set! accessors
      (map
        (lambda (field-name)
          (define getter #f)
          (define setter #f)

          (unless (symbol? field-name)
            (raise 'syntax "define-record fields must be symbols" (list x name)))

          (set! getter (string->symbol (string-append name-string "/" (symbol->string field-name))))
          (set! getter `(,#'define (,getter ,#'rec) (,#'record-ref ,name ,#'rec ,field-count)))

          (set! setter (string->symbol (string-append name-string "/" (symbol->string field-name) "!")))
          (set! setter `(,#'define (,setter ,#'rec ,#'value) (,#'record-set! ,name ,#'rec ,field-count ,#'value)))

          (set! field-count (fx+ field-count 1))

          `(,#'begin ,getter ,setter))
      fields))

    (set! constructor
      `(,#'define (,(string->symbol (string-append name-string "/make")) ,@(map rename fields))
        (,#'let ((,#'instance (,#'make-record ,name)))
        ,@(map-i
          (lambda (i x)
            `(,#'record-set! ,name ,#'instance ,i ,(rename (list-ref fields i))))
          fields)
        ,#'instance)))

    (set! predicate
      `(,#'define ,(list (string->symbol (string-append name-string "?")) #'instance)
        (,#'record-isa? ,#'instance ,name)))

    `(,#'begin
       (,#'define ,name
        (,#'let ((,#'fields (list ,@(map (lambda (x) `(,#'quote ,x)) fields))))
          (begin
            ;(print ,#'fields)
            (,#'register-record-type ,name-string ,field-count 0 ,#'fields ,parent))))
       ,predicate
       ,constructor
       ,@accessors
      )
    
  )) ;; lambda (x)

#|(define-syntax (define-record name (? parent (of 1 List Symbol)) (? fields Symbol ...))
  (let ((accessors
          ,(map 
             (lambda (field-name)|#

;(define-record Thing name field)

;(define thing1 (Thing/make "one" (make-vector)))
;(pretty-print thing1)



#|
(define-record Thing name field2 field3)

(define thing1 (Thing/make "one" #f (make-vector)))

(pretty-print thing1)

(define thing2 (Thing/make "two" thing1 (make-vector)))

(pretty-print thing2)

(Thing/field2! thing1 thing2)

(pretty-print thing2)

(pretty-print thing1)

(define vec (make-vector))
(vector-append! vec thing1)
(Thing/field2! thing1 vec)
(pretty-print thing1)
;(Thing/field2! thing1 #f)

;(define thing3 (list thing1 thing2))

(define vec3 (make-vector))
(vector-append! vec3 thing1)
(vector-append! vec3 thing2)

(pretty-print vec3)

;(pretty-print (Thing/make "three" thing1))


|#
