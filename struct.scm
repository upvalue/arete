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

    ;(print fields)
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
       (,#'define ,name (,#'register-record-type ,name-string ,field-count 0))
       ,predicate
       ,constructor
       ,@accessors
      )
    
  )) ;; lambda (x)

#|
(define Box (register-record-type "box" 1 0))

(define-record Box2 (Box) item2)

(define box (make-record Box2))
(set! box (Box2/make 123))
(print box)

(Box2/item2! box 123)
(print (Box2/item2 box))

(print (Box2? box))
|#


;(print (record-ref Box2 box 0))

