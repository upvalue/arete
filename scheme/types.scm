;; types.scm - records, methods, things for extending and interacting with the type system

;; Turns something like

;; (1 kwarg1: #t 2 3 kwarg2: #t)
;; into a list like
;; (<keywords> <body>)
;; (((kwarg1: #t) (kwarg2 #t)) (1 2 3))
;; Source code information is preserved, if possible
(define (extract-keywords expr)
  (define keywords '())

  (define body
    (let loop ((x expr))
      (unless (null? x)
        (if (keyword? (car x))
          (let ((elt (car x))
                (rest (cdr x)))
            (if (null? rest)
              (raise-source x 'expand "expected expression after keyword but got end of list" (list x))
              (if (keyword? (car rest))
                (raise-source x 'expand "keyword immediately after keyword" (list x))
                (begin 
                  (set! keywords (cons (list-source (cdr x) elt (car rest)) keywords))
                  (if (null? (cdr rest))
                    '()
                    (loop (cdr rest))
                    
                    )))))
          (cons-source x (car x) (if (null? (cdr x)) '() (loop (cdr x))))))))
  
  (list keywords (if (eq? body unspecified) '() body))
)

;; TODO: HIGHLIGHTING OF KEYWORD INFORMATION
;; Should be possible to do something like this
;; (predicate: "cat")
;;  ^^^^^^^^^^^^^^^^
;; highlight the keyword and the following.

;; Record implementation

(define-syntax define-record
  (lambda (x)
    (define name #f)
    (define parent #f)
    (define fields #f)
    (define kwargs #f)

    (syntax-assert x (fx> (length x) 1) "define-record must have at least one argument (record name)")
    (syntax-assert (cadr x) (identifier? (cadr x)) "define-record's first argument (name) must be an identifier")

    (set! name (cadr x))

    (if (and (fx> (length x) 2) (pair? (list-ref x 2)))
      (begin
        (syntax-assert (list-ref x 2) (and (identifier? (car (list-ref x 2))) (fx= (length (list-ref x 2)) 1)) "define-record parent argument must be a list with a single identifier")

        (set! parent (caaddr x))
        (set! fields (cdddr x)))
      (begin
        (set! fields (cddr x))))


    (define keywords-and-fields (extract-keywords fields))
    (set! kwargs (car keywords-and-fields))
    (set! fields (cadr keywords-and-fields))

    (define field-names 
      (map-i
        (lambda (i id)
          (cond ((identifier? id)
                 id)
                ((pair? id)
                 (unless (identifier? (car id))
                   (syntax-error id "define-record field first element must be an identifier"))
                 (car id))
                (else
                  (syntax-error (list-tail fields i) "define-record argument must be a keyword argument (such as constructor:) or a field specifier"))))
          fields))

    (define field-count (length field-names))

    (define name-string (symbol->string name))

    (define accessors
      (map-i
        (lambda (i field)
          (define getter-name (print-symbol name "/" field))
          (define getter
            #`(define (,getter-name rec)
                (record-ref ,name rec ,i)))

          (define setter-name (print-symbol name "/" field "!"))
          (define setter
            #`(define (,setter-name rec value)
                (record-set! ,name rec ,i value)))

          #`(begin ,getter ,setter))
        fields))

    (define constructor-name
      (aif (assq constructor-name: kwargs)
           (if (identifier? (cadr it))
             (cadr it)
             (print-symbol name "/make"))
        (print-symbol name "/make")))

    (define constructor
      (aif (assq constructor: kwargs)
         ;; User overridden constructor
         #`(define ,constructor-name
             (,(cadr it)
                (lambda ,fields
                  (let ((instance (make-record ,name)))
                    ,@(map-i
                        (lambda (i f) #`(record-set! ,name instance ,i ,f))
                        fields)
                    instance))))
        ;; Simple constructor
        #`(define (,constructor-name ,@fields)
            (let ((instance (make-record ,name)))
              ,@(map-i
                  (lambda (i f)
                    #`(record-set! ,name instance ,i ,f))
                  fields)
              instance))))

    (print field-names)
    (print kwargs)
    (print accessors)

    (define predicate-name
      (aif (assq 'predicate: kwargs)
           (begin
             (syntax-assert it (identifier? (cadr it)) "predicate name must be an identifier")
             (cadr it))
           (print-symbol name "?")))

    (define predicate
      #`(define (,predicate-name rec)
          (record-isa? rec ,name)))

    (define type-name (print-symbol "%" name "/type"))

    #`(begin
        (define ,type-name (register-record-type ,(symbol->string name) ,field-count 0 (quote ,fields) ,parent))

        ;; This is the syntatic introspection facility
        (define-syntax ,name
          (combined-transformer
            (let ((all-fields
                    (if ,parent
                      (append (,parent fields:) (list ,@(map (lambda (x) (list (rename 'quote) x)) fields)))
                      (quote ,fields))))
              (lambda (x)
                (if (identifier? x)
                  `,type-name
                  (begin
                    (cond
                      ((eq? (cadr x) fields:)
                       (syntax-assert-length= x 2)
                       #;(if ,parent
                         (list (rename 'quote) (append (,parent fields:) (list ,@(map (lambda (x) (list (rename 'quote) x)) fields))))
                         (list (rename 'quote) (list ,@(map (lambda (x) (list (rename 'quote) x)) fields))))
                      (list (rename 'quote) all-fields))
                      ((eq? (cadr x) get:)
                        (syntax-assert-length= x 3)
                        (let lp ((fields all-fields) (i 0))
                          (if (null? fields)
                            (syntax-error x (print-string "record type" (quote ,name) "has no field named" (caddr x)))
                            (if (eq? (car fields) (caddr x))
                              i
                              (lp (cdr fields) (fx+ i 1))))))
                      (else
                        (syntax-error (cdr x) (print-string "unsupported record type inspection syntax" x))))))))))

        ,predicate
        ,constructor
        ,@accessors
    ))
)

(define-syntax with-record
  (lambda (x)
    (syntax-check-length<> x 3)

    (define args (cadr x))

    (syntax-assert (cdr x) (and (list? args) (fx> (length args) 2)) "with-record expects first argument to be a list of identifier")

    (syntax-assert args (identifier? (car args)) "with-record first element (expression evaluating to record) must be an identifier")
    (syntax-assert (cdr args) (identifier? (cadr args)) "with-record second element (expression evaluating to record type) must be an identifier")
    (syntax-assert args (every identfiier? (cddr args)) "with-record elements must all be valid identifier")

    (define record-type (cadr args))
    (define fields (cddr args))

    #t))

(define-record Shape)

;; 
#|

(with-record (p Point x y)
  #t)

(define-record <point> ()
  (x type: fixnum? getter: point-x setter: set-point-x!)
  (y type: fixnum? getter: point-y setter: set-point-y!)
  constructor:
    (lambda (k)
      (lambda (#!optional (x 0) (y 0))
        (k x y)))
  predicate: point?
)

;; we can get record-name...

;; We need a list of field names at the very least, right.

;; '(x y)
;; '(0 1)

;; but we need this at expansion time.
;; okay, we need to return a macro what does the thing with the stuff.

;; (with-record (p Point x y)
;;   #t)

;; becomes

;; (%with-record (%Point/fields) x y)

;; (%Point/predicate) 

;; (%Point/field p 'x)
;; (record-ref Point p 'x ??)

;; (let ((x (%Point/field p 'x)) (y (%Point/field 'y))
;;   #t)



;; ???

;; (let ((x (Point/x p)) (y (Point/y p))) #t)

(with-record (p Point x y)
  (print x))

;; how is record information communicated at expand-time?

;; maybe it returns another macro...


(with-record (p Point x y)
  (print x))

;; (record-fields (Point x y))
;; 
(let-record (p Point) (x y)
  (print x))

(define-generic thing)

(define-method thing (x type: point?)
  (print x))

(let-record (Point p x y)
  (set! x #t))

(let-record p Point (x y)

(let-record p Point (x y)
  x
  y
  (set! x #t)
  (set! y #t))

|#

;(define-generic print)
;(define-method (print 'asdf)


