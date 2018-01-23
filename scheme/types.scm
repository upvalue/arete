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

;; Record implementation

;; TODO: While "syntactic inspection" works, it's very unwieldy to write large macros with. How do large Scheme systems
;; handle things like this?

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

        (set! parent (caar (cddr x)))
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

    (define constructor (aif (assq constructor: kwargs) (cadr it) #f))

    #;(begin
      (print field-names)
      (print kwargs)
      (print accessors))

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
        ;; This is the syntatic introspection facility
        (define-syntax ,name
          (combined-transformer
            (let ((all-fields
                    (if ,parent
                      (append (,parent fields:) (list ,@(map (lambda (x) (list (rename 'quote) x)) fields)))
                      (quote ,fields))))
              (lambda (x r c)
                (if (identifier? x)
                  #',type-name
                  (begin
                    (cond
                      ((eq? (cadr x) constructor:)
                       (list (r 'lambda) all-fields
                             (list (r 'define) (r 'instance) (list (r 'make-record) (quote ,type-name)))
                             (cons 'begin
                               (map-i
                                 (lambda (i f)
                                   (list (r 'record-set!) (quote ,type-name) (r 'instance) i f))
                                 all-fields))
                             (r 'instance)))
                      ((eq? (cadr x) fields-count:)
                       (length all-fields))
                      ((eq? (cadr x) fields:)
                       (syntax-assert-length= x 2)
                       #;(if ,parent
                         (list (rename 'quote) (append (,parent fields:) (list ,@(map (lambda (x) (list (rename 'quote) x)) fields))))
                         (list (rename 'quote) (list ,@(map (lambda (x) (list (rename 'quote) x)) fields))))
                      (list (rename 'quote) all-fields))

                      ;; (type getter: var-name field-name)
                      ;; returns 
                      ;; (record-ref type-name var-name field-idx)
                      ((eq? (cadr x) getter:)
                        (syntax-assert-length= x 4)
                        (let lp ((fields all-fields) (i 0))
                          (if (null? fields)
                            (syntax-error x (print-string "record type" (quote ,name) "has no field named" (list-ref x 3)))
                            (if (eq? (car fields) (list-ref x 3))
                              (list #'record-ref #',type-name (list-ref x 2) i)
                              (lp (cdr fields) (fx+ i 1))))))

                      ;; (type setter: var-name field-name value)
                      ;; returns
                      ;; (record-set! type-name var-name field-idx value)

                      ((eq? (cadr x) setter:)
                       (syntax-assert-length= x 5)
                       (let lp ((fields all-fields) (i 0))
                         (if (null? fields)
                           (syntax-error x (print-string "record type" ',name "has no field named" (list-ref x 3)))
                           (if (eq? (car fields) (list-ref x 3))
                             (list #'record-set! #',type-name (list-ref x 2) i (list-ref x 4))
                             (lp (cdr fields) (fx+ i 1))))))

                      (else
                        (syntax-error (cdr x) (print-string "unsupported record type inspection syntax" x))))))))))

        (define ,type-name (register-record-type ,(symbol->string name) ,field-count 0 (,name fields:) ,parent))

        ,(if constructor
           #`(define ,constructor-name 
               (,constructor (,name constructor:)))
           #`(define ,constructor-name (,name constructor:)))

        ,predicate
        ,@accessors
    ))
)

;; with-record, convenience syntax for unwrapping records in a type-checked fashion
(define-syntax with-record
  (lambda (x r c)
    (syntax-assert-length<> x 3)

    (define args (cadr x))

    (syntax-assert (cdr x) (and (list? args) (fx> (length args) 2)) "with-record expects first argument to be a list of 3 or more identifiers")

    (syntax-assert args (identifier? (car args)) "with-record first element (expression evaluating to record) must be an identifier")
    (syntax-assert (cdr args) (identifier? (cadr args)) "with-record second element (expression evaluating to record type) must be an identifier")
    ;(syntax-assert args (every identifier? (cddr args)) "with-record elements must all be valid identifiers")
    (syntax-assert args (list? args) "with-record elements must be a list")

    (define var (car args))
    (define record-type (cadr args))

    (for-each1
      (lambda (x)
        (if (identifier? x)
          x
          (begin
            (syntax-assert (cddr args) (and (list? x) (fx= (length x) 2) (every identifier? x)) "with-record binding must be either an identifier or a list with two identifiers")
            (car x))))
      (cddr args))

    (define body (cddr x))

    (define bindings
      (map1
        (lambda (item)
          (define name (if (pair? item) (cadr item) item))
          (list (if (pair? item) (car item) item) #`(,record-type getter: ,var ,name)))
        (cddr args)))

    #`(let ,bindings
        (let-syntax ((,'set! 
                       (lambda (x r c)
                         (syntax-assert-length= x 3)
                         ;(print "kount:" (,record-type fields-count:))
                         (if (memq (cadr x) (,record-type fields:))
                           (list ',record-type setter: ,var (cadr x) (caddr x))
                           (list #'set! (cadr x) (caddr x)))
                         )))
          ,@body))))

;; TODO. What if this were a more generic macro, somewhat like pattern matching but rather having the effect of replacing
;; identifier bindings with accessors, e.g.

;; (with ((record p Point x y)
;;        (pair asdf (x car) (y cdr))        
;; Would it be more useful?

