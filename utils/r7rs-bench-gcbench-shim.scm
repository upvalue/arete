;; gcbench-specific record shim. This is appended only for gcbench because
;; injecting this macro into every benchmark perturbs unrelated workloads.

(define-syntax define-record-type
  (lambda (x r c)
    (define name (list-ref x 1))
    (define constructor-spec (list-ref x 2))
    (define predicate (list-ref x 3))
    (define fields (list-tail x 4))
    (define constructor-name (car constructor-spec))
    (define constructor-args (cdr constructor-spec))
    (define type-id (string->symbol (string-append "%" (symbol->string name) "/type")))

    (define (field-name field-spec) (list-ref field-spec 0))
    (define (field-accessor field-spec) (list-ref field-spec 1))
    (define (field-mutator field-spec) (list-ref field-spec 2))

    (define (build-accessors specs i)
      (if (null? specs)
          '()
          (cons
            `(,(r 'define) (,(field-accessor (car specs)) rec)
               (,(r 'record-ref) ,type-id rec ,i))
            (cons
            `(,(r 'define) (,(field-mutator (car specs)) rec value)
                 (,(r 'record-set!) ,type-id rec ,i value))
              (build-accessors (cdr specs) (+ i 1))))))

    `(,(r 'begin)
       (,(r 'define) ,type-id
         (,(r 'register-record-type)
          ,(symbol->string name)
          ,(length fields)
          0
          ',(map field-name fields)
          #f))
       (,(r 'define) (,constructor-name ,@constructor-args)
         (,(r 'let) ((instance (,(r 'make-record) ,type-id)))
           ,@(let build ((specs fields))
               (if (null? specs)
                   '()
                   (cons
                     `(,(field-mutator (car specs)) instance ,(field-name (car specs)))
                     (build (cdr specs)))))
           instance))
       (,(r 'define) (,predicate obj)
         (,(r 'record-isa?) obj ,type-id))
       ,@(build-accessors fields 0))))
