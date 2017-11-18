
(define-syntax let
  (lambda (x r c)
    (define let-fn-name #f)
    (define bindings #f)
    (define body #f)
    (define names #f)
    (define vals #f)
    (define result #f)

    (if (< (length x) 3)
      (raise-source x 'syntax "let must have at least two arguments (bindings and body)" (list x)))

    (if (identifier? (list-ref x 1))
      (begin
        (set! let-fn-name (list-ref x 1))
        (set! bindings (list-ref x 2))
        (set! body (cdddr x)))
      (begin
        (set! bindings (list-ref x 1))
        (set! body (cddr x))))

    ;(print let-fn-name)
    ;(print "crashy crashy: " let-fn-name)

    (set! names
      (map-i (lambda (i binding)
             (define name #f)
             (if (not (list? binding))
               (raise-source (list-tail bindings i) 'syntax "let binding should be a list with a name and a value" (list x)))

             (if (not (fx= (length binding) 2))
               (raise-source binding 'syntax "let binding should have exactly 2 elements (name and value)" (list binding)))

             (set! name (car binding))

             (if (not (identifier? name))
               (raise-source binding 'syntax "let binding name should be an identifier" (list binding)))
             name)
           bindings))

    (set! vals 
      (map (lambda (binding)
             ;; TODO expand.
             (cadr binding)
             ) bindings))

    (set! result 
       (cons-source x #'lambda
         (cons-source x names body)))

    (set! result
      (if let-fn-name
        ;; named function application
        (cons-source x (list-source x #'lambda '()
          (list-source x #'define let-fn-name result)
          (cons-source x let-fn-name vals)) '())
        ;; anonymous function application
        (cons-source x result vals)))

    ;; let return
    result))

(define (append x y)
  (if (pair? x)
      (cons (car x) (append (cdr x) y))
      y))

(define (qq-list c lst)
  (if (pair? lst)
    (let ((obj (car lst)))
      (if (and (pair? obj) (c #'unquote-splicing (car obj)))
        (if (cdr lst)
          (list #'append (cadr obj) (qq-list c (cdr lst)))
          (cadr obj))
        ;; TODO: This could be replaced with cons* for less calls and less confusing output
        (list #'cons (qq-object c obj) (qq-list c (cdr lst)))
      ))
    (begin
      (if (null? lst) 
        '()
        (list #'quote lst)))))

(define (qq-element c lst)
  (if (c #'unquote (car lst))
      (cadr lst)
      (qq-list c lst)))
         
(define (qq-object c object)
  (if (pair? object)
      (qq-element c object)
      (list #'quote object)))

(define-syntax quasiquote
  (lambda (x c)
    (qq-object c (cadr x))
    ))

(define-syntax when
  (lambda (x)
    (if (fx< (length x) 3)
      (raise-source x 'syntax "when expects a condition and a body" (list x)))

    `(,#'if ,(list-ref x 1)
        (,#'begin ,@(cddr x)))))

(define-syntax unless
  (lambda (x)
    (if (fx< (length x) 3)
      (raise-source x 'syntax "unless expects a condition and a body" (list x)))

    `(,#'if (,#'not ,(list-ref x 1))
        (,#'begin ,@(cddr x)))))

;; This actually seriously slows down the compiler (which uses memv via case) vs using a C++ function.
;;Need some basic inlining for sure.

#|
(define (%member-impl cmp? obj lst)
  (let loop ((lst lst))
    (if (null? lst)
      #f
      (if (cmp? (car lst) obj)
        lst
        (loop (cdr lst))))))

(define (memq obj lst) (%member-impl eq? obj lst))
(define (memv obj lst) (%member-impl eqv? obj lst))
(define (member obj lst) (%member-impl equal? obj lst))
|#

#;(define (memv obj lst)
  (if (null? lst) 
    #f
    (if (eqv? (car lst) obj)
      lst
      (memv obj (cdr lst)))))

;; case
(define-syntax case
  (lambda (x c)
    (define key #f)
    (define clauses #f)

    (if (fx< (length x) 3)
      (raise-source x 'syntax "case expects at least three arguments (a key and a clause)" x))

    (set! key (cadr x))
    (set! clauses (cddr x))

    (if (not (list? clauses))
      (raise-source x 'syntax "case expects a list of clauses" x))

    (define code (let loop ((clause (car clauses))
               (clauses clauses))
      (unless (pair? clause)
        (raise 'syntax "case expects clause to be a datum" x))

      ;; TODO: Most Schemes seem to support a direct comparison as well e.g.
      ;; (case 5 (5 #t))
      (unless (or (symbol? (car clause)) (list? (car clause)) (self-evaluating? (car clause)))
        (raise 'syntax "case expects an else clause, a literal, or a list of literals as its datum"))

      (define condition
        (if (c (car clause) #'else)
          #t 
          (if (or (symbol? (car clause)) (self-evaluating? (car clause)))
            ;; Immediate values result in an eqv? call
            `(,#'eqv? ,#'result (,#'quote ,(car clause)))            
            ;; Lists will be memv'd
            `(,#'memv ,#'result (,#'quote ,(car clause))))))

      (define branch
        (if (and (c (car clause) #'else) (c (cadr clause) #'=>))
          `(,(list-ref clause 2) ,#'result)
          `(,#'begin ,@(cdr clause))))

      (if (null? (cdr clauses))
        `(,#'if ,condition
          (,#'begin ,branch)
          unspecified)
        `(,#'if ,condition
          (,#'begin ,branch)
          ,(loop (cadr clauses) (cdr clauses))))
      ))

    `(,#'let ((,#'result ,key))
      ,code)))

(define-syntax aif
  (lambda (x)
    (unless (or (fx= (length x) 4) (fx= (length x) 3))
      (raise 'syntax "aif expects three or four arguments" (list x)))

    `(,#'let ((it ,(list-ref x 1)))
        (,#'if it ,(list-ref x 2) ,(if (fx= (length x) 4) (list-ref x 3) unspecified)))))

(define-syntax let*
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "let* expects at least two arguments (bindings and body)" (list x)))

    (let ((bindings (list-ref x 1))
          (body (cddr x)))
      (for-each-i
        (lambda (i binding)
          (unless (and (list? binding) (fx= (length binding) 2) (identifier? (car binding)))
            (raise-source (list-tail (cadr x) i) 'syntax "let* binding must be a list with two elements" (list binding))))
        bindings)

    `(,#'let (,@(map (lambda (b) (list-source b (car b) #'unspecified)) bindings))
      ,@(map (lambda (b) (list-source b #'set! (car b) (cadr b))) bindings)
      ,@body))))

;; TODO: letrec restrictions.
(define-syntax letrec
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "let* expects at least two arguments (bindings and body)" (list x)))

    (let ((bindings (list-ref x 1))
          (body (cddr x)))
      (for-each-i
        (lambda (i binding)
          (unless (and (list? binding) (fx= (length binding) 2) (identifier? (car binding)))
            (raise-source (list-tail (cadr x) i) 'syntax "let* binding must be a list with two elements" (list binding))))
        bindings)

    `(,#'let (,@(map (lambda (b) (list-source b (car b) #'unspecified)) bindings))
      ,@(map (lambda (b) (list-source b #'set! (car b) (cadr b))) bindings)
      ,@body))))

(define (max a b) (if (< a b) b a))
(define (min a b) (if (> a b) b a))

(define (values . rest)
  (cons (list 'values) rest))

;; Records.

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
      (raise-source x 'syntax "define-record expects at least one argument: a record name" (list x)))

    (set! name (cadr x))

    (unless (symbol? name)
      (raise-source (cdr x) 'syntax (print-string "define-record name must be a symbol but got" name) (list x)))

    (set! name-string (symbol->string name))

    (when (fx> (length x) 2)
      (if (list? (list-ref x 2))
        (begin
          (when (null? (list-ref x 2))
            (raise-source (caddr x) 'syntax "define-record inheritance argument must be a list with exactly one symbol but got none" (list x (cddr x))))
          (when (> (length (list-ref x 2)) 1)
            (raise-source (caddr x) 'syntax "define-record inheritance argument must be a list with exactly one symbol but more than one" (list x (cddr x))))
          (set! parent (car (list-ref x 2)))
          (set! fields (cdddr x)))
        (set! fields (cddr x))))

    (unless (or (symbol? parent) (not parent))
      (raise-source (caddr x) 'syntax "define-record inheritance argument should be a list with exactly one symbol" (list x (caddr x))))

    (set! accessors
      (map-i
        (lambda (i field-name)
          (define getter #f)
          (define setter #f)

          (unless (symbol? field-name)
            (raise-source (list-tail fields i) 'syntax "define-record fields must be symbols" (list x name)))

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

;; PORTS

(define (call-with-input-file path thunk)
  (let* ((file (open-input-file path))
         (result (thunk file)))
    (close-input-port file)
    result))

(define (call-with-output-file path thunk)
  (let* ((file (open-output-file path))
         (result (thunk file)))
    (close-output-port file)
    file))

;; LISTS

(define (assoc-impl cmp? obj alist)
  (if (null? alist)
    #f
    (let loop ((elt (car alist))
               (rest (cdr alist)))
      (if (and (pair? elt) (cmp? (car elt) obj))
        elt
        (if (null? rest)
          #f
          (loop (car rest) (cdr rest)))))))

;(define (assq obj alist) (assoc-impl eq? obj alist))
(define (assv obj alist) (assoc-impl eqv? obj alist))
(define (assoc obj alist) (assoc-impl equal? obj alist))

(define (reduce fn lst)
  (if (null? lst)
    '()
    (let ((len (length lst)))
      (let loop ((i 0)
                 (result (car lst)))
        (if (fx= i (fx- len 1))
          result
          (begin
            (loop (fx+ i 1) (fn result (list-ref lst (fx+ i 1))))))))))

#;(define (filter fn lst)
  (if (null? lst)
    '()
    (if (fn (car lst))
      (cons (car lst) (filter fn (cdr lst)))
      (filter fn (cdr lst)))))

;; VECTORS

(define (list->vector lst)
  (if (null? lst)
    (make-vector)
    (let ((vec (make-vector (length lst))))
      (let loop ((elt (car lst))
                 (rest (cdr lst))
                 (i 0))
        (vector-set! vec i elt)
        (if (null? rest) vec (loop (car rest) (cdr rest) (+ i 1)))))))

;; SRFI-0

(define (cond-expand-check-feature x form)
  (unless (and (list? form) (not (null? form)))
    (raise-source form 'syntax "cond-expand feature requirement must be a list with at least one element" (list form)))

  (case (car form)
    ((and)
     (reduce (lambda (a b) (and a b)) (map (lambda (x) (if (memq x *features*) #t #f)) (cdr form))))
    ((or)
     (reduce (lambda (a b) (or a b)) (map (lambda (x) (if (memq x *features*) #t #f)) (cdr form))))
    ((else) #t)
    ((library) (raise 'syntax "cond-expand does not support library yet" (list form))))
)

;; Print and return an intermediate value. For printf-debugging convenience.
(define (pi x)
  (print x)
  x)

(define-syntax cond-expand
  (lambda (x)

    (if (null? (cdr x))
      (if #f #f)

      (cons-source x #'begin
            (let loop ((clause (cadr x))
                 (clauses (cddr x))
                 (results '()))

              (unless (= (length clause) 2)
                (begin
                  (raise 'syntax "cond-expand clause length must be exactly 2 (feature list and command or definition)"))
              )

              (let* ((feature-list (car clause))
                     (commands (cadr clause))
                     (new-results
                      (if (cond-expand-check-feature x feature-list)
                        (cons-source x commands results)
                        results)))
                (if (null? clauses)
                  new-results
                  (loop (cadr clauses) (cddr clauses) new-results)
                  )))))))
;; While loop

(define-syntax while
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "while loop expected at least two arguments: condition and body" (list x)))
    `(,#'let ,#'loop ((,#'condition ,(cadr x)))
        (,#'when ,#'condition
          ,@(cddr x)
          (,#'loop ,(cadr x))))))

;; compatibility

(define-syntax er-macro-transformer
  (lambda (x)
    (cadr x)))

