;; syntax.scm - syntax and other standard library functions
;; loaded second

(define-syntax let
  (lambda (x r c)
    (define let-fn-name #f)
    (define bindings #f)
    (define body #f)
    (define names #f)
    (define vals #f)
    (define result #f)

    (if (fx< (length x) 3)
      (raise-source x 'syntax "let must have at least two arguments (bindings and body)" (list x)))

    (if (identifier? (list-ref x 1))
      (begin
        (set! let-fn-name (list-ref x 1))
        (set! bindings (list-ref x 2))
        (set! body (cdddr x)))
      (begin
        (set! bindings (list-ref x 1))
        (set! body (cddr x))))

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
      (map1 (lambda (binding)
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

(define (append . lsts)
  (reduce (lambda (a b) (append1 a b)) lsts))

(define (alist-check-cell lst cell)
  (if (not (pair? cell))
    (raise 'type (print-string "alist method encountered invalid alist cell" cell) (list lst cell))))

(define (alist-copy lst)
  (map1 (lambda (x)
         (alist-check-cell lst x)
           (cons (car x) (cdr x))) lst))

(define (fold-left f init seq)
  (if (null? seq)
    init
    (fold-left f (f (car seq) init) (cdr seq))))

(define (fold-right f init seq) 
  (if (null? seq) 
    init 
    (f (car seq) 
      (fold-right f init (cdr seq))))) 

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
    (if (c #'quasiquote (car lst))
      (begin 
        (qq-list c lst))
      (qq-list c lst))))
         
(define (qq-object c object)
  (if (pair? object)
      (qq-element c object)
      (list #'quote object)))

;; TODO: Needs to allow nesting
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

(define (syntax-error source . rest)
  (raise-source source 'syntax (apply print-string rest) (list source)))

(define (syntax-assert-length= src eq . desc)
  (let ((len (length src))
        (name (top-level-value '*current-macro-name* #f)))
    (when (not (fx= len eq))
      (raise-source src 'syntax (print-string (or name "macro") "expected" (if (null? desc) "expression" (car desc)) "to be exactly" eq " elements but only got" len) (list src)))))

(define (syntax-assert-length> src gt)
  (let ((len (length src))
        (name (top-level-value '*current-macro-name* #f)))
    (when (fx<= len gt)
      (raise-source src 'syntax (print-string (or name "macro") "expected expression to be at most "  gt "elements but only got" len) (list src)))))

(define (syntax-assert-length<> src lt . gt)
  (let ((len (length src))
        (name (top-level-value '*current-macro-name* #f)))
    (when (fx< len lt)
      (raise-source src 'syntax (print-string (or name "macro") "expected expression to be at least" lt "elements but only got" len) (list src))
      (when (and (not (null? gt)) (fx> len (car gt)))
        (raise-source src 'syntax (print-string (or name "macro") "expected expression to be at most" (car gt) "elements but only got" len) (list src))))))


(define (take lst limit)
  (unless (fixnum? limit) (raise 'type "take expected second argument (limit) to be a fixnum"))
  (let loop ((got 0)
             (newlst '())
             (lst lst))
    (if (fx= got limit)
      (reverse newlst)
      (if (null? lst)
        (raise 'bounds (print-string limit "values requested by take invocation but only got" got) (list lst limit))
        (loop (fx+ got 1) (cons (car lst) newlst) (cdr lst))))))
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

#|
(define (memqi obj lst)
  (let lp ((lst lst))
    (if (null? lst)
      #f
      (if (not (pair? obj))
        (raise 'type (print-string "memqi expected a valid list but found" obj) obj)
        (if (eq? (car obj)
                 |#

#;(define (memv obj lst)
  (if (null? lst) 
    #f
    (if (eqv? (car lst) obj)
      lst
      (memv obj (cdr lst)))))

(define-syntax case
  (lambda (x c)
    (if (fx< (length x) 3)
      (raise-source x 'syntax "case expects at least three arguments (a key and a clause)" x))

    (define key (cadr x))
    (define clauses (cddr x))

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

    `(,#'let (,@(map1 (lambda (b) (list-source b (car b) #'unspecified)) bindings))
      ,@(map1 (lambda (b) (list-source b #'set! (car b) (cadr b))) bindings)
      ,@body))))

;; TODO: letrec restrictions.
(define-syntax letrec
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "letrec expects at least two arguments (bindings and body)" (list x)))

    (let ((bindings (list-ref x 1))
          (body (cddr x)))
      (for-each-i
        (lambda (i binding)
          (unless (and (list? binding) (fx= (length binding) 2) (identifier? (car binding)))
            (raise-source (list-tail (cadr x) i) 'syntax "letrec binding must be a list with two elements" (list binding))))
        bindings)

    `(,#'let (,@(map1 (lambda (b) (list-source b (car b) #'unspecified)) bindings))
      ,@(map1 (lambda (b) (list-source b #'set! (car b) (cadr b))) bindings)
      ,@body))))

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

(define (assv obj alist) (assoc-impl eqv? obj alist))
(define (assoc obj alist) (assoc-impl equal? obj alist))

(define (map pred . lsts)
  (if (null? (car lsts))
    '()
    (let loop ((args (map1 (lambda (lst) (car lst)) lsts))
               (rest (map1 (lambda (lst) (cdr lst)) lsts)))
      (cons (apply pred args)
            (if (null? (car rest))
              '()
              (loop (map1 (lambda (lst) (car lst)) rest) (map1 (lambda (lst) (cdr lst)) rest)))))))

(define (for-each pred . lsts)
  (if (null? (car lsts))
    unspecified
    (let loop ((args (map1 (lambda (lst) (car lst)) lsts))
               (rest (map1 (lambda (lst) (cdr lst)) lsts)))
      (apply pred args)
      (if (null? (car rest))
        unspecified
        (loop (map1 (lambda (lst) (car lst)) rest) (map1 (lambda (lst) (cdr lst)) rest))))))

#;(define (filter fn lst)
  (if (null? lst)
    '()
    (if (fn (car lst))
      (cons (car lst) (filter fn (cdr lst)))
      (filter fn (cdr lst)))))

;; Print and return an intermediate value. For printf-debugging convenience.
(define (pi . rst)
  (if (null? (cdr rst))
    (begin
      (print (car rst))
      (car rst))
    (begin
      (print (car rst))
      (print (cadr rst))
      (cadr rst))))

;; While loop

;; TODO: Add break/continue

(define-syntax while
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "while loop expected at least two arguments: condition and body" (list x)))
    `(,#'let ,#'loop ((,#'condition ,(cadr x)))
        (,#'when ,#'condition
          ,@(cddr x)
          (,#'loop ,(cadr x))))))

;; TODO: Simple for loop

;; Do loop

(define-syntax do
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "do loop needs at least two elements: variables and test" (list x)))

    (let ((varb (list-ref x 1))
          (test (list-ref x 2))
          (body (if (= (length x) 3) unspecified (cdddr x))))

      (define vars
        (map
          (lambda (v)
            (unless (and (> (length v) 1) (< (length v) 4))
              (raise-source v 'syntax "do loop variable binding expects at least two but no more than three elements: variable name and initial value" (list v)))

            (list (list-ref v 0) (list-ref v 1) (if (eq? (length v) 2) (list-ref v 0) (list-ref v 2))))
          varb))

      #`(let loop (,@(map (lambda (v) (list (list-ref v 0) (list-ref v 1))) vars))
          (if ,(list-ref test 0)
            (begin ,@(cdr test))
            (begin
              ,@body
              (loop ,@(map (lambda (v) (list-ref v 2)) vars)))))

      )))

;; SRFI 1

(define (every1 pred lst)
  (if (null? lst)
    #t
    (let loop ((elt (car lst)) (rest (cdr lst)))
      (if (not (pred elt))
        #f
        (if (null? rest)
          #t
          (loop (car rest) (cdr rest)))))))

(define (every pred . lsts)
  (let loop ((lst (car lsts)) (rest (cdr lsts)))
    (if (not (every1 pred lst))
      #f
      (if (null? rest)
        #t
        (loop (car rest) (cdr rest))))))

;; ASSERTIONS

(define-syntax syntax-assert
  (lambda (x)
    (syntax-assert-length= x 4)
    #`(let ((result ,(list-ref x 2))
            (name (top-level-value '*current-macro-name* "unknown macro")))
        (unless result
          (raise-source ,(list-ref x 1) 'syntax (print-string name ":" ,(list-ref x 3)) ,(list-ref x 1))))))

(define-syntax assert
  (lambda (x)
    (syntax-assert-length= x 2)
    #`(let ((result ,(list-ref x 1)))
        (unless result
          (raise 'assert (print-string "Assertion failure: expected expression" (quote ,(list-ref x 1)) " to be true"))))))

;; SRFI 39: parameter objects

(define (make-parameter initial . maybe-converter)
  (let ((storage initial)
        (converter (if (null? maybe-converter) #f (car maybe-converter))))

    (if converter
      (lambda value
        (unless (null? value)
          (set! storage (converter (car value))))
        storage)
      (lambda value
        (unless (null? value)
          (set! storage (car value)))
        storage))))

;; Extension: Make a parameter that fetches and sets a toplevel value
;; Conversion is not supported (there's no way to prevent setting a toplevel value to something arbitrary)
(define (make-top-level-parameter name . initial)
  (if (not (null? initial))
    (set-top-level-value! name (car initial)))
  (lambda value
    (unless (null? value)
      (set-top-level-value! name (car value)))
    (top-level-value name)))

(define-syntax define-top-level-parameter
  (lambda (x)
    (unless (fx= (length x) 3)
      (raise-source x 'syntax "define-top-level-parameter takes exactly two arguments"))

    #`(define ,(cadr x)
        (lambda value
          (if (null? value)
            (top-level-value ',(caddr x))
            (begin
              (set-top-level-value! ',(caddr x) value)
              (top-level-value ',(caddr x))))))))

(define-top-level-parameter current-output-port *current-output-port*)
(define-top-level-parameter current-input-port *current-input-port*)
(define-top-level-parameter *print-readably* PRINT-READABLY)
(define-top-level-parameter *print-table-max* PRINT-TABLE-MAX)

(define-syntax parameterize
  (lambda (x)
    (unless (fx> (length x) 2)
      (raise-source 'syntax x "parameterize requires at least two arguments (bindings and body)"))

    (define bindings (cadr x))
    (define body (cddr x))

    (for-each1
      (lambda (x)
        (if (not (fx= (length x) 2))
          (raise-source 'syntax x "parameterize binding must have two elements (parameter object and value)" (list x)))
      )
    bindings)

    (if (null? bindings)
      body
      ;; Save bindings in a list
      #`(let ((bindings (list ,@(map1 (lambda (x) #`(cons ,(car x) (,(car x)))) bindings))))
          ,@(map1 (lambda (x) #`(,(car x) ,(cadr x))) bindings)
          (unwind-protect
            (lambda () ,@body)
            (lambda ()
              (let loop ((item (car bindings)) (rest (cdr bindings)))
                ((car item) (cdr item))
                (unless (null? rest)
                  (begin
                    (loop (car rest) (cdr rest)))))))))))

