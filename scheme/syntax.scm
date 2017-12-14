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

(define (append1 x y)
  (if (pair? x)
      (cons (car x) (append (cdr x) y))
      y))

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
      (list #'quote (cadr lst))
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
      `(,#'define (,(string->symbol (string-append name-string "/make")) ,@(map1 rename fields))
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
        (,#'let ((,#'fields (list ,@(map1 (lambda (x) `(,#'quote ,x)) fields))))
          (begin
            ;(print ,#'fields)
            (,#'register-record-type ,name-string ,field-count 0 ,#'fields ,parent))))
       ,predicate
       ,constructor
       ,@accessors
      )
    
  )) ;; lambda (x)

;;;;; NUMBERS

(define (zero? x) (or (eq? x 0) (eqv? x 0.0)))
(define (positive? x) (>= x 0))
(define (negative? x) (< x 0))
(define (odd? x) (= (modulo x 2) 1))
(define (even? x) (= (modulo x 2) 0))
(define exact? fixnum?)
(define inexact? flonum?)
(define (integer? x) (or (fixnum? x) (eqv? (floor x) x)))

#|
TODO: Casting
(define (max1 a b) (if (> a b) a b))
(define (min1 a b) (if (> a b) b a))

(define (max a b . rest)
  (if (null? rest)
    (if (> b a) b a)
    (max1 (if (> b a) b a) (reduce max1 rest))))

(define (min a b . rest)
  (if (null? rest)
    (if (> b a) a b)
    (min1 (if (> b a) a b) (reduce min1 rest))))
|#


(define (abs a) (if (< a 0) (- a) a))
(define (complex? x) (or (fixnum? x) (flonum? x)))
(define (rational? x) (or (fixnum? x) (flonum? x)))
;; TODO NaN
(define (real? x) (or (fixnum? x) (flonum? x)))

;;;;; DELAY/FORCE

(define-syntax delay
  (lambda (x)
    (if (not (fx= (length x) 2))
      (raise-source x 'syntax "delay only takes a single argument" (list x)))

    `(,#'list '##delayed #f (,#'lambda () ,(cadr x)))))

(define (force x)
  (if (and (pair? x) (eq? (car x) '##delayed))
    (if (eq? (cadr x) #f)
      (let ((result ((caddr x))))
        (set-car! (cdr x) #t)
        (set-car! (cddr x) result)
        result)
      (caddr x))))

;;;;; MULTIPLE RETURN VALUES

(define (values . rest)
  (if (and (not (null? rest)) (null? (cdr rest)))
    (car rest)
    (cons (list 'values) rest)))

(define (call-with-values producer consumer)
  (apply consumer (cdr (producer))))

;;;;; CHARACTERS

(define (char<? a b)
  (< (char->integer a) (char->integer b)))

(define (char>? a b)
  (> (char->integer a) (char->integer b)))

(define char-downcase char-case-fold)

(define (char-numeric? c)
  (let ((i (char->integer c)))
    (and (fx> i 47) (fx< i 58))))

(define (char-upper-case? c)
  (let ((i (char->integer c)))
    (and (fx> i 64) (fx< i 91))))

(define (char-lower-case? c)
  (let ((i (char->integer c)))
    (and (fx> i 96) (fx< i 122))))

(define (char-alphabetic? c)
  (or (char-lower-case? c) (char-upper-case? c)))

(define (char-whitespace? c)
  (and (memq (char->integer c) '(9 10 11 12 13 32)) #t))

(define (char-upcase c)
  (if (not (and (char-alphabetic? (char>=? char #\A) (char<=? char #\Z))))
    c
    (integer->char (fx- (char->integer c) 32))))

(define (char<=? a b)
  (not (char>? a b)))

(define (char>=? a b)
  (not (char<? a b)))

(define (char-ci=? a b)
  (char=? (char-case-fold a) (char-case-fold b)))

(define (char-ci<? a b)
  (char<? (char-case-fold a) (char-case-fold b)))

(define (char-ci>? a b)
  (char>? (char-case-fold a) (char-case-fold b)))

(define (char-ci<=? a b)
  (char<=? (char-case-fold a) (char-case-fold b)))

(define (char-ci>=? a b)
  (char>=? (char-case-fold a) (char-case-fold b)))

(define (string-ci=? a b)
  (string=? (string-map char-case-fold a) (string-map char-case-fold b)))

(define (string-sum s1)
  (let loop ((i 0)
             (sum 0))
    (if (fx= i (string-length s1))
      sum
      (loop (fx+ i 1) (fx+ sum (char->integer (string-ref s1 i)))))))

(define (string-sum-ci s1)
  (let loop ((i 0)
             (sum 0))
    (if (fx= i (string-length s1))
      sum
      (loop (fx+ i 1) (fx+ sum (char->integer (char-case-fold (string-ref s1 i))))))))

(define (string<? a b) (< (string-sum a) (string-sum b)))
(define (string>? a b) (> (string-sum a) (string-sum b)))
(define (string<=? a b) (not (string>? a b)))
(define (string>=? a b) (not (string<? a b)))
(define (string-ci<? a b) (< (string-sum-ci a) (string-sum-ci b)))
(define (string-ci>? a b) (> (string-sum-ci a) (string-sum-ci b)))
(define (string-ci<=? a b) (<= (string-sum-ci a) (string-sum-ci b)))
(define (string-ci>=? a b) (>= (string-sum-ci a) (string-sum-ci b)))

;;;;; STRINGS

(define (list->string lst)
  (unless (list? lst)
    (raise 'type "string->list expected list as first argument" (list lst)))
  (if (null? lst)
    ""
    (let ((str (make-string (length lst))))
      (let loop ((elt (car lst))
                 (rest (cdr lst))
                 (i 0))
        (unless (char? elt)
          (raise 'type (print-string "string->list expected a list of characters but got " elt) (list elt)))
        (string-set! str i elt)
        (if (null? rest)
          str
          (loop (car rest) (cdr rest) (fx+ i 1)))))))

(define (string->list str)
  (if (fx= (string-length str) 0)
    '()
    (let loop ((i (string-length str))
               (lst '()))
      (if (fx= i 0)
        lst
        (loop (fx- i 1) (cons (string-ref str (fx- i 1)) lst))))))

(define (string . lst) (list->string lst))

;; PORTS

(define (call-with-input-file path thunk)
  (let ((file (open-input-file)))
    (unwind-protect (lambda () (thunk file))
                    (lambda () (close-input-port file)))))

(define (call-with-output-file path thunk)
  (let ((file (open-output-file)))
    (unwind-protect (lambda () (thunk file))
                    (lambda () (close-output-port file)))))

(define (call-with-output-string thunk)
  (let ((file (open-output-string)))
    (unwind-protect (lambda ()
                      (thunk file)
                      (get-output-string file)
                      )
                    (lambda () (close-output-port file)))))

(define (call-with-input-string thunk)
  (let ((file (open-input-string)))
    (unwind-protect (lambda () (thunk file))
                   (lambda () (close-input-port file)))))

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

(define (map2 f ls1 ls2)
  (let loop ((ls1 ls1) (ls2 ls2))
    (if (null? ls1)
      '()
      (cons (apply f (list (car ls1) (car ls2))) (loop (cdr ls1) (cdr ls2))))))

#;(define (filter fn lst)
  (if (null? lst)
    '()
    (if (fn (car lst))
      (cons (car lst) (filter fn (cdr lst)))
      (filter fn (cdr lst)))))

;; Redefine map


;; VECTORS

(define (vector->list vec)
  (if (fx= (vector-length vec) 0)
    '()
    (let loop ((i (vector-length vec)) (lst '()))
      (if (fx= i 0)
        lst
        (loop (fx- i 1) (cons (vector-ref vec (fx- i 1)) lst))))))


(define (list->vector lst)
  (if (null? lst)
    (make-vector)
    (let ((vec (make-vector (length lst))))
      (let loop ((elt (car lst))
                 (rest (cdr lst))
                 (i 0))
        (vector-set! vec i elt)
        (if (null? rest) vec (loop (car rest) (cdr rest) (+ i 1)))))))

(define (vector . lst) (list->vector lst))

;; SRFI-0

;; TODO: This needs to be rewritten in the style of module-imports, to allow recursion.

(define (cond-expand-check-feature x form)
  (unless (and (list? form) (not (null? form)))
    (raise-source form 'syntax "cond-expand feature requirement must be a list with at least one element" (list form)))

  (case (car form)
    ((and)
     (reduce (lambda (a b) (and a b)) (map (lambda (x) (if (memq x (top-level-value '*features*)) #t #f)) (cdr form))))
    ((or)
     (reduce (lambda (a b) (or a b)) (map (lambda (x) (if (memq x (top-level-value '*features*)) #t #f)) (cdr form))))
    ((else) #t)
    ((library)
     (unless (fx= (length form) 2)
       (raise 'syntax "cond-expand library expression must have only one argument" (list form)))
     ;(print (cdr x))
     (if (table-ref (top-level-value 'module-table) (module-spec->string (cdr form) (cadr form)))
       #t
       #f)))
)

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

(define-syntax cond-expand
  (lambda (x)

    (if (null? (cdr x))
      (if #f #f)

      (cons-source x #'begin
            (let loop ((clause (cadr x))
                 (clauses (cddr x))
                 (results '()))

              (unless (> (length clause) 2)
                (begin
                  (raise 'syntax "cond-expand clause length must be at least 2 (feature list and command or definition)"))
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

;; (do 1: ((var init step)) 
;;     2: (test expression)
;;    body)
;; (let loop ((lambda 

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

      `(,#'let ,#'loop (,@(map (lambda (v) (list (list-ref v 0) (list-ref v 1))) vars))
         (,#'if ,(list-ref test 0)
           (,#'begin ,@(cdr test))
           (,#'begin
             ,@body
             (,#'loop ,@(map (lambda (v) (list-ref v 2)) vars)))))))

  );;do

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

(define-syntax assert-equal?
  (lambda (x)
    `(,#'let ((,#'result1 ,(list-ref x 1))
              (,#'result2 ,(list-ref x 2)))
        (,#'unless (,#'equal? ,#'result1 ,#'result2)
          (,#'raise 'assert (,#'print-string "Assertion failure: expected expression" (quote ,(list-ref x 1)) "to equal" (quote ,(list-ref x 2))) (,#'list ,#'result1 ,#'result2))))))

;; ONE-SHOT CONTINUATIONS

(define (call/1cc thunk)
  (define tag (gensym))
  (define (trigger-continuation value)
    (raise-continuation tag value))
  (define result
    (try
      (lambda () (thunk (lambda (value) (trigger-continuation value))))
      (lambda (exc)
        (if (and (eq? (exception-tag exc) 'continuation) (eq? (exception-message exc) tag))
          (exception-irritants exc)
          #f))))
  (set! trigger-continuation
    (lambda (value)
      (raise 'eval "Attempt to invoke spent one-shot continuation" tag)))
  result)

;; We define call/cc and dynamic-wind on the simpler unwind-protect and call/1cc. It won't support more extreme uses
;; of continuations but for many of them, is sufficient.

(define call/cc call/1cc)
(define call-with-current-continuation call/1cc)

(define (dynamic-wind before thunk after)
  (before)
  (unwind-protect thunk after))

;; SRFI 39: parameter objects

(define (make-parameter initial . maybe-converter)
  (let ((storage initial)
        (converter (if (null? maybe-converter) #f (car maybe-converter))))

    (lambda value
      (unless (null? value)
        (set! storage (if converter (converter (car value)) (car value))))
      storage)))

;; Extension: Make a parameter that fetches and sets a toplevel value
;; Conversion is not supported (there's no way to prevent setting a toplevel value to something arbitrary)
(define (make-top-level-parameter name . initial)
  (if (not (null? initial))
    (set-top-level-value! name (car initial)))
  (lambda value
    (unless (null? value)
      (set-top-level-value! name (car value)))
    (top-level-value name)))

;; We have to set these later because the compiler can't compile an interpreted closure correctly
(define current-input-port #f)
(define current-output-port #f)

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
                  (loop (car rest) (cdr rest))))))))))

;; COMPATIBILITY

(define-syntax er-macro-transformer
  (lambda (x)
    (cadr x)))

