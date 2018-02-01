;; library.scm - standard library functions that are not required for the expander or compiler

;; Do loop

(define-syntax do
  (lambda (x)
    (unless (> (length x) 2)
      (raise-source x 'syntax "do loop needs at least two elements: variables and test" (list x)))

    (let ((varb (list-ref x 1))
          (test (list-ref x 2))
          (body (if (= (length x) 3) '() (cdddr x))))

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

;; really scheme, really? these functions should just throw an error for public indecency
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

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
(define complex? number?)
(define real? number?)
;; TODO NaN

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

(define (string-for-each-i s i fn end)
  (if (fx= i end)
    unspecified
    (begin
      (fn (string-ref s i))
      (string-for-each-i s (fx+ i 1) fn end))))

(define (string-for-each fn s)
  (string-for-each-i s 0 fn (string-length s))
  unspecified)

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
  (let ((file (open-input-file path)))
    (unwind-protect (lambda () (thunk file))
                    (lambda () (close-input-port file)))))

(define (call-with-output-file path thunk)
  (let ((file (open-output-file path)))
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

;; SRFI 0

;; recursively "evaluate" a cond-expand clause to true or false
(define (cond-expand-eval cmp? form)
  (let ((features (top-level-value '*features*)) (type (if (pair? form) (car form) #f)))
    (cond 
      ((identifier? form) (and (memq form features) #t))
      ((not (pair? form))
       (syntax-error form "cond-expand clause must be either a pair or a symbol"))

      ((cmp? type 'and)
       (reduce
         (lambda (a b) (and a b))
         (map (lambda (x) (cond-expand-eval cmp? x)) (cdr form))))

      ((cmp? type 'or)
       (reduce
         (lambda (a b) (or a b))
         (map (lambda (x) (cond-expand-eval cmp? x)) (cdr form))))

      ((cmp? type 'not)
       (not (cond-expand-eval cmp? (cdr form))))

      ((cmp? type 'library)
       (syntax-assert-length= form 2 "library specifier clause")

       (table-ref (top-level-value 'module-table) (module-spec->string form (cadr form))))

      ((cmp? type 'else) #t)
      (else (and (memq (car form) features) #t)))))

(define-syntax cond-expand
  (lambda (x c)
    (syntax-assert-length<> x 2)
    (let loop ((clause (cadr x)) (clauses (cddr x)))
      (unless (list? clause)
        (syntax-error x "cond-expand clause must be a list with two elements"))

      (if (cond-expand-eval c (car clause))
        #`(begin ,@(cdr clause))
        (if (null? clauses)
          #f
          (loop (car clauses) (cdr clauses)))))))

;; ONE-SHOT CONTINUATIONS

(define (call/1cc thunk)
  (define tag (gensym))
  (define (trigger-continuation value)
    (raise-continuation tag value))
  (define result #f)
  (try
    (lambda () (thunk (lambda (value) (trigger-continuation value))))
    (lambda (exc)
      (if (and (eq? (exception-tag exc) 'continuation) (eq? (exception-message exc) tag))
        (begin
          (set! result
            (exception-irritants exc))
          #t)
        #f)))
  (set! trigger-continuation
    (lambda (value)
      (raise 'eval "Attempt to invoke spent one-shot continuation" tag)))
  result)

;; We define call/cc and dynamic-wind on the simpler unwind-protect and call/1cc. It will throw an error for more
;; extreme uses of continuations but is sufficient for many of them.

(define call/cc call/1cc)
(define call-with-current-continuation call/1cc)

(define (dynamic-wind before thunk after)
  (before)
  (unwind-protect thunk after))

;; VECTORS

(define (vector-copy vec)
  (define copy (make-vector (vector-length vec)))
  (define limit (vector-length vec))
  (do ((i 0 (fx+ i 1)))
       ((fx>= i limit))
    (begin
      (vector-set! copy i (vector-ref vec i))))
  copy)

(define (vector->list vec)
  (if (fx= (vector-length vec) 0)
    '()
    (let loop ((i (vector-length vec)) (lst '()))
      (if (fx= i 0)
        lst
        (loop (fx- i 1) (cons (vector-ref vec (fx- i 1)) lst))))))

(define (vector-for-each fn vec)
  (define len (vector-length vec))
  (let loop ((i 0))
    (if (fx= i len)
      unspecified
      (begin
        (fn (vector-ref vec i))
        (loop (fx+ i 1))))))

(define (vector-map fn vec)
  (define len (vector-length vec))
  (define vec2 (make-vector len))
  (let loop ((i 0))
    (if (fx= i len)
      vec2
      (begin
        (vector-set! vec2 i (fn (vector-ref vec i)))
        (loop (fx+ i 1))))))

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

;; COMPATIBILITY

(define-syntax er-macro-transformer
  (lambda (x)
    (cadr x)))

;; SRFI 1 / LISTS
(define (fold-left f init seq)
  (if (null? seq)
    init
    (fold-left f (f (car seq) init) (cdr seq))))

(define (fold-right f init seq) 
  (if (null? seq) 
    init 
    (f (car seq) 
      (fold-right f init (cdr seq))))) 

(define (%iota i n)
  (if (fx= i n)
    '()
    (cons i (%iota (fx+ i 1) n))))

(define (iota i)
  (if (fx= i 0)
    '()
    (%iota 0 i)))

;; SRFI 8

(define-syntax receive
  (lambda (x)
    (syntax-assert-length<> x 3)
    
    #`(call-with-values
        (lambda () ,(caddr x))
        (lambda ,(cadr x) ,@(cdddr x)))))

