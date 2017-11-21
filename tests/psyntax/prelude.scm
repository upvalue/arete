(define (void) (if #f #f))


(define andmap
 (lambda (f first . rest)
   (or (null? first)
       (if (null? rest)
           (let andmap ((first first))
             (let ((x (car first)) (first (cdr first)))
               (if (null? first)
                   (f x)
                   (and (f x) (andmap first)))))
           (let andmap ((first first) (rest rest))
             (let ((x (car first))
                   (xr (map car rest))
                   (first (cdr first))
                   (rest (map cdr rest)))
               (if (null? first)
                   (apply f (cons x xr))
                   (and (apply f (cons x xr)) (andmap first rest)))))))))

 (define ormap
   (lambda (proc list1)
     (and (not (null? list1))
          (or (proc (car list1)) (ormap proc (cdr list1))))))

(define props (make-table))
(define (symbol-props symbol)
  (aif (table-ref props symbol)
    it
    (let ((table (make-table)))
      (table-set! props symbol table)
      table)))

(define (getprop symbol key)
  (table-ref (symbol-props symbol) key))

(define (putprop symbol key value)
  (table-set! (symbol-props symbol) key value))

(define (remprop symbol key)
  (table-delete! (symbol-props symbol) key))


(define $sc-put-cte #f)
(define sc-expand #f)
(define $make-environment #f)
(define environment? #f)
(define interaction-environment #f)
(define identifier? #f)
(define syntax->list #f)
(define syntax-object->datum #f)
(define datum->syntax-object #f)
(define generate-temporaries #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define literal-identifier=? #f)
(define syntax-error #f)
(define $syntax-dispatch #f)
(define syntax->vector #f)
