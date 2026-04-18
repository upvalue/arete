;;; optional-keyword-helpers.scm -- prototype helpers for parsing optional/keyword args
;;;
;;; These helpers will eventually live as ##arete#... qualified builtins.
;;; The expander will rewrite lambdas with #!optional/#!key parameters into
;;; plain (lambda (required . rest) ...) forms that use these helpers plus
;;; inline code to destructure the rest list.
;;;
;;; Key design constraint: default expressions must be lazily evaluated.
;;; Only evaluate a default when the argument was not supplied.

;; ---------------------------------------------------------------------------
;; Test harness
;; ---------------------------------------------------------------------------

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (check name expected actual)
  (if (equal? expected actual)
    (begin
      (set! *tests-passed* (fx+ *tests-passed* 1))
      (print "  PASS " name))
    (begin
      (set! *tests-failed* (fx+ *tests-failed* 1))
      (print "  FAIL " name)
      (print "    expected: " expected)
      (print "    actual:   " actual))))

(define (check-error name tag-expected thunk)
  (define caught #f)
  (try
    (lambda () (thunk))
    (lambda (exn)
      (set! caught exn)
      #t))
  (if caught
    (let ((tag (exception-tag caught))
          (msg (exception-message caught)))
      (if (eq? tag tag-expected)
        (begin
          (set! *tests-passed* (fx+ *tests-passed* 1))
          (print "  PASS " name " (error: " msg ")"))
        (begin
          (set! *tests-failed* (fx+ *tests-failed* 1))
          (print "  FAIL " name)
          (print "    expected tag: " tag-expected)
          (print "    actual tag:   " tag)
          (print "    message:      " msg))))
    (begin
      (set! *tests-failed* (fx+ *tests-failed* 1))
      (print "  FAIL " name " (no error raised)"))))

;; ---------------------------------------------------------------------------
;; Helpers for optional arguments
;; ---------------------------------------------------------------------------

;; No helper function needed for optionals -- the expander will emit inline
;; code using pair?/car/cdr. But we do need a helper to check for too many
;; arguments (used when there's no #!rest parameter to absorb the tail).

;; (##arete#check-arity rest-list max-optional fn-name)
;; Called after all optionals have been consumed from rest.
;; If rest is non-empty, the caller passed too many args.
(define (check-arity rest max-optional fn-name)
  (if (pair? rest)
    (raise 'arity
           (string-append fn-name ": too many arguments; expected at most "
                          (number->string max-optional)
                          " optional argument"
                          (if (fx= max-optional 1) "" "s")
                          " but got extra")
           (list rest))))

;; ---------------------------------------------------------------------------
;; Helpers for keyword arguments
;; ---------------------------------------------------------------------------

;; Sentinel value: used to detect "keyword not supplied" so we can lazily
;; evaluate the default expression only when needed.
(define absent (gensym 'absent))

;; (check-keywords rest valid-keywords fn-name)
;; Validates that rest is a well-formed keyword argument list:
;;   - even length (every keyword has a value)
;;   - keyword? in every even position
;;   - only keywords from valid-keywords appear
(define (check-keywords rest valid-keywords fn-name)
  (define (keywords-string kws)
    (let loop ((ks kws) (acc ""))
      (if (null? ks)
        acc
        (loop (cdr ks)
              (string-append acc (if (string=? acc "") "" ", ")
                             (symbol->string (car ks)))))))
  (let check ((r rest))
    (cond
      ((null? r) #t)
      ((null? (cdr r))
       (raise 'arity
              (string-append fn-name ": keyword argument list has odd length; "
                             "missing value after " (symbol->string (car r)))
              (list rest)))
      ((not (keyword? (car r)))
       (raise 'type
              (string-append fn-name ": expected a keyword argument but got "
                             (symbol->string (car r))
                             "; valid keywords are "
                             (keywords-string valid-keywords))
              (list (car r) rest)))
      ((not (memq (car r) valid-keywords))
       (raise 'arity
              (string-append fn-name ": unknown keyword " (symbol->string (car r))
                             "; valid keywords are "
                             (keywords-string valid-keywords))
              (list (car r) rest)))
      (else (check (cddr r))))))

;; (keyword-lookup rest keyword) -> value or absent
;; Pure lookup, no validation (check-keywords already ran).
(define (keyword-lookup rest keyword)
  (cond
    ((null? rest) absent)
    ((eq? (car rest) keyword) (cadr rest))
    (else (keyword-lookup (cddr rest) keyword))))

;; ---------------------------------------------------------------------------
;; Tests: inline optional argument pattern
;; ---------------------------------------------------------------------------
;; The expander would generate code like this for:
;;   (lambda (a #!optional b (c 10)) body)
;; =>
;;   (lambda (a . %rest)
;;     (define b (if (pair? %rest) (car %rest) #f))
;;     (set! %rest (if (pair? %rest) (cdr %rest) '()))
;;     (define c (if (pair? %rest) (car %rest) <default-expr>))
;;     (set! %rest (if (pair? %rest) (cdr %rest) '()))
;;     (check-arity %rest 2 "fn-name")  ;; only if no #!rest
;;     body)

(print "--- inline optional pattern ---")

;; Simulate: (lambda (a #!optional b (c 10)) (list a b c))
(define (test-opt a . %rest)
  (define b (if (pair? %rest) (car %rest) #f))
  (set! %rest (if (pair? %rest) (cdr %rest) '()))
  (define c (if (pair? %rest) (car %rest) 10))
  (set! %rest (if (pair? %rest) (cdr %rest) '()))
  (check-arity %rest 2 "test-opt")
  (list a b c))

(check "opt: none supplied"    '(x #f 10)  (test-opt 'x))
(check "opt: first supplied"   '(x 1 10)   (test-opt 'x 1))
(check "opt: both supplied"    '(x 1 2)    (test-opt 'x 1 2))
(check-error "opt: too many"   'arity      (lambda () (test-opt 'x 1 2 3)))

;; Test lazy evaluation of defaults
(define *side-effect-count* 0)
(define (expensive-default)
  (set! *side-effect-count* (fx+ *side-effect-count* 1))
  42)

;; Simulate: (lambda (#!optional (x (expensive-default))) x)
(define (test-lazy . %rest)
  (define x (if (pair? %rest) (car %rest) (expensive-default)))
  (set! %rest (if (pair? %rest) (cdr %rest) '()))
  (check-arity %rest 1 "test-lazy")
  x)

(set! *side-effect-count* 0)
(test-lazy 99)
(check "lazy: default not evaluated when arg supplied" 0 *side-effect-count*)

(set! *side-effect-count* 0)
(check "lazy: default evaluated when arg missing" 42 (test-lazy))
(check "lazy: default evaluated exactly once" 1 *side-effect-count*)

;; Test #f as a legitimate supplied value (not confused with default)
;; Simulate: (lambda (#!optional (x 'default)) x)
(define (test-false-value . %rest)
  (define x (if (pair? %rest) (car %rest) 'default))
  (set! %rest (if (pair? %rest) (cdr %rest) '()))
  x)

(check "opt: #f is a valid supplied value" #f (test-false-value #f))
(check "opt: default when not supplied"    'default (test-false-value))

;; ---------------------------------------------------------------------------
;; Tests: optional + rest
;; ---------------------------------------------------------------------------
;; (lambda (a #!optional (b 10) #!rest r) (list a b r))
;; => no check-arity needed since rest absorbs the tail

(print "--- optional + rest ---")

(define (test-opt-rest a . %rest)
  (define b (if (pair? %rest) (car %rest) 10))
  (set! %rest (if (pair? %rest) (cdr %rest) '()))
  ;; remaining %rest IS the rest argument
  (define r %rest)
  (list a b r))

(check "opt+rest: none"          '(x 10 ())      (test-opt-rest 'x))
(check "opt+rest: optional"      '(x 1 ())       (test-opt-rest 'x 1))
(check "opt+rest: optional+rest" '(x 1 (2 3))    (test-opt-rest 'x 1 2 3))

;; ---------------------------------------------------------------------------
;; Tests: keyword arguments with lazy defaults
;; ---------------------------------------------------------------------------
;; The expander would generate code like this for:
;;   (lambda (a #!key (b 10) (c 20)) body)
;; =>
;;   (lambda (a . %rest)
;;     (check-keywords %rest '(b: c:) "fn-name")
;;     (define b (keyword-lookup %rest b:))
;;     (if (eq? b absent) (set! b <default-expr>))
;;     (define c (keyword-lookup %rest c:))
;;     (if (eq? c absent) (set! c <default-expr>))
;;     body)

(print "--- keyword arguments (lazy) ---")

;; Simulate: (lambda (host #!key (port 80) (timeout 30)) (list host port timeout))
(define (connect host . %rest)
  (check-keywords %rest '(port: timeout:) "connect")
  (define port (keyword-lookup %rest port:))
  (if (eq? port absent) (set! port 80))
  (define timeout (keyword-lookup %rest timeout:))
  (if (eq? timeout absent) (set! timeout 30))
  (list host port timeout))

(check "kw: defaults"         '(localhost 80 30)    (connect 'localhost))
(check "kw: one keyword"      '(localhost 8080 30)  (connect 'localhost port: 8080))
(check "kw: other keyword"    '(localhost 80 5)     (connect 'localhost timeout: 5))
(check "kw: both keywords"    '(localhost 443 10)   (connect 'localhost port: 443 timeout: 10))
(check "kw: reversed order"   '(localhost 443 10)   (connect 'localhost timeout: 10 port: 443))
(check-error "kw: odd list"   'arity  (lambda () (connect 'localhost port: 42 timeout:)))
(check-error "kw: non-keyword" 'type  (lambda () (connect 'localhost 'notakeyword 42)))
(check-error "kw: unknown kw" 'arity  (lambda () (connect 'localhost foo: 1)))

;; Test lazy keyword defaults
(set! *side-effect-count* 0)

(define (test-lazy-kw . %rest)
  (check-keywords %rest '(x:) "test-lazy-kw")
  (define x (keyword-lookup %rest x:))
  (if (eq? x absent) (set! x (expensive-default)))
  x)

(set! *side-effect-count* 0)
(test-lazy-kw x: 99)
(check "kw lazy: default not evaluated when supplied" 0 *side-effect-count*)

(set! *side-effect-count* 0)
(check "kw lazy: default evaluated when missing" 42 (test-lazy-kw))
(check "kw lazy: evaluated exactly once" 1 *side-effect-count*)

;; Test #f as a legitimate keyword value
(define (test-kw-false . %rest)
  (check-keywords %rest '(x:) "test-kw-false")
  (define x (keyword-lookup %rest x:))
  (if (eq? x absent) (set! x 'default))
  x)

(check "kw: #f is a valid value"       #f       (test-kw-false x: #f))
(check "kw: default when not supplied"  'default (test-kw-false))

;; ---------------------------------------------------------------------------
;; Tests: keyword with no default (default is #f)
;; ---------------------------------------------------------------------------

(print "--- keyword no default ---")

;; Simulate: (lambda (#!key b) ...)
;; b has no default, so it defaults to #f
(define (test-kw-no-default . %rest)
  (check-keywords %rest '(b:) "test-kw-no-default")
  (define b (keyword-lookup %rest b:))
  (if (eq? b absent) (set! b #f))
  b)

(check "kw no-default: not supplied" #f  (test-kw-no-default))
(check "kw no-default: supplied"     42  (test-kw-no-default b: 42))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------

(print "")
(print "Results: " *tests-passed* " passed, " *tests-failed* " failed")
(if (fx> *tests-failed* 0) (raise 'test "test failures" '()))
