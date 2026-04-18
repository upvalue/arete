;;; optional-keyword-expansion.scm -- test that the expander correctly rewrites
;;; #!optional and #!key lambdas into rest-arg + inline destructuring code.

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
;; #!optional tests
;; ---------------------------------------------------------------------------

(print "--- #!optional ---")

(define (opt-test a #!optional b (c . 10))
  (list a b c))

(check "opt: none"    '(x #f 10)  (opt-test 'x))
(check "opt: one"     '(x 1 10)   (opt-test 'x 1))
(check "opt: two"     '(x 1 2)    (opt-test 'x 1 2))
(check-error "opt: too many" 'arity (lambda () (opt-test 'x 1 2 3)))

;; Lazy evaluation of defaults
(define *side-effects* 0)
(define (make-default)
  (set! *side-effects* (fx+ *side-effects* 1))
  42)

(define (lazy-test #!optional (x . (make-default)))
  x)

(set! *side-effects* 0)
(lazy-test 99)
(check "opt lazy: not evaluated when supplied" 0 *side-effects*)

(set! *side-effects* 0)
(check "opt lazy: evaluated when missing" 42 (lazy-test))
(check "opt lazy: exactly once" 1 *side-effects*)

;; #f as a valid supplied value
(define (false-test #!optional (x . 'default))
  x)
(check "opt: #f is valid" #f (false-test #f))
(check "opt: default"     'default (false-test))

;; ---------------------------------------------------------------------------
;; #!optional + #!rest
;; ---------------------------------------------------------------------------

(print "--- #!optional + #!rest ---")

(define (opt-rest-test a #!optional (b . 10) #!rest r)
  (list a b r))

(check "opt+rest: none"       '(x 10 ())     (opt-rest-test 'x))
(check "opt+rest: optional"   '(x 1 ())      (opt-rest-test 'x 1))
(check "opt+rest: opt+rest"   '(x 1 (2 3))   (opt-rest-test 'x 1 2 3))

;; ---------------------------------------------------------------------------
;; #!key tests
;; ---------------------------------------------------------------------------

(print "--- #!key ---")

(define (kw-test host #!key (port . 80) (timeout . 30))
  (list host port timeout))

(check "kw: defaults"    '(localhost 80 30)    (kw-test 'localhost))
(check "kw: one"         '(localhost 8080 30)  (kw-test 'localhost port: 8080))
(check "kw: other"       '(localhost 80 5)     (kw-test 'localhost timeout: 5))
(check "kw: both"        '(localhost 443 10)   (kw-test 'localhost port: 443 timeout: 10))
(check "kw: reversed"    '(localhost 443 10)   (kw-test 'localhost timeout: 10 port: 443))
(check-error "kw: odd"   'arity  (lambda () (kw-test 'localhost port: 42 timeout:)))
(check-error "kw: bad"   'arity  (lambda () (kw-test 'localhost foo: 1)))

;; Lazy keyword defaults
(define (lazy-kw-test #!key (x . (make-default)))
  x)

(set! *side-effects* 0)
(lazy-kw-test x: 99)
(check "kw lazy: not evaluated" 0 *side-effects*)

(set! *side-effects* 0)
(check "kw lazy: evaluated" 42 (lazy-kw-test))
(check "kw lazy: once" 1 *side-effects*)

;; #!key with no default
(define (kw-no-default #!key b)
  b)
(check "kw no-default: absent"  #f (kw-no-default))
(check "kw no-default: present" 42 (kw-no-default b: 42))

;; ---------------------------------------------------------------------------
;; Plain variadic (should still work)
;; ---------------------------------------------------------------------------

(print "--- plain variadic ---")

(define (variadic . args) args)
(check "variadic: none"  '()      (variadic))
(check "variadic: some"  '(1 2 3) (variadic 1 2 3))

(define (variadic2 a b . rest) (list a b rest))
(check "variadic2"  '(1 2 (3 4)) (variadic2 1 2 3 4))

;; ---------------------------------------------------------------------------
;; Plain fixed arity (should still work)
;; ---------------------------------------------------------------------------

(print "--- fixed arity ---")

(define (fixed a b) (list a b))
(check "fixed"  '(1 2) (fixed 1 2))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------

(print "")
(print "Results: " *tests-passed* " passed, " *tests-failed* " failed")
(if (fx> *tests-failed* 0) (raise 'test "test failures" '()))
