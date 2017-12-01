;; compiler-test.scm - run a compiler test from toplevel
;; assumes a toplevel variable compiler-test-file has been defined, and core has been loaded

(define fn (OpenFn/make (string->symbol (top-level-value 'compiler-test-file))))
(define fn-body (slurp-file (top-level-value 'compiler-test-file)))

;(set-top-level-value! 'COMPILER-LOG #t)
(define proc (compile-toplevel (cons 'begin fn-body)))

(print (proc))
