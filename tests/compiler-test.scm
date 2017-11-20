;; compiler-test.scm - run a compiler test from toplevel
;; assumes a toplevel variable compiler-test-file has been defined, and core has been loaded

(define fn (OpenFn/make (string->symbol (top-level-value 'compiler-test-file))))
(define fn-body (slurp-file (top-level-value 'compiler-test-file)))

(compile fn fn-body)
(compile-finish fn)

(define proc (OpenFn->procedure fn))

(print (proc))
