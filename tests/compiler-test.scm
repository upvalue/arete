;; compiler-test.scm - run a compiler test from toplevel
;; assumes a toplevel variable compiler-test-file has been defined

(load "scheme/expand.scm")
(load "scheme/syntax.scm")
(load "scheme/compiler.scm")

;((compile-toplevel (slurp-file compiler-test-file)))

(define fn (OpenFn/make (string->symbol (top-level-value 'compiler-test-file))))
(define fn-body (slurp-file (top-level-value 'compiler-test-file)))

(compile fn fn-body)
(compile-finish fn)

(define proc (OpenFn->procedure fn))

(print (proc))
;(pretty-print (proc))
