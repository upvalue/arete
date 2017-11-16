;; compiler-test.scm - run a compiler test from toplevel
;; assumes compiler-test-file has been defined

(load "scheme/syntax.scm")
(load "scheme/compiler.scm")

;((compile-toplevel (slurp-file compiler-test-file)))

(define fn (OpenFn/make (string->symbol compiler-test-file)))
(define fn-body (slurp-file compiler-test-file))

(compile fn fn-body)
(compile-finish fn)

(define proc (OpenFn->procedure fn))

(print (proc))
;(pretty-print (proc))
