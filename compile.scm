;; compile.scm - full compile under the VM

(define (compile-file f)
  (define body (slurp-file f))

  (define expanded (expand-toplevel (cons 'begin body) (top-level-value '*core-module*)))
  (compile-toplevel expanded)

  #t)


(time-function "full expand and compile on VM"
  (lambda ()
    (compile-file "scheme/expand.scm")
    (compile-file "scheme/syntax.scm")
    (compile-file "scheme/types.scm")
    (compile-file "scheme/compiler.scm")
    (compile-file "scheme/library.scm")
    ;(for-each1 compile-file '("scheme/expand.scm" "scheme/syntax.scm"))
))

