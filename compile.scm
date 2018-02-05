;; compile.scm - full compile under the VM

(define (compile-file f)
  (define body (slurp-file f))

  (define expanded (expand-toplevel (cons 'begin body) (top-level-value '*core-module*)))
  (compile-toplevel expanded)

  #t)


(time-function "full expand and compile on VM"
  (lambda ()
    (for-each1 compile-file '("scheme/expand.scm" "scheme/syntax.scm" "scheme/types.scm" "scheme/compiler.scm"
                              "scheme/library.scm"))
))

