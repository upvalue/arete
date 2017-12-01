;; bootstrap-and-compile.scm
;; bootstrap, then run a full expand and compile on the source

(time-function "full expand & compile on VM"
  (lambda ()
    (define (compile-file file) (compile-toplevel (expand-toplevel (slurp-file file) (top-level-value '*core-module*))))

    (define install-expander (compile-file "scheme/expand.scm"))
    (compile-file "scheme/syntax.scm")
    (compile-file "scheme/compiler.scm")
))
