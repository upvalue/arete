;; bootstrap-and-compile.scm
;; bootstrap, then run a full expand and compile on the source

(load "bootstrap.scm")

(time-function "full expand & compile on VM"
  (lambda ()
    (define (compile-file file)
      (compile-toplevel (expand-toplevel (slurp-file file) (top-level-value '*core-module*))))

    (compile-file "expand.scm")
    (compile-file "syntax.scm")
    (compile-file "scheme/compiler.scm")
))
