;; bootstrap-and-compile.scm
;; bootstrap, then run a full expand and compile on the source

(load "bootstrap.scm")

(time-function "full expand & compile on VM"
  (lambda ()
    (let ((expander-src (slurp-file "scheme/syntax.scm"))
          (compiler-src (slurp-file "scheme/compiler.scm")))

      (compile-toplevel (expand-toplevel expander-src #f))
      (compile-toplevel (expand-toplevel compiler-src #f)))))
