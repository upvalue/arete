;; bootstrap.scm - Compiles all interpreted functions, then does a timed full expand and compile of the system 

(load "scheme/syntax.scm")
(load "scheme/compiler.scm")

;(set-top-level-value! 'compiler compile-toplevel)

(pull-up-bootstraps)

(time-function "full expand & compile on VM"
  (lambda ()
    (let ((expander (slurp-file "scheme/syntax.scm"))
          (compiler (slurp-file "scheme/compiler.scm")))
      (compile-toplevel (expand expander #f))
      (compile-toplevel (expand compiler #f))
    )))

