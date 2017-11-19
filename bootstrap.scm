;; bootstrap.scm - Compiles all interpreted functions, then does a timed full expand and compile of the system 

(load "scheme/expand.scm")
(load "scheme/syntax.scm")
(load "scheme/compiler.scm")

(pull-up-bootstraps)

