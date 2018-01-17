;; bootstrap.scm - Compiles all interpreted functions, then does a timed full expand and compile of the system 

(load "scheme/expand.scm")

;; We have to use fully qualified names here, because this code will not be expanded
(##arete#set-top-level-value! '*current-module* (##arete#top-level-value '*core-module*))
(load "scheme/syntax.scm")
(load "scheme/types.scm")
(load "scheme/compiler.scm")
(load "scheme/library.scm")

(pull-up-bootstraps)


