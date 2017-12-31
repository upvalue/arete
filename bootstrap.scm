;; bootstrap.scm - Compiles all interpreted functions, then does a timed full expand and compile of the system 

(load "scheme/expand.scm")

;; We have to use fully qualified names here, because this code will not be expanded
(##arete#set-top-level-value! '*current-module* (##arete#top-level-value '*core-module*))
(##arete#load "scheme/syntax.scm")
(##arete#load "scheme/compiler.scm")

(pull-up-bootstraps)

