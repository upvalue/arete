;; Load psyntax, and do a full expand of it

(load "boot.scm")
(##arete#set-top-level-value! '*current-module* (##arete#top-level-value '*core-module*))
(##arete#load "scheme/syntax.scm")
(##arete#load "scheme/compiler.scm")

(pull-up-bootstraps)

(##arete#set-top-level-value! 'COMPILER-WARN-UNDEFINED #f)
(##arete#load "tests/psyntax/load.scm")

(##arete#print (##arete#map ##user#sc-expand (slurp-file "tests/psyntax/psyntax.ss")))

