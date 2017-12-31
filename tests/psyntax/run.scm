(set-top-level-value!'COMPILER-WARN-UNDEFINED #f)
(##arete#load "tests/psyntax/load.scm")

(##arete#print (##arete#map (##user#sc-expand (##arete#slurp-file "tests/psyntax/psyntax.ss"))))

