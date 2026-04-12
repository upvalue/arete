;; Load psyntax, and do a full expand of it

(load "boot.scm")
(begin
  (set-top-level-value! 'COMPILER-WARN-UNDEFINED #f)
  (load "tests/psyntax/load.scm")
  (print (map sc-expand (slurp-file "tests/psyntax/psyntax.ss"))))
