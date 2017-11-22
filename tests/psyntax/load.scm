;(set-top-level-value! '*module-paths* (append (top-level-value '*module-paths*) "./tests/psyntax"))

(load "./tests/psyntax/prelude.scm")
(load "./tests/psyntax/psyntax.pp")
