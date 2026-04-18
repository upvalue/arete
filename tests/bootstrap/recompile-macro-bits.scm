(set-top-level-value! '*current-module* (top-level-value '*core-module*))

(define-syntax idm
  (identifier-transformer
    (lambda (x) #t)))

(define-syntax combo
  (combined-transformer
    (lambda (x r c) #t)))

(pull-up-bootstraps)

(define ridm (table-ref (top-level-value '*core-module*) 'idm))
(define rcombo (table-ref (top-level-value '*core-module*) 'combo))

(print (function-procedural-macro? ridm))
(print (function-identifier-macro? ridm))
(print (function-procedural-macro? rcombo))
(print (function-identifier-macro? rcombo))
