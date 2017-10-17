;; case.scm - case macro

(print
  (case 5 
    ((2 4 6 8) 'even)
    ((1 3 5 7 9) 'odd)))

(print
  (case 2
    ((1 3 5 7 9) 'odd)
    (else 'even)))

;; compare direct value
(print 
  (case 5
    (3 3)
    (4 4)
    (5 'success)
    (else 6)))
