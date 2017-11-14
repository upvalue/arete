(print 1 (equal? 2 2)) ; #t
(print 2 (equal? 2 3)) ; #f
(print 3 (equal? 'a 'a)) ; #t
(print 4 (equal? 'a 'b)) ; #f
(print 5 (equal? '(a b c) '(a b c))) ; #t
(print 6 (equal? '(a b . c) '(a b . c))) ;#t
(print 7 (equal? '(a b) '(a b c))) ;#f
(print 8 (equal? #(1 2 a b c) #(1 2 a b c))) ; #t
(print 9 (equal? #(a b #(c d)) #(a b #(c d)))) ; #t
(print 10 (equal? #(a b #(c d)) #(a b (c e)))) ;#f
(print 11 (equal? "string1" "string1")) ;; #t
(print 12 (equal? "string1" "string2")) ;; #f
(print 13 (equal? #\a #\a)) ; #t
