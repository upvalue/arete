(print (equal? 2 2)) ; #t
(print (equal? 2 3)) ; #f
(print (equal? 'a 'a)) ; #t
(print (equal? 'a 'b)) ; #f
(print (equal? '(a b c) '(a b c))) ; #t
(print (equal? '(a b . c) '(a b . c))) ;#t
(print (equal? '(a b) '(a b c))) ;#f
(print (equal? #(1 2 a b c) #(1 2 a b c))) ; #t
(print (equal? #(a b #(c d)) #(a b #(c d)))) ; #t
(print (equal? #(a b #(c d)) #(a b (c e)))) ;#f
(print (equal? "string1" "string1"))
(print (equal? "string1" "string2"))
