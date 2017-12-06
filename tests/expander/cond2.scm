;; test arrow variation and returning of conditions without body

(print (cond ((eq? #t #t))))

(cond ((eq? #t #t) => print))

