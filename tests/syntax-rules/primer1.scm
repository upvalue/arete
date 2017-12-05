;; syntax-rules

(display
(let ((a 1))
 (letrec-syntax
	 ((foo (syntax-rules ()
		 ((_ b)
		  (bar a b))))
	  (bar (syntax-rules ()
		 ((_ c d)
		  (cons c (let ((c 3)) (list d c (quote c))))))))

   (let ((a 2))
     ;; what's the issue here? is QUOTE a special case?
     (foo a)
  )
   
   ))
)

(newline)

