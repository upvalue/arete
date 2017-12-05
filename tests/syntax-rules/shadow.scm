;; shadowing some built-in syntax
(display
  (let ((name 'incorrect))
    (let-syntax
      ((set! (syntax-rules (return)
               ((_ return value) value)
               ((_ name value) (set! name value)))))

      (set! name 'correct)
      (cons (set! return 'correct) name)))
  )
(newline)
