;; primer for the mildly insane, problem #3

(display
  (let ((xec 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                           ((bar (syntax-rules ()
                                   ((_ "hello" x) y))))
                         (bar "hello" 2))))))
      (foo xec)
      )))
(newline)
