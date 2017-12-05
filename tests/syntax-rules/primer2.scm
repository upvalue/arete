;; syntax-rules primer for the mildly insane, examples #2

(let ((x 1))
  (let-syntax
      ((foo (syntax-rules ()
              ((_ y) (let-syntax
                         ((bar (syntax-rules ()
                                 ((_ "hello" x) x))))
                       (bar "hello" 2))))))
    (display (foo x))
    #t))
(newline)


