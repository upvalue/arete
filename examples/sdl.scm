;; sdl.scm - basic SDL example

(define-syntax while
  (lambda (x)
    (unless (> (length x) 2)
      (raise 'syntax "while loop expected at least two arguments: condition and body" (list x)))
    `(,#'let ,#'loop ((,#'condition ,(cadr x)))
        (,#'when ,#'condition
          (,#'begin ,(cddr body))
          (,#'loop)))))




(sdl:init)

(define event (sdl:make-event))
(define done #f)

(let loop ()
  (when (sdl:poll-event event)
    (case (sdl:event-type event)
      ((quit) (set! done #t))))

  (unless done
    (sdl:delay 10)
    (loop)))

(print "quit normally.")

(sdl:quit)
