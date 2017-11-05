;; sdl.scm - basic SDL example

(sdl:init)

(define event (sdl:make-event))
(define done #f)

(while (not done)
  (when (sdl:poll-event event)
    (case (sdl:event-type event)
      ((quit) (set! done #t))))

  (sdl:delay 10))

(print "Quit normally.")

(sdl:quit)
