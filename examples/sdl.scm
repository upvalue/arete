;; sdl.scm - minimal sdl example

(sdl:init 640 480)

(define event (sdl:make-event))
(define done #f)

(while (not done)
  (if (sdl:poll-event event)
    (case (sdl:event-type event)
      ((key-down) (when (eqv? (sdl:event-key event) #\q) (set! done #t)))
      ((quit) (set! done #t))))

  (sdl:clear)
  (sdl:set-color 0 0 0)
  (sdl:fill-rect 0 0 640 480)
  (sdl:render)
  (sdl:delay 10))

(sdl:quit)
