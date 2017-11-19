;; sdl.scm - minimal sdl example that runs on the interpreter

(import (prefix (sdl) sdl:))

(sdl:init 640 480)

(define event (sdl:make-event))
(define done #f)

(define (loop)
  (if (sdl:poll-event event)
    (if (eq? (sdl:event-type event) 'key-down)
      (if (eqv? (sdl:event-key event) #\q)
        (set! done #t))
      (if (eq? (sdl:event-type event) 'quit)
        (set! done #t))))

  (sdl:clear)
  (sdl:set-color 0 0 0)
  (sdl:fill-rect 0 0 640 480)
  (sdl:render)
  (sdl:delay 10)

  (if (eq? done #f) (loop)))

(loop)

(print "done!")

(sdl:quit)
