;; sdl.scm - basic SDL example
(define *width* 640)
(define *height* 480)

(sdl:init *width* *height*)

(define event (sdl:make-event))
(define done #f)

(define font (sdl:open-font "examples/assets/DroidSans.ttf" 12))

(while (not done)
  (when (sdl:poll-event event)
    (case (sdl:event-type event)
      ((quit) (set! done #t))))

  (sdl:clear)
  (sdl:fill-rect 0 0 640 480 0 0 0)
  (sdl:draw-text font "hello world")
  (sdl:render)
)


(print "Quit normally.")

(sdl:close-font font)

(sdl:quit)
