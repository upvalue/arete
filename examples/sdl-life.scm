;; sdl.scm - basic SDL example

(import (prefix (sdl) sdl:))

;; PART 1: Basic game of life functionality

(define (make-grid x y)
  (define vec (make-vector y #f))
  (let loop ((i 0))
    (if (= i y)
      vec
      (begin
        (vector-set! vec i (make-vector x 0))
        (loop (+ i 1))))))

(define grid (make-grid 8 8))

(define (print-grid g)
  (let loop ((i 0))
    (unless (= i (vector-length g))
      (display "(")
      (let loop2 ((sg (vector-ref g i))
                  (j 0))
        (unless (= j (vector-length sg))
          (display
            (if (eq? (vector-ref sg j) 1) "#" (if (= j (- (vector-length sg) 1)) "-" "-")))
          (unless (= j (- (vector-length sg) 1))
            (display " "))
          (loop2 sg (+ j 1))))
      (display ")")
      (newline)
      (loop (+ i 1))      
      )))

(define (get-cell g x y)
  ;(print x y)
  (cond 
    ((< x 0) 0)
    ((< y 0) 0)
    ((>= x (vector-length (vector-ref g 0))) 0)
    ((>= y (vector-length g)) 0)
    (else (vector-ref (vector-ref g y) x))))

(define (set-cell! g x y n)
  (vector-set! (vector-ref g y) x n))

(define (set-cell! g x y n)
  (vector-set! (vector-ref g y) x n))

(define (next-cell g value x y)
  (let ((n (+ (get-cell g (- x 1) (- y 1))
              (get-cell g (- x 1) y)
              (get-cell g (- x 1) (+ y 1))
              (get-cell g x (- y 1))
              (get-cell g x (+ y 1))
              (get-cell g (+ x 1) (- y 1))
              (get-cell g (+ x 1) y)
              (get-cell g (+ x 1) (+ y 1)))))
    (cond 
      ;; live cell with two or three living cells becomes dead 
      ((and (= value 1) (or (= n 2) (= n 3))) 1)
      ;; dead cell with three live cells becomes live
      ((and (= value 0) (= n 3)) 1)
      ;; everything else is dead
      (else 0))))

(define (next-grid g)
  (let* ((y (vector-length g))
         (x (vector-length (vector-ref g 0)))
         (new-grid (make-grid x y)))
    (let loop ((xi 0)
               (yi 0))
      (unless (= yi y)
        (set-cell! new-grid xi yi (next-cell g (get-cell g xi yi) xi yi))
        (loop
          (if (= (+ xi 1) x) 0 (+ xi 1))
          (if (= (+ xi 1) x) (+ yi 1) yi))))
    new-grid))


(define (grid-width g) (vector-length (vector-ref g 0)))
(define (grid-height g) (vector-length g))

;;

(define *iterate* #f)

(set-cell! grid 2 0 1)
(set-cell! grid 3 1 1)
(set-cell! grid 1 2 1)
(set-cell! grid 2 2 1)
(set-cell! grid 3 2 1)

;; PART 2: STATE MANAGEMENT

(define-record State grid-size overlay-visible grid
               entering-number entering-number-field
               error-message turn)

(define state (State/make (grid-width grid) #t grid #f #f #f 0))
(define state-dirty #t)

(define (state/toggle-overlay!)
  (State/overlay-visible! state (not (State/overlay-visible state) ))
  (set! state-dirty #t))

(define (state/next-turn!)
  (define grid (next-grid (State/grid state)))
  (State/grid! state grid)
  (State/turn! state (+ (State/turn state) 1))
  (set! state-dirty #t))

(define (State/error! msg)
  (State/error-message! state msg)
  (set! state-dirty #t))

(define (State/begin-entering!)
  (State/entering-number! state 0)
  (set! state-dirty #t))

(define (State/finish-entering!)
  (let ((n (State/entering-number state)))
    (State/grid-size! state n)
    (State/grid! state (make-grid n n))
    (State/entering-number! state #f)
    (set! state-dirty #t)))

(define (State/append-number! n)
  (let ((n2 (* (State/entering-number state) 10)))
    (State/entering-number! state (+ n2 n)))
  (set! state-dirty #t))

(define (State/backspace-number!)
  (if (eq? (State/entering-number state) 0)
    (State/entering-number! state #f)
    (let ((n2 (floor (/ (State/entering-number state) 10))))
        (State/entering-number! state n2)))
  (set! state-dirty #t))

;; PART 3 RENDERING

(define *width* 640)
(define *height* 480)

(define *grid-display-height* (- *height* 100))

(define *tile-width* (/ *width* (grid-width grid)))
;(define *tile-height* (/ *grid-display-height* (grid-height grid)))
(define *tile-height* (/ *height* (grid-height grid)))

(define timer (sdl:add-timer 'tick 500))

(sdl:init *width* *height*)

(define event (sdl:make-event))
(define done #f)
(define autorun #f)

(define font (sdl:open-font "examples/assets/DroidSans.ttf" 12))

(define (render-grid grid x y width height grid-width grid-height)
  ;; find closest square tile height
  (let* ((tile-height (/ width grid-width))
         (tile-width (/ height grid-height))
         (tile-dimension (min (floor tile-height) (floor tile-width))))
    (print "tile-dimension" tile-width "x" tile-height)
    (sdl:set-color 255 255 255)
    ;; draw border
    ;(sdl:draw-rect x y width height)
    (let loop ((xi 0) (yi 0))
      (unless (= yi grid-height)
        (if (= xi grid-width)
          (loop 0 (+ yi 1))
          (begin
            (sdl:set-color 255 255 255)
            (sdl:draw-rect (+ (* xi tile-dimension) x) (+ (* yi tile-dimension) y) tile-dimension tile-dimension)
            (when (= (get-cell (State/grid state) xi yi) 1)
              (sdl:set-color 255 180 0)
              (print "fill-rect" (+ (* xi tile-dimension) x 1) (+ (* yi tile-dimension) y 1) (- tile-dimension 1) (- tile-dimension 1))
              (sdl:fill-rect (+ (* xi tile-dimension) x 1) (+ (* yi tile-dimension) y 1) (- tile-dimension 1) (- tile-dimension 1)))
            (loop (+ xi 1) yi)))))
    (print "drawing grid with tile dimension" tile-dimension " x " tile-dimension)
  #t))

(define (click-grid grid x y width height grid-width grid-height mouse-x mouse-y)
  (let* ((mx (- mouse-x x))
         (my (- mouse-y y))
         (tile-height (/ height grid-height))
         (tile-width (/ width  grid-width))
         (tile-dimension (min (floor tile-height) (floor tile-width))))
    (when (and (> mx 0) (> my 0) (< mx width) (< my height))
      (let ((x-tile (/ mx tile-dimension))
            (y-tile (/ my tile-dimension)))
        (when (and (< x-tile grid-width) (< y-tile grid-height))
          (set-cell! (State/grid state) x-tile y-tile (if (= (get-cell (State/grid state) x-tile y-tile) 0) 1 0))
          (set! state-dirty #t))))))


(sdl:clear)
(sdl:render)

(while (not done)
  (when (sdl:poll-event event)
    (case (sdl:event-type event)
      ((quit) (set! done #t))

      ((mouse-down) 
       
       (click-grid #f 5 5 (- *width* 10) (- *height* 40) (State/grid-size state) (State/grid-size state)
                   (sdl:event-mouse-x event) (sdl:event-mouse-y event))
       (print (sdl:event-mouse-x event)  (sdl:event-mouse-y event))
       (print "mouse click"))

      ((timer)
       (when autorun
         (state/next-turn!)))

      ((key-down)
       (State/error-message! state #f)
       (if (State/entering-number state)
         ;; entering a number
         (let ((key (sdl:event-key event)))
           (cond 
             ((and (char? key) (char-numeric? key))
              (State/append-number! (- (char->integer key) (char->integer #\0))))
             ((eq? key 'return)
              (State/finish-entering!))
             ((eq? key 'backspace)
              (State/backspace-number!))
             (else
               (State/error! "Enter a number from 0-9 or press ENTER to finish"))))
         ;; normal input
         (case (sdl:event-key event)
           ((#\d) (set! state-dirty #t))
           ((#\s) (State/begin-entering!))
           ((#\r) (repl))
           ((#\a) (set! autorun (not autorun)))
           ((#\c) (begin
                    (State/turn! state 0)
                    (State/grid! state (make-grid (State/grid-size state) (State/grid-size state)))
                    (set! state-dirty #t)))

           ((#\i)
            (unless autorun
              (state/next-turn!))
            
            )
           (else => (lambda (k) (print k))))))
    )
  )


  (sdl:delay 10)
  ;(sdl:clear)

  (when state-dirty

    (print "rendering stuff")
    (sdl:set-color 0 0 0)
    (sdl:clear)

    (render-grid #f 5 5 (- *width* 10) (- *height* 40) (State/grid-size state) (State/grid-size state))

    (if (State/error-message state)
      (begin
        (sdl:set-color 255 0 0)
        (sdl:draw-text font (State/error-message state) 10 (- *height* 40)))
      (if (State/entering-number state)
        (begin
          (sdl:set-color 255 255 255)
          (sdl:draw-text font (print-string "size:" (State/entering-number state)) 10 (- *height* 40)))))

    (sdl:set-color 255 255 255)
    (sdl:draw-text font (print-string "turn:" (State/turn state) "controls: (c) clear (i) iterate one step (a) auto-iterate (mouse-down): flip cell state") 10 (- *height* 20))
    ;; Draw grid

    (sdl:render)
    (set! state-dirty #f))
)

(print "Quit normally.")

(sdl:remove-timer timer)
(sdl:close-font font)

(sdl:quit)
