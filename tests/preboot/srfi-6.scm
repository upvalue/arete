(define thing (open-output-string))

(write "asdf" thing)

(display (get-output-string thing))
(newline)

(close-output-port thing)

