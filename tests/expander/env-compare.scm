(print (env-compare #f 5 5))
(print (env-compare #f 'asdf 'asdf))
(print (env-compare #(#f #f asdf variable) 'asdf (make-rename #f 'asdf)))

