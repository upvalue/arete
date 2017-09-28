(print (env-compare #f 5 5))
(print (env-compare #f 'asdf 'asdf))
(print (env-compare #(#f asdf variable) 'asdf (make-rename 'asdf #f)))

