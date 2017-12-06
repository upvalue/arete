;; srfi-46/r7rs - allow specifying the ellipses
(display
  (let-syntax
    ((hello 
      (syntax-rules ::: ()
        ((hello ... args :::)
         (args ::: ...)))))
    (hello 3 - 5))
  )
(newline)
