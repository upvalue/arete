;; check that defines are disallowed in some contexts

(if (define x #t)
  #t #f)
