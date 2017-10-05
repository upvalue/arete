;; cyclic import test.
(define-library (cyclic1)
  (import (cyclic2))
  (begin))
