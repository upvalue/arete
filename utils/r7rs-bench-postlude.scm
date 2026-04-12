;; r7rs-bench-postlude.scm - appended after src/<bench>.scm + src/common.scm
;; but before src/common-postlude.scm. common.scm references
;; this-scheme-implementation-name, so we supply it here.

(define (this-scheme-implementation-name) "arete")
