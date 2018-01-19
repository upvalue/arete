;; boot.scm - load all code, initialize module system, and compile everything in-place.

;; order is important

;; Step 1) Load expander
(begin
  (load "scheme/expand.scm")
  ;; Initial module system setup
  (define *core-module* (module-make "arete"))

  (table-set! (top-level-value 'module-table) "arete" *core-module*)
  (table-set! *core-module* "module-export-all" #t)
  (table-set! *core-module* "module-stage" 2)

  (define *current-module* *core-module*)

  ;; populate core module with already-defined things
  (top-level-for-each
    (lambda (k v)
      (if (or (procedure? v) (memq k '(unspecified)))
        ((lambda (name)
           (set-top-level-value! name v)
           (table-set! (table-ref *core-module* "module-exports") k k)
           (table-set! *core-module* k k))
         (module-qualify *core-module* k)))))

  (define (make-empty-module name)
    (define mod (module-make name))
    (table-set! (top-level-value 'module-table) name mod)
    (table-set! mod "module-export-all" #t)
    (table-set! mod "module-stage" 2)
    mod)

  ;; This just allows us to run some R7RS benchmarks. 
  (for-each1 make-empty-module '("scheme#base" "scheme#cxr" "scheme#file" "scheme#inexact" "scheme#write"
                                 "scheme#time" "scheme#read" "scheme#char" "scheme#complex"))

  ;; User module is where toplevel code outside of libraries runs (REPL, random scripts)
  (define *user-module* (module-make "user"))

  (table-set! (top-level-value 'module-table) "user" *user-module*)
  (table-set! *user-module* "module-export-all" #t)
  (table-set! *user-module* "module-stage" 2)

  (set-top-level-value! '*current-module* *core-module*)

  ; Install expander
  (set-top-level-value! 'expander expand-toplevel)
)

;; Step 2) Load core syntax (things like let, record,s etc)
(load "scheme/syntax.scm")
(load "scheme/types.scm")

;; Step 3) Load compiler and bootstrap
(begin
  (load "scheme/compiler.scm")

  ;; Install compiler. All code loaded after this point (not including the currently executing file) will be compiled
  ;; and run on the virtual machine
  (set-top-level-value! 'compiler compile-toplevel)
  
  (pull-up-bootstraps)
)

;; Step 4) Load modules
(load "scheme/library.scm")

;; Finish bootstrapping by importing all functions defined in (arete) to the (user) module
(expand-import (top-level-value '*user-module*) '(arete))
(set-top-level-value! '*current-module* (top-level-value '*user-module*))
