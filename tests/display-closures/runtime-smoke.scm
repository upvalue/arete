;; runtime-smoke.scm - hand-crafted bytecode tests for the new display-closure
;; opcodes (OP_CAPTURE_FROM_LOCAL, OP_CAPTURE_FROM_CLOSURE, OP_CAPTURE_GET).
;;
;; Runtime support (vm.cpp / vm-native-x64 / compile-x64) lands before the
;; compiler learns to emit these opcodes. This harness validates runtime in
;; isolation by hand-assembling parent+child VMFunction pairs that exercise
;; each new opcode end-to-end.
;;
;; Invoke: bin/arete heap.boot tests/display-closures/runtime-smoke.scm
;;
;; Auto-install puts any all-supported-opcode VMFunction on the native-VM
;; path, so the default path validates native-VM.  JIT is validated by
;; explicitly running `vmfunction->native!` + `native-call` on the same
;; bytecode.

(define failures '())

(define (check! label expected actual)
  (if (equal? expected actual)
    (print ":) " label " => " actual)
    (begin
      (print ":( " label " expected " expected " but got " actual)
      (set! failures (cons label failures)))))

;; Build a VMFunction from explicit fields.  Mutates an OpenFn record in the
;; same shape `compile-lambda` does, skipping compile-time bookkeeping that
;; only matters for source compilation.
(define (build-fn name insns constants min-arity max-arity local-count stack-max)
  (define fn (OpenFn/make name))
  (OpenFn/insns! fn (list->vector insns))
  (OpenFn/constants! fn (list->vector constants))
  (OpenFn/min-arity! fn min-arity)
  (OpenFn/max-arity! fn max-arity)
  (OpenFn/local-count! fn local-count)
  (OpenFn/stack-max! fn stack-max)
  (compile-finish fn)
  (OpenFn->procedure fn))

;; --- Test 1: single immutable capture via CAPTURE_FROM_LOCAL + CAPTURE_GET.
;;
;; child(): (capture-get 0) ; return
;;   — reads the captured value directly (no Box dispatch).
;; parent(x): build a closure over child whose slot 0 is locals[0] raw, then
;;   apply the fresh closure with 0 args.
;;
;; bytecode walk-through for parent (stack effects in brackets):
;;   argc-eq 1               []
;;   push-constant 0 (child) [+1 child-fn]
;;   capture-from-local 0    [+1 x]
;;   close-over 1            [-2 pops x + fn, +1 pushes closure]
;;   apply 0                 [-0 (pops callee only), +1 result]
;;   return                  [-1 result]
;; stack-max = 2 during the push-constant+capture-from-local window.

(define child1
  (build-fn 'child1
            '(argc-eq 0 capture-get 0 return)
            '()
            0 0 0 1))

(define parent1
  (build-fn 'parent1
            '(argc-eq 1 push-constant 0 capture-from-local 0 close-over 1 apply 0 return)
            (list child1)
            1 1 1 2))

(check! "1. native-vm-installed? parent" #t (native-vm-function? parent1))
(check! "1. native-vm-installed? child"  #t (native-vm-function? child1))
(check! "1. display capture via native-vm"       42 (parent1 42))
(check! "1. display capture, second call, diff value" 'sym (parent1 'sym))

;; --- Test 2: two-level nested capture via CAPTURE_FROM_CLOSURE.
;;
;; grandchild(): (capture-get 0) ; return              — reads captured x.
;; child-builder(): (push-constant 0 grandchild;
;;                   capture-from-closure 0;           — pull x from our own closure.
;;                   close-over 1;
;;                   return)
;;     → returns a fresh closure over grandchild whose slot 0 = x.
;; outer(x): build child-builder closure that captures x raw;
;;           apply child-builder() -> inner-closure;
;;           apply inner-closure() -> x.

(define grandchild
  (build-fn 'grandchild
            '(argc-eq 0 capture-get 0 return)
            '()
            0 0 0 1))

(define child-builder
  (build-fn 'child-builder
            '(argc-eq 0
              push-constant 0
              capture-from-closure 0
              close-over 1
              return)
            (list grandchild)
            0 0 0 2))

(define outer2
  (build-fn 'outer2
            '(argc-eq 1
              push-constant 0
              capture-from-local 0
              close-over 1
              apply 0
              apply 0
              return)
            (list child-builder)
            1 1 1 2))

(check! "2. two-level capture" 99 (outer2 99))

;; --- Test 3: close-over with mixed slot shapes — pass a heap value (pair)
;; and a fixnum through the capture slot.

(define child3
  (build-fn 'child3
            '(argc-eq 0 capture-get 0 return)
            '()
            0 0 0 1))

(define outer3
  (build-fn 'outer3
            '(argc-eq 1 push-constant 0 capture-from-local 0 close-over 1 apply 0 return)
            (list child3)
            1 1 1 2))

(check! "3. heap value through capture" '(1 2 3) (outer3 '(1 2 3)))
(check! "3. string through capture"    "hi"    (outer3 "hi"))
(check! "3. boolean #f through capture" #f      (outer3 #f))

;; --- Test 4: GC pressure between CLOSE_OVER and CAPTURE_GET.
;;
;; Build the closure in one call, then allocate a bunch of garbage via a
;; normal Scheme call, then invoke the closure.  A captured heap value must
;; survive the collection and be reachable through the closure's capture
;; vector.

(define (make-captured-closure)
  (outer3 (cons 'survivor '())))

;; outer3 returns the value from calling the inner closure, so we need a
;; variant that returns the closure itself.  Build one:
;;   returner(x): push-constant 0 child ; capture-from-local 0 ; close-over 1 ; return
(define returner
  (build-fn 'returner
            '(argc-eq 1 push-constant 0 capture-from-local 0 close-over 1 return)
            (list child3)
            1 1 1 2))

(define captured-closure (returner (cons 'survivor '())))

(let loop ((i 0))
  (unless (fx= i 1000)
    ;; Allocate pressure.  Don't hold onto it.
    (list->vector (list i i i i))
    (loop (fx+ i 1))))

(check! "4. captured heap value survives GC" '(survivor) (captured-closure))

;; --- Test 5: JIT path.  Re-run test 1's parent under the JIT.
;;
;; vmfunction->native! replaces the function's procedure_addr with a JIT'd
;; entry.  Calling `(parent1-jit 42)` then goes through the JIT-emitted
;; OP_CAPTURE_FROM_LOCAL and OP_CLOSE_OVER, and the child still executes
;; under native-VM/interp (we did not JIT the child).

(vmfunction->native! parent1)
(check! "5. JIT'd parent, display capture"          100 (parent1 100))
(check! "5. JIT'd parent, heap value through capture" '(a b) (parent1 '(a b)))

;; --- Test 6: JIT both levels — JIT the grandchild too so CAPTURE_GET is
;; exercised inside JIT'd code.  outer2 stays interpreted / native-VM.

(vmfunction->native! grandchild)
(check! "6. JIT'd grandchild, CAPTURE_GET in JIT" 'xyz (outer2 'xyz))

;; --- Test 7: JIT all three layers.

(vmfunction->native! child-builder)
(vmfunction->native! outer2)
(check! "7. JIT'd full chain" 7777 (outer2 7777))

;; --- Test 8: mixed slots — one immutable (CAPTURE_*) + one mutable (BOX_*)
;; in the same closure.  This is the real production shape once Phase C lands.
;;
;; child8() reads its slot 0 directly (capture-get) and its slot 1 through the
;; a Box (box-get).  The set! path is exercised with box-set,
;; which must not corrupt slot 0.
;;
;; parent8(imm mut):
;;   — imm is captured display-style (raw Value in slot 0)
;;   — mut is captured Box-style (Box in slot 1)
;;   — calls child8 twice, second call reads the mutated Box.
;;
;; Parent bytecode walk:
;;   argc-eq 2
;;   push-constant 0 (child8)                   stack=[child]
;;   capture-from-local 0                       stack=[child, imm]
;;   box-from-local 0                       stack=[child, imm, Box(mut)]
;;   close-over 2                               stack=[closure]
;;   local-set 2                                locals[2]=closure; stack=[]
;;   local-get 2                                stack=[closure]
;;   apply 0                                    stack=[result1]
;;   pop                                        stack=[]
;;   local-get 2                                stack=[closure]
;;   apply 0                                    stack=[result2]
;;   return
;;
;; child8 bytecode: set! the mutable box on entry, then read both and
;;   return them as a 2-element vector via `vector` (global).
;;
;; To exercise the set! path without depending on globals we simplify: child8
;; reads slot 0 via capture-get and slot 1 via box-get, returning slot 0
;; on odd calls and slot 1 on even calls — but we can't keep call state.
;; Simpler: have two different children, one reads imm via capture-get, the
;; other reads mut via box-get.  parent8 calls both and returns a pair.

(define child8-imm
  (build-fn 'child8-imm
            '(argc-eq 0 capture-get 0 return)
            '()
            0 0 0 1))

(define child8-mut
  (build-fn 'child8-mut
            '(argc-eq 0 box-get 1 return)
            '()
            0 0 0 1))

;; To build a shared closure (same captures vector for both children) we'd
;; need explicit work.  Easier: one parent emits two separate closures sharing
;; the same mutable Box by way of propagation.  Simplest validation: build one
;; closure for imm-read, one closure for mut-read, both with the same two
;; slots, mutate mut via box-set inside the mut-reader.

;; Actually, this cross-closure aliasing is overkill for the smoke harness —
;; the set! semantics for mutable boxes are already covered by the existing
;; bootstrap.  What's new here is the mixed-slot CLOSE_OVER.  Verify that:
(define child8
  (build-fn 'child8
            '(argc-eq 0 capture-get 0 box-get 1 eq? return)
            '()
            0 0 0 2))

(define parent8
  (build-fn 'parent8
            '(argc-eq 2
              push-constant 0
              capture-from-local 0
              box-from-local 0
              close-over 2
              apply 0
              return)
            (list child8)
            2 2 2 3))

;; The mutable slot has free-variables set: we need free-variables=#(1) so
;; that openfn_to_procedure allocates one per-frame Box for local idx 1.
;; Set free-variable-count and free-variables manually.
(define parent8-rebuild
  (let ((fn (OpenFn/make 'parent8)))
    (OpenFn/insns! fn (vector 'argc-eq 2
                               'push-constant 0
                               'capture-from-local 0
                               'box-from-local 0
                               'close-over 2
                               'apply 0
                               'return))
    (OpenFn/constants! fn (vector child8))
    (OpenFn/min-arity! fn 2)
    (OpenFn/max-arity! fn 2)
    (OpenFn/local-count! fn 2)
    (OpenFn/stack-max! fn 3)
    (OpenFn/free-variables! fn (vector 1))
    (OpenFn/free-variable-count! fn 1)
    (compile-finish fn)
    (OpenFn->procedure fn)))

(check! "8. mixed slots: capture-get == box-get (same value)" #t (parent8-rebuild 55 55))
(check! "8. mixed slots: capture-get != box-get"              #f (parent8-rebuild 55 66))

(if (null? failures)
  (print "ALL RUNTIME SMOKE TESTS PASSED")
  (begin
    (print "FAILURES:")
    (for-each1 (lambda (f) (print "  " f)) (reverse failures))
    (exit 1)))
