# Plan: Display Closures for Arete

## What this means here

Per the TODO at `scheme/compiler.scm:7`, "display closures" means: for free variables that are **never `set!`'d**, store the value **directly** in the closure's capture vector instead of boxing it in a heap object. Every current free-variable access pays an extra pointer dereference even though the vast majority are immutable (e.g., the `loop` in `(let loop () …)`, or any function captured by name).

Mutated captures stay as boxes (their current representation) so that aliased `set!` semantics are preserved.

---

## Current state (baseline)

| Aspect | Current Implementation | File:Line |
|---|---|---|
| Closure struct | `{function: VMFunction*, upvalues: VectorStorage*}` | `arete.hpp:1250` |
| Upvalue struct | `{local*, vm_local_idx, converted}` union | `arete.hpp:1203` |
| Environment model | Parent-chain linked tables per OpenFn | `compiler.scm:748-789` |
| Free var capture | Flat vector walk, propagate up parent chain | `compiler.scm:738-747` |
| Bytecode ops (5 total) | GET, SET, CLOSE_OVER, FROM_LOCAL, FROM_CLOSURE | `arete.hpp:2470-2474` |
| Interp dispatch | Stack of Upvalue pointers; dereference on get/set | `vm.cpp:376-471` |
| JIT codegen | DynASM with upvalue dispatch macros; make_closure helper | `compile-x64.cpp:1100-1549` |
| Mutable capture opt | None — all upvalue-indirected | `compiler.scm:7` |
| Recent perf work | Lazy allocation attempt (rejected; slower) | `.tickets/aov-md9k.md` |

---

## Phase 1 — Mutability analysis pass (pure, no behavior change)

**File:** `scheme/compiler.scm`

1. Add a `mutable?` field on the `Var` record.
2. Before `compile-lambda` finishes each `OpenFn`, walk the already-compiled IR (or do it during body walk) marking any `Var` that appears as the target of `set!`. Crucially, propagate `mutable?` **through the parent chain** the same way `register-free-variable` does — a variable is mutable if set! anywhere it's visible.
3. Add a debug dump flag so we can print per-lambda `(name, captured?, mutable?)` for audit.

**Test:** no functional change expected. Run `boot.scm` and the existing test suite. Print the mutability table for `scheme/expand.scm` and spot-check: `loop` names, top-level lambdas, most letrec-bound functions should be immutable; counter-style `let` bindings should be mutable.

**Commit boundary.** Merge before touching runtime.

---

## Phase 1.5 — Rename `Upvalue` → `Box`, introduce `captures` vocabulary

**Goal:** land the naming before new opcodes, so new code uses the new vocabulary from day one and we don't have a mixed `UPVALUE_*` / `CAPTURE_*` opcode table to live with forever.

**Pure rename, zero behavior change.** Do it as one commit so `git log --follow` stays clean.

### Renames

| Old | New |
|---|---|
| `struct Upvalue` | `struct Box` |
| `Closure::upvalues` (field) | `Closure::captures` |
| `VectorStorage* upvalues` (locals) | `VectorStorage* captures` |
| `upvalue_count` on `VMFunction` | `capture_count` |
| `OP_UPVALUE_GET` | `OP_BOX_GET` |
| `OP_UPVALUE_SET` | `OP_BOX_SET` |
| `OP_CLOSE_OVER` | keep (still correct) |
| `OP_UPVALUE_FROM_LOCAL` | `OP_BOX_FROM_LOCAL` |
| `OP_UPVALUE_FROM_CLOSURE` | `OP_BOX_FROM_CLOSURE` |
| `make_upvalue` / `upvalue_close` helpers | `make_box` / `box_close` |
| Scheme-side `OpenFn/closure` vector | leave as-is unless it reads awkwardly; it's the compile-time capture list, not the runtime representation |
| `free_variables` bytevector on VMFunction | keep — it's still the list of locals to capture |

### Files touched (rename-only)

- `arete.hpp` — struct decl (`:1203`), Closure field (`:1250`), opcode table (`:2470-2474`), `CLASS_TYPE` enum entry
- `src/vm.cpp` — handlers at `:376-471`
- `src/vm-native.cpp` — the unsupported-opcode markers at `:72-76`
- `src/compile-x64.cpp` + `.dasc` — handlers around `:1100-1549`
- `src/gc.cpp` — tracer for the renamed type
- `scheme/compiler.scm` — opcode emission, any debug names
- `scheme/library.scm` — opcode name table
- `scheme/types.scm` — if it mirrors C++ types
- Any debug printer / disassembler that names opcodes
- `docs/` — grep for "upvalue" in the wiki and update

**Opcode numbers do NOT change** — just the symbolic names. `heap.boot` compatibility preserved.

### Gate

1. `make bin/arete` clean.
2. `bin/arete boot.scm` produces an identical `heap.boot` byte-for-byte to the pre-rename boot (or with only trivially-explainable differences — if opcode *names* get embedded in the heap, regenerate and verify).
3. Full test suite + psyntax round-trip.
4. `grep -n -i upvalue` across the tree returns nothing outside of historical tickets / changelogs.

---

## Phase 2 — New bytecode ops (runtime support for direct captures)

**Files:** `arete.hpp` (opcode table `~line 2470`), `src/vm.cpp`, `scheme/compiler.scm` (emit), `scheme/library.scm` (opcode name table if one exists).

Add three opcodes; keep the renamed `BOX_*` ops for mutable captures:

| New op | Semantics |
|---|---|
| `OP_CAPTURE_FROM_LOCAL idx` | Push `frame.locals[idx]` as a raw Value onto the capture-building stack |
| `OP_CAPTURE_FROM_CLOSURE idx` | Push `closure->captures[idx]` directly (caller knows it's already an immutable direct slot) |
| `OP_CAPTURE_GET idx` | Read `closure->captures[idx]` as a Value (no Box deref) |

`OP_CLOSE_OVER` is reused — it already pops N items into a VectorStorage, and a direct Value and a `Box*` both fit in a slot.

The `BOX_*` ops (from Phase 1.5) stay for mutable captures. `compile-lambda` (`scheme/compiler.scm:896`) picks per-slot: if the captured Var is `mutable?`, emit `BOX_*`; otherwise emit `CAPTURE_*`. Access sites (`compile-reference`) similarly pick `BOX_GET` vs `CAPTURE_GET` by slot index.

**Interpreter (`src/vm.cpp:376–471`):** add the three handlers. `CAPTURE_GET` is literally `stack.push(closure->captures->data[idx])`.

**Critical invariant:** GC tracing of a Closure's `captures` VectorStorage already traces each slot as a Value — and `Box*` *is* a Value with the heap tag. Direct captures are also Values. So **no GC change is required**, but add an assertion in the tracer dev build that each slot is either a Box or a non-Box heap/immediate.

**Test:** bootstrap (`make bin/arete && bin/arete boot.scm`). Then run the full test suite. Then re-bootstrap with the new heap (`bin/arete heap.boot …`) — a real integration check because it means the new compiler compiled itself correctly with the new opcodes.

**Commit boundary.**

---

## Phase 3 — Native VM + JIT

**Files:** `src/vm-native.cpp:72-76`, `src/compile-x64.cpp`, any `.dasc`.

1. **Native VM:** `CAPTURE_GET` becomes supported (unlike `BOX_GET`, it's just an indexed load). That's a meaningful widening of native-VM eligibility — many functions currently bounce to the interpreter only because they read free variables.
2. **JIT (`src/compile-x64.cpp:1364-1487`):** `CAPTURE_GET` emits a single `mov` through `closure->captures->data[idx]` — no dispatch, no branch. Compare the current `OP_BOX_GET` path which tests open-vs-closed state.
3. **JIT closure creation (`:1522-1549`):** `make_closure` doesn't change shape; only the contents of slots differ (some are raw Values).
4. **Lazy-allocation regression (ticket `aov-md9k`) — avoid it.** That attempt failed because moving Box allocation off the entry path shifted cost to capture. *This change is different:* we're eliminating Box *existence* for immutable slots entirely, not deferring it. Still, measure per-phase to make sure we don't accidentally regress the mutable path.

**Test:** same as Phase 2 but now also run with the JIT enabled. Note: this is dangerous territory per the memory note about `apply_native_vm`'s fast-path and `compile-x64` being three separate things — make sure all three paths handle the new opcodes.

---

## Phase 4 — Incremental test strategy

1. **Tier 0 — smoke:** `bin/arete -e '((lambda (x) ((lambda () x))) 42)'` must print 42. Hand-write 5–10 tiny Scheme snippets covering: no-capture, one immutable capture, one mutable capture, mixed, deeply nested, `set!` in inner function on outer's variable, self-recursive `letrec`.
2. **Tier 1 — bootstrap:** `boot.scm` completes, byte-for-byte output match on any deterministic test.
3. **Tier 2 — re-bootstrap:** use the new `heap.boot` to compile itself. This catches miscompilation of the compiler.
4. **Tier 3 — psyntax / expand:** the most closure-heavy code. The failed `aov-md9k` ticket crashed here intermittently — treat a clean psyntax run as the real gate.
5. **Tier 4 — full suite:** existing test files under `tests/` (or wherever the project keeps them — check during Phase 1).
6. **Tier 5 — differential:** add a `--trace-captures` flag that logs every `CAPTURE_GET`/`BOX_GET`. Run a canonical program before and after; diff should show immutable accesses migrated to `CAPTURE_GET` and mutable ones unchanged in count.

At each tier, if it fails, narrow per the debugging-technique memory (`arete_debugging_native_vm.md`): binary-search with minimal two-line defines.

---

## Phase 5 — Proving the speedup

Establish the baseline **before Phase 1** so numbers are comparable:

1. **Wall-clock benchmarks** (hot + cold):
   - Bootstrap time (`time bin/arete boot.scm`)
   - The benchmark already invoked in `compile.scm` (`time-function "full expand and compile on VM" …`) — run it 10× and report median + IQR.
   - Standard Scheme micro-benchmarks if present: `tak`, `fib`, `ctak` (closure-heavy). If not present, add a small `bench/closures.scm` with deliberately closure-heavy patterns: named-`let` counted loops, CPS-style passing, curried functions.
2. **Allocation counts:** add counters for Box allocations and Closure allocations, dump at exit. Expectation: Box allocations **drop significantly** (order-of-magnitude for closure-heavy code) while Closure allocations stay flat.
3. **Native-VM eligibility:** count functions executed via native VM vs fallback-to-interpreter before and after. Expectation: eligibility increases because `CAPTURE_GET` is native-supported while `BOX_GET` is not.
4. **Success criteria:** the `compile.scm` benchmark gets faster by a measurable margin (target ≥5% median; anything <2% is within noise). Bootstrap gets faster or ties. No regression on `tak` (no-capture baseline). No new psyntax crashes over 100 runs.

Record numbers in a ticket (`ticket new "display closures: perf report"`) with the baseline → each phase → final, so the revert gate is clear if something regresses.

---

## Risk register

- **Same failure mode as `aov-md9k`:** psyntax crashed intermittently. Mitigation: run psyntax ≥100× before declaring Phase 2 done.
- **GC invariant drift:** Closure slots are heterogeneous (`Box*` vs direct Value). Both are tagged Values, so tracing works, but any code that *assumes* slots are Boxes will silently break. Audit every read of `Closure::captures` — grep across `src/` and `arete.hpp` after Phase 1.5.
- **Self-hosting hazard:** the compiler compiles itself. A bug in mutability analysis can produce a compiler that miscompiles its next run. Mitigation: keep the old opcodes live and gate new emission behind a flag for the first bootstrap; flip the flag only after a round-trip succeeds.
- **Opcode numbering:** heap.boot is probably sensitive to opcode numbers. Append new opcodes, don't renumber.
- **Unrelated known bug:** `char=?` inline is wrong (per memory). Don't conflate perf regressions with that.

---

## Summary of files that change

| File | Change |
|---|---|
| `scheme/compiler.scm` | Add mutability analysis; emit new vs old opcodes per-slot |
| `arete.hpp` | Add 3 opcode constants (near `:2470`); rename Upvalue→Box; no struct-shape change |
| `src/vm.cpp` | Rename handlers; add 3 new handlers near `:376-471` |
| `src/vm-native.cpp` | Remove the unsupported marker for `CAPTURE_GET` at `:72-76` |
| `src/compile-x64.cpp` | Add DynASM emission for 3 ops around `:1364-1549` |
| `src/gc.cpp` | Rename tracer refs |
| `scheme/library.scm` | Opcode name table, if present |
| `scheme/types.scm` | If it mirrors C++ types |
| `bench/closures.scm` | New, optional — only if existing benches are thin |
| `docs/` | Update any wiki pages that name "upvalue" |
| `.tickets/…` | New ticket for tracking + perf numbers |

No changes to: macro expander, types.cpp (struct shape), VectorStorage, GC tracer logic (rename only).

---

## Phase order

**0 (baseline) → 1 (analysis) → 1.5 (rename) → 2 (new opcodes + interp) → 3 (native VM + JIT) → 4 (tests) → 5 (perf report).**
