# Display Closures — Implementation Status

**Branch:** `opt-display-closures`
**Baseline:** `master` (commit `01ded4a`)

Original plan is preserved below in the "Original Plan" section. This top section is the live status / handoff for the next agent.

---

## TL;DR

- **Runtime fully landed** — `OP_CAPTURE_FROM_LOCAL` / `OP_CAPTURE_FROM_CLOSURE` / `OP_CAPTURE_GET` work in interpreter, native VM, and JIT. Validated by 8 hand-crafted bytecode tests in `tests/display-closures/runtime-smoke.scm`.
- **Compiler emission landed, flag-gated** — `COMPILER-DISPLAY-CLOSURES` (default `#t` in `scheme/compiler.scm:53`) controls whether `CAPTURE_*` is emitted for immutable captures.
- **One pre-existing bug found and fixed** — `compile-inline-call` clobbered `local-count` after nested inline-calls bumped it. Fix at `scheme/compiler.scm:720-728`.
- **Save+load blocker FIXED** — the interpreter's computed-goto `dispatch_table` in `src/vm.cpp:280-319` was missing entries for the three new opcodes. Off-the-end reads returned null → `goto *NULL` on first `CAPTURE_*` dispatch post-load. Invisible pre-save because `native_vm_function_eligible` routed all `CAPTURE_*`-bearing functions to `apply_native_vm`; `image.cpp:141-145` strips `VMFUNCTION_NATIVE_VM_BIT` on save, forcing the interpreter path on load, which is when the missing table entries crash. Fix: append the three labels to the dispatch table.

## What works right now

Commands to verify:

```
bin/arete boot.scm                                # boots cleanly
bin/arete boot.scm --save-image heap.boot         # heap saves cleanly
bin/arete bootstrap-and-psyntax.scm               # psyntax.pp compiles + sc-expand works (byte-identical to master modulo timing line)
bin/arete heap.boot tests/display-closures/runtime-smoke.scm   # all 8 runtime tests pass
python3 utils/run-tests.py                        # 82/82
bin/arete boot.scm tests/jit.scm                  # 41/41
```

For the psyntax stress class (historical flake per `.tickets/aov-md9k.md`): 30/30 clean, 100/100 clean earlier.

## What's broken

Nothing known. The save+load segfault is resolved by the `src/vm.cpp` dispatch-table patch.

## Files touched

| File | What |
|---|---|
| `arete.hpp:2551-2562` | Added `OP_CAPTURE_FROM_LOCAL=51`, `OP_CAPTURE_FROM_CLOSURE=52`, `OP_CAPTURE_GET=53`. Opcode numbers appended so `heap.boot` compat preserved. |
| `src/vm.cpp:442, 488-507` | Relaxed `AR_ASSERT(...==BOX)` in `OP_CLOSE_OVER` to allow raw-value slots. Added 3 interp handlers. |
| `src/vm-native-x64.cpp.dasc:70, 121-124, 795-829` | Resized `dispatch_table`. Added to `NATIVE_VM_DISPATCH_OPS` macro. Added 3 DynASM handlers next to `op_close_over`. |
| `src/vm-native.cpp:77-80, 212-214` | Added `OP_CAPTURE_*` to `native_vm_opcode_supported` **and** to the stride switch in `native_vm_function_eligible` (the eligibility walker — easy to miss; its omission caused an early auto-install bug, see commit history / first run of runtime-smoke). |
| `src/compile-x64.cpp.dasc:467, 961-963, 1259-1303` | Relaxed `AR_ASSERT` in `make_closure`. Added to label-gen stride switch. Added 3 JIT handlers. |
| `scheme/compiler.scm` | Whole feature. See next section. |
| `tests/display-closures/runtime-smoke.scm` | 8 hand-crafted tests across all 3 engines. New file. |
| `tests/display-closures/run-tests-flag-on.sh` | All existing suites under flag on. New file. |

## Key compiler.scm changes

Line refs approximate — source moves.

- `COMPILER-DISPLAY-CLOSURES` flag at `:53` (default `#t`). Opt out with `--set COMPILER-DISPLAY-CLOSURES "#f"`. **Note:** the reader parses bare `t`/`f` as **symbols**; the boolean needs quoting as `"#t"` / `"#f"`.
- `Var` record gained a `mutable?` field at `:103`. Default `#f`.
- `scan-mutable-names-into!` at `:82-112` — pre-scan populates a per-compile table of all names that need a Box capture. Treats **both** `set!` and `define` targets (internal defines are forward-declared, so between scan-local-defines and local-set the slot is uninitialized — a closure built mid-window needs late-binding via Box. Classic case: `(lambda () (define loop (lambda () (loop))))`). Keys by `rename-strip` so lookup sites that pass stripped symbols hit.
- `var-name-mutable?` at `:66-69` — looks up with `rename-strip`.
- `compile-toplevel` at `:1478-1490` and `recompile-function` at `:1525-1543` populate `*compiler-mutable-names*` before any lambda compiles, restore afterward (nested compile entries are reentrancy-safe).
- Every `Var/make` site now sets `mutable?`:
  - `OpenFn/define!` at `:1000`
  - `register-free-variable` at `:913-917` (copies from parent Var)
  - `compile-inline-call` at `:712-714`
  - `compile-named-loop` at `:605-607`
- **Critical bug fix:** `compile-inline-call` final `local-count` update at `:720-728`. Was an unconditional set; now uses max-of-current-and-proposed. See "The compile-inline-call bug" section below.
- Close-over emission at `:1094-1111` — per-slot picks `capture-from-*` vs `box-from-*` based on `Var/mutable?` and flag.
- `compile-identifier` captured branch at `:992-1000` — picks `capture-get` vs `box-get`.
- `register-free-variable` skips the free-variable-id / free-variables bookkeeping when the capture is immutable and flag is on (no Box needed).

## The compile-inline-call bug

This was the core fix that unblocked psyntax. Worth reading top-to-bottom since it's the most surprising part of the branch.

**Pre-existing**: `compile-inline-call` finalizes `(OpenFn/local-count! fn (fx+ locals N))` unconditionally after compiling the inline-lambda's args and body. Nested inline-calls inside the arg evaluation can bump `local-count` higher (e.g., to `locals+N+K`). The unconditional set shrinks it back to `locals+N`, losing the higher watermark.

**Why it was invisible on master**: all free-variable captures went through Boxes. Boxes are heap objects pointing at a specific stack slot. Even if the VMFunction's `local_count` was set too small (so the VM's computation stack overlapped with slots the bytecode still used as locals), the Box pointer held a stable address — reads/writes through the Box still landed on the right stack word. Any computation-stack writes to the same word were transient and didn't outlive a closure capture.

**Why display closures exposed it**: `OP_CAPTURE_GET` snapshots the raw slot value. If the VM's `local_count` is too small, the snapshot can race with stack operations that write the same word during arg evaluation of a later expression. Result: a closure's display capture holds the wrong value. In psyntax, the symptom was that `sym2077` (the arg of an outer inline-lambda whose arg-eval chain contained two more inline-lambdas) came out as `#f` instead of `'x`. Cascaded into psyntax's `(eq? (id-var-name id empty-wrap) valsym)` check failing with `"definition not permitted"`.

**Fix** at `scheme/compiler.scm:720-728`:

```scheme
(let ((proposed (fx+ locals (length (cdr x))))
      (current (OpenFn/local-count fn)))
  (when (fx< current proposed)
    (OpenFn/local-count! fn proposed)))
```

**How this was found**: instrumented the failing psyntax.pp site (the compiled chi-definition branch) to print captured values; saw `e2082` coming out as the wrong symbol ('DBG-e2082 instead of 'x), traced that to slot-value drift, traced that to `compile-inline-call`'s local-count shrinkage. The technique (print values inside psyntax.pp, temporarily) is the right move if the next debugging session also needs internal psyntax visibility — see `tests/psyntax/psyntax.pp:2077, 2175` for where I added `(print 'DBG-... ...)` wrappers and matched the extra `)` on line 2174 before reverting.

## The save+load segfault (remaining blocker)

**Repro:**

```
bin/arete boot.scm --save-image /tmp/h.boot     # flag is on by default
bin/arete /tmp/h.boot --eval '1'                # segfault, exit 139
```

With flag off (`--set COMPILER-DISPLAY-CLOSURES "#f"` before `boot.scm`, AND flipping the `set-top-level-value!` line in `scheme/compiler.scm:53` to `#f` so it doesn't overwrite the CLI value — the current setter is unconditional), save+load works.

**Single-process operation is fine.** Everything in one `bin/arete` invocation works: `bin/arete bootstrap-and-psyntax.scm` runs psyntax correctly under flag on. The bug only manifests when closures with display-captured heap-pointer slots pass through image save → image load.

### Evidence / things already checked

- Not the `compile-inline-call` fix — reverting it keeps the save+load segfault.
- Not about the flag being overwritten at load — `(top-level-value 'COMPILER-DISPLAY-CLOSURES)` correctly shows `#t` after loading a flag-on heap (tested via wrapping `--eval` tricks).
- `runtime-smoke.scm` under `heap.boot` loads and passes. So the load path itself works; the bug is about specific closures saved with raw-value display captures.

### Most likely suspects

1. **Image load of a `Closure` with raw Values in its `captures` VectorStorage**. See `src/image.cpp:155-160` (Closure case) and `:195-200` (VECTOR_STORAGE case). `update_value` at `:65-70` handles immediates vs heap pointers correctly on the surface, so the slot contents should relocate properly. Worth re-reading for a second pair of eyes.

2. **GC after load but before first function call**. The initial `boot_common` / `boot_from_image` path may collect. A Closure's `captures` VectorStorage tracer (in `src/gc.cpp:605-611`) treats each slot as a Value via `copy()` — same as VectorStorage elsewhere. In theory that's correct.

3. **Native-VM bit handling**. `src/image.cpp:141-145` strips `VMFUNCTION_NATIVE_VM_BIT` on save and resets `procedure_addr` to `apply_vm_` on load. Closures (`:155-160`) reset procedure_addr but don't touch `VMFUNCTION_NATIVE_VM_BIT` on the underlying VMFunction (which is reached via `Closure::function`). After load, `native_vm_install_tree_visit` is **not** called automatically — so closures are reachable with procedure_addr = apply_vm but their function's native-VM bit state might be stale. This is pre-existing though; not specific to display closures. Worth confirming nothing new interacts badly.

4. **`make_closure` helpers in `vm.cpp` and `compile-x64.cpp.dasc`**. After my assertion relaxations, both helpers iterate the stack-snapshot slots and insert them into the VectorStorage. At load time these helpers aren't called (closures come from the image directly). But if a flag-on-compiled function runs after load, it may create new closures with display captures — that path should be fine since the runtime handlers are tested by `runtime-smoke`.

### Suggested next-step debugging

- Narrow with `bin/arete /tmp/h.boot --eval '(car (quote (1 2)))'` and similar — does a pure immediate eval crash, or only ones that touch globals/closures?
- Try `bin/arete --debug-gc /tmp/h.boot --eval '1'` to flush out a GC bug.
- Save a heap with flag on, load it with `--interp-only` (clears bytecode compiler, code runs tree-walker). If that works, it narrows to the native-VM / JIT / apply_vm path. If it still crashes, it's in the tree-walker's touch of a closure.
- Compare `heap.boot` byte size between flag-on save and flag-off save (both from the same `boot.scm` run). A wildly different size hints at structural difference.
- Add a C++-side assertion in `Value` slot access that heap pointers are live GC objects, run under a flag-on heap's early post-load code, catch the exact first invalid deref.

### Where I'd look first

`src/image.cpp` VECTOR_STORAGE (`:195-200`) and Closure (`:155-160`) paths. The invariant "every slot is either a Box or a non-Box heap/immediate" is correct on the Scheme side but the image code doesn't know the difference — it treats each slot as a Value and calls `update_value`. For a Box slot, update_value follows the Box heap pointer. For a raw heap-value slot (e.g., a pair, a closure), same mechanism. Should work. But something subtle is off.

## Known-good test bundle

```bash
# Single-process everything (flag default on)
bin/arete bootstrap-and-psyntax.scm         # psyntax
python3 utils/run-tests.py                  # 82/82
bin/arete heap.boot tests/display-closures/runtime-smoke.scm  # 8 runtime tests
bin/arete boot.scm tests/jit.scm            # 41 JIT tests
bash tests/display-closures/run-tests-flag-on.sh    # 63 tests via --set CLI

# Stress
for i in $(seq 1 30); do bin/arete bootstrap-and-psyntax.scm > /dev/null 2>&1 || echo FAIL; done
```

All green. The only red cell is `bin/arete heap.boot --eval …`.

## Tasks, in order of priority

1. **Fix save+load segfault** (blocker). Probably in `src/image.cpp` around the Closure/VectorStorage paths. Once fixed, flag stays default on.
2. **Perf measurement**. User deferred this to later. Baseline: `time bin/arete boot.scm`, `time bin/arete heap.boot compile.scm`, median of 10. Expectation: fewer Box allocations, `compile.scm` faster ≥5%.
3. **Rename pass (`Upvalue` → `Box`)**. *Done.* Struct, enum, opcodes, bits, helpers, field names, and Scheme-level symbol names all migrated; historical references remain only in the "Original Plan" section below and in `.tickets/`.
4. **Consider `EXP-*` flag naming style**. The codebase has `EXP-3`, `EXP-8` style naming for past experiments. Unifying this one would make the flag family consistent.

---

# Original Plan (preserved)

## What this means here

Per the TODO at `scheme/compiler.scm:7`, "display closures" means: for free variables that are **never `set!`'d**, store the value **directly** in the closure's capture vector instead of boxing it in a heap object. Every current free-variable access pays an extra pointer dereference even though the vast majority are immutable (e.g., the `loop` in `(let loop () …)`, or any function captured by name).

Mutated captures stay as boxes (their current representation) so that aliased `set!` semantics are preserved.

---

## Current state (baseline)

| Aspect | Current Implementation | File:Line |
|---|---|---|
| Closure struct | `{function: VMFunction*, captures: VectorStorage*}` | `arete.hpp:1250` |
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
