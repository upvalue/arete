# Flat VM Calling Convention

Restructure `apply_vm` so Scheme→Scheme calls and returns are handled inside
one dispatch loop with an explicit frame stack, instead of recursing through
the C stack with a heavyweight per-call prologue.

Development and benchmarking target: **x86-64**. The benchmark goal is to
improve the ecraven/r7rs and project-native workloads there. (This plan was
drafted on an arm64 machine, where the native VM dispatch core never runs;
all arm64 numbers are pure C++ core and are useful only as a secondary
signal.)

Related: [[Benchmarking]], [[Benchmarks Arete]], [[Performance Reports]],
tickets `are-vfam` (prologue cost), `are-8qgc` (native VM call machinery,
blocked on this work), `aonv-2taa` (native VM epic), PLAN.org "Virtual
machine" section (stack hunger).

## Motivation (measured, not speculative)

- On peval there are **322M apply calls against 774M executed opcodes — 2.4
  opcodes per call**. At that ratio the calling convention *is* the workload.
- Per-call cost is ~95 ns vs femtolisp's ~52 ns (`are-vfam`). fib:40 spends
  99.99% of ~28 s inside `apply_vm` with zero GC.
- The native x86-64 dispatch core makes opcodes faster but still funnels
  calls through the C `apply_vm` prologue: 69% of peval calls take the C
  path (`native_vm_stat_apply_c_*` histogram, src/vm-native.cpp:34-39), and
  the net effect is currently a regression (fib +43%). `are-8qgc` (move
  call/return into asm) is blocked on exactly this protocol work.
- Every non-tail Scheme call consumes a C stack frame, which is why
  ack/takl/cpstak can't run at reasonable RECURSION-LIMIT values (PLAN.org).

Per-call costs to remove (src/vm.cpp:161-269): VMFrame2 ctor/dtor, separate
stack-growth and recursion-limit branches, `AR_FRAME` root registration,
whole-frame `memset` (vm.cpp:245, `// TODO: Necessary?`), argv `memcpy`,
`state.temps` round-trip on every tail call (vm.cpp:541-544), and the
pointer-reconstruction division in `VM2_RESTORE` (vm.cpp:143-152).

## Facts the design relies on

1. **Exceptions propagate by return value** — every call site checks
   `is_active_exception()` (vm.cpp:516). No longjmp/C++ unwinding to fight;
   the `exception:` label becomes a frame-popping loop.
2. **`call/cc` is `call/1cc`** — one-shot, escape-only, built on exceptions
   (scheme/library.scm:333-337). No continuation capture of the frame stack.
3. **The value stack is already unified and GC-scanned**: locals, boxes, and
   eval stack all live in `state.gc.vm_stack`, a contiguous growable array
   (arete.hpp:1625-1672). Only *control state* lives on the C stack today.
4. C→VM entry points are few and auditable: eval.cpp:775,788,865,
   `fn_apply` (builtins.cpp), and the native-VM shims.

## Design

### Frame records

A growable array on `State` (traced as a GC root), one record per active VM
frame:

```cpp
struct VMCallFrame {
  Value    closure;      // traced by GC; vfn derived via closure_unbox
  size_t   return_cp;    // bytecode OFFSET, not pointer (code moves with GC)
  size_t   frame_base;   // offset into vm_stack where locals start
  size_t   caller_stack; // caller eval-stack offset to restore on return
  uint32_t frames_lost;  // tail-call accounting for stack traces
};
```

Offsets everywhere, never raw pointers — this retires the `VM2_RESTORE`
division and makes GC/realloc safety mechanical.

Alternative considered (decide during Phase 1): inline frame metadata into
`vm_stack` itself as tagged fixnum slots + the closure Value, so the GC
traces it for free and locality improves. Start with the separate array; it
is simpler to assert on and to dump.

### Call protocol

- `OP_APPLY` / `OP_APPLY_GLOBAL` / `OP_APPLY_LOCAL` on a VMFunction/Closure
  callee (vm.cpp:505,602,693): push a frame record, set `vfn/cp/locals/stack`
  for the callee, `continue` the dispatch loop. No C call, no VMFrame2, no
  AR_FRAME.
- **Zero-copy arguments**: at a call site the args already sit contiguously
  at `stack - fargc`. The callee's `frame_base` is arg0's offset — args
  become locals in place. (Phase 2; Phase 1 keeps the memcpy.)
- `OP_RETURN` (vm.cpp:776): pop the record — close any open boxes (current
  `~VMFrame2` logic, vm.cpp:102-120, guarded by `if (box_count)`), write the
  return value into the callee's fn slot, restore caller offsets, continue.
  Only at the session base does the C function actually return.
- **Tail calls** (vm.cpp:527-594): `memmove` args down to the current
  `frame_base`, reuse the frame record in place, bump `frames_lost`. The
  `state.temps` copy path dies.
- **CFunction callees**: unchanged — direct call from the loop, exception
  check, continue. `fn_apply` flattening (vm.cpp:52-80) stays, but flattens
  directly into the target frame.
- **Exception unwind** (vm.cpp:1223): pop frames in a loop — close boxes,
  call `trace_function` per frame with its `frames_lost` — down to the
  session base, then return the exception value. Observable traces must
  match today's dtor-driven output.

### Sessions (re-entrancy)

Each C-level `apply_vm` invocation records the frame-stack height at entry
as its session base. A CFunction called from the VM that re-enters Scheme
(eval, callbacks, fn_apply) opens a nested session above the current
`vm_stack_used`; frames never pop past their session base. Same nesting the
C stack provides today, made explicit.

### Entry checks and frame initialization

- One fused check per call: vm_stack room AND frame-record room AND
  recursion depth (`vm_depth` == frame count), replacing the two branches at
  vm.cpp:178-196.
- `memset` shrinks to box slots + locals beyond supplied args. Full
  elimination requires the GC to scan `vm_stack` precisely per frame
  (records make this possible) — a separately measured Phase 2 experiment,
  not part of the core change.
- `argc` stays a register variable: arity opcodes (OP_ARGC_EQ/GTE,
  OP_ARGV_REST) only run in a function's prologue, before any nested call.

### Consequences

- VM depth costs ~40 bytes of frame record + vm_stack slots instead of a
  full C frame. RECURSION-LIMIT can rise 10-100x; ack/takl/cpstak become
  runnable. Set the new default from measured per-frame footprint in Phase 3.
- The native VM (x86-64) shares this protocol: `are-8qgc`'s asm call path
  implements push/pop of the same records. During this work, build with
  `NATIVE_VM_DEFAULT=0`; re-enable and re-scope `are-8qgc` afterward.

### Untouched

eval.cpp, the bytecode format, compile.scm, image save/load, MSVC switch
dispatch (the `VM_CASE`/`VM_DISPATCH` macros already abstract it — keep the
Nmakefile build compiling).

## Phases

Lesson from T5/T9 (aborted big-bang rewrites): stage it, gate each step.

### Phase 0 — Baseline lock-in (x86-64, ~half a day)

On the x86-64 dev box:

1. Build and pin a baseline binary (`bin/arete-baseline` from current
   master), with the normal flag set.
2. Capture to `scratch/flatcall/baseline/`:
   - `make bench-report-arete` (boot, bootstrap-and-psyntax), RUNS=5.
   - `bench/interp` microbenchmark suite, best-of-3 protocol.
   - r7rs subset: fib, tak, peval, nboyer, browse, destruc, mazefun.
   - Perf-report JSONs: `vm_calls`, wall, GC counters.
   - All of the above **twice**: `NATIVE_VM_DEFAULT=1` and `=0`, so the C
     path and native-fallback path are separable later.
   - A CPU profile (perf) of fib and peval: snapshot of prologue cost
     (VMFrame2, memset, memcpy, temps) for before/after comparison.
3. Add a call-rate microbenchmark if none exists: 2-arg self-recursive
   function, reports **ns/call** via wall ÷ `state.perf.vm_calls`.
   Baseline expectation: ~95 ns.

### Phase 1 — Frame records + flat calls, correctness-first

Implement the frame stack and in-loop call/return/tail/unwind in
src/vm.cpp. Keep argument copying as-is (memcpy into the new frame) so the
diff is purely control-flow. Delete VMFrame2. `vm_depth` = frame count.
Build with `NATIVE_VM_DEFAULT=0`.

Gates, in order — each must pass before the next:

1. `tests/preboot` snapshots.
2. `python3 utils/run-tests.py` — 105/105.
3. `tests/test-semispace` — 18/18.
4. Full `bin/arete boot.scm` bootstrap + image save/reload smoke test.
5. `bin/arete bootstrap-and-psyntax.scm` stress.
6. r7rs correctness subset (compare outputs, not times).
7. **Entire suite again under GC stress mode**
   (`collect_before_every_allocation`) — the main defense against
   stale-pointer and rooting bugs.
8. Differential run vs `bin/arete-baseline`: stdout, stderr class, exit
   status; exception stack traces must match (frames_lost lines included).

Checkpoint measurement: ns/call and fib on x86-64. **Kill criterion: if
Phase 1 lands functionally correct but ns/call is >85, stop and re-profile
before proceeding** — the thesis is wrong or the implementation missed.

### Phase 2 — Zero-copy and prologue diet (one measured change at a time)

Each lands or reverts on its own benchmark delta, experiment-ticket style:

- (a) Args-in-place locals (callee `frame_base` = arg0 offset); delete the
  entry memcpy for VM→VM calls.
- (b) memmove tail calls; delete the `state.temps` tail path.
- (c) Fused entry check (single branch).
- (d) memset reduction to box slots + unsupplied locals.
- (e) *Experiment*: precise per-frame GC scanning of `vm_stack` to eliminate
  eval-region zeroing entirely. Higher risk, gated on (d)'s numbers.

### Phase 3 — Re-baseline and unblock the JIT

- Full benchmark sweep on x86-64, both NATIVE_VM states; update
  [[Benchmarking]] and docs numbers; refresh `results.Arete`.
- Raise default RECURSION-LIMIT from measured per-frame footprint; confirm
  ack/takl/cpstak complete.
- Re-scope `are-8qgc`: the native core's call fast path now means
  "push/pop a VMCallFrame in asm," with the C++ loop as the proven
  reference implementation and fallback.

## Measurement plan (all primary numbers from x86-64)

| Metric | Workload | How | Target / kill |
|---|---|---|---|
| ns per non-tail call | call-rate microbench; peval (322M calls) | wall ÷ `perf.vm_calls` | ≤55 ns (from ~95); kill >85 after Phase 1 |
| Pure call throughput | fib:40 (99.99% VM, no GC) | benchmark runner, RUNS=5 best-of | ≥1.5x; stretch 2x |
| Mixed workloads | peval, nboyer, browse, destruc | r7rs runner, geomean | ≥25%; stretch: peval ≤ femtolisp's 16.6 s |
| Deep recursion | takl, cpstak, ack | r7rs runner, raised limit | complete at all (today: crash/timeout) |
| No-regression guard | boot, bootstrap-and-psyntax | `make bench-report-arete`, RUNS=5 | within ±2% (VM ≈ 9% of boot) |
| Tail-call cost | mazefun, tak | suite + microbench | improvement expected from temps removal; flat acceptable |
| Native interplay | full subset, NATIVE_VM on vs off | both builds, Phase 3 | document; informs are-8qgc |
| Memory | bytes/frame, vm_stack peak | counters | informational; sets RECURSION-LIMIT |

Protocol: best-of-N (N≥5) per the existing runner; one before/after JSON
pair per Phase 2 sub-change in `scratch/flatcall/`; post-change CPU profile
of fib must show prologue symbols (memset/memcpy/frame setup) gone from the
top — the mechanistic confirmation, independent of wall-clock noise.

## Risks

1. **Stale pointers after `vm_stack` realloc or GC.** Mitigation: offsets in
   records, raw pointers recomputed only after potential-GC operations, GC
   stress mode in every gate.
2. **Box-close ordering on unwind** must match today's innermost-first dtor
   order. Covered by closure tests under stress mode and differential runs.
3. **Stack-trace drift** (per-frame `trace_function`, `frames_lost`).
   Covered by differential stderr comparison vs baseline.
4. **Native VM is knowingly benched** during Phases 1-2 (`NATIVE_VM_DEFAULT=0`).
   x86-64 benchmark comparisons during that window use the C-core baseline.
5. **Blast radius**: confined to src/vm.cpp (1,288 lines; `apply_vm` is the
   only restructured function), a small State/GC root addition, and the
   vm-native build gate.
