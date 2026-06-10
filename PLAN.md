# Native VM: Flat Calls in Asm

Port the flat calling convention (commit 177166a, [[VM Calling Convention]])
into the DynASM dispatch core (src/vm-native-x64.cpp.dasc) so native→native
Scheme calls — including tail calls from nested frames — never re-enter C,
and so both engines share one frame representation (`VMCallFrame` records in
`state.gc.vm_frames`).

Development and benchmarking target: **x86-64**, `NATIVE_VM_DEFAULT=1`
builds throughout. Predecessor plan: "Flat VM Calling Convention"
(PLAN.md at commit 177166a; results in scratch/flatcall/NOTES.md).
Ticket: `are-8qgc` (re-scoped 2026-06-10; histogram numbers below are in
its notes). Related: `aonv-2taa` postmortem, [[Benchmarking]],
docs/Native VM.md.

## Motivation (measured, 2026-06-10, best-of-5 unless noted)

The flat C++ core now **beats** the native-VM build on call-heavy
workloads; the asm core's only losses are its call machinery:

| bench | flat C core (`NATIVE_VM_DEFAULT=0`) | native build (`=1`) |
|---|---|---|
| peval | **13.397** | 15.106 |
| nboyer | **8.109** | 8.921 |
| tak | **9.335** | 9.911 |
| fib | 22.174 | **20.946** |
| browse | 1.499 | **1.549**\* |
| callrate ns/call | 13.49 | 14.20 |

\* browse is within noise; fib is the real native win (dispatch-bound).

The native-vm-stats histogram on peval (flat-native1 build) localizes the
problem precisely:

- Non-tail: 217.9M asm fast path vs 105.1M through C. C-path reasons:
  **70.9M no-native-bit**, 19.1M cfunction (legitimate), 15.1M boxed,
  0 arity-range, 0 argc-mismatch, 0 variadic.
- Tail: **only 48.9M asm vs 125.0M through C** — tail calls are the
  dominant hole. Nested fast-path frames may not raise the tail flag at
  all (`allow_tail=0`, vm-native-x64.cpp.dasc ~line 444): every tail call
  made from a nested native frame becomes a non-tail C call — no TCO and
  full SysV cost.

Secondary motivations: the asm prologue still runs the full-frame
`apply_zero_loop` zero-fill the C++ core no longer needs; native builds
keep a 10,000 RECURSION-LIMIT (vs 100,000 flat) only because native
non-tail calls still burn a C stack frame; and two competing frame
formats (VMCallFrame records vs the asm core's 5-word inline frame links)
are a standing maintenance hazard.

## Facts the design relies on

1. **The asm core already does flat calls in asm** — the "C1 fast path"
   (op_apply at vm-native-x64.cpp.dasc ~1054): exact-arity, non-variadic,
   box-free, native-bit callees get an inline 5-word frame
   {saved_vfn, saved_cp_offset, current_closure, saved_sff_offset,
   saved_ret_slot} pushed on vm_stack and control stays in the dispatch
   loop. This work is a *re-targeting* of proven machinery, not a first
   implementation.
2. **Pinned registers** (callee-saved across the loop): r12=STATE, r13=CP,
   r14=STACK, r15=LOCALS, rbx=VFN; r10=ARGC at entry; r11=DTABLE
   (reloaded after C helpers via `reloadDtable`). C-side slots accessed
   from asm: vfn_ref (GC-updated fn slot), sff_ref (current frame base),
   closure_ref. The dispatch entry signature is in the .dasc header.
3. **The flat C++ core is the reference implementation.** Its semantics
   are pinned by the gate suite: run-tests 121, test-semispace 18,
   GC-stress scripts, and scratch/flatcall/differential.sh (stdout/stderr/
   exit/trace comparison against a pinned binary).
4. **VMCallFrame + the vm_stack high-water invariant** are documented in
   [[VM Calling Convention]]; `gc.vm_frames` is a `std::vector<VMCallFrame>`
   (reserve 1024) traced as roots; vm_stack_used only shrinks at session
   exit; growth zeroes exactly the newly included region.
5. **Stats plumbing exists**: native_vm_stat_* counters, `(native-vm-stats)`,
   `(native-vm-stats-reset!)` (vm-native.cpp) — per-change measurement of
   path mix is one eval away.
6. **Eligibility is per-function** (`native_vm_function_eligible`,
   VMFUNCTION_NATIVE_VM_BIT set at OpenFn->procedure time and on image
   load); `native-vm-install-tree!` exists for forced installation.

## Design

### One frame representation

Replace the 5-word inline frame links with pushes/pops of the same
`VMCallFrame` records the C++ core uses:

- Asm call fast path: bounds-check `vm_frames` (size < capacity) and
  recursion depth, write the record fields, bump size — all inline.
  Overflow or depth-fail tail-calls a C helper (grow/raise). Pin the
  vector layout with static_asserts (data/size/capacity offsets for this
  libstdc++; a tiny POD mirror struct if that proves fragile).
- Asm OP_RETURN: pop the record, restore VFN/CP/LOCALS/STACK from it
  (offsets → pointers via gc.vm_stack base), write result to result_slot.
  A session-base record returns to C as today.
- `vm_depth` stays equal to frame count; sff_ref/closure_ref C-side slots
  become derivable from the top record (keep them during transition,
  delete at the end).
- Exception unwind: once both engines speak VMCallFrame, the C++
  unwind loop can pop asm-pushed frames identically — traces and box
  closing unify. (Today's asm frames are invisible to trace_function
  except via its own bookkeeping.)

### Adopt the flat-call wins

- **Zero-copy args**: callee frame_base = arg0's offset (args are already
  on the caller's eval stack); kill the arg-copy loop for `argc <=
  max_arity`. Over-arity: C helper (temps stash), same rule as the C++
  core — never allocate a fresh frame above the high-water mark for a
  recycled call (TCO space-leak rule; see NOTES on the mazefun
  regression).
- **Memset diet**: delete `apply_zero_loop`; zero only box slots +
  unsupplied locals. The high-water invariant transfers as-is; the grow
  helper already runs in C where the growth-zeroing lives.
- **In-asm tail calls**: recycle the top record (inherit return linkage,
  frames_lost++), memmove args to frame_base, jump to the callee's code.
  This works from *nested* frames too — record recycling is exactly what
  the tail_flag unwind dance was approximating from the top frame only.
  The native_vm_tail_flag/buf machinery shrinks to the non-native-target
  path (or dies entirely if that path goes through procedure_addr).

### Coverage, in histogram order

1. **Tail calls to native callees from any depth** (kills most of the
   125M apply-tail-c on peval).
2. **no-native-bit (70.9M)**: diagnose before fixing — add a one-shot
   per-callee log (or extend the histogram with callee name sampling) to
   learn whether these are ineligible opcodes, functions created before
   native_vm_ready, closures missing the bit propagation, or interpreted
   functions. Fix is whatever the data says (eligibility widening,
   boot-time install-tree, or bit propagation on closure creation).
3. **Boxed callees (15.1M)**: C helper allocates the boxes (it can GC),
   then re-enters the asm frame-entry path; the record roots the closure
   across the helper.
4. CFunction calls (19.1M) stay C calls — correct by design.

### Untouched

Bytecode format, compile.scm, eval.cpp, image format, the C++ apply_vm
(reference), MSVC/non-x64 builds (NATIVE_VM_ENABLE=0 path), the method
JIT (compile-x64.cpp.dasc — decision point *after* this work: if the asm
interpreter saturates below compiled-Scheme targets, the next plan is the
method JIT, which now inherits cheap calls).

## Phases

### Phase 0 — Baseline + diagnosis (~half a day)

1. Pin binaries: current master (177166a) built `NATIVE_VM_DEFAULT=1` and
   `=0`, plus heap images. NOTE: build artifacts from before 2026-06-10
   may reference a GC'd Nix store glibc — rebuild from scratch, verify
   `bin/arete --eval '(display 1)'` runs.
2. Capture with scratch/flatcall/capture-baseline.sh (RUNS=5) into
   scratch/asmcall/baseline/{native1,native0}; carry over the existing
   flat sweeps if the toolchain hasn't shifted timings (re-run fib+peval
   to verify ±2% first).
3. Histograms: `(native-vm-stats)` after peval, psyntax, nboyer, tak,
   destruc — saved as JSON-ish text next to the captures.
4. **no-native-bit diagnosis**: per-callee sample of bit-less callees on
   peval (temporary counter/log build). This decides Phase 3's scope.

### Phase 1 — VMCallFrame records replace inline frames (correctness-first)

Keep the zero-loop and the arg copy; only swap the frame representation
and the return path. The tail_flag mechanism stays. Build
`NATIVE_VM_DEFAULT=1`.

Gates (each must pass before the next):
1. run-tests 121/121, test-semispace 18/18.
2. boot + image save/reload; psyntax.
3. r7rs subset output-correct (fib tak peval nboyer browse destruc mazefun).
4. GC stress: preboot suite + the VM-heavy stress script under
   `--debug-gc` (full bootstrap under stress is prohibitively slow).
5. differential.sh vs pinned native1 baseline — traces, frames_lost,
   exit classes identical.
6. Histogram sanity: apply-vm/apply-c mix unchanged (±1%) vs Phase 0.

Checkpoint: full subset timings. **Kill criterion: any benchmark >3%
slower than Phase 0 native1 — the record push/pop must be cost-neutral
against the 5-word inline frame before the wins land on top.**

### Phase 2 — Flat-call wins in asm (one measured change at a time)

Experiment-ticket style; each lands or reverts on its own delta, gates
re-run per change:
- (a) zero-copy args + memset diet (these interlock in asm; one change).
- (b) in-asm tail calls with record recycling, nested frames included.
  Watch mazefun and tak specifically (tail-heavy), and the
  apply-tail-vm/apply-tail-c mix.
- (c) delete the tail_flag/tail_buf machinery if (b) made it dead.

Checkpoint after (b): **target: flat-native1 ≥ flat-native0 on peval and
nboyer (the native build stops losing on call-heavy workloads). Kill: if
after (a)+(b) peval is still >5% behind flat-native0, stop and re-profile
— the C-fallback share, not per-call cost, is the bottleneck, and Phase 3
must come first.**

### Phase 3 — Coverage (histogram-ordered)

- (a) Whatever the no-native-bit diagnosis prescribes (70.9M on peval).
- (b) Boxed callees via C box-alloc helper (15.1M).
- Re-measure the histogram after each; stop when apply-c (non-cfunction)
  + apply-tail-c are each <20% of their totals on the perf suite, or when
  remaining items are CFunction-legitimate.

### Phase 4 — Re-baseline and conclude

- Full sweeps both flag states; update scratch/asmcall/NOTES.md,
  [[Benchmarking]] numbers, [[Benchmarks ecraven]] if outcomes change,
  results.Arete via a canonical perf-group run.
- Unify RECURSION-LIMIT defaults (asm frames no longer consume C stack →
  100,000 for both builds; verify ack/takl/cpstak on the native build).
- Decide NATIVE_VM_DEFAULT's default with data (it should now be a
  strict win; if not, document why).
- Close or re-scope are-8qgc with the numbers; open the method-JIT
  decision ticket with the post-asm-interpreter gap to a compiled Scheme
  (build femtolisp from the vendored checkout for a same-box h2h).

## Measurement plan (primary numbers from x86-64, NATIVE_VM_DEFAULT=1)

| Metric | Workload | Target / kill |
|---|---|---|
| Call-heavy parity then win | peval, nboyer, tak vs flat-native0 | ≥ parity after Phase 2; ≥10% win after Phase 3 |
| Dispatch-bound win retained | fib, browse vs Phase 0 native1 | no regression >2% |
| ns/call | callrate (utils/run-callrate.sh, analytic count) | ≤ 13.49 (flat C core); stretch ≤ 8 |
| Tail coverage | apply-tail-vm / (vm+c) on peval | from 28% to >80% |
| Non-tail C share | apply-c minus cfunction, peval | <20% of non-tail calls |
| No-regression guard | boot, psyntax, RUNS=5 | within ±2% |
| Frame-representation cost | Phase 1 full subset | within 3% of Phase 0 (kill) |
| Deep recursion | ack/takl/cpstak on native build, Phase 4 | complete at unified 100K limit |

Protocol: best-of-5 via capture-baseline.sh; histogram snapshot per
sub-change; differential.sh after every change; per-change patches in
scratch/asmcall/. The callrate caveat from [[Benchmarking]] applies:
perf.vm_calls misses native-path calls — use the analytic count.

## Risks

1. **GC invariant violations in asm** — records root closures; every
   helper that can allocate must be followed by VFN/CP reloads through
   vfn_ref (existing pattern). GC-stress gate per change is the defense.
2. **std::vector layout coupling** for gc.vm_frames asm access.
   static_assert offsets; fall back to a hand-rolled POD
   {data,size,capacity} frame stack on State if libstdc++ internals are
   unstable across the toolchains in use (note: the build env on this box
   shifted toolchains mid-June — pin and record `gcc --version` in
   captures).
3. **Tail-call semantics drift** (frames_lost counts, trace dedup,
   box-close order on recycle). differential.sh covers traces; add a
   tail-heavy trace case if coverage feels thin.
4. **Histogram-blind optimization**: Phase 2's win is capped by the
   C-fallback share — hence the Phase 2 kill criterion routing to
   Phase 3 instead of asm micro-tuning.
5. **Blast radius**: vm-native-x64.cpp.dasc call/return/tail sections,
   vm-native.cpp helpers, no C++-core changes expected. The
   NATIVE_VM_ENABLE=0 build must stay green (it compiles none of this).
