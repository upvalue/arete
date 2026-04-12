---
id: aonv-2taa
status: closed
deps: []
links: []
created: 2026-04-12T20:48:48Z
closed: 2026-04-12T23:00:00Z
type: epic
priority: 1
assignee: Phil
tags: [native-vm, vm-opt]
---
# Native x86-64 VM (DynASM) — epic

## Outcome

Shipped S1–S11 with an OP_APPLY_TAIL inlined fast path on top. Test
suite stays 81/81 with `NATIVE_VM_ENABLE=1 NATIVE_VM_DEFAULT=1`. But
**measured end-to-end performance is a regression**, not a win:

| workload          | baseline | native VM | delta      |
| ----------------- | -------- | --------- | ---------- |
| psyntax expand    | ~490 ms  | ~525 ms   | +7% slower |
| 10M tail-call loop | ~208 ms | ~215 ms   | even       |
| fib(30) recursive | ~46 ms   | ~66 ms    | +43% slower |

Reason in one sentence: a "LuaJIT-style dispatch core" layered on top
of an existing C interpreter *does not* match LuaJIT, because LuaJIT
runs the entire call/return machinery in assembly — it never returns
to C during dispatch. Arete's current design still re-enters C on
every Scheme call via `apply_native_vm`, paying a full SysV frame
save/restore + prologue + GC root register per call, which eats the
dispatch savings. See [[Native VM]] postmortem for the full analysis
and what would need to change to actually win.

The work stays merged (gated behind the env vars `NATIVE_VM_ENABLE` +
`NATIVE_VM_DEFAULT`) so a future attempt at the inlined-call-machinery
rewrite has the dispatch layer and bug fixes already in place.

## Mid-flight bug: stale CP across GC

A non-obvious correctness bug dominated the second half of the work.
The interpreter's `VM2_RESTORE_GC` macro recomputes `cp` after any C
call that can trigger GC, because the `Bytevector` holding bytecode
can move independently of the `VMFunction` under the semispace GC.
The native VM initially only reloaded `VFN` — a stale `CP` pointer
survived, reading freed bytecode and segfaulting unpredictably
(symptom: psyntax crashed in map/apply-tail deep recursion).

Fix: stash `&code->data` at `[rsp+24]` on entry (grew the stack
reserve 24→40 bytes), and after any C call that may GC, recompute
`CP = VFN->code->data + (CP - old_base)` and update the stash. Five
handlers got the reload: `op_close_over`, `op_apply`, `op_add`,
`op_sub`, `op_lt`. See `src/vm-native-x64.cpp.dasc` CP_RELOAD blocks.

## OP_APPLY_TAIL fast path

Per-tail-call, the original design called `native_vm_op_apply_tail` (a
C helper) which did type dispatch + `state.temps.clear()` +
`state.temps.insert()` (STL vector ops with cache cost) + set three
globals. For VM-target tail calls — the hot path in Scheme loops —
the asm handler now:

1. Checks heap type (VMFUNCTION or CLOSURE) inline
2. If fargc ≤ 64, copies args to a dedicated fixed-size buffer
   (`native_vm_tail_buf`) via an 8-byte-at-a-time loop
3. Sets `native_vm_tail_{fnp,argc,use_buf,flag}` and jumps to
   `tail_return_marker`

No C call for the common case. Large-argc tail calls and non-VM
targets still go through the slow helper (state.temps path).

Impact: pure tail loops become competitive (within noise of baseline)
— without the fast path they ran slightly slower. This handler change
is strictly a win on its slice of work; the overall regression comes
from OP_APPLY (non-tail), which remains a C-recursing call and is
where fib time is spent.

# Native x86-64 VM (DynASM)

Replace `apply_vm`'s switch/computed-goto dispatch with a hand-written,
DynASM-assembled dispatch loop, LuaJIT interpreter-style. Separate
subsystem from the existing (unused) method-JIT in
`src/compile-x64.cpp.dasc`; that JIT stays as-is.

Design: [[Native VM]] in the wiki.

## Why

`--perf-report` baselines (HEAD b6109e6, g++ 15.2.0 -O2, Linux 6.12.74 x86_64):

- Cold `bin/arete bootstrap-and-psyntax.scm`: 1012 ms wall, **VM 77.6%**, interp 14.2%, GC 4.8%.
- `bin/arete heap.boot boot.scm` (hot heap): 61 ms wall, **VM 97.0%**.
- `make heap.boot` best-of-5: 100/101/101/101/102 ms, image 913 kb.

R7RS subset (CPU_LIMIT=120, heap.boot warm):

| bench                  | seconds |
| ---------------------- | ------- |
| fib:40:5               | 27.7    |
| nqueens:13:10          | 62.4    |
| destruc:600:50:4000    | 21.0    |
| peval:2000             | 44.2    |
| nboyer:5:1             | 16.0    |
| browse:2000            | 4.7     |
| deriv:10000000         | 26.7    |
| quicksort:10000:2500   | 27.4    |

(Earley crashed on the current HEAD and was excluded from the
baseline; that crash is a separate pre-existing bug and is not a
blocker for this epic.)

VM is the obvious lever. A LuaJIT-style dispatch core is the standard
play.

## Approach

One shared DynASM blob assembled at startup; every opt-in `VMFunction`
points its `procedure_addr` at `apply_native_vm`, which sets up the
frame in C and jumps into the dispatch core. Bytecode (the `size_t`
wordcode in `VMFunction::code`) is reused verbatim.

Pinned registers across the loop (SysV): r12=State*, r13=cp, r14=stack
top, r15=locals, rbx=VMFunction*. Each opcode handler ends with a
tail-dispatch through a static table; no return to C between
instructions.

Opt-in: new header flag `VMFUNCTION_NATIVE_VM_BIT` and Scheme builtin
`(native-vm-install! fn)`. At install time we walk the bytecode against
a supported-opcode table; if every opcode is covered we flip the bit
and rewrite `procedure_addr`, otherwise we return `#f`. This is what
lets the work be genuinely incremental.

## Incremental steps (each a child ticket)

- **S1 — Opt-in plumbing.** Header bit + builtin + coverage check + stub `apply_native_vm` that forwards to `apply_vm`. Ships no DynASM code.
- **S2 — MVP dispatch.** DynASM file, dispatch table, opcode handlers for `ARGC_EQ`, `PUSH_IMMEDIATE`, `RETURN`. Acceptance: `((lambda () #t))` returns `#t` under native VM.
- **S3 — Locals + pop.** `LOCAL_GET`, `LOCAL_SET`, `POP`, `PUSH_CONSTANT`, `ARGC_GTE`.
- **S4 — Flow control.** `JUMP`, `JUMP_WHEN`, `JUMP_UNLESS`, `JUMP_WHEN_POP`, `ARG_OPTIONAL`.
- **S5 — Globals.** `GLOBAL_GET`, `GLOBAL_SET`.
- **S6 — Primitives.** `FX_ADD`, `FX_SUB`, `FX_LT`, `ADD`, `SUB`, `LT`, `EQ`, `NOT`, `CAR`, `CDR`, `FIXNUMP`, `TYPE_CHECK`.
- **S7 — Calls.** `APPLY` with register save/restore across `procedure_addr` tails, exception propagation.
- **S8 — Tail calls.** `APPLY_TAIL`; native-to-native restart in-loop, rest via slow path.
- **S9 — Closures + upvalues.** `UPVALUE_GET`, `UPVALUE_SET`, `CLOSE_OVER`, `UPVALUE_FROM_LOCAL`, `UPVALUE_FROM_CLOSURE`.
- **S10 — Rest / keys.** `ARGV_REST`, `ARG_KEY`, `ARGV_KEYS`. Full coverage.
- **S11 — Default on.** Flip install at `OpenFn->procedure`, remove coverage check, measure.
- **S12 — Retire `apply_vm`** or keep as debug path; decision after S11 numbers.

## Acceptance (epic-level)

- Step 2 landed: `((lambda () #t))` runs through native VM end-to-end.
- Step 11 landed: all bytecode runs through native VM, test suite green, bootstrap and bootstrap-and-psyntax faster than baseline, R7RS subset no slower on any benchmark.
- Final perf-report ratio of `vm.exclusive_time_pct` drops materially on bootstrap-and-psyntax.

## Non-goals

- Windows / Emscripten support (deferred; guard phase 1 on SysV).
- Register allocation across opcodes; trace compilation; any form of
  tiered JIT. This epic is a single shared interpreter core.
- Touching the existing method-JIT in `compile-x64.cpp.dasc`.
- Bytecode changes; wordcode layout stays as `VMFunction::code`.

## References

- `src/vm.cpp` — current `apply_vm` (reference for frame setup + opcode semantics).
- `src/compile-x64.cpp.dasc` — existing DynASM usage in-repo; reuse the Makefile minilua pattern.
- `arete.hpp` — `VMFunction`, `Procedure`, `State::gc.vm_stack`, opcode enum (~line 2435).
- `src/builtins.cpp:1373` — `OpenFn->procedure`, the VMFunction constructor where the native-VM bit gets consulted at step 11.
- [[Native VM]] for the full design writeup.

