# Native VM

Plan for a hand-written, DynASM-assembled x86-64 virtual machine that
replaces `apply_vm` as the dispatch loop for bytecode. Inspired by the
LuaJIT interpreter, which Mike Pall credits as one of the main reasons
LuaJIT is much faster than baseline Lua.

This is a separate subsystem from the existing native-code JIT
(`src/compile-x64.cpp.dasc`). That JIT is currently unused but stays as
is; the new native VM lives in its own file(s) and opts in via a header
flag on `VMFunction`.

## Motivation

From `--perf-report` on the current build (see [[Benchmarks Arete]] and
the baselines recorded in the epic ticket): bootstrap-and-psyntax is
**~77% VM time**, ~14% interp, ~5% GC, balance reader/idle. Cold
`boot.scm` via `heap.boot` is **~97% VM time**. The VM is the hot
component; any speedup there moves the bootstrap and psyntax numbers
directly.

LuaJIT-style dispatch avoids the C switch/computed-goto overhead by (1)
keeping dispatch state (cp, stack, locals, function) in pinned
callee-saved registers across the entire function; (2) ending every
opcode handler with a tail-dispatch to the next handler rather than
returning to a C loop; (3) letting the assembler pack stores to hot
slots tighter than the C compiler will.

## Relationship to the existing JIT

- `src/compile-x64.cpp.dasc` — per-function method-JIT; currently disabled (guarded `if(0)` in `vm.cpp`). Untouched.
- `src/vm.cpp` — `apply_vm`, the switch/computed-goto dispatch loop. Stays as the slow-path / fallback while the native VM rolls out, removed or demoted after full coverage.
- `src/vm-native-x64.cpp.dasc` (new) — LuaJIT-style dispatch core. Shared across all native-VM-eligible `VMFunction`s; bytecode is reused verbatim.

## High-level design

### Entry point and calling convention

New freestanding `apply_native_vm(State&, size_t argc, Value* argv, void* fnp)` with the same `c_closure_t` signature as `apply_vm`. A `VMFunction` is switched to it by flipping a header bit and re-running `procedure_install` so `Procedure::procedure_addr` now points at `apply_native_vm`.

### Opt-in mechanism (step 1, the smallest visible unit)

- New header flag: `VMFUNCTION_NATIVE_VM_BIT = 1 << 15` (next free slot on `VMFunction`).
- New Scheme-visible builtin: `(native-vm-install! fn)` — sets the bit and rewrites `procedure_addr`, or returns `#f` if the function's opcode set is not yet covered by the native VM.
- New predicate: `(native-vm-function? fn)` — reports bit state.
- Opcode coverage is checked at install time by walking `VMFunction::code` against a static `supported_opcodes` table. This is what lets development be incremental without mid-function fallback.

The MVP ships with only enough opcodes to run `(lambda () #t)`:
`OP_ARGC_EQ`, `OP_PUSH_IMMEDIATE`, `OP_RETURN`. `(native-vm-install! (lambda () #t))` then `((lambda () #t))` returning `#t` is the step-2 acceptance test.

### Register map (SysV AMD64)

Pinned across the dispatch loop:

| reg | role                                 |
| --- | ------------------------------------ |
| r12 | `State*`                             |
| r13 | `cp` (current instruction pointer)   |
| r14 | computation-stack top (`stack`)      |
| r15 | `locals` base pointer                |
| rbx | `VMFunction*` (`vfn`)                |
| rbp | saved frame pointer                  |

Scratch: `rax`, `rcx`, `rdx`, `rdi`, `rsi`, `r8`-`r11`.

Windows x64 would remap `r12`/`r13` identically (still callee-saved) but use different arg registers. Phase 1 is SysV-only; Windows support deferred.

### Dispatch

Opcodes are 64-bit words (`size_t`). Handler tail:

```
  ; ...opcode body...
  mov rax, [r13]           ; load next opcode
  add r13, 8
  jmp [dispatch_table + rax*8]
```

`dispatch_table` is a static array of code addresses populated by DynASM labels, accessed through a global registered via `|.globals`/`dasm_setupglobal`. Only one DynASM blob is assembled at startup (or image load); every native-VM `VMFunction` shares it.

### Memory model

No change to `state.gc.vm_stack`, `VMFrame2`, or `Procedure::procedure_addr`. Frame setup is done by a short C trampoline at the top of `apply_native_vm` (the code that would otherwise be open-coded at the top of `apply_vm`: grow stack, zero slots, allocate upvalues if any). The trampoline hands control to the DynASM entry point once registers are loaded. This avoids rewriting frame setup in assembly for step 1.

GC safety: all live values continue to live in `state.gc.vm_stack` slots. Registers `r13`-`r15`, `rbx` contain C pointers, not `Value`s the GC needs to see. The only delicate path is tail into C helpers (allocation, applying a CFunction, etc.), where `stack` (r14) must be written back into `state.gc.vm_stack_used` before the call and reloaded after. This mirrors what `VM2_RESTORE` does in `apply_vm`.

## Incremental rollout

Each step is a separate ticket under the epic. Each ends with the test
suite passing and bootstrap succeeding.

- **S0 — Baseline.** Numbers captured in the epic ticket.
- **S1 — Opt-in plumbing.** `VMFUNCTION_NATIVE_VM_BIT`, `native-vm-install!` builtin, opcode coverage check, stub `apply_native_vm` that forwards to `apply_vm`. No DynASM code yet. Verify a tagged function still executes correctly.
- **S2 — Trivial dispatch.** First DynASM blob. Opcodes: `ARGC_EQ`, `PUSH_IMMEDIATE`, `RETURN`. Acceptance: `((lambda () #t))` → `#t` with the bit set. `arith`/`tak` etc. are still rejected by the opcode-coverage check, unaffected.
- **S3 — Locals + pop.** Add `LOCAL_GET`, `LOCAL_SET`, `POP`, `PUSH_CONSTANT`. `(lambda (x) x)`, constant-returning functions. `ARGC_GTE`.
- **S4 — Flow control.** `JUMP`, `JUMP_WHEN`, `JUMP_UNLESS`, `JUMP_WHEN_POP`, `ARG_OPTIONAL`. `if`, `cond`, `when`, `and`/`or` compile.
- **S5 — Globals.** `GLOBAL_GET`, `GLOBAL_SET`.
- **S6 — Primitives.** `FX_ADD`, `FX_SUB`, `FX_LT`, `ADD`, `SUB`, `LT`, `EQ`, `NOT`, `CAR`, `CDR`, `FIXNUMP`, `TYPE_CHECK`. At this point most leaf benchmarks become eligible.
- **S7 — Calls.** `APPLY` — preserve pinned registers across `procedure_addr` tail, write back stack top, reload on return. Exception propagation.
- **S8 — Tail calls.** `APPLY_TAIL`. First pass: for in-language tail calls (VMFunction → VMFunction), remain in the native VM loop; restart dispatch against the callee. Out-of-language tail calls take the slow path via `apply_vm`-style tail restart.
- **S9 — Closures + upvalues.** `UPVALUE_GET`, `UPVALUE_SET`, `CLOSE_OVER`, `UPVALUE_FROM_LOCAL`, `UPVALUE_FROM_CLOSURE`. Upvalue allocation is GC'd — write back, call `state.gc.allocate`, reload.
- **S10 — Rest / key arguments.** `ARGV_REST`, `ARG_KEY`, `ARGV_KEYS`. Full opcode coverage.
- **S11 — Default on.** Flip the header bit at `OpenFn->procedure` time. Remove the coverage check (all opcodes supported). Measure.
- **S12 — Retire `apply_vm`** or keep it as the assertion/debug path. Decide based on step-11 numbers.

## Build wiring

- New file `src/vm-native-x64.cpp.dasc`.
- Mirror the existing `src/compile-x64.cpp.dasc` makefile rule so minilua processes it into `src/vm-native-x64.cpp` before compile.
- Guard the whole file behind `#if AR_64_BIT == 1 && !defined(_MSC_VER)` for phase 1. 32-bit and Windows link against a stub that only defines `apply_native_vm = apply_vm` so nothing else has to `#ifdef`.

## Risk / open questions

- **GC scan of the computation stack** — `apply_vm` relies on `state.gc.vm_stack_used` accurately describing the live range. The native VM must write back `stack` before any allocation. Missing a write-back is silently wrong and manifests as GC-relocated pointers. Plan: put every C call through one macro that does the write-back + reload dance, and assert the invariant in debug builds.
- **Exception handling** — `apply_vm` uses `goto exception` to unwind to a single handler. The native VM needs an equivalent: either a shared error sink label that all opcodes branch to, or a C-level helper that returns into `apply_native_vm`'s C frame via a preserved return address.
- **Windows / Emscripten** — neither is on the critical path for benchmarking. Stub both out in phase 1; revisit after step 11.
- **Interaction with the existing JIT** — the native JIT in `compile-x64.cpp.dasc` also rewrites `procedure_addr`. Since the JIT is disabled, there is no live interaction, but the two flags (`VMFUNCTION_NATIVE_BIT`, `VMFUNCTION_NATIVE_VM_BIT`) must be mutually exclusive if the JIT is ever re-enabled.

## Postmortem (after S11)

The plumbing works (81/81 tests with `NATIVE_VM_ENABLE=1
NATIVE_VM_DEFAULT=1`, bootstrap-and-psyntax passes) but measured
end-to-end performance is a regression vs. the C interpreter:

| workload          | baseline | native VM | delta        |
| ----------------- | -------- | --------- | ------------ |
| psyntax expand    | ~490 ms  | ~525 ms   | +7%  slower  |
| 10M tail-call loop | ~208 ms | ~215 ms   | even         |
| fib(30)           | ~46 ms   | ~66 ms    | +43% slower  |

### Why

The architectural shape of the win described in LuaJIT is not what
this epic delivered. LuaJIT's interpreter runs the **entire
call/return machinery** in assembly:

- `CALL` opcode: save caller's PC + function to a return-info slot
  in the value stack; advance BASE; load callee's PC; dispatch. ~15
  instructions. **No C call.**
- `RETURN` opcode: pop return value; restore PC + function from the
  caller's return-info slot; pull BASE back; dispatch. ~15
  instructions. **No C call.**

So a Lua function calling another Lua function never grows the C
stack, never saves/restores SysV callee-saved registers, never passes
through a helper. C is entered once (`lj_vm_call`), the interpreter
runs a flat loop, and C is re-entered only for builtins (CFUNCTION
targets) or errors/GC.

Arete's current design is fundamentally different:

- `apply_native_vm` is called per Scheme call.
- Each invocation is a new C frame: push rbx/rbp/r12–r15 + 40-byte
  reserve + SysV register spills, plus a prologue that computes
  `sff_offset`, grows the stack, memcpy's args, and registers `closure`
  and `fn` as GC roots via `AR_FRAME`.
- The asm dispatch loop is a tenant inside each C frame, not the owner
  of execution.
- `OP_APPLY` in asm is literally `call [rax + procedure_addr]`, which
  for a VM-target callee is `call apply_native_vm` — recursing through
  C. On return, the asm restores VFN and CP via `CP_RELOAD` (6
  instructions) and calls `native_vm_refresh` to recompute
  LOCALS/STACK.

Per-call overhead in the native VM path is therefore **strictly
greater** than in the C interpreter (`apply_vm`, which also recurses
but stays in C with no asm/C boundary crossings). The C interpreter
already uses computed gotos and is tuned with `-O2`. The switch-vs-asm
dispatch win is real but small, and it is negated by the extra
call-site overhead the native path introduced.

`fib(30)` is the cleanest demonstration: fib is dominated by OP_APPLY
(non-tail recursive), so it exercises exactly the path where the
native VM has the most extra cost per call.

### The OP_APPLY_TAIL fast path

One mitigation did land. The tail-call handler in asm now inlines the
VM-target case:

- Heap-type check inline (VMFUNCTION | CLOSURE)
- If fargc ≤ 64, copy args to a dedicated fixed-size buffer
  (`native_vm_tail_buf[64]`) via an 8-byte-at-a-time loop
- Set tail-flag/argc/fnp globals, jump to `tail_return_marker`

No C call for the common case. Before this, a pure 10M-iteration tail
loop ran ~5–10% slower than the C interpreter; with the fast path it's
within noise.

This is strictly better than what was there before, but on real
workloads (psyntax, fib) OP_APPLY (non-tail) dominates, so the fast
path doesn't move the needle end-to-end.

### CP reload: the bug that took half the session

A subtle correctness bug dominated the second half of implementation.
Arete's semispace GC moves the `Bytevector` holding bytecode
independently of the `VMFunction`. The C interpreter handles this in
`VM2_RESTORE_GC` by recomputing `cp` relative to the old and new
`code_pointer()` bases after any C call that can trigger GC. The
native VM was reloading `VFN` but not `CP` — stale CP, reading freed
bytecode, segfaulting in deep recursion through `fn_map → lambda → fn_map → …`.

Fix: stash `VFN->code->data` at `[rsp+24]` on function entry (grew
the per-invocation reserve from 24 to 40 bytes); after any C call
that may GC, recompute `CP = new_base + (CP - old_base)` and update
the stash. Applied to `op_close_over`, `op_apply`, `op_add`, `op_sub`,
`op_lt`. `op_upvalue_get`/`op_upvalue_set`/`op_type_check` are
non-allocating and don't need the reload.

### What would actually match LuaJIT

If the user wants this to win, this is what needs doing:

1. Rewrite `apply_native_vm` so it doesn't recurse on Scheme calls.
   `OP_APPLY` and `OP_RETURN` become full asm handlers that save/
   restore `VFN`, `CP`, `sff_offset` (and closure, upvalues) to/from
   the Scheme value stack, advance BASE, and dispatch in-place.
2. The only C re-entry is CFUNCTION calls, GC, and errors.
3. GC root registration moves out of `AR_FRAME`-style per-C-frame
   tracking and into a single `apply_native_vm` frame that knows how
   to walk the active Scheme frames on the value stack.
4. Exception propagation goes through a shared error label in asm
   that returns from the outer `apply_native_vm` C frame.

This is essentially a rewrite of the VM, not a dispatch-core drop-in.
It's the work Mike Pall was referring to — "hand-writing the VM in
assembly" means all of it, not just the dispatch tail. The current
implementation is closer to 20% of the rewrite and unsurprisingly
gets 0% of the speedup.

### Current state

Kept in tree, gated by two env vars:

- `NATIVE_VM_ENABLE=1` — initializes the dispatch core at boot.
- `NATIVE_VM_DEFAULT=1` — additionally installs the native VM on
  every eligible VMFunction at `OpenFn->procedure` time.

Neither is on by default. `bin/arete` without these env vars is
unchanged from baseline. The native VM path has no known correctness
bugs (81/81 tests + bootstrap-and-psyntax clean across many runs)
so the infrastructure is a valid starting point for a future rewrite
that fixes the actual architectural problem.
