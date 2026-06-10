# VM Calling Convention

How `apply_vm` (src/vm.cpp) runs Scheme→Scheme calls since the flat-frame
rewrite (2026-06). For measurement methodology see [[Benchmarking]]; for the
numbers see [[Benchmarks Arete]] and `scratch/flatcall/NOTES.md`.

## Flat frames

Scheme→Scheme calls do not recurse through the C stack. One C-level
`apply_vm` invocation hosts a whole tree of calls inside a single dispatch
loop, with control state held in an explicit stack of `VMCallFrame` records
(`state.gc.vm_frames`, defined in arete.hpp next to `GCCommon`):

```cpp
struct VMCallFrame {
  Value closure;        // active Closure/VMFunction; GC root
  size_t frame_base;    // vm_stack offset where this frame's locals start
  size_t return_cp;     // caller bytecode offset (words) to resume at
  size_t result_slot;   // vm_stack offset the return value lands in
  uint32_t frames_lost; // tail calls collapsed into this frame
  uint32_t flags;       // SESSION_BASE
};
```

Everything is an **offset**, never a raw pointer: `vm_stack` can realloc and
bytecode moves with the copying GC. The old `VM2_RESTORE` pointer
reconstruction (and its division) is gone; registers (`vfn`, `code`, `cp`,
`locals`, `stack`, `cur_closure`) are re-derived from the top record after
anything that can allocate or re-enter the VM.

A call site is eligible for the flat path iff the callee's
`procedure_addr == &apply_vm`. CFunctions, interpreted functions and
native-VM-installed functions take the ordinary C call, exactly as before.

## Sessions

Each C-level entry into `apply_vm` records the frame-stack height as its
*session base* and tags its first frame `SESSION_BASE`. Returns and
exception unwinds never pop past it; popping the session-base frame is what
returns control to C. CFunctions that re-enter Scheme (eval, `apply`,
callbacks) simply open a nested session on top — the same nesting the C
stack used to provide, made explicit.

## The vm_stack high-water invariant

`gc.vm_stack_used` is a **session high-water mark**, not a current-depth
counter. It only shrinks when a session exits. The invariant that makes
this safe:

> Every slot below `vm_stack_used` is, at every GC point, either zero or a
> valid Value that each collection has kept updated.

Consequences:

- **Zero-copy calls.** At a flat call site the args already sit contiguously
  on the caller's eval stack; the callee's `frame_base` is arg0's offset and
  the args become locals in place. No memcpy. (Over-arity calls — `argc >
  max_arity` — stash args in `state.temps` and reuse the same base; falling
  back to a fresh frame here would climb the high-water mark on every
  iteration of an over-arity tail loop, i.e. a TCO space leak. This was
  caught as a mazefun regression and is load-bearing.)
- **Tail calls** memmove the args down to the recycled frame's base and
  re-enter; the `state.temps` round-trip exists only for the over-arity and
  `apply`-flatten paths.
- **Minimal frame init.** Only box slots and unsupplied locals are zeroed on
  entry. The eval region is write-before-read by the stack discipline, and
  GC-safety of scanning it comes from the invariant — not from zeroing.
  The one place that must zero is *growth*: when `vm_stack_used` extends
  (realloc garbage, or slots a GC ran past while a nested session had
  shrunk the mark), exactly the newly included region is memset.

If you touch any of this, the invariant is the thing to re-verify; the GC
stress mode (`--debug-gc`) plus `scratch/flatcall/differential.sh` are the
tools. A frame record's `closure` is traced in `GCCommon::visit_roots`.

## Returns, unwinds, traces

`OP_RETURN` closes the frame's still-open boxes (innermost-first, same as
the old `~VMFrame2`), pops the record, writes the return value to the
caller's `result_slot` and resumes at `return_cp` — all without leaving the
loop. Exceptions unwind frame-by-frame to the session base, emitting one
`trace_function` entry per frame (the top frame at the faulting
instruction, parents at their call sites, `frames_lost` per record). Trace
offsets are deltas of possibly-stale pointers, which stay correct across
GC — the old code mixed a stale `cp` with a fresh `code_pointer()` and
produced garbage source locations under `--debug-gc`.

## RECURSION-LIMIT

A frame costs ~130 bytes of malloc'd memory (record + vm_stack slots), so
the default limit is 100,000 for flat-only builds (`NATIVE_VM_DEFAULT=0`)
and 10,000 for native-VM builds, whose non-tail native calls still consume
a C stack frame each (see src/state.cpp `AR_DEFAULT_RECURSION_LIMIT`).
ack/takl/cpstak/ctak from the ecraven suite all complete under the flat
convention; ack:3:12 peaks at 131072 vm_stack slots (~1MB).

Caveat: the cached `state.recursion_limit` is synced by `set_global_value`
and by `set-top-level-value!`; a plain Scheme `set!` of `RECURSION-LIMIT`
still bypasses the cache (pre-existing; see [[Performance Reports]] notes
and the flatcall NOTES).

## Native VM interplay

The DynASM core (src/vm-native-x64.cpp.dasc) predates the flat convention:
it still re-enters C per call and maintains `vm_depth` itself. Mixed builds
work because the native core always allocates its frames fresh at
`vm_stack_used` and zero-fills them, which satisfies the high-water
invariant. Ticket `are-8qgc` is the follow-up to teach the native core to
push/pop `VMCallFrame` records in asm, with the C++ loop as the reference
implementation.
