---
id: are-bs52
status: closed
deps: []
links: []
created: 2026-04-18T21:53:55Z
type: task
priority: 2
assignee: Phil
tags: [perf, compiler, autoresearch]
---
# Exp 8: known-arity apply fast path (fused callee-load + apply)

Fused local/global callee-load with apply-tail/apply into 4 new opcodes (OP_APPLY{_TAIL}_{GLOBAL,LOCAL}). Saves one dispatch + one stack push/pop per fused call site. Kept: geomean ~-10.5%, peval ~-11.3%, browse ~-17%, nboyer ~-3% (noise). All 82 tests pass.


## Notes

**2026-04-18T21:54:13Z**

Implemented the narrower "don't push callee on operand stack" variant, not the
full "known-arity skips argc-eq" variant. Callee is still a dynamic dispatch
via procedure_addr — the only savings are one bytecode dispatch + one
push/pop per call. The fact that this still bought ~11% on peval and ~17% on
browse says most of the remaining apply overhead is in procedure_addr (indirect
call + type checks) and frame setup, not in the argc check.

Scope:
- arete.hpp: 4 new OP_* enum values (47-50)
- src/vm.cpp: 4 new VM_CASE handlers (~180 lines)
- scheme/compiler.scm: fused-callee detection in compile-apply/generic
  (~30 lines) + insn-list entries + stack-effect rule

Coverage: fires for any application where (car x) is a bare symbol that
resolves to 'global or 'local via fn-lookup. Upvalues intentionally not
fused (more complex path through f.closure->upvalues). Lambda-in-car,
renames, and non-identifiers fall back to the existing OP_APPLY path.
Named-loop and inline-lambda paths were already handled upstream in
compile-apply before /generic, so they are unaffected.

Native VM (src/vm-native.cpp): new opcodes are NOT in
native_vm_opcode_supported, so any function that uses the fused ops
becomes ineligible for native-vm-install!. The native VM is opt-in and
not in use by default or by the benchmarks, so this is a measured
no-op today. Extending the DynASM core (src/vm-native-x64.cpp.dasc) to
cover the 4 new opcodes is follow-up work if/when the native VM is
promoted to the default path.

Next-agent notes on where apply cost still sits:
1. apply_vm's VMFrame2 constructor + grow_stack + memcpy-of-locals +
   zero-out. Every non-tail VMFUNCTION call pays this.
2. The procedure_addr indirect call for CFUNCTION targets. Can't avoid
   without knowing the target is VMFUNCTION at compile time.
3. On the TAIL path, state.temps.clear() + state.temps.insert() is a
   heap allocation round-trip. For fixed-arity self-recursion this is
   wasted; could be a direct in-place copy. That is what the
   compile-x64/native-vm fast path for OP_APPLY_TAIL already does — we
   could do similar in apply_vm for OP_APPLY_TAIL_LOCAL when callee
   matches the current frame's vfn.
