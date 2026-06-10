---
id: are-8qgc
status: open
deps: [are-vfam]
links: []
created: 2026-05-09T01:50:45Z
type: task
priority: 2
assignee: Phil
tags: [perf, native-vm, vm-opt]
---
# Finish native VM: move call/return machinery into asm

Follow-up to the aonv-2taa epic postmortem. The DynASM dispatch core (src/vm-native-x64.cpp.dasc) ships behind NATIVE_VM_DEFAULT=1 and covers 49 opcodes, but it is a system-level perf regression on real workloads (psyntax +7%, fib +43%) — measurable gain near zero on the r7rs suite.

Empirical state on peval (regressing 1.84x vs femtolisp): native VM IS engaged, but native-vm-stats shows only 31% of calls take the native fast path; 69% fall back through C apply_vm. Toggling NATIVE_VM_DEFAULT=0 changes peval by 0.4s — noise.

Two reasons the native VM is not winning, and what fixing them looks like:

  A. Native fast path replicates apply_vm's prologue inline. The zero-fill loop at vm-native-x64.cpp.dasc:1167-1173 is the same `// TODO: Necessary?` work as the C++ memset. Locals/STACK/CP setup follows. Saves only the SysV call boundary, not the real work. Step 1 in are-vfam slims that prologue and benefits both paths; this ticket is the larger structural step that keeps the dispatch loop in asm across calls — never returning to C between Scheme calls. Postmortem one-liner: 'LuaJIT runs the entire call/return machinery in assembly — it never returns to C during dispatch. Arete still re-enters C on every Scheme call via apply_native_vm.'

  B. C-fallback rate is 69% of peval's calls. Diagnose why before deciding scope. Hypothesis: a mix of (a) closures whose underlying VMFunction has an opcode native_vm_opcode_supported() doesn't yet cover, and (b) calls to CFunction builtins. Need a per-callee histogram. native-vm-install-tree! exists for forced installation of user code; (a) and (b) are different fixes.

Prerequisite to making this ticket worth doing: complete are-vfam first. If slimming the prologue alone closes most of the gap, the structural rewrite has worse effort/payoff. If after are-vfam there's still a substantial gap and native fast path is hot, this ticket becomes priority-1.

Scope for an actual rewrite (post-prerequisite):
  - Move apply / apply-tail / return frame setup entirely into asm; eliminate the SysV-boundary path for VM->VM calls.
  - Maintain a separate small fast-call buffer for non-VM callees that still avoids re-entering apply_vm.
  - Re-evaluate compile-x64 (per-function method JIT, currently #if 0 in src/vm.cpp) once the dispatch core no longer wastes its savings.

Acceptance: peval and nboyer within 10% of femtolisp h2h; psyntax expand within 1% of baseline (no regression); test suite stays green; native-vm-stats apply-c rate < 20% on the r7rs perf suite.


## Notes

**2026-06-10T04:26:35Z**

Re-scoped after are-vfam closed via the flat calling convention (docs/VM Calling Convention.md). The C++ apply_vm now runs all Scheme->Scheme calls in one dispatch loop over explicit VMCallFrame records; profiles show the remaining per-call cost is pure opcode dispatch. This ticket's scope is therefore now: implement push/pop of the same VMCallFrame records in the DynASM core so the asm dispatch loop never returns to C between Scheme calls, using the C++ loop as the proven reference implementation and fallback. The native core currently still re-enters C per call and keeps its own VMFrame2-style prologue (vm-native-x64.cpp.dasc apply_zero_loop) — that prologue should also adopt the high-water invariant (init only locals+boxes, zero growth regions) instead of full-frame zero-fill. Mixed builds remain correct meanwhile because native frames allocate fresh at vm_stack_used and zero-fill (invariant-compatible); gates incl. GC stress + differential pass on NATIVE_VM_DEFAULT=1.

Prerequisite numbers update (this box, best-of-5): flat C core fib 22.17s vs native1 baseline 20.77s — on this machine the old native core HELPS fib (unlike the +43% regression noted above), so combining flat frames with the asm core is the expected next win. fresh flat-native1 sweep numbers land in scratch/flatcall/baseline/flat-native1. Also note NATIVE_VM_DEFAULT=1 builds keep a conservative RECURSION-LIMIT default (10K vs 100K) because native non-tail calls still consume C stack — moving call/return into asm with VMCallFrame records removes that distinction too.

**2026-06-10T04:36:49Z**

Phase 3 sweep results (best-of-5, scratch/flatcall/baseline/): the flat C core now BEATS the NATIVE_VM_DEFAULT=1 build on call-heavy workloads — peval 13.40s vs 15.11s, nboyer 8.11 vs 8.92, tak 9.34 vs 9.91 — while fib (20.95 vs 22.17) and browse still favor the native core's opcode dispatch. flat-native1 vs old native1 is ~neutral (±2%) because natively-dispatched calls bypass apply_vm entirely. Net: the asm core's per-call C re-entry is now a measured regression on the workloads this epic targets; adopting VMCallFrame push/pop in asm is the unlock, and until then NATIVE_VM_DEFAULT=1 is a workload-dependent tradeoff rather than a win.
