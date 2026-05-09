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

