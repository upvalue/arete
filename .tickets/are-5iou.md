---
id: are-5iou
status: closed
deps: []
links: []
created: 2026-04-18T20:41:15Z
type: task
priority: 2
assignee: Phil
tags: [perf, compiler, autoresearch]
---
# Exp 3: OP_JUMP_IF_NULL fusion

Fuse null?+conditional-jump pattern into OP_JUMP_IF_NOT_NIL / OP_JUMP_IF_NIL single-opcode jumps, replacing the (push-immediate '(); eq?; jump-when-pop L) sequence compile-if emits after (null? X) and (not (null? X)). Wired through interpreter, shared native-VM dispatch blob, eligibility check, and per-function JIT.


## Notes

**2026-04-18T20:41:26Z**

Implementation: arete.hpp (opcodes 36/37), src/vm.cpp (dispatch), src/vm-native-x64.cpp.dasc (DynASM stubs), src/vm-native.cpp (eligibility), src/compile-x64.cpp.dasc (JIT codegen+label gen), scheme/compiler.scm (compile-if detection).

Fused sites in compiled code:
  bootstrap: ~95 (82 NN + 13 N)
  peval:     16 (14 NN + 2 N)
  browse:    13 (13 NN)
  nboyer:    11 (11 NN)

Benchmarks (3 runs vs scratch/compiler-baseline-2026-04-18-peval.json):
  browse: -4.21% / -8.67% / -7.54%  (consistent real improvement)
  nboyer: -2.80% / -6.09% / -2.40%  (variable, trending better)
  peval:  -1.15% / -2.11% / -2.37%  (consistent but within noise)
  geomean:-2.73% / -5.66% / -4.13%

Decision: KEPT as commit dcf1861.

The signal is modest but real and consistent. browse (a list-walk benchmark)
benefits most; peval barely moves, meaning the hot path there is not the
null?+branch pattern (likely pair? check / cdr / closure call dominating).

Observation for the next agent: if peval is the target, instrument its hot
functions directly — the null?+jump isn't where the time goes. Likely
candidates are the generic function-call sequence, pair? type-checks on
cdr-chains, or the upvalue/closure access in partial-evaluator helpers.
