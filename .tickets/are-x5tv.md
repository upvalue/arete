---
id: are-x5tv
status: open
deps: []
links: []
created: 2026-04-18T20:53:57Z
type: task
priority: 2
assignee: Phil
tags: [perf, compiler, autoresearch]
---
# Exp 3b: OP_JUMP_IF_PAIR fusion

Mirrors exp 3 (commit dcf1861) fusion pattern for pair?. Adds OP_JUMP_IF_NOT_PAIR / OP_JUMP_IF_PAIR opcodes that test top-of-stack for pair-ness, pop, and branch — replacing the type-check+jump-when-pop sequence emitted by compile-if after (pair? X) and the not-variant.


## Notes

**2026-04-18T20:54:05Z**

Implemented as commit 8ef9e5e.

Files touched:
  arete.hpp                  (+8 lines, new opcodes 38/39)
  src/vm.cpp                 (+25 lines, interpreter dispatch)
  src/vm-native-x64.cpp.dasc (+43 lines, dispatch stubs + DISPATCH_OPS + table size)
  src/vm-native.cpp          (+5 lines, two eligibility entries)
  src/compile-x64.cpp.dasc   (+45 lines, label-generation + JIT codegen)
  scheme/compiler.scm        (+21 lines, insn-list, stack-effects, compile-if cond clauses; re-uses if-primitive-call-match from exp 3)

Pair test semantics: bits != 0 AND (bits & 3) == 0 AND heap->get_type() == PAIR. Returns #f for '() (no crash).

Fusion sites: 34 during bootstrap, 3 peval, 2 browse, 8 nboyer.

Three runs vs scratch/compiler-baseline-2026-04-18-peval.json:
  browse:  -11.12% / -8.03% / -10.48%
  nboyer:   -7.89% / -5.53% /  -5.67%
  peval:    -2.13% / -2.18% /  -2.52%
Geomean:    -7.12% / -5.28% /  -6.28%

Kept. Browse & nboyer consistently IMPROVED past 5% noise floor; peval within noise but consistent three-way downtrend. Baseline was 2.042 / 10.495 / 33.749 s.
