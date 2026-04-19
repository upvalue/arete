---
id: are-mczo
status: open
deps: []
links: []
created: 2026-04-18T21:36:44Z
type: task
priority: 2
assignee: Phil
tags: [perf, compiler, autoresearch]
---
# Exp 7: fused cadr/cddr/caar opcodes

Single-dispatch fused composite-pair-accessor opcodes (OP_CADR, OP_CDDR, OP_CAAR, OP_CDAR, OP_CADDR) replacing the 2- or 3-op OP_CAR/OP_CDR sequences from da07122. Each op does 2 or 3 PAIR-type-checked dereferences inline with a single opcode dispatch.


## Notes

**2026-04-18T21:36:50Z**

KEPT. Three runs vs scratch/compiler-baseline-2026-04-18-peval.json:
  browse:2000  -7.30% / -7.88% / -9.89%  (IMPROVED)
  nboyer:5:1   -5.75% / -8.08% / -9.78%  (IMPROVED)
  peval:2000   -1.27% / -2.85% / -3.27%  (within noise but consistent)
  geomean      -4.81% / -6.30% / -7.70%

Grep counts: peval has 61 sites, nboyer 13, browse 2. Despite peval having
by far the most sites, its signal is small — confirming prior agent's
observation that peval's hot time is closure-call / env-lookup / apply-tail,
not the accessor steps.

Files touched: arete.hpp, scheme/compiler.scm, src/vm.cpp, src/vm-native.cpp,
src/vm-native-x64.cpp.dasc, src/compile-x64.cpp.dasc. +265 -13 lines.
Commit: fdffb8f
