---
id: are-fsnn
status: open
deps: []
links: []
created: 2026-04-18T21:24:52Z
type: task
priority: 2
assignee: Phil
tags: [perf, compiler, autoresearch]
---
# Exp 3d: OP_JUMP_IF_EQ_IMM fusion

Fuse (if (eq? X 'LIT) ...) into OP_JUMP_IF_EQ_IMM / OP_JUMP_IF_NOT_EQ_IMM for safe-immediate LITs (symbols, fixnums, booleans, '()). Generalizes exp 3 (null?) and exp 3b (pair?). Kept.


## Notes

**2026-04-18T21:24:56Z**

Three benchmark runs vs scratch/compiler-baseline-2026-04-18-peval.json:
  browse:  -9.16% / -8.91% / -9.70%  (strongly improved)
  nboyer:  -6.70% / -6.65% / -8.16%  (strongly improved)
  peval:   -1.62% / -2.52% / -3.14%  (within noise but consistent)
Geomean:  -5.88% / -6.07% / -7.04%.

Fusion sites: bootstrap=75, peval=18, browse=1, nboyer=0. nboyer/browse still improved because the expander/compiler libraries (shared via heap.boot) contain many fusion sites.

Commit: a1ff0eb
