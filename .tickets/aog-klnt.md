---
id: aog-klnt
status: closed
deps: []
links: []
created: 2026-04-12T19:56:55Z
type: task
priority: 1
assignee: Phil
parent: aog-3dp0
tags: [gc, optimization]
---
# GC experiment: reduce zero-fill cost on bump allocation

Investigate whether allocator-side memset in GCSemispace::allocate is doing more work than necessary. Measure a conservative change that preserves object invariants but avoids unnecessary bulk clearing on hot allocation paths if possible. Compare against scratch/2026-04-12-gc-baseline and reject if complexity or correctness risk outweighs speedup.


## Notes

**2026-04-12T20:30:47Z**

Experiment rejected. Variant v1 skipped payload zeroing for FLONUM, CHARACTER, SYMBOL, RENAME, EXCEPTION, and VECTOR allocations by introducing an uninitialized allocation path.
Artifacts: scratch/2026-04-12-gc-aog-klnt-v1/manifest.json
Compared against scratch/2026-04-12-gc-aog-rw9e-clean:
- gcbench 12.0s -> 12.0s
- mperm 35.0s -> 36.0s
- nboyer 15.0s -> 15.0s
- earley 20.0s -> 20.0s
- destruc 21.0s -> 21.0s
- peval 43.0s -> 43.0s
- nqueens 62.0s -> 64.0s
- Arete workloads mixed: boot 0.239594s -> 0.236076s, bootstrap-and-psyntax 1.004478s -> 1.013900s
Decision: do not keep. The patch did not improve GC-sensitive benchmarks and regressed mperm/nqueens plus one native workload.
