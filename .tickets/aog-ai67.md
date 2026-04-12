---
id: aog-ai67
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
# GC experiment: specialize copy/scan hot paths for common object shapes

Investigate reducing collector overhead in the Cheney copy/scan loop by tightening hot paths for common object layouts such as PAIR, SYMBOL, CLOSURE, and VECTOR_STORAGE. Keep the change local to src/gc.cpp and validate with before/after numbers against scratch/2026-04-12-gc-baseline.


## Notes

**2026-04-12T20:20:55Z**

Experiment rejected. Change: reorder reject paths in GCSemispace::copy() so already-forwarded objects short-circuit before the to-space membership check.
Artifacts: scratch/2026-04-12-gc-aog-ai67/manifest.json
Compared against scratch/2026-04-12-gc-aog-rw9e-clean:
- gcbench 12.0s -> 12.0s
- mperm 35.0s -> 36.0s
- nboyer 15.0s -> 15.0s
- earley 20.0s -> 20.0s
- destruc 21.0s -> 21.0s
- peval 43.0s -> 44.0s
- nqueens 62.0s -> 63.0s
- Arete workloads regressed slightly: boot 0.239594s -> 0.239664s, bootstrap-and-psyntax 1.004478s -> 1.011571s
Decision: do not keep. The change is too small to help and modestly hurts the heavier workloads.
