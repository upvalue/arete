---
id: aog-il5a
status: open
deps: []
links: []
created: 2026-04-12T19:56:55Z
type: task
priority: 1
assignee: Phil
parent: aog-3dp0
tags: [gc, optimization]
---
# GC experiment: tune semispace growth and reuse policy

Experiment with the semispace collector's growth/reuse policy in collect(). The current collector doubles aggressively and sometimes discards the inactive semispace after growth. Test whether a different reuse strategy or growth trigger improves the archived GC benchmark set without unacceptable memory growth.


## Notes

**2026-04-12T20:24:34Z**

Variant v1 rejected. Change: after a heap growth, keep the inactive semispace for reuse instead of deleting it immediately.
Artifacts: scratch/2026-04-12-gc-aog-il5a-v1/manifest.json
Results vs scratch/2026-04-12-gc-aog-rw9e-clean:
- gcbench 12.0s -> 12.0s
- mperm 35.0s -> 36.0s
- nboyer PASS 15.0s -> CRASHED
- earley PASS 20.0s -> CRASHED
- destruc 21.0s -> 21.0s
- peval 43.0s -> 42.0s
- nqueens 62.0s -> 61.0s
Decision: do not keep this variant. Retaining the grown semispace changes runtime stability in a way that is not acceptable.
