---
id: aog-lgjr
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
# GC experiment: remove avoidable symbol-table root update overhead

Test whether GC root walking can avoid redundant symbol-table work. Current root visitation updates root symbols with state.symbol_table->at(x->first)=v while already iterating that entry. Change should be minimal, validated against scratch/2026-04-12-gc-baseline, and kept only if it produces measurable benefit without complicating collector correctness.


## Notes

**2026-04-12T20:03:13Z**

Experiment rejected. Change: replace state.symbol_table->at(x->first)=v with x->second=v during GC root walking.
Artifacts: scratch/2026-04-12-gc-aog-lgjr/manifest.json
Comparison against baseline scratch/2026-04-12-gc-baseline:
- Arete workloads improved slightly: boot 0.241798s -> 0.239594s, bootstrap-and-psyntax 1.051705s -> 1.004478s
- R7RS GC-oriented set was flat to worse overall: mperm 36.0s -> 39.0s, nboyer 15.0s -> 15.0s, earley 20.0s -> 20.0s, destruc 21.0s -> 21.0s, peval 43.0s -> 42.0s, nqueens 63.0s -> 63.0s
- gcbench remained blocked by missing define-record-type
Decision: do not keep. Regression on mperm outweighs small wins elsewhere.
