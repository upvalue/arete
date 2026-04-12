---
id: aov-2sov
status: closed
deps: []
links: []
created: 2026-04-12T20:44:35Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T27: Cache VM constants pointer locally in apply_vm


## Notes

**2026-04-12T20:44:42Z**

Baseline for this experiment is current kept state with cached recursion limit plus OP_POP cleanup: boot 0.235422s, bootstrap-and-psyntax 1.011845s, fib 25.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 41.0s. Fib perf-report: 26.321734s wall / 26.319679s VM.

**2026-04-12T20:46:15Z**

Experiment result: not kept. Cached the current VMFunction constants pointer locally in apply_vm and refreshed it in the restore macros, then switched constant-using opcodes to use the local pointer. Direct global-read smoke test passed, but fib perf-report regressed (26.321734s -> 26.823818s wall) and again changed allocation/apply counters (45510/4049 -> 58113/5231). Reverted immediately.
