---
id: aov-5gqi
status: closed
deps: []
links: []
created: 2026-04-12T20:40:12Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T25: Skip VM2_RESTORE_GC in non-allocating OP_SUB paths


## Notes

**2026-04-12T20:40:24Z**

Baseline for this experiment is current kept state with cached recursion limit plus OP_POP cleanup: boot 0.235422s, bootstrap-and-psyntax 1.011845s, fib 25.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 41.0s. Fib perf-report: 26.321734s wall / 26.319679s VM.

**2026-04-12T20:41:37Z**

Experiment result: not kept. Added an  flag to OP_SUB and only ran VM2_RESTORE_GC on flonum-allocating paths. Direct subtraction smoke test passed, but fib perf-report regressed and, more importantly, changed allocation counts on the same benchmark: wall 26.321734s -> 27.038839s and allocations 45510 -> 58113. Reverted immediately.
