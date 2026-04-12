---
id: aov-b2nv
status: closed
deps: []
links: []
created: 2026-04-12T20:31:57Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T22: Skip full VM2_RESTORE after provably non-moving calls


## Notes

**2026-04-12T20:32:05Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:35:24Z**

Experiment result: not kept. Skipped VM2_RESTORE after OP_APPLY only when neither GC allocations nor VM stack-base changes occurred during the callee. Single fib perf-report looked slightly positive (26.468537s -> 26.383101s wall), but the full suite regressed materially/noisily in the wrong direction: fib 26.0s -> 28.0s, tak 12.0s -> 12.0s, earley 20.0s -> 20.0s, nboyer 15.0s -> 15.0s, peval 42.0s -> 43.0s. Reverted.
