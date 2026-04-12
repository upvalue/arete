---
id: aov-culm
status: closed
deps: []
links: []
created: 2026-04-12T20:43:04Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T26: Simplify grow_stack and argv relocation in apply_vm


## Notes

**2026-04-12T20:43:11Z**

Baseline for this experiment is current kept state with cached recursion limit plus OP_POP cleanup: boot 0.235422s, bootstrap-and-psyntax 1.011845s, fib 25.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 41.0s. Fib perf-report: 26.321734s wall / 26.319679s VM.

**2026-04-12T20:44:20Z**

Experiment result: not kept. Simplified apply_vm stack growth to call grow_stack once and only repair argv when the grow actually reallocated and argv pointed into the old VM stack. Direct smoke test passed, but the fib perf-report regressed and changed allocation/apply counters (wall 26.321734s -> 26.534658s; allocations 45510 -> 58113; apply_calls 4049 -> 5231). Reverted immediately.
