---
id: aov-a3kr
status: closed
deps: []
links: []
created: 2026-04-12T20:36:50Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T24: Make OP_POP only move the stack pointer


## Notes

**2026-04-12T20:36:58Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:40:12Z**

Experiment result: keep. Changed OP_POP from  to , removing an unnecessary dereference and the compiler warning. Full benchmark comparison against the prior kept baseline: boot 0.238299s -> 0.235422s, bootstrap-and-psyntax 0.978386s -> 1.011845s, fib 26.0s -> 25.0s, tak 12.0s -> 12.0s, earley 20.0s -> 20.0s, nboyer 15.0s -> 15.0s, peval 42.0s -> 41.0s, gcbench still CRASHED. Perf-report: fib wall 26.468537s -> 26.321734s, VM exclusive 26.466860s -> 26.319679s; boot wall 250.485ms -> 243.464ms. Small code simplification with measurable net win.
