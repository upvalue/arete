---
id: aov-m3rw
status: closed
deps: []
links: []
created: 2026-04-12T20:21:37Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T17: Remove duplicate symbol-value lookup in OP_GLOBAL_GET


## Notes

**2026-04-12T20:21:47Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:22:42Z**

Experiment result: not kept. Removed duplicate symbol value lookup in OP_GLOBAL_GET by loading Symbol::value once and reusing it. Smoke tests passed, but fib perf-report regressed from 26.468537s wall / 26.466860s VM to 27.089795s wall / 27.088163s VM, so the change was reverted immediately without a full suite run.

**2026-04-12T20:24:08Z**

Reverted immediately after negative fib perf signal; worktree restored before next experiment.
