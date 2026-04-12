---
id: aov-8brd
status: closed
deps: []
links: []
created: 2026-04-12T20:24:08Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T18: Add binary-fixnum fast path only for OP_LT


## Notes

**2026-04-12T20:24:18Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:25:16Z**

Experiment result: not kept. Added only a binary-fixnum fast path for OP_LT. Smoke test passed, but fib perf-report regressed from 26.468537s wall / 26.466860s VM to 27.169895s wall / 27.167395s VM, so the change was reverted immediately without a full suite run.
