---
id: aov-3jwl
status: closed
deps: []
links: []
created: 2026-04-12T20:25:25Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T19: Special-case tiny-arity locals copy on VM entry


## Notes

**2026-04-12T20:25:33Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:26:23Z**

Experiment result: not kept. Replaced tiny-arity locals memcpy with direct assignments for init_count 0/1/2. Smoke tests passed, but fib perf-report was effectively flat-to-worse: 26.468537s wall / 26.466860s VM -> 26.531921s wall / 26.530293s VM. Reverted immediately without a full suite run.
