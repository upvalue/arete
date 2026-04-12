---
id: aov-9xmd
status: closed
deps: []
links: []
created: 2026-04-12T20:35:40Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T23: Use saved slot/code offsets for OP_APPLY restore


## Notes

**2026-04-12T20:35:52Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:36:35Z**

Experiment result: not kept. Replaced OP_APPLY restore macro usage with a manual restore using saved stack/code offsets. Fib perf-report moved only slightly in the positive direction (26.468537s -> 26.403040s wall), but the effect is too small and noisy to justify keeping extra hand-written restore code without broader validation. Reverted before next experiment.
