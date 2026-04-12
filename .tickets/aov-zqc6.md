---
id: aov-zqc6
status: closed
deps: []
links: []
created: 2026-04-12T20:26:35Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T20: Special-case small-frame zeroing on VM entry


## Notes

**2026-04-12T20:26:44Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:29:54Z**

Experiment result: not kept. Replaced memset with direct Value() stores for small clear sizes 0..4, preserving full zeroing semantics. Fib perf-report improved slightly (26.468537s -> 26.313714s wall), but the full suite regressed or was inconclusive in the wrong direction: fib 26.0s -> 27.0s, tak 12.0s -> 12.0s, earley 20.0s -> 20.0s, nboyer 15.0s -> 16.0s, peval 42.0s -> 42.0s. Also introduced a compiler warning about memset on non-trivial Value for larger cases. Reverted.
