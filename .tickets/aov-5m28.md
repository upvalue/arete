---
id: aov-5m28
status: closed
deps: []
links: []
created: 2026-04-12T20:30:08Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm, build]
---
# T21: Compile src/vm.cpp at -O3 and compare


## Notes

**2026-04-12T20:30:17Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:31:47Z**

Experiment result: not kept. Compiled only src/vm.cpp at -O3 via a target-specific makefile override. After forcing a real rebuild, fib perf-report regressed from 26.468537s wall / 26.466860s VM to 26.958081s wall / 26.955945s VM, so the build tweak was reverted immediately without a full suite run.
