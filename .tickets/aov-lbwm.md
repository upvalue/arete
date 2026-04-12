---
id: aov-lbwm
status: closed
deps: []
links: []
created: 2026-04-12T20:20:14Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T16: Isolate direct apply_vm fast path for VM-to-VM calls


## Notes

**2026-04-12T20:20:21Z**

Baseline for this experiment is current kept state with cached recursion limit: fib perf-report 26.468537s wall / 26.466860s VM, full suite boot 0.238299s, bootstrap-and-psyntax 0.978386s, fib 26.0s, tak 12.0s, earley 20.0s, nboyer 15.0s, peval 42.0s.

**2026-04-12T20:21:22Z**

Experiment result: not kept. Isolated only the direct apply_vm fast path inside OP_APPLY for VMFUNCTION/CLOSURE callees whose procedure_addr is still apply_vm. Smoke tests passed, but fib perf-report regressed from 26.468537s wall / 26.466860s VM to 26.947299s wall / 26.945714s VM, so the change was reverted immediately without a full suite run.
