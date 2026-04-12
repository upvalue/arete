---
id: aov-la42
status: closed
deps: []
links: []
created: 2026-04-12T20:15:17Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm, compiler]
---
# T15: Improve fixnum primitive selection for VM arithmetic


## Notes

**2026-04-12T20:15:47Z**

Compiler inspection: generic +, -, and < are emitted as VM primitives, but there is no obvious type inference path turning fib-style arithmetic into OP_FX_* automatically. Experiment will target runtime fast paths in OP_ADD / OP_SUB / OP_LT for argc=2 fixnum-heavy calls.

**2026-04-12T20:20:06Z**

Experiment result: not kept. Added binary-fixnum fast paths for generic OP_ADD / OP_SUB / OP_LT and removed restore overhead on non-allocating - paths. Early fib perf improved from 26.468537s to 25.009470s, but full benchmark comparison regressed elsewhere: boot 0.238299s -> 0.237528s, bootstrap-and-psyntax 0.978386s -> 1.024934s, fib 26.0s -> 25.0s, tak 12.0s -> 13.0s, earley 20.0s -> 20.0s, nboyer 15.0s -> 15.0s, peval 42.0s -> 44.0s, gcbench still CRASHED. Boot perf-report was neutral-to-slightly-better, but suite-level regressions make this a reject.
