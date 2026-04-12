---
id: aov-ol5a
status: closed
deps: []
links: []
created: 2026-04-12T20:55:55Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm, compiler]
---
# Fuse primitive compare and conditional branch opcodes in the VM


## Notes

**2026-04-12T21:03:16Z**

T29 fused < and conditional-branch opcode was rejected and reverted.

Experiment artifacts: scratch/vm-exp-aov-ol5a-2026-04-12

Results versus retained baseline:
- fib 25.0s -> 26.0s
- tak 12.0s -> 12.0s
- earley 20.0s -> 20.0s
- nboyer 15.0s -> 15.0s
- peval 41.0s -> 42.0s
- boot 0.235422s -> CRASHED
- bootstrap-and-psyntax 1.011845s -> CRASHED

The narrowed transform passed 81/81 tests, but it still regressed fib and peval while breaking the bootstrap workloads. Reverted.
