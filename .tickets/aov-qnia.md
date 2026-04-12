---
id: aov-qnia
status: closed
deps: []
links: []
created: 2026-04-12T19:57:43Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T12: Stop zeroing unused VM temp stack slots on frame entry


## Notes

**2026-04-12T20:01:07Z**

Experiment result: not kept. Limited frame-entry zeroing to locals only (stopped clearing temp stack and upvalue area).  still passed 81/81, but the VM benchmark suite showed earley crashing with SIGSEGV after ~23.8s. Other numbers were fib 27.0s -> 27.0s, tak 12.0s -> 12.0s, nboyer 15.0s -> 15.0s, peval 42.0s -> 42.0s, gcbench still CRASHED. Conclusion: some VM temp slots are read before explicit writes on real workloads, so full-frame zeroing is semantically relied upon today.
