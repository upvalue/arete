---
id: aov-qfo3
status: closed
deps: []
links: []
created: 2026-04-12T19:51:28Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T10: Reduce fixed apply_vm entry and hot opcode overhead


## Notes

**2026-04-12T19:51:41Z**

Baseline north-star: boot=0.234s, bootstrap-and-psyntax=0.984s, fib=27.0s, tak=12.0s, earley=20.0s, nboyer=15.0s, peval=42.0s, gcbench=CRASHED. Perf: boot VM 23.6ms/255.9ms (9.2%), fib VM 28.184s/28.187s (99.99%).

**2026-04-12T20:01:07Z**

Experiment result: not kept. Patch combined cheaper GLOBAL_GET, direct upvalue vector fill in CLOSE_OVER, and a VM fast path in OP_APPLY. Full baseline comparison: boot 0.234s -> 0.240s, bootstrap-and-psyntax 0.984s -> 0.994s, fib 27.0s -> 27.0s, tak 12.0s -> 12.0s, earley 20.0s -> 20.0s, nboyer 15.0s -> 16.0s, peval 42.0s -> 44.0s, gcbench still CRASHED. Perf-only signal was mildly positive (fib wall 28.1868s -> 28.1251s; boot wall 255.9ms -> 246.2ms) but suite-level wins were not convincing enough to justify keeping the complexity.
