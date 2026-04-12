---
id: aov-0gte
status: closed
deps: []
links: []
created: 2026-04-12T21:30:52Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# VM fast path for simple function entry

Split apply_vm entry into a fast path for exact-arity functions without rest args, keyword args, or upvalues, leaving the current generic path as fallback. Baseline north star: scratch/bench-20260412T213000Z-baseline plus scratch/perf-20260412T213000Z/{boot,tak}.json.


## Notes

**2026-04-12T21:31:32Z**

Baseline 2026-04-12:
- Arete workloads (scratch/bench-20260412T213000Z-baseline): boot 236.714ms best-of-3, bootstrap-and-psyntax 1.024648s best-of-3.
- R7RS subset: tak 12.434s, destruc 20.936s, nboyer 15.555s, earley CRASHED.
- Perf boot: wall 248180us, vm 22154us (8.93%), interpreter 142227us (57.31%), gc 7861us.
- Perf tak: wall 12571000us, vm 12569477us (99.99%), 611458736 vm calls, no GC collections.

**2026-04-12T21:33:51Z**

Attempted change recorded in commits 7f4360f and 1fd28f3 (revert).

Result: rejected.
- Patch: added a conservative simple-entry gate in src/vm.cpp for exact-arity VM functions with no upvalues and OP_ARGC_EQ prologue, then skipped the eager upvalue-allocation path in that case.
- Measurement from scratch/perf-aov-0gte: boot 245.945ms best-of-3 vs baseline 236.714ms; tak 12.707s vs baseline 12.434s.
- Conclusion: the gate only removed work for zero-upvalue functions, so the extra branch/opcode peek on every VM entry outweighed the benefit.
