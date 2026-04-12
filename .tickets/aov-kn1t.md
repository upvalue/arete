---
id: aov-kn1t
status: closed
deps: []
links: []
created: 2026-04-12T21:30:56Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# Recycle VM frames on tail calls

Rework OP_APPLY_TAIL so VM-to-VM tail calls can reuse the active frame instead of copying through state.temps and re-entering the full apply_vm prologue. Baseline north star: scratch/bench-20260412T213000Z-baseline plus scratch/perf-20260412T213000Z/{boot,tak}.json.


## Notes

**2026-04-12T21:34:18Z**

Baseline 2026-04-12:
- Arete workloads: boot 236.714ms, bootstrap-and-psyntax 1.024648s (scratch/bench-20260412T213000Z-baseline).
- R7RS subset: tak 12.434s, destruc 20.936s, nboyer 15.555s, earley CRASHED.
- Perf tak: wall 12571000us, vm 12569477us (99.99%), 611458736 vm calls, no GC collections.
- Explorer design note: recycle VM-to-VM tail calls in OP_APPLY_TAIL conservatively for non-rest/non-keyword cases, closing caller upvalues before overwriting locals and keeping the existing slow path as fallback.

**2026-04-12T21:38:17Z**

Result for this run: no patch landed; closing as unimplemented/deferred.
- Two subagent passes converged on the same safe subset: recycle only non-variable-arity VM-to-VM tail calls, explicitly closing caller upvalues before locals are overwritten and keeping the current slow path as fallback.
- No code change was produced in this run, so there are no benchmark numbers beyond the baseline.
- Reason for closing: implementation complexity/risk around frame rebinding and upvalue lifetime was too high relative to the progress made in this iteration.
- Follow-up path remains valid if we revisit VM tail-call recycling later.

**2026-04-12T21:53:49Z**

Resuming this experiment for an actual implementation attempt.
Scope remains conservative: recycle only non-variable-arity VM-to-VM tail calls, preserve the existing slow path for all other cases, and preserve frame trace accounting.

**2026-04-12T22:05:50Z**

Implemented conservative VM tail-call frame recycling in src/vm.cpp. Validation: make bin/arete, make heap.boot, closure smoke test bin/arete heap.boot --eval '(((lambda (x) (lambda () x)) 42))', and tests/compiler/tco.scm all passed. 3-run benchmark series: boot 241.970ms vs 236.714ms baseline (+2.22%); bootstrap-and-psyntax 1.003186s vs 1.024648s baseline (-2.09%); tak 12.209s vs 12.434s baseline (-1.81%).

**2026-04-12T22:09:04Z**

Follow-up implementation attempt rejected.
- Added an even safer recycle gate: exact-arity VM-to-VM tail calls only, with both caller and callee requiring zero VM upvalues.
- This fixed `./bin/arete --perf-report ... boot.scm`, which had previously segfaulted under the broader attempt.
- It did not preserve the earlier speedups. Fresh measurements vs scratch/bench-20260412T213000Z-baseline:
  - boot 246.760ms vs 236.714ms (+4.24%)
  - bootstrap-and-psyntax 1.038923s vs 1.024648s (+1.39%)
  - tak 12.993s vs 12.434s (+4.50%)
  - destruc 20.075s vs 20.936s (-4.11%)
  - nboyer 15.432s vs 15.555s (-0.79%)
  - earley CRASHED (same as baseline)
- Perf check for tak in scratch/aov-kn1t-safe/tak-perf.json showed the same VM call count (611,458,736) but higher VM time than baseline, so the change is not a real VM win in this safe form.
- Conclusion: reject and revert. The broader variant had better benchmark numbers but was not correct under perf-report / additional validation.

**2026-04-12T22:09:23Z**

Posterity commits for the rejected safe variant:
- experiment commit: 211682f (vm: try conservative tail-call frame recycling)
- revert commit: 906b7d3 (Revert "vm: try conservative tail-call frame recycling")
