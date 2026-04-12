---
id: aov-dm18
status: closed
deps: []
links: []
created: 2026-04-12T21:47:14Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# Outline cold VM arithmetic slow paths

Move generic numeric fallback and exception-heavy handling out of apply_vm for arithmetic/comparison opcodes so the dispatch loop's hot path stays smaller. Baseline north star: scratch/bench-20260412T213000Z-baseline plus scratch/perf-20260412T213000Z/{boot,tak}.json.


## Notes

**2026-04-12T21:47:35Z**

Baseline 2026-04-12:
- Arete workloads: boot 236.714ms, bootstrap-and-psyntax 1.024648s (scratch/bench-20260412T213000Z-baseline).
- R7RS subset: tak 12.434s, destruc 20.936s, nboyer 15.555s, earley CRASHED.
- Perf tak: wall 12571000us, vm 12569477us (99.99%), 611458736 vm calls, no GC collections.
- Explorer scoping: outline cold fallback/error handling first for OP_ADD, OP_SUB, OP_LT and small guard failures, keeping VM2_RESTORE/exception flow in the caller.

**2026-04-12T21:49:43Z**

Attempt recorded in commits 41751ff and e6278dc (revert).

Result: rejected.
- Change outlined mixed numeric fallback and exception-heavy type-check paths from OP_ADD / OP_SUB / OP_LT plus pair/fixnum guard errors into cold helpers, keeping the fixnum fast paths inline.
- Validation: rebuilt, bootstrap save-image passed, compiler closure1/closure3 harness checks passed.
- Benchmark series in scratch/perf-aov-dm18: boot 243.135ms vs 236.714ms baseline, bootstrap-and-psyntax 1.023832s vs 1.024648s baseline, tak 12.858s vs 12.434s baseline.
- Conclusion: cold outlining did not compensate for the extra call/branch structure; tak regressed materially while project-native workloads were flat-to-worse.
