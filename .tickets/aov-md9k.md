---
id: aov-md9k
status: closed
deps: []
links: []
created: 2026-04-12T21:31:00Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# Make VM upvalue allocation lazy

Stop allocating per-frame Upvalue objects eagerly on function entry when no closure is formed. Explore lazy capture or cheaper closure setup without regressing semantics. Baseline north star: scratch/bench-20260412T213000Z-baseline plus scratch/perf-20260412T213000Z/{boot,tak}.json.


## Notes

**2026-04-12T21:34:38Z**

Explorer scoping result:
- Best first experiment is lazy allocation on first OP_UPVALUE_FROM_LOCAL while keeping the existing per-frame upvalue slot area.
- This preserves sharing between closures from the same activation and avoids entry-time upvalue allocations in functions that never form closures.
- Lower-risk than changing open-upvalue representation or deferring boxing into OP_CLOSE_OVER.

**2026-04-12T21:40:49Z**

Implemented lazy upvalue allocation in src/vm.cpp only. The VM now allocates an Upvalue on first OP_UPVALUE_FROM_LOCAL for a capture slot, caches it in the frame slot, and reuses it for later loads from the same activation. Kept the existing frame teardown path for closing live upvalues. Validation: make bin/arete, make heap.boot, closure smoke test via bin/arete heap.boot --eval '(((lambda (x) (lambda () x)) 42))', and a 3-run benchmark series for boot and tak. Results vs baseline: boot 243.547ms vs 236.714ms, tak 12.474s vs 12.434s. This first cut is functionally correct but slightly slower than baseline.

**2026-04-12T21:41:49Z**

Additional local validation after the initial note showed the first-cut patch is not acceptable.

Attempt recorded in commits 5975f41 and 26b301a (revert).

Broader result set:
- Functional smoke: boot.scm --save-image passed; compiler closure harness checks for closure1/closure3 passed.
- Benchmark series in scratch/perf-aov-md9k: boot improved to 233.487ms vs 236.714ms baseline, tak regressed to 12.72s vs 12.434s baseline, and bootstrap-and-psyntax crashed on the first run (returncode -11).
- Direct standalone bootstrap-and-psyntax rerun did not immediately reproduce the segfault, so the instability looks intermittent, which is still disqualifying for this experiment.

Conclusion: reject and revert. The change likely shifts allocation into a hotter capture path and appears to destabilize the larger psyntax workload.
