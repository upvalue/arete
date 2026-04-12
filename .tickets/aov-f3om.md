---
id: aov-f3om
status: closed
deps: []
links: []
created: 2026-04-12T20:46:45Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm, compiler]
---
# T28: Add current-function opcode for self recursion


## Notes

**2026-04-12T20:55:34Z**

T28 current-function opcode for self recursion was rejected after measurement.

Retained baseline before experiment:
- boot 0.235422s
- bootstrap-and-psyntax 1.011845s
- fib 25.0s
- tak 12.0s
- earley 20.0s
- nboyer 15.0s
- peval 41.0s

Experiment results from scratch/vm-exp-aov-f3om-2026-04-12:
- boot 0.246244s
- bootstrap-and-psyntax 1.014949s
- fib 26.0s
- tak 12.0s
- earley 20.0s
- nboyer 14.0s
- peval 43.0s

Conclusion: despite helping some self-recursive cases versus the original baseline, it regressed the current retained tree on fib, boot, and peval. It also needed extra compiler machinery and initially broke hygienic renamed identifiers before narrowing the match. Reverted.
