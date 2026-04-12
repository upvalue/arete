---
id: feb-q23r
status: closed
deps: []
links: []
created: 2026-04-12T16:35:00Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Raise or disable RECURSION-LIMIT for ack benchmark

ack exceeds RECURSION-LIMIT 1500 (Arete's non-tail recursive call cap — see src/vm.cpp:151, src/state.cpp:92). The cap is exposed as a runtime flag, so the prelude should (set! RECURSION-LIMIT <larger>) before the benchmark body, or the harness should bump it. Note: ack 3 12 genuinely needs a deep call stack; picking a limit is a judgement call. 50k should comfortably cover it.

## Acceptance Criteria

ack PASSes or TIMEOUTs under CPU_LIMIT=30 without hitting RECURSION-LIMIT.


## Notes

**2026-04-12T16:47:52Z**

Prelude bumps RECURSION-LIMIT to 75000 via set-top-level-value!. Also needed: the benchmark script now runs 'ulimit -s unlimited' because each Scheme frame eats a C-stack frame and the default 8 MiB cap SIGSEGVs before 75k. ack now PASSES (takes ~50s wall-clock, so TIMEOUTs under CPU_LIMIT<=45 — but that's a performance issue, not a breakage).
