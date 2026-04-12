---
id: aog-rw9e
status: closed
deps: []
links: []
created: 2026-04-12T19:57:02Z
type: task
priority: 1
assignee: Phil
parent: aog-3dp0
tags: [gc, benchmarking, compatibility]
---
# GC benchmark enablement: unblock gcbench define-record-type

Enable gcbench to run under Arete by addressing the missing define-record-type support visible in the archived baseline. This is not itself a collector optimization, but it restores an important GC-focused benchmark so later experiment tickets can use it. Record whether the fix lands in runtime support or in the benchmark prelude/shim layer.


## Notes

**2026-04-12T20:16:28Z**

Implemented benchmark-side compatibility support by adding a gcbench-only shim in utils/r7rs-bench-gcbench-shim.scm and appending it only for gcbench in utils/run-r7rs-benchmarks.sh.

Crash investigation:
- A global unused top-level define-syntax in the shared benchmark prelude can make earley segfault.
- The issue was reduced to a dedicated follow-up ticket: aog-ed9d.
- Scoping the define-record-type shim to gcbench avoids perturbing unrelated benchmarks.

Clean archived run:
- scratch/2026-04-12-gc-aog-rw9e-clean/manifest.json
- gcbench 12.0s, mperm 35.0s, nboyer 15.0s, earley 20.0s, destruc 21.0s, peval 43.0s, nqueens 62.0s
Decision: keep. This becomes the benchmark reference for subsequent GC optimization experiments because it restores gcbench without regressing the rest of the suite.
