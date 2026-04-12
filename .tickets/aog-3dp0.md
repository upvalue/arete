---
id: aog-3dp0
status: open
deps: []
links: []
created: 2026-04-12T19:50:04Z
type: epic
priority: 1
assignee: Phil
tags: [gc, performance]
---
# GC optimization campaign

Coordinate end-to-end garbage collector optimization work for Arete. Workflow: establish a saved benchmark baseline with the benchmark runner in a dated scratch directory; create tagged child tasks for each concrete GC experiment; measure before/after on the same benchmark set; close each child with results; keep the epic as the umbrella record for benchmark scope, experiment flow, and retained wins.


## Notes

**2026-04-12T19:50:41Z**

Baseline scope for GC work:
- Arete workloads: boot, bootstrap-and-psyntax
- R7RS benchmarks: gcbench, mperm, nboyer, earley, destruc, peval, nqueens
- Baseline artifacts live under scratch/2026-04-12-gc-baseline
- Child tasks must be tagged gc and closed with before/after results against this archived baseline

**2026-04-12T19:57:07Z**

Initial GC experiment queue:
- aog-lgjr: remove avoidable symbol-table root update overhead
- aog-klnt: reduce zero-fill cost on bump allocation
- aog-il5a: tune semispace growth and reuse policy
- aog-ai67: specialize copy/scan hot paths for common object shapes
- aog-rw9e: unblock gcbench define-record-type so the canonical GC benchmark can run
All child tasks are expected to compare against scratch/2026-04-12-gc-baseline and close with measured results.

**2026-04-12T20:30:47Z**

Current state after first experiment loop:
- kept: aog-rw9e (gcbench enabled via gcbench-only shim; clean reference is scratch/2026-04-12-gc-aog-rw9e-clean)
- rejected: aog-lgjr, aog-ai67, aog-il5a v1, aog-klnt v1
- open follow-up bug: aog-ed9d for the unused top-level define-syntax crash affecting earley when macros are injected globally into the benchmark prelude
No collector optimization has beaten the gcbench-enabled reference yet.

**2026-04-12T20:45:33Z**

Campaign summary as of 2026-04-12:

Workflow:
- use one epic plus child tickets for baseline, benchmark enablement, crash investigation, and one concrete GC experiment per ticket
- compare experiments against archived benchmark artifacts under scratch/
- close each child with a keep/reject decision and recorded numbers

Current benchmark reference:
- scratch/2026-04-12-gc-aog-rw9e-clean/manifest.json
- Arete workloads: boot, bootstrap-and-psyntax
- R7RS benchmarks: gcbench, mperm, nboyer, earley, destruc, peval, nqueens
- headline numbers: gcbench 12.0s, mperm 35.0s, nboyer 15.0s, earley 20.0s, destruc 21.0s, peval 43.0s, nqueens 62.0s

What we have tried so far:
- kept: aog-rw9e, which enabled gcbench through a gcbench-only define-record-type shim in utils/r7rs-bench-gcbench-shim.scm appended only for gcbench by utils/run-r7rs-benchmarks.sh
- rejected: aog-lgjr (symbol-table root update tweak; mperm regressed)
- rejected: aog-ai67 (copy() fast-path reorder; flat to slightly worse)
- rejected: aog-il5a v1 (retain grown inactive semispace; caused nboyer and earley crashes)
- rejected: aog-klnt v1 (reduced zero-fill for a small allowlist; no GC win, mperm/nqueens regressed)

Open related investigation:
- aog-ed9d documents that a trivial unused top-level define-syntax injected into the shared benchmark prelude can make earley segfault, while the same shim is safe when injected only for gcbench

Current conclusion:
- no GC optimization has beaten the gcbench-enabled reference yet
- the next serious direction should probably be structural rather than micro-optimizing the current semispace collector
