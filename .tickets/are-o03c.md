---
id: are-o03c
status: open
deps: []
links: []
created: 2026-04-18T18:19:30Z
type: task
priority: 3
assignee: Phil
tags: [perf, build, benchmarking]
---
# Investigate compiler-only optimizations against global workload mix

Investigate compiler-only build optimizations for Arete without changing source semantics.

2026-04-18 experiment summary on this host (g++ 15.2.0, Linux x86_64):
- Baseline non-PGO build is current `-O2`.
- Straightforward flag variants did not beat baseline on `bin/arete bootstrap-and-psyntax.scm` when measured as pinned repeated runs.
- Screened variants: `-O3`, `-march=native`, `-flto`, `-fno-gcse -fno-crossjumping`, and combinations. All were neutral to worse on median runtime; none reached the +5% goal.
- Mixed-training PGO (`boot.scm` + `bootstrap-and-psyntax.scm`) did improve `bootstrap-and-psyntax` by about +5.12% median (0.850512s -> 0.806942s).
- That same PGO build regressed the R7RS `peval` benchmark by about 5.88% on the benchmark's own elapsed time and about 6.08% on full wall clock (34.0s -> 36.0s benchmark median; 34.48s -> 36.58s wall median).

Conclusion from this pass: no global compiler-only optimization is ready to keep. The best result found so far is a workload-local PGO profile that helps the psyntax stress case but hurts a heavier R7RS workload.

Artifacts:
- scratch/compiler-flag-exp-2026-04-18/screening.json
- scratch/compiler-flag-exp-2026-04-18/pgo.json
- scratch/compiler-flag-exp-2026-04-18/peval-vs-pgo.json

## Acceptance Criteria

A compiler-level change is only kept if it shows a convincing end-to-end win on more than one workload.

Minimum bar for revisiting:
- compare against the baseline `-O2` build
- include `bootstrap-and-psyntax`
- include `peval`
- ideally include a small cross-section from the existing perf set such as `nboyer`, `earley`, and `nqueens`
- do not keep a build/profile that wins on psyntax but regresses `peval`

