---
id: aobi-huf5
status: closed
deps: [aobi-tp6u]
links: []
created: 2026-04-12T17:40:37Z
type: chore
priority: 2
assignee: Phil
external-ref: commit:7dc17ca
tags: [interp-opt, measurement]
---
# T3: Build interpreter-focused microbenchmark suite

Added bench/interp/ with 9 single-axis microbenchmarks runnable under bin/arete heap.boot --interp-only, plus utils/run-interp-bench.sh best-of-3 driver emitting +!CSVLINE!+arete-interp,<name>,<sec>.

Ecraven is mostly too slow or outright fails under pure interpretation (see docs/Benchmarks ecraven.md), so this is our own curated set.

Benchmarks (isolates / baseline best-of-3 sec on pre-optimization tip):
- arith         (arithmetic dispatch + tail loop)      0.868
- global-ref    (top-level env lookup)                 0.987
- deep-env      (6 nested let frames, chain walk)      1.078
- cons-list     (pair alloc + car/cdr)                 0.545
- closure       (make-adder closure creation)          1.242
- higher-order  (repeat apply via fold)                0.515
- cond-chain    (9-clause cond dispatch)               0.468
- tak 24 16 8   (non-tail recursion + int arith)       0.779
- gc-churn      (nursery throughput)                   0.419
Total best-of-3 sum: 6.901s. Driver wall-clock ~21s for 3x9 runs.

Note: a standalone (fib 30) ran in ~0.64s interp-only, so cherry-picked ecraven benches with reduced inputs would likely be tractable if we ever want them. Full ecraven under --interp-only remains too slow/fragile.

Commit 7dc17ca on arete-opt-base-interp. Doc section added to docs/Benchmarking.md.

