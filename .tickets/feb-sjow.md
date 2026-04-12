---
id: feb-sjow
status: open
deps: []
links: []
created: 2026-04-12T16:34:34Z
type: epic
priority: 2
assignee: Phil
tags: [ecraven-benchmarks]
---
# Fix broken ecraven r7rs benchmarks

Umbrella for fixing breakage in the ecraven/r7rs-benchmarks suite running under Arete via utils/run-r7rs-benchmarks.sh. Scope: benchmarks that CRASH, fail with INCORRECT, or hit reader/expander/VM errors. Out of scope: benchmarks that merely TIMEOUT under CPU_LIMIT=30 — those are performance issues, not breakages.

## Design

Fixes land in utils/r7rs-bench-prelude.scm where they are shims for missing R7RS procedures, in scheme/ or src/ for real expander/VM bugs. Do not modify the vendored benchmark tree. Subtickets are grouped by root cause so that fixes can be batched (e.g. one exact-integer? shim unblocks three benchmarks).

## Acceptance Criteria

All currently-broken benchmarks either (a) PASS, (b) TIMEOUT (acceptable — means Arete is merely slow, not broken), or (c) have an explicit follow-up ticket explaining why the fix is deferred.

