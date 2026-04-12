---
id: feb-urwt
status: open
deps: []
links: []
created: 2026-04-12T16:35:27Z
type: bug
priority: 3
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Diagnose incorrect result from simplex

simplex runs to completion under 30s but returns #f where the harness expects a specific numeric result. Benchmark doc speculates numerical-precision or missing-primitive inside the body. Needs instrumentation: run the benchmark's kernel with prints to find where result diverges from a known-correct Scheme. Not understood yet.

## Acceptance Criteria

Either simplex PASSes, or the point of divergence from a reference Scheme is documented.


## Notes

**2026-04-12T17:05:07Z**

simplex still returns #f after the call/cc fix (which was suspected to be related). The benchmark's top-level form is (and pass2? (let loop () ...)); pass2? is being set to #f somewhere inside the main loop. Likely a floating-point comparison or set! propagation issue, not a missing primitive. Still needs investigation.
