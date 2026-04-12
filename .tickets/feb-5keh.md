---
id: feb-5keh
status: closed
deps: []
links: []
created: 2026-04-12T16:34:47Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Shim rational? in r7rs-bench-prelude

Benchmark scheme crashes with 'reference to undefined variable rational?'. Arete has no rationals, so the safe shim is (define rational? real?) — every Arete real is representable as a rational in the R7RS sense when stored as a fixnum/flonum. Unblocks 1 benchmark.

## Acceptance Criteria

scheme PASSes or TIMEOUTs under CPU_LIMIT=30.


## Notes

**2026-04-12T16:47:46Z**

Added rational?=real?, plus gcd/lcm and a string->number wrapper that catches Arete's reader error on non-numeric input and returns #f per R7RS. scheme benchmark now runs to completion but returns INCORRECT due to a string<? comparison bug in Arete (filed separately).
