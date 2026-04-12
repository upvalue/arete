---
id: feb-gnhh
status: closed
deps: []
links: []
created: 2026-04-12T16:34:41Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Shim exact-integer? in r7rs-bench-prelude

Benchmarks compiler, matrix, maze all crash immediately with 'reference to undefined variable exact-integer?'. Arete has exact? and integer?, so the shim is one line: (define (exact-integer? x) (and (integer? x) (exact? x))). See other-impl reference impls at vendor/r7rs-benchmarks/src/Bones-prelude.scm:10 and S7-prelude.scm:3. Unblocks 3 benchmarks.

## Acceptance Criteria

compiler, matrix, maze each either PASS or reach TIMEOUT under CPU_LIMIT=30.


## Notes

**2026-04-12T16:47:44Z**

Shim added in utils/r7rs-bench-prelude.scm. maze now PASSES (19s). compiler and matrix get further but now crash on unrelated bugs (compiler: string->number on non-numeric input, now also wrapped; matrix: VM constant-tag leak, tracked in feb-9xbk).
