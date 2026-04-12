---
id: feb-6evf
status: closed
deps: []
links: []
created: 2026-04-12T16:34:54Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Shim read-line for tail benchmark

Benchmark tail crashes at tail.scm:13 with 'reference to undefined variable read-line'. Arete has read-char, so read-line can be implemented in the prelude as a read-char loop terminating at #\newline / eof-object. Unblocks 1 benchmark.

## Acceptance Criteria

tail PASSes or TIMEOUTs under CPU_LIMIT=30.


## Notes

**2026-04-12T16:47:47Z**

Shim added. Combined with the new delete-file builtin (feb-c9vd), tail now PASSES (27s).
