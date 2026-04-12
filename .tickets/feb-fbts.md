---
id: feb-fbts
status: open
deps: []
links: []
created: 2026-04-12T17:05:02Z
type: bug
priority: 3
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Shim utf8->string / string->utf8 for bv2string

bv2string needs utf8->string and string->utf8. Arete has no UTF-8 primitives and currently stores strings as raw bytes. For the benchmark, an identity-style shim (treating strings as byte sequences) would work: (define (string->utf8 s) (bytevector ... (map char->integer (string->list s)))) and inverse. But Arete may lack bytevector constructor APIs — check.

## Acceptance Criteria

bv2string PASSes or TIMEOUTs.

