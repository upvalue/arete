---
id: feb-uotk
status: in_progress
deps: [feb-9335]
links: []
created: 2026-04-12T16:34:45Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Shim exact-integer-sqrt in r7rs-bench-prelude

Benchmarks pi and chudnovsky crash with 'reference to undefined variable exact-integer-sqrt'. Arete already supports multiple values (call-with-values + (values ...)) verified interactively. A minimal impl lives in vendor/r7rs-benchmarks/src/S7-prelude.scm:4: (define (exact-integer-sqrt i) (let ((sq (floor (sqrt i)))) (values sq (- i (* sq sq))))). chudnovsky uses it at chudnovsky.scm:31. Unblocks 2 benchmarks.

## Acceptance Criteria

pi and chudnovsky each PASS or TIMEOUT under CPU_LIMIT=30.


## Notes

**2026-04-12T16:48:18Z**

Shim added using (floor (sqrt n)) plus remainder. This works for fixnum-range n but is not accurate past fixnum precision. pi and chudnovsky both require arbitrary-precision integers (expt 10 50, etc.) — blocked on feb-9335 (bignum support). Keeping this ticket open until pi/chudnovsky either PASS or the bignum ticket is explicitly deferred.
