---
id: feb-vjl3
status: open
deps: []
links: []
created: 2026-04-12T16:35:04Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Shim define-record-type for gcbench

gcbench uses R7RS define-record-type at gcbench.scm:76. Arete has its own non-R7RS define-record in scheme/types.scm:40 and a register-record-type primitive, but no R7RS define-record-type macro. Shim as a macro that translates R7RS syntax ((define-record-type name (ctor field...) pred? (field accessor [setter])...)) into Arete's define-record form. Reference impls at vendor/r7rs-benchmarks/src/IronScheme-prelude.scm:7 and S7-prelude.scm:55. Medium difficulty: not a one-liner, but scope is constrained to a single macro in the prelude.

## Acceptance Criteria

gcbench PASSes or TIMEOUTs under CPU_LIMIT=30.

