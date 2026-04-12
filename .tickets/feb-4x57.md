---
id: feb-4x57
status: open
deps: []
links: []
created: 2026-04-12T16:35:32Z
type: feature
priority: 4
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Complex number support for mbrotZ (deferred)

mbrotZ uses the complex literal -1.0-0.5i at mbrotZ.scm:81. Arete's reader rejects it as invalid numeric syntax and the language has no complex number type at all. Real fix is implementing R7RS complex numbers (reader + arithmetic primitives), which is a significantly larger effort than the rest of this epic. Ticket kept for tracking; low priority.

## Acceptance Criteria

Either complex numbers are implemented and mbrotZ PASSes, or a decision to permanently skip mbrotZ is recorded in docs/Benchmarks ecraven.md.

