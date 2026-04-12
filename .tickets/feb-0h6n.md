---
id: feb-0h6n
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
# graphs: function expected 1 argument but got 3

After the let* fix (feb-16an), graphs no longer fails in the expander but now crashes at runtime with 'function expected exactly 1 arguments but got 3'. Likely a closure/proc arity mismatch triggered by how Arete compiles one of graphs's higher-order constructions. Needs a minimal repro.

## Acceptance Criteria

graphs PASSes or has a localized repro filed.

