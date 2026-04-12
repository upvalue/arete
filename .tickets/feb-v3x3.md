---
id: feb-v3x3
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
# Closure leaking into matrix and compiler predicates

After the let* and call/cc fixes, matrix crashes with 'vector-ref expected vector but got closure' (matrix.scm:505) and compiler crashes with 'integer? got a closure' via disp?/opnd->mode/reg (compiler.scm:7599). Both shapes look like a closure value is being stored where a plain value is expected — likely a compiler-pass variable-capture or slot-allocation bug in Arete, not specific to let*/call/cc. Needs investigation with a minimal repro.

## Acceptance Criteria

matrix and compiler PASS or have a localized repro filed.

