---
id: feb-16an
status: closed
deps: []
links: []
created: 2026-04-12T16:35:12Z
type: bug
priority: 3
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Investigate expander 'duplicate argument name' on graphs/slatex/bv2string

graphs, slatex, bv2string all fail during expansion with 'duplicate argument name <x>' coming from scheme/expand.scm:709 (define-argument!) via expand-lambda / expand-delayed. For graphs the duplicated name is 'state'. The benchmark sources are valid R7RS — this is a bug in Arete's expander, likely triggered by let-values / define-values / define-record-type expansion producing lambdas with repeated formals. Needs diagnosis in scheme/expand.scm. Not understood yet.

## Design

Reproduce with: CPU_LIMIT=30 utils/run-r7rs-benchmarks.sh graphs. Then trace which macro expansion in the benchmark source introduces the duplicate formal. Compare to how other Schemes expand the same construct.

## Acceptance Criteria

Root cause identified; either fix in expander or a documented workaround in the prelude. At minimum graphs starts expanding without this error.


## Notes

**2026-04-12T17:04:45Z**

slatex now PASSES. graphs and bv2string no longer hit 'duplicate argument name' — the root cause was the same let* bug as feb-9xbk (the old expansion used (let ((a unspec) (b unspec) ...) (set! a e1) ...) which created duplicate formals if a let* had shadowing bindings). After the nested-let rewrite, graphs crashes with a different error ('function expected exactly 1 arguments but got 3') and bv2string needs utf8->string. Both issues filed/tracked separately.
