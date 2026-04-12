---
id: aov-6ym1
status: closed
deps: []
links: []
created: 2026-04-12T21:07:07Z
type: task
priority: 0
assignee: Phil
tags: [perf, vm, compiler]
---
# Add direct-call superinstructions for local/global/upvalue VM calls


## Notes

**2026-04-12T21:10:49Z**

Rejected direct-call superinstructions for local/global/upvalue calls. The transformation changed operator evaluation order by fetching the callee after argument evaluation, which is not semantically equivalent in Scheme when arguments can mutate the binding. This surfaced as runtime failures in closure, tco, varargs, macro-import, and cond-expand tests. Reverted before benchmarking.
