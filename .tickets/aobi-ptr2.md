---
id: aobi-ptr2
status: open
deps: [aobi-yqnf]
links: []
created: 2026-04-12T17:42:12Z
type: feature
priority: 4
assignee: Phil
tags: [interp-opt, deferred]
---
# T9: Prepared-AST / direct-threaded eval — deferred

Deferred. Not attempted.

Original hypothesis: eval_body itself is 47.5% self-time. Inside its giant switch, each PAIR invocation re-discovers the form — symbol-identity check against C_SYNTAX, else-if chain across up to 9 symbol comparisons, second switch on form constant, per-form logic. All of this dispatch cost is paid every time a given form is evaluated, even though shape is fully known after expansion.

Fix shape: one-pass prepare that rewrites each post-expansion form into a tagged node (form-kind enum + pre-cached slots). Interpreter switches on tag directly. Would have subsumed T5's env-lookup pre-resolution.

Expected payoff: large-but-speculative, 20-30% on top of T5-T8. The 2x interpreter speedup ceiling named in T1 was only reachable with something in this family.

Why deferred: T9 is the same class of AST-mutation optimization as T5 at larger scale. T5's blast radius through expander + compiler + image serializer already made the cost-benefit unfavorable for a throwaway base interpreter; T9 would extend that radius further (tagged nodes for every form, not just local refs). The real home for this optimization is the bytecode compiler / VM, where resolved form kinds can be baked into bytecode without downstream consumers needing to reinterpret them.

Left open as a pointer for anyone picking up VM-side interpreter optimization later.

