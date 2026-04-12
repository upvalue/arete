---
id: aobi-vg03
status: closed
deps: [aobi-f4zz]
links: []
created: 2026-04-12T17:41:20Z
type: feature
priority: 2
assignee: Phil
external-ref: commit:0b31255
tags: [interp-opt, optimization]
---
# T6: Collapse call-frame construction into one allocation

Added State::make_call_env(parent, npairs) that allocates the env vector once at tight size 3+2*npairs and installs the parent/flag header in one pass, plus env_push_binding() which writes a name/value pair with no bounds check or resize. Rewired apply_interpreter and eval_body's FUNCTION-apply branch. apply_interpreter also now writes directly from argv and only touches state.temps when packing rest-args.

Before this change, each interpreter function application did: make_env (itself make_vector + 2x vector_append), then 2*argc more vector_appends (name + value). A 3-arg call was 7 mutations + 1 allocation per frame. Per-bootstrap counts: 41M make_env, 41M make_vector, 223M vector_append, 41M eval_check_arity.

Results (vs pre-T6 baseline on its worktree):
- Bootstrap: 0.244s -> 0.235s (-3.7%)
- Aggregate microbench: 4.053s -> 3.888s (-4.1%)
- Call-heavy benches moved most: higher-order, closure, cons-list, tak

Gotcha for future work: the env vector is now TIGHT (3 + 2*npairs, one slack slot). If future code pre-allocates binding slots by index on a call-frame env, it MUST use env_push_binding (no bounds check) not vector_append. Only one slack slot exists and it's reserved for legitimate (define ...) in the body.

Commit 0b31255 on arete-opt-base-interp. 81/81 Scheme tests pass.

