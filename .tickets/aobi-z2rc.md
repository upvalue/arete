---
id: aobi-z2rc
status: closed
deps: [aobi-f4zz, aobi-vg03]
links: []
created: 2026-04-12T17:41:27Z
type: feature
priority: 3
assignee: Phil
external-ref: commit:5f1f2a6
tags: [interp-opt, optimization]
---
# T8: Fold argument list-length checks into one walk

In the apply path inside eval_body, dropped the up-front args.list_length() call. argc is now counted while evaluating-and-binding in lockstep. Arity error fires lazily: if args exhausts early, or surplus remains with no rest. fn_args.list_length() retained to size the env tightly.

Before: two full list walks (one per list_length call) purely to feed eval_check_arity, then both lists walked AGAIN to evaluate and bind. list_length was 180M calls (3.52% self) during bootstrap.

Results (on top of T6 in the same worktree):
- Bootstrap: 0.235s -> 0.226s (-3.8%)
- Aggregate microbench: 3.888s -> 3.782s (-2.7%)
- Cumulative T6+T8 vs pre-T6: bootstrap -7.4%, aggregate bench -6.7%, higher-order -11.2%, tak -8.5%, cons-list -9.1%

Commit 5f1f2a6 on arete-opt-base-interp. 81/81 Scheme tests pass.

