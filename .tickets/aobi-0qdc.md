---
id: aobi-0qdc
status: closed
deps: [aobi-f4zz]
links: []
created: 2026-04-12T17:41:08Z
type: feature
priority: 3
assignee: Phil
external-ref: commit:0a0e7b2
tags: [interp-opt, optimization]
---
# T7: Cache G_TCO_ENABLED and G_FORBID_INTERPRETER on State

Hoisted two per-form global reads out of eval_body's hot loop. Cached G_FORBID_INTERPRETER and G_TCO_ENABLED as plain fields on State, synced from set_global_value.

Baseline had 786M calls to get_global_value during bootstrap (5.81% self), largely from these two globals read on every eval_body entry and every tail-call goto. Neither value changes during a normal run.

Results (best-of-5 bootstrap min, best-of-3 microbench):
- Bootstrap: 107ms -> 107ms (theoretical 3ms gain below measurement noise floor)
- All 9 microbenches improved 4.3-7.2%, no regressions:
  - arith 0.501 -> 0.478 (-4.6%), closure 0.793 -> 0.736 (-7.2%)
  - cond-chain 0.279 -> 0.264 (-5.4%), cons-list 0.304 -> 0.291 (-4.3%)
  - deep-env 0.692 -> 0.642 (-7.2%), gc-churn 0.247 -> 0.234 (-5.3%)
  - global-ref 0.557 -> 0.520 (-6.6%), higher-order 0.300 -> 0.285 (-5.0%)
  - tak 0.470 -> 0.445 (-5.3%)
- 81/81 Scheme tests pass.

Committed despite flat bootstrap because the microbench signature was exactly what the fix predicts (uniform small win across everything with zero regressions), and locks in a win that later changes can compound on.

Commit 0a0e7b2 on arete-opt-base-interp.

