---
id: aobi-f4zz
status: closed
deps: [aobi-huf5]
links: []
created: 2026-04-12T17:40:58Z
type: chore
priority: 2
assignee: Phil
external-ref: commit:945ce43
tags: [interp-opt, measurement]
---
# T4: Profile base interpreter hot spots

Produced a ranked profile of where src/eval.cpp spends cycles during bootstrap and under the 9 interpreter microbenchmarks, to direct optimization tickets. Research-only; no interpreter code changed.

Tooling constraint: perf and valgrind are not installed on this sandbox; kernel.perf_event_paranoid=2. Used GNU gprof over a separately-built bin/arete-dbg (-O2 -g -fno-omit-frame-pointer -pg). Bootstrap profiled over 30 aggregated runs (~5.68s sampled CPU), each microbench over 5 runs. Rankings are solid; mcount inflates absolute % slightly but call counts are exact.

TOP 10 self-time during bootstrap:
  47.54%  State::eval_body           31,598,486 calls
  13.20%  apply_vm                    7,115,740  (out of scope — bytecode VM)
   5.99%  State::env_lookup_impl    240,331,778
   5.81%  State::get_global_value   785,928,399
   3.70%  State::vector_append      223,377,568
   3.52%  Value::list_length        180,495,516
   2.11%  State::make_vector         41,960,887
   2.11%  State::eval_form           20,730,103
   1.41%  GCSemispace::copy          43,145,614
   0.88%  make_env / emplace / etc.

Per-microbench: eval_body dominates everywhere (41-76% self). env_lookup_impl is #2 almost everywhere; explodes to 36% on deep-env (nested lets). get_global_value is structurally #3 due to the G_FORBID_INTERPRETER / G_TCO_ENABLED reads on every eval_body entry.

Filed 5 opportunity tickets from this profile: T5 (env lookup pre-resolve), T6 (call-frame alloc), T7 (hoist globals), T8 (list-length fold), T9 (prepared AST — stretch).

Full write-up lived in docs/Interpreter Profile.md (deleted during project wind-down — this ticket carries the summary). Profile doc reconstructible from commit 945ce43.

