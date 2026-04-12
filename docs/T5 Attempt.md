# T5 Attempt — Pre-resolve environment lookups (aborted)

Branch: `opt/t5-env-lookup`. Task: #6. Outcome: aborted, not merged.

## Decision

The base interpreter is thrown away once bootstrap completes — everything real
runs on the VM. Invasive optimizations that require teaching the compiler,
expander, GC, and image serializer about a new heap type pay too high a tax
for a component that gets jettisoned. Future work on the same hot spot should
target the VM-resident lookup path instead.

## What we tried

Added a new heap type `LOCAL_REF { name, depth, slot }` and a lazy
self-rewriting mutation in the interpreter: the SYMBOL case of `eval_body`
calls `prelookup_car(state, env, pair)` which walks the env chain once,
writes the resolved `(depth, slot)` back into the car of the containing pair,
and from then on hits a fast path — `d` parent hops via `vector_ref(0)`
followed by one indexed load. Globals are cached through the same cell using
a sentinel depth `LOCAL_REF_GLOBAL = 0xFFFFFFFFu`, so the second read hits
`name.symbol_value()` directly with no hashtable probe and no second chain
walk.

Changes landed on-disk (uncommitted at abort):

- `arete.hpp` — `LOCAL_REF` enum, `LocalRef` struct (name/depth/slot),
  accessors, `make_local_ref`, `env_resolve_local` decl.
- `src/eval.cpp` — `env_resolve_local`, `prelookup_car`, `case LOCAL_REF`
  in `eval_body`, four prelookup call sites at the arg-eval loops.
- `src/gc.cpp`, `src/image.cpp` — copy + image update for the new heap type.
- `src/writer.cpp`, `src/builtins.cpp` — printing + `local-ref?` /
  `local-ref-name` primitives.
- `scheme/expand.scm`, `scheme/compiler.scm` — one-line unwrap
  `(if (local-ref? x) (set! x (local-ref-name x)))` at the top of `expand`
  and `compile-expr`.

## Where it broke

Measurements (min-of-3/5, second-pass tree after globals-cache fix):

- Bootstrap min-of-5: **103ms vs 99ms baseline (+4%, regression)**.
- deep-env: 0.480s vs 0.626s (−23%). Target was ≥25%.
- global-ref: 0.449s (−9.7%) — good.
- arith: +9.1%, cons-list: +7.3%, higher-order: +6.0% — all exceed the
  5% regression threshold.
- Aggregate microbench: −1.8% vs −5% threshold.
- 81/81 Scheme tests passing — correctness intact.

None of the three commit criteria was met. The regressions on arith /
cons-list / higher-order are the clearest signal that the extra branch in
the SYMBOL case and the `prelookup_car` call at every arg-eval site cost
more than the saved lookups return on code that doesn't repeatedly hit the
same binding site.

## Lessons reusable later

1. **AST mutation has a long blast radius in a self-bootstrapping system.**
   The first working version broke bootstrap because `pull-up-bootstraps`
   feeds interpreted `Function.body` back into the compiler, and `expand`
   also re-walks bodies. Any future AST-rewrite scheme in the base
   interpreter must either (a) carry the original name so downstream
   consumers can unwrap, or (b) guarantee no downstream consumer ever
   re-reads the mutated tree. Option (a) is what we did; the unwrap then
   has to be taught to *every* walker (expander, compiler, any future
   pass), and the heap type has to be taught to GC + image serializer.
   The tax is real.

2. **The hot spot is real but lives on the wrong side of the bootstrap
   boundary.** `env_lookup_impl` being 36% self on `deep-env` is a genuine
   signal, but on the base interpreter that cost is amortized over a
   one-shot bootstrap. The 30%+ win this ticket was chasing lives in the
   VM-resident lookup path, where the same pre-resolution trick doesn't
   need a new heap type, an expander tweak, or a compiler unwrap — just a
   bytecode that carries `(depth, slot)` literally.

3. **Caching globals is necessary, not optional.** First pass regressed
   `global-ref` by +13% because `prelookup_car` walked the env chain for
   globals (returning false) and `eval_body` then walked it *again* via
   `env_lookup`. The sentinel-depth fix removed the double walk. Any
   future env-cache scheme must handle globals on the same fast path as
   locals, not punt them.

4. **A 5% regression threshold on individual microbenches is tight.** Three
   benches regressed in the 6–9% range from what should be a pure win on
   symbol lookup. The per-SYMBOL branch and prelookup call are the
   prime suspects; a future attempt should lift the prelookup out of the
   arg-eval loop and hoist the LOCAL_REF dispatch above the type switch,
   or fold it into the existing SYMBOL case with no extra branch on the
   cold path.

## Unattempted

- **T9 (first-interpretation prepare-pass)** was explicitly out of scope
  for T5 but has the same class of problem: any prepare-pass that mutates
  or decorates the AST in the base interpreter has to teach every
  downstream walker. Worth deferring on the same grounds unless a design
  emerges that keeps the AST immutable and stores resolution data
  side-channel (e.g., keyed by pair identity in a weak hashtable — which
  then costs a hash on every lookup, probably defeating the point).

- **Hoisting prelookup out of the four arg-eval sites** and doing it once
  at lambda-body first-entry. Would reduce the per-arg overhead we're
  paying on arith/cons-list but adds a one-time walk cost and still
  doesn't fix the bootstrap regression.

- **A tighter LOCAL_REF layout** (packing depth+slot into one immediate
  to skip the heap allocation) was considered but not built; it would
  have cut GC pressure but not solved the downstream-consumer tax.

## Status of the branch

All code changes above are committed on `opt/t5-env-lookup` alongside this
retrospective, as a record of the attempt. The branch is **not** merged
and should not be merged — it's kept for future reference when the same
optimization is revisited on the VM side.
