# Optimizer Implementation Plan

This document turns the original cp0 feasibility study into an implementation
plan for future agents. The goal is still not to port
`ChezScheme/s/cp0.ss` verbatim. Chez `cp0` is tied to Chez's nanopass
languages, binding records, primitive metadata, cross-library optimizer,
record and ftype knowledge, and continuation details. Arete should instead
borrow the parts that make cp0 durable: clear optimizer boundaries, context
tracking, conservative binding analysis, guarded primitive folds, and a large
correctness harness.

The first implementation slice already introduced `scheme/optimizer.scm`,
`COMPILER-OPTIMIZE`, `OPTIMIZER-INLINE-CAR`, `COMPILER-OPTIMIZER-LOG`, and the
internal compiler form `##arete#inline-let`. The next work should continue the
same direction: move remaining source-level optimizations out of
`scheme/compiler.scm`, then add small cp0-inspired rewrites behind flags.

Related context:

- [[Benchmarking]]
- [[Benchmarks Arete]]
- [[Benchmarks ecraven]]
- [[Psyntax]]
- Ticket `are-o03c`, which sets a benchmark bar for compiler-level work.

## Direction

`scheme/compiler.scm` should become mostly bytecode lowering. It should keep
stack accounting, closure capture, destination-driven emission, primitive
opcode selection, source locations, and VM-specific choices. Source rewrites,
normalization, and rewrite refusal logic should live in `scheme/optimizer.scm`
or helper files loaded before the compiler.

The optimizer should remain optional and configurable:

- Snapshot top-level flags into an optimizer config once per compile.
- Avoid top-level flag lookups inside recursive optimizer walks.
- Keep a global kill switch with `COMPILER-OPTIMIZE`.
- Give each optimization family its own flag while it is young.
- Prefer internal forms only when existing source syntax cannot express the
  lowered shape safely.
- Preserve optimizer-off behavior as a permanent compatibility path.

The pass should initially emit expanded source forms plus a small set of
compiler-private forms such as `##arete#inline-let`. Do not add VM opcodes as
part of the source optimizer unless a later measured phase proves the need.

## Current State

The compiler still contains several optimization responsibilities:

- Named-let recognition and loop lowering:
  `named-loop-match`, `named-loop-usage-ok?`, `compile-named-loop`, and
  `compile-named-loop-call`.
- Primitive opcode selection through `primitive-table`.
- Fused conditional emission in `compile-if` for patterns such as `null?`,
  `pair?`, `not`, and safe-immediate `eq?`.
- Fused callee-load/apply opcodes in `compile-apply/generic`.
- Display-closure decisions based on `scan-mutable-names`.
- Destination-driven code generation controlled by
  `COMPILER-CONTROL-DESTINATION`.

These are not all source optimizations. Some are code generation choices and
should stay in the compiler until Arete has an optimizer IR that can carry the
same information cleanly.

`scheme/analysis.scm` is not part of the boot path and should not be treated
as the active optimizer. It can be deleted later or replaced with shared
analysis once the new optimizer has enough shape.

## Phase 1: Stabilize The Optimizer Boundary

Purpose: make the new pass easy to inspect, disable, test, and extend before
moving more behavior into it.

Files:

- `scheme/optimizer.scm`: extend `OptimizerConfig` with per-pass flags,
  counters, and optional logging helpers.
- `scheme/compiler.scm`: keep only the optimizer call sites and internal-form
  lowering needed by the optimizer.
- `tests/optimizer/`: add tests for optimizer configuration, logging-safe
  traversal, and optimizer-off behavior.
- `utils/run-tests.py`: keep `compiler`, `compiler-noopt`, and `optimizer`
  suites available.
- `docs/Benchmarking.md`: document the optimizer acceptance workflow once the
  next functional pass lands.

Acceptance:

- `COMPILER-OPTIMIZE #f` fully bypasses source rewrites.
- Each optimizer family can be disabled independently.
- Optimizer config is read once per top-level compile or recompile.
- `make test-all` passes.
- `rm -f heap.boot && make heap.boot` succeeds.

## Phase 2: Migrate Named-Let Lowering

Purpose: move the largest remaining source-pattern optimization out of code
generation while preserving the current conservative safety checks.

Current compiler ownership:

- `named-loop-match`
- `named-loop-usage-ok?`
- `compile-named-loop`
- `compile-named-loop-call`

Plan:

1. Add a flag, likely `OPTIMIZER-NAMED-LET`.
2. Move the named-let expansion recognizer into `scheme/optimizer.scm`.
3. Have the optimizer emit an internal form:

   ```scheme
   (##arete#named-loop name (arg ...) (init ...) body ...)
   ```

4. Keep the tail-recursive jump lowering in `scheme/compiler.scm`, but make it
   lower `##arete#named-loop` instead of rediscovering the source pattern.
5. Keep `compile-named-loop-call` or its replacement as codegen, because the
   backward jump is VM-specific.
6. Preserve the current refusal behavior when the named loop is captured,
   called in non-tail position, shadowed, assigned, or used with the wrong
   arity.

Tests to add:

- A normal named loop still compiles to the optimized path.
- Non-tail self-recursion is refused and behaves like normal procedure calls.
- Capturing the loop procedure is refused.
- Shadowing the loop name is refused.
- Assigning the loop name is refused.
- Wrong-arity loop calls still report an error.
- Optimizer-off runs the old generic behavior correctly.

Acceptance:

- The compiler no longer recognizes the expanded named-let source shape in
  `compile-apply`.
- Named-loop bytecode output is unchanged or intentionally explained.
- `make test-all` and `make heap.boot` pass.

## Phase 3: Separate Primitive Metadata From Opcode Selection

Purpose: prepare cp0-style primitive reasoning without moving VM emission into
the optimizer.

Current compiler ownership:

- `primitive-table`
- `compile-primcall`
- primitive-related branches in `compile-if`

Plan:

1. Do not move `primitive-table` wholesale at first. It is primarily bytecode
   selection.
2. Create a small optimizer-visible primitive metadata table. This can live in
   `scheme/optimizer.scm` until it is large enough to split out.
3. Track only source-level facts at first:
   name, arity, foldability, effect freedom, allocation behavior, and whether
   global redefinition must block the rewrite.
4. Keep final opcode choice in `scheme/compiler.scm`.
5. Add tests proving that shadowed or redefined primitive names are not folded
   unless Arete already treats them as VM primitives under the same rules.

Good first metadata entries:

- `not`
- `eq?`
- `null?`
- `pair?`
- `fixnum?`
- `symbol?`
- `fx+`
- `fx-`
- `fx<`

Acceptance:

- The optimizer can ask whether a primitive is safe to fold or treat as simple.
- The compiler still owns opcode emission.
- Existing primitive behavior and errors are unchanged with optimization on.

## Phase 4: Move Source-Level Conditional Simplification

Purpose: handle source rewrites in optimizer contexts while leaving conditional
bytecode fusion in the compiler.

Current compiler ownership:

- `compile-if` fuses selected predicate shapes into conditional jumps.
- `COMPILER-CONTROL-DESTINATION` controls destination-driven emission.

Plan:

1. Add optimizer contexts for at least value, effect, test, and tail position.
2. Implement source-level simplifications only:
   constant-condition `if`, `(not (not x))` in test context when safe, and
   simple `and`/`or` reductions that preserve short-circuit behavior.
3. Do not move fused jump opcode selection yet.
4. Do not fold a branch away unless the discarded branch is not evaluated by
   Scheme semantics.

Tests to add:

- Constant true and false `if` conditions.
- Branches with side effects are not evaluated after simplification.
- `and` and `or` preserve left-to-right effects.
- Test-context `not` simplification does not change returned values where
  value context would observe `#t` versus a non-false object.

Acceptance:

- `compile-if` still chooses opcodes.
- Source-level conditional tests pass with optimizer on and off.
- Differential output is identical for existing compiler tests.

## Phase 5: Move Or Share Mutability Analysis Carefully

Purpose: avoid duplicating binding facts once the optimizer starts using
assignment and reference counts.

Current compiler ownership:

- `scan-mutable-names`
- display-closure decisions for immutable captures

Plan:

1. Keep `scan-mutable-names` in the compiler until source rewrites are stable.
2. When optimizer binding records exist, add an analysis helper that can record
   assigned names, referenced names, local define shape, and lambda capture.
3. Make the compiler consume the final optimized source, not stale facts from
   pre-optimized source.
4. Only then consider moving mutability scanning into a shared file.

Acceptance:

- Closure capture tests pass before and after each optimizer rewrite.
- Local mutation blocks copy propagation and inlining.
- Internal define and recursive bindings remain correct.

## Phase 6: Cp0-Inspired Binding Framework

Purpose: introduce the smallest useful version of cp0's operand and context
discipline.

New optimizer concepts:

- Contexts: value, effect, test, tail, and application.
- Binding records: name, assigned?, reference count, RHS expression,
  integration score, and source location.
- Operand forcing: analyze an RHS once and reuse the optimized result.
- Effort limits: cap recursive optimization and inlining.
- Refusal reasons: record why a candidate rewrite was skipped when logging is
  enabled.

Implementation order:

1. Add analysis without transformations.
2. Add logging or counters for references, assignments, and refused candidates.
3. Run the full suite and bootstrap with no behavioral changes.
4. Use the analysis in one tiny rewrite.

Acceptance:

- Turning the framework on without rewrites is behaviorally neutral.
- The framework is covered by optimizer tests with assigned locals, aliases,
  closures, local defines, and nested lambdas.

## Phase 7: Conservative Cp0-Inspired Rewrites

Purpose: start taking the high-impact, easy-to-reason-about cp0 ideas.

Implement these one family at a time, each behind a flag:

- Constant-condition simplification.
- Constant folding for a tiny primitive whitelist.
- Copy propagation for quoted constants and unassigned single-use local aliases.
- Effect-context dead expression removal when `simple?` proves no effects and
  no observable allocation.
- Direct local function integration in application position, fixed arity only,
  with a small score limit.

Initial primitive folds should be boring:

- `(not '#f)` and `(not <known-nonfalse-constant>)`
- `(eq? '<safe-immediate> '<safe-immediate>)`
- `(null? '())`
- `(pair? '<quoted-constant>)`
- small fixnum-only folds where Arete's existing runtime behavior is clear

Defer:

- Recursive function integration and unrolling.
- Cross-library or cross-module optimization.
- Record, vector, string, or allocation-sensitive folds.
- Multiple-values and continuation-sensitive rewrites.
- New bytecode opcodes.
- Any rewrite that changes error timing or error class.

Acceptance for each rewrite:

- Add optimizer-on tests for applied rewrites.
- Add optimizer-on tests for refused rewrites.
- Add optimizer-off tests when the behavior is subtle.
- Run `python3 utils/run-tests.py optimizer compiler compiler-noopt`.
- Run `make test-all`.
- Run `make heap.boot` if the pass affects bootstrap code.

## Battle Testing

Correctness gate for optimizer changes:

1. Run targeted optimizer tests.
2. Run compiler tests with `COMPILER-OPTIMIZE #t`.
3. Run compiler tests with `COMPILER-OPTIMIZE #f`.
4. Run `make test-all`.
5. Rebuild `heap.boot` from scratch for changes that affect boot-loaded code.
6. Differentially run existing Scheme tests with optimizer off and on, comparing
   stdout, stderr class, and exit status.
7. Add fuzzing for a supported core subset before enabling broad copy
   propagation or inlining.
8. Run the psyntax stress path from [[Psyntax]] before considering the pass
   mature.

Performance gate:

1. Save a baseline with `make bench-compare SAVE_BASELINE=1`.
2. Compare after each optimization family, not after a large batch.
3. Include `boot`, `bootstrap-and-psyntax`, and a small R7RS cross-section.
4. Keep a change only when it improves multiple workloads or demonstrates a
   clear opcode/allocation reduction without correctness risk.
5. Treat regressions in `peval`, `bootstrap-and-psyntax`, or image size as
   blockers unless the flag remains experimental and disabled by default.

## File Ownership

`scheme/optimizer.scm`: optimizer config, source rewrites, contexts, primitive
metadata used by rewrites, counters, logging, and internal-form construction.

`scheme/compiler.scm`: bytecode lowering, VM opcode selection, stack and local
allocation, closure capture, source location emission, and lowering of
optimizer-private forms.

`boot.scm`, `compile.scm`, `GNUmakefile`: load order and source lists.

`utils/run-tests.py`: optimizer-on and optimizer-off suites.

`tests/optimizer/`: focused rewrite and refusal tests.

`docs/Benchmarking.md`: accepted benchmark and differential workflows.

Runtime files such as `arete.hpp`, `src/vm.cpp`, `src/vm-native.cpp`, and
`src/vm-native-x64.cpp.dasc`: out of scope unless a later, separately measured
opcode project is approved.

## Agent Work Item Template

Each future optimizer task should state:

- The single optimization family being changed.
- The flag controlling it.
- The source forms it may rewrite.
- The exact safety conditions that must hold.
- The refusal cases covered by tests.
- The files touched.
- The test and bootstrap commands run.
- Any benchmark result if performance is part of the claim.

Do not merge several optimizer families in one step. Small reversible changes
are the main way to keep this pass trustworthy.

## Open Questions

- Should optimizer-private forms remain symbol-tagged lists, or should Arete
  grow a small internal IR record layer?
- Should primitive metadata live in `scheme/optimizer.scm` or a separate file
  once it exceeds a small table?
- Should `scheme/analysis.scm` be deleted or repurposed after the new binding
  framework exists?
- Should optimizer flags remain top-level values, or should compilation entry
  points accept an explicit config object later?

## Non-Goals

- Do not port Chez `cp0` line for line.
- Do not start with recursive inlining.
- Do not add new VM opcodes in the source optimizer phase.
- Do not fold primitives whose error behavior is unclear.
- Do not optimize through assignment, local define, letrec, or captured
  bindings until the binding framework proves it is safe.
- Do not remove the optimizer-off path.
