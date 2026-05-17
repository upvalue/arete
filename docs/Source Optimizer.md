# Source Optimizer

This is the active working note for Arete's optional source-to-source
optimizer. It replaces the old cp0 feasibility framing: Chez `cp0` remains a
useful reference point, but Arete is now building its own small optimizer around
the current compiler and bootstrap constraints.

The optimizer lives in `scheme/optimizer.scm`, is loaded before
`scheme/compiler.scm`, and runs from `optimize-toplevel` before bytecode
lowering. It emits ordinary expanded source plus a small number of
compiler-private forms. The compiler still owns VM opcode selection, stack
accounting, closure capture, source locations, and final bytecode emission.

Related context:

- [[Benchmarking]]
- [[Benchmarks Arete]]
- [[Benchmarks ecraven]]
- [[Psyntax]]
- Ticket `are-o03c`, which sets a benchmark bar for compiler-level work.

## Current Shape

Top-level controls:

- `COMPILER-OPTIMIZE`: global optimizer kill switch.
- `OPTIMIZER-INLINE-CAR`: historical name for direct lambda-application
  lowering. It also respects `COMPILER-INLINE-CAR`.
- `OPTIMIZER-NAMED-LET`: named-let recognition and lowering.
- `OPTIMIZER-PRIMITIVE-FOLD`: conservative primitive constant folding.
- `OPTIMIZER-CONSTANT-IF`: constant-condition `if` simplification. Disabled
  by default until it shows a runtime benchmark win.
- `OPTIMIZER-CONSTANT-PROP`: immediate constant propagation through direct
  lambda applications before `##arete#inline-let` lowering.
- `OPTIMIZER-TEST-IF`: boolean-wrapper `if` simplification and safe test
  branch inversion.
- `COMPILER-OPTIMIZER-LOG`: text logging only.
- `COMPILER-OPTIMIZER-COUNTERS`: opt-in applied/refused rewrite counters.

Implemented optimizer families:

- Direct lambda application inside lambda bodies lowers to
  `##arete#inline-let`.
- Expanded named-let shapes lower to `##arete#named-loop` after conservative
  tail-use, assignment, and capture checks.
- Primitive metadata exists for `not`, `eq?`, `null?`, `pair?`, `fixnum?`,
  `symbol?`, `fx+`, `fx-`, and `fx<`.
- Constant folds currently cover `not`, safe-immediate `eq?`, `null?`,
  `fixnum?`, and small fixnum `fx+`, `fx-`, and `fx<`.
- Constant-condition `if` simplification removes unreachable branches when the
  test is known from a self-evaluating value, `quote`, or a prior fold in the
  same optimizer walk. A false test with an expander-inserted unspecified else
  branch is left as an `if`, so the optimizer does not make unspecified results
  more visible. It currently remains available behind its flag but disabled by
  default after source-heavy benchmark isolation showed no clear runtime win.
- Direct lambda application can substitute safe immediate constant arguments
  (`#t`, `#f`, fixnums, symbols, and `()`) into the body before emitting
  `##arete#inline-let`, when the formal is not assigned or redefined and nested
  lambdas are respected.
- In `test` context, `(if e #t #f)` simplifies to `e`. In value-like contexts,
  the same wrapper also simplifies when `e` is already known to return exactly
  `#t` or `#f`; the current exact-boolean primitive set is unshadowed `not`,
  `eq?`, `null?`, `fixnum?`, and `fx<` with VM primitives enabled. When a
  4-armed outer `if` tests `(if e #f #t)`, the outer branches are inverted to
  avoid the boolean wrapper without introducing a user-visible `not` binding.
  A 4-armed outer `if` testing unshadowed primitive `(not e)` is similarly
  inverted to `(if e else then)`.
- Optional counters record applied rewrites for inline-let, named-loop,
  primitive-fold, and constant-if, plus narrow refusal counts for unknown
  primitive folds, false unspecified `if`, constant-prop, test-if, boolean-if,
  primitive-not inversion, and named-loop refusal categories.
  Use `optimizer-reset-counters!`, `optimizer-counter-value`, and
  `optimizer-counter-snapshot` after enabling `COMPILER-OPTIMIZER-COUNTERS`.
- The traversal now carries explicit cp0-style result contexts: `value`,
  `effect`, `test`, `tail`, and `application`, while still separately tracking
  whether compiler-private lambda-body rewrites are legal.

Known limits:

- There are no general binding records, reference counts, effect summaries, or
  structured refusal reasons yet. Constant propagation has only a narrow local
  assignment scan for direct-lambda formals.
- `pair?` is metadata-only until the compiled VM primitive path returns a
  normal Scheme true value consistently in optimizer-off compiler tests.
- `symbol?` is metadata-only because the compiler does not currently list it
  in `primitive-table`.
- Conditional simplification is intentionally narrow: it does not reason about
  effects, bindings, or general predicates yet. Exact-boolean value-context
  simplification is limited to the primitive set listed above.
- Context tracking is still narrow: `test` powers safe boolean-wrapper rewrites
  and branch inversion, while `effect` and `application` positions preserve
  existing rewrite decisions and do not yet trigger effect pruning or function
  integration.
- The lexical model is still just lambda-body eligibility plus a small
  primitive-shadow set.

## Execution Model

The optimizer is deliberately a single recursive source walk. It receives
already-expanded source from the expander and returns expanded source for the
compiler. It does not own parsing, macro expansion, opcode selection, stack
layout, closure conversion, or VM instruction emission.

The entry point is:

```scheme
(optimize-toplevel expanded-form)
```

`optimize-toplevel` snapshots all top-level flags into one `OptimizerConfig`
record before walking the form. This matters because optimization itself can
call helpers that inspect or rewrite expanded code; reading mutable top-level
flags repeatedly during the walk would make one form sensitive to flag changes
that happen halfway through optimization.

The main worker is:

```scheme
(optimizer-expr config expr ctxt in-lambda? shadowed)
```

Its parameters are:

- `config`: the immutable snapshot of optimizer and compiler flags.
- `expr`: the expanded source expression being optimized.
- `ctxt`: the result context requested by the parent expression.
- `in-lambda?`: whether compiler-private lambda-body rewrites are legal here.
- `shadowed`: primitive names that cannot be trusted as global primitives in
  the current lexical scope.

The walk is local and syntax-directed. It does not build an AST with binding
records. Each rewrite either recognizes a precise expanded-source shape or
delegates to a generic recursive walk.

The optimizer emits two compiler-private forms:

```scheme
(##arete#inline-let FORMALS VALUES BODY ...)
(##arete#named-loop NAME FORMALS INITS BODY ...)
```

These forms are not user-facing Scheme. They are source-level markers for the
compiler, and the compiler is responsible for lowering them into bytecode.

## Source Representation

The optimizer works on Arete's expanded source representation, where ordinary
lists may carry source metadata. When constructing rewritten source, preserve
metadata with the local constructors:

- `list-source` when constructing a fresh proper list from a source form.
- `cons-source` when rebuilding a pair while preserving the old pair's source.

Do not use plain `cons` or `map` for source-tree rewrites unless source loss is
intentional and harmless. Constant propagation is the most source-sensitive
rewrite because it substitutes inside arbitrary subtrees; its generic list
rebuild uses `cons-source` for this reason.

Special-form heads are identifiers, often renames. Use `optimizer-head-name`
instead of raw `car` comparison when matching forms:

```scheme
(eq? (optimizer-head-name x) 'if)
```

Use `identifier=?` for local binding equality. Use `rename-strip` only when
comparing a head name to a core syntactic keyword or primitive name.

Quote is a hard boundary for the optimizer. Rewrites must not inspect or
substitute inside quoted data.

## Result Contexts

The optimizer carries explicit cp0-style result contexts:

| Context | Meaning |
| --- | --- |
| `value` | The expression's value is needed as a normal value. |
| `effect` | The expression is evaluated before a later body expression. |
| `test` | The expression controls a conditional branch. |
| `tail` | The expression is in tail position for the current lambda or loop. |
| `application` | The expression is the callee of an application. |

Current context use is intentionally modest:

- `tail` controls named-loop lowering; the loop procedure may only be called
  from tail positions.
- `test` enables safe boolean-wrapper simplification and branch inversion.
- `value`, `effect`, and `application` make traversal intent explicit and leave
  room for later effect and callee analyses.

Do not remove an `effect` expression merely because it appears simple. The
optimizer does not yet have a complete effect model, and preserving error
timing is part of the source optimizer contract.

## Shadowing Model

Primitive rewrites are guarded by a small lexical shadow set. The set contains
primitive names that have been introduced as lambda formals or local defines in
the current scope. If a name is in `shadowed`, the optimizer must not treat a
use of that name as the global primitive.

This model is intentionally conservative:

- It tracks only primitive names that matter to optimizer metadata.
- It is threaded through lambda bodies and body-level defines.
- It is not a general lexical environment and does not know reference counts.

When adding a primitive-dependent rewrite, require all of the following unless
the rewrite has stronger binding facts:

- The head name matches the primitive after stripping renames.
- The primitive is not present in `shadowed`.
- `COMPILER-VM-PRIMITIVES` is enabled if the rewrite depends on VM primitive
  result behavior.
- The arity matches `OptimizerPrimitive` metadata.

## Rewrite Details

### Direct Lambda Lowering

Inside lambda bodies, a direct lambda application:

```scheme
((lambda (x y) body ...) a b)
```

lowers to:

```scheme
(##arete#inline-let (x y) (a b) body ...)
```

This is controlled by `OPTIMIZER-INLINE-CAR` and the compatibility flag
`COMPILER-INLINE-CAR`. The rewrite is limited to lambda-body contexts because
the compiler-private form is a compiler convention, not a general source form.

Arguments are optimized in `value` context before the inline-let is emitted.
The body is optimized in the parent context, so a direct lambda call in tail
position keeps its body in tail context.

### Constant Propagation

`OPTIMIZER-CONSTANT-PROP` extends direct lambda lowering. Before emitting
`##arete#inline-let`, it may substitute safe immediate argument constants into
the direct lambda body and remove those formals from the inline-let binding
list.

Allowed propagated values are the same source-level immediates used for safe
`eq?` reasoning:

```scheme
#t #f fixnums symbols ()
```

The rewrite requires:

- Exact argument count.
- A proper formal list of unique identifiers.
- A value that `optimizer-immediate-constant-expr` can reify safely.
- No `set!` or `define` of that formal in the body region where substitution
  would apply.
- Nested lambdas are scanned only when they do not shadow the formal.

Pairs, vectors, strings, closures, and other heap objects are not propagated.
Even when quoted, they may expose allocation or identity behavior that this
optimizer does not yet model.

Example:

```scheme
((lambda (x y) (fx+ x y)) 1 n)
```

can become an inline-let for only `y`, with `x` replaced by `1` in the body.

### Named Let Lowering

The expander currently produces a recognizable shape for named `let`:

```scheme
((lambda ()
   (define loop (lambda (args ...) body ...))
   (loop inits ...)))
```

`OPTIMIZER-NAMED-LET` lowers this to `##arete#named-loop` when all checks pass.
The optimizer verifies that the loop procedure is used only as the head of tail
calls, is not assigned, and is not captured by a nested lambda.

The usage scan is intentionally strict. If the loop name appears as a value,
inside a non-tail call, or inside a nested lambda that can capture it, the
rewrite is refused.

### Primitive Folding

Primitive folding is controlled by `OPTIMIZER-PRIMITIVE-FOLD`. A fold requires:

- `COMPILER-VM-PRIMITIVES` enabled.
- The primitive not shadowed.
- The primitive marked foldable and VM-backed in `OptimizerPrimitive`.
- Exact arity.
- All required arguments known as constants.

Current folds cover:

- `not` over a known constant.
- `eq?` over safe immediates only.
- `null?` over a known constant.
- `fixnum?` over a known constant.
- `fx+`, `fx-`, and `fx<` over small known fixnums.

Small-fixnum checks deliberately refuse very large values. The optimizer should
not create source constants that are outside the assumptions of the current VM
fixnum fast paths.

`pair?` and `symbol?` have metadata but are not broadly folded as source
optimizer facts. `pair?` has compiled true-value caveats, and `symbol?` is not
currently a compiler VM primitive.

### Constant If

`OPTIMIZER-CONSTANT-IF` simplifies conditionals whose optimized test is known.
It is disabled by default because benchmark isolation did not show a runtime
win.

The false branch of a 4-armed `if` is not removed when it is the expander's
unspecified value. This avoids making unspecified results more observable than
they are in optimizer-off compiler behavior.

### Boolean If And Test Rewrites

`OPTIMIZER-TEST-IF` contains several conditional rewrites that rely on Scheme
truthiness and exact boolean results.

In `test` context:

```scheme
(if e #t #f)
```

can simplify to `e`, because both forms branch the same way. This is not
generally valid in value context, where `(if e #t #f)` canonicalizes any
truthy value to `#t`.

In value-like contexts, the same wrapper is simplified only when the optimized
condition is known to return exactly `#t` or `#f`. The current exact-boolean
primitive set is:

```scheme
not eq? null? fixnum? fx<
```

These require VM primitives enabled, exact arity, and no primitive shadowing.

The optimizer also inverts branches to remove negated test wrappers:

```scheme
(if (if e #f #t) then else)
```

becomes:

```scheme
(if e else then)
```

For primitive `not`, this:

```scheme
(if (not e) then else)
```

becomes:

```scheme
(if e else then)
```

The new condition is optimized in `test` context so composed wrappers can keep
simplifying. The original branches are optimized in the original outer context
after swapping, preserving tail/test/effect intent.

These rewrites intentionally do not introduce a new `(not e)` call. Introducing
`not` would add a binding reference that may be shadowed.

## Counters And Logging

Enable counters with:

```scheme
(set-top-level-value! 'COMPILER-OPTIMIZER-COUNTERS #t)
(optimizer-reset-counters!)
...
(optimizer-counter-snapshot)
```

Counters are low-overhead attribution tools, not a full trace. Some refusal
counts are expected to be noisy. For example, a primitive application can count
as `primitive-fold-refused-unknown` before a later conditional rewrite removes
the surrounding form.

Enable textual logs with:

```scheme
(set-top-level-value! 'COMPILER-OPTIMIZER-LOG #t)
```

Logs should be used for short local investigations only. Tests should assert
source shape, runtime behavior, or counters rather than exact log output.

## Adding A Rewrite

For each new optimizer rewrite:

1. Add or reuse a dedicated top-level flag unless the rewrite is clearly part
   of an existing family.
2. Snapshot the flag in `OptimizerConfig`.
3. Define the exact source shape accepted by the rewrite.
4. State the safety proof in terms of value, effects, allocation, error timing,
   and primitive shadowing.
5. Preserve source metadata with `list-source` and `cons-source`.
6. Add optimizer shape tests for positive and refusal cases.
7. Add compiler/runtime tests when generated code behavior can change.
8. Measure the family in isolation with a family-disabled heap.
9. Document the result and leave the family disabled by default if benchmark
   evidence is weak or mixed.

## Invariants

Keep optimizer-off behavior as a permanent compatibility path. Every optimizer
family needs its own flag while it is young, and `COMPILER-OPTIMIZE #f` must
fully bypass source rewrites.

Snapshot top-level flags once per `optimize-toplevel` call through
`OptimizerConfig`; avoid recursive top-level flag lookups inside optimizer
walks.

Do not move `primitive-table` out of the compiler just to make a fold easier.
The optimizer may keep descriptive primitive metadata, but opcode choice stays
in `scheme/compiler.scm`.

Do not add VM opcodes as part of source optimizer work. Treat VM opcode changes
as separate measured projects.

Do not fold or remove code when doing so changes error timing, error class,
effect order, allocation visibility, or the value observed in optimizer-off
compiler tests.

When a primitive name may be shadowed, do not fold it unless the optimizer has
binding facts strong enough to prove it still refers to the same global
primitive rule used by the compiler.

## Correctness Gates

Run these before treating an optimizer change as complete:

```sh
python3 utils/run-tests.py optimizer compiler compiler-noopt
make test-all
rm -f heap.boot && make heap.boot
```

For changes that touch bootstrap-loaded behavior, also run the psyntax stress
path:

```sh
bin/arete bootstrap-and-psyntax.scm
```

Add tests in `tests/optimizer/` for optimized source shape and refusal cases.
Add runtime/compiler tests when the rewrite changes code that later reaches
bytecode lowering.

## Benchmark Workflow

Benchmark source optimizer work against workloads where source rewrites can
matter. For this campaign, prefer symbolic/parser/compiler-like benchmarks:

```text
browse deriv earley parsing peval
```

Avoid using numeric-heavy or allocation-only workloads as the main signal for
source-level rewrites. `fib`, `tak`, `gcbench`, bignum/flonum-heavy kernels,
and pure GC churn are useful only when the changed family plausibly affects
calls, arithmetic, or allocation behavior.

### Build Comparison Heaps

Compare whole-optimizer behavior by building two heaps:

```sh
mkdir -p scratch/optimizer-bench
bin/arete boot.scm --save-image scratch/optimizer-bench/heap.opt.boot
bin/arete --set COMPILER-OPTIMIZE '#f' boot.scm \
  --save-image scratch/optimizer-bench/heap.noopt.boot
```

Compare one optimizer family by building a third heap with just that family
disabled:

```sh
bin/arete --set OPTIMIZER-PRIMITIVE-FOLD '#f' boot.scm \
  --save-image scratch/optimizer-bench/heap.noprimfold.boot
```

Use the same pattern for future families:

```sh
bin/arete --set OPTIMIZER-SOME-FAMILY '#f' boot.scm \
  --save-image scratch/optimizer-bench/heap.no-some-family.boot
```

### Run Source-Heavy R7RS Benchmarks

Use absolute heap paths because the R7RS runner executes from the vendored
benchmark directory:

```sh
ROOT=$(pwd)
BENCHES="browse deriv earley parsing peval"

RESULTS=$ROOT/scratch/optimizer-bench/r7rs-opt.log \
HEAP=$ROOT/scratch/optimizer-bench/heap.opt.boot \
CPU_LIMIT=180 \
utils/run-r7rs-benchmarks.sh $BENCHES

RESULTS=$ROOT/scratch/optimizer-bench/r7rs-noopt.log \
HEAP=$ROOT/scratch/optimizer-bench/heap.noopt.boot \
CPU_LIMIT=180 \
utils/run-r7rs-benchmarks.sh $BENCHES
```

For one-family isolation, compare `heap.opt.boot` with the family-disabled
heap, not with `heap.noopt.boot`.

The runner emits grep-friendly rows:

```sh
grep '^+!CSVLINE!+' scratch/optimizer-bench/r7rs-*.log
```

### Run Bootstrap Timings

Bootstrap is both a correctness workload and a cost signal. Use repeated runs;
single samples are too noisy:

```sh
python3 - <<'PY'
import statistics, subprocess, time
from pathlib import Path

root = Path.cwd()
cases = [
    ("save-opt", [str(root / "bin/arete"), "boot.scm", "--save-image"]),
    ("save-noopt", [str(root / "bin/arete"), "--set", "COMPILER-OPTIMIZE", "#f",
                    "boot.scm", "--save-image"]),
]

for name, prefix in cases:
    samples = []
    sizes = []
    for i in range(7):
        path = root / "scratch/optimizer-bench" / f"{name}-{i}.boot"
        start = time.perf_counter()
        process = subprocess.run(prefix + [str(path)], cwd=root,
                                 text=True, stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT)
        elapsed = time.perf_counter() - start
        if process.returncode:
            print(process.stdout)
            raise SystemExit(process.returncode)
        samples.append(elapsed)
        sizes.append(path.stat().st_size)
        path.unlink()
    print(name, "best", min(samples), "median", statistics.median(samples),
          "size", sizes[-1])
PY
```

Also time `bootstrap-and-psyntax.scm` when changes touch macro-heavy or
expander-heavy paths:

```sh
bin/arete bootstrap-and-psyntax.scm
bin/arete --set COMPILER-OPTIMIZE '#f' bootstrap-and-psyntax.scm
```

### Interpreting Results

Use source-heavy R7RS results to decide whether a source rewrite is worth
continuing. Use bootstrap timings to decide whether compile-time overhead and
image size are acceptable.

Keep a rewrite when it improves multiple relevant source-heavy workloads or has
a clear structural reason to help later work, and does not regress bootstrap,
`bootstrap-and-psyntax`, or image size beyond noise.

Treat `peval`, `earley`, `parsing`, `bootstrap-and-psyntax`, and heap image
size as high-signal blockers for source optimizer work. If one of these
regresses, either explain why the family should remain experimental or leave
the family disabled by default.

Measure each optimizer family independently. Large wins from the whole
optimizer can hide a new family that is only noise or a regression.

## Recent Benchmark Baseline

The May 16, 2026 source-heavy comparison used:

```text
browse deriv earley parsing peval
```

Whole optimizer on versus `COMPILER-OPTIMIZE #f`:

| Benchmark | Optimizer on | Optimizer off | Delta |
| --- | ---: | ---: | ---: |
| `browse:2000` | 1.555s | 2.664s | -41.6% |
| `deriv:10000000` | 10.868s | 11.600s | -6.3% |
| `earley:1` | 15.777s | 26.414s | -40.3% |
| `parsing:2500` | 15.915s | 19.730s | -19.3% |
| `peval:2000` | 18.251s | 24.534s | -25.6% |

`OPTIMIZER-PRIMITIVE-FOLD` isolated against the rest of the optimizer:

| Benchmark | Primitive fold on | Primitive fold off | Delta |
| --- | ---: | ---: | ---: |
| `browse:2000` | 1.569s | 1.607s | -2.4% |
| `deriv:10000000` | 10.776s | 10.671s | +1.0% |
| `earley:1` | 16.000s | 15.736s | +1.7% |
| `parsing:2500` | 15.710s | 16.308s | -3.7% |
| `peval:2000` | 18.260s | 18.500s | -1.3% |

Takeaway: the optimizer as a whole is clearly useful on the right workloads.
Primitive folding is currently noise-level to modest; future primitive work
needs either broader folds, lower optimizer overhead, or a clearer benchmark
win.

`OPTIMIZER-CONSTANT-IF` isolated against the rest of the optimizer, using the
average of two samples:

| Benchmark | Constant-if on | Constant-if off | Delta |
| --- | ---: | ---: | ---: |
| `browse:2000` | 1.543s | 1.564s | -1.3% |
| `deriv:10000000` | 10.793s | 10.773s | +0.2% |
| `earley:1` | 15.929s | 15.957s | -0.2% |
| `parsing:2500` | 16.166s | 15.773s | +2.5% |
| `peval:2000` | 18.462s | 18.210s | +1.4% |

Constant-if reduced the boot image by about 3 KB but did not show a clear
runtime win, so it is disabled by default for now.

`OPTIMIZER-CONSTANT-PROP` isolated against the rest of the optimizer:

- Focused direct-lambda constant microbenchmark, 20M loop iterations:
  `0.175s` best with constant propagation versus `0.211s` best without,
  a `-17.1%` best-time delta.
- Source-heavy single samples were mixed/noisy: `parsing` improved
  `16.394s -> 16.184s`, `peval` improved `18.282s -> 18.192s`, `deriv` and
  `earley` were flat, and repeated `browse` samples were effectively neutral
  (`1.573s` median on versus `1.552s` median off).
- Bootstrap counters showed no hits in the current boot workload, so this
  family is mainly a targeted cleanup for direct-lambda constant arguments.

`OPTIMIZER-TEST-IF` isolated against the rest of the optimizer:

- Focused boolean-wrapper test microbenchmark, 30M loop iterations:
  `0.306s` best with test-if simplification versus `0.355s` best without,
  a `-13.8%` best-time delta.
- Focused negated-wrapper branch-inversion microbenchmark, 30M loop iterations:
  `0.306s` best with inversion versus `0.368s` best with `OPTIMIZER-TEST-IF`
  disabled, a `-16.8%` best-time delta.
- Focused primitive-`not` branch-inversion microbenchmark, 30M loop iterations:
  `0.307s` best with inversion versus `0.327s` best with `OPTIMIZER-TEST-IF`
  disabled, a `-6.1%` best-time delta.
- Focused value-context exact-boolean wrapper microbenchmark, 30M loop
  iterations: final rerun was noise-level at `0.375s` best with simplification
  versus `0.380s` best with `OPTIMIZER-TEST-IF` disabled.
- Final current source-heavy single samples after the value-context and
  primitive-`not` additions were mixed/noisy:

| Benchmark | Test-if on | Test-if off | Delta |
| --- | ---: | ---: | ---: |
| `browse:2000` | 1.598s | 1.547s | +3.3% |
| `deriv:10000000` | 10.751s | 10.717s | +0.3% |
| `earley:1` | 15.570s | 15.621s | -0.3% |
| `parsing:2500` | 16.340s | 15.965s | +2.3% |
| `peval:2000` | 18.300s | 18.372s | -0.4% |

## Next Work

Good next slices:

- Broaden counters into structured refusal reasons where that helps explain
  optimizer decisions.
- Revisit `OPTIMIZER-CONSTANT-IF` after test-context tracking or other changes
  make it more likely to reduce runtime work.
- Use the explicit contexts to add measured effect/application-specific
  rewrites only after the required binding and effect facts exist.
- Build small binding records for local names, assignment, and reference
  counts before attempting copy propagation or function integration.
- Decide whether `OPTIMIZER-INLINE-CAR` should be aliased or renamed to
  `OPTIMIZER-DIRECT-LAMBDA`.

Do not start with recursive inlining, cross-library optimization, record/vector
knowledge, continuation-sensitive rewrites, or new VM opcodes.

## Work Item Template

Each optimizer task should record:

- The single optimization family being changed.
- The flag controlling it.
- The source forms it may rewrite.
- The exact safety conditions that must hold.
- The refusal cases covered by tests.
- The files touched.
- The correctness commands run.
- The source-heavy benchmark comparison, if the change makes a performance
  claim.
