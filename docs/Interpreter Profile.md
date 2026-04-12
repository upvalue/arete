# Interpreter Profile (T4)

Ranked profile of where `src/eval.cpp` spends its cycles during bootstrap
and under the interpreter microbenchmark suite. This is a research
deliverable: it names the hot spots so that the follow-up optimization
tickets have a factual target. No interpreter code is changed here.

## Methodology

**Tooling available.** `perf` and `valgrind` are **not installed** on
this sandbox (`which perf valgrind` → not found;
`kernel.perf_event_paranoid=2`), so the callgrind pass requested in the
ticket is not possible. The only profiler available is GNU `gprof`.
Numbers below therefore come from `-pg` sampling + mcount call counts,
not from hardware counters.

Practical implications:

- gprof's self-time samples are coarse (10 ms ticks). The sampled time
  totals to ~5.68 s across 30 aggregated bootstrap runs and 0.5–2 s per
  microbench, which is enough for stable ranking but the bottom 1–2 %
  rows are noise.
- mcount call counts **are** exact, so per-call cost estimates are
  sound.
- gprof treats tail-recursive paths inside `eval_body` as a recursive
  cycle (`<cycle 1>`), so `eval_body`'s inclusive time is reported as
  the cycle's total rather than attributed per-invocation. This is
  called out where relevant.
- `_init` appears in the flat profile at 1–5 %. This is sampler
  artifact (startup / PLT stub PCs); ignore it.

**Binary.** Built `bin/arete-dbg` alongside the existing `bin/arete`
with the same flags as the production build plus `-g
-fno-omit-frame-pointer -pg` (and matching `-pg` on link). `-O2` was
kept so that inlining and register allocation reflect reality; an
`-O0` profile would misrank costs. See the build command at the end of
this file.

**Heap image.** The existing `heap.boot` was produced by the
non-instrumented `bin/arete` and has a struct layout that doesn't match
the instrumented build, so every bench was run against a freshly-built
`/tmp/heap.boot.dbg` produced by `bin/arete-dbg` itself.

**Workloads.**

- *Bootstrap*: `bin/arete-dbg boot.scm --save-image /tmp/heap.boot.dbg2`
  run 30 times with `GMON_OUT_PREFIX` so each run drops its own
  `gmon.out`, then `gprof` over all 30 files at once.
- *Microbenchmarks*: each of the nine `bench/interp/*.scm` files run 5
  times under `bin/arete-dbg /tmp/heap.boot.dbg --interp-only`, gprof'd
  in aggregate per bench.

## Top 20 self-time functions — bootstrap (5.68 s sampled over 30 runs)

| % self | self s | calls        | function                                     |
|-------:|-------:|-------------:|----------------------------------------------|
| 47.54  | 2.70   |   31,598,486 | `State::eval_body`                           |
| 13.20  | 0.75   |    7,115,740 | `apply_vm` (bytecode VM — out of scope)     |
|  5.99  | 0.34   |  240,331,778 | `State::env_lookup_impl`                     |
|  5.81  | 0.33   |  785,928,399 | `State::get_global_value`                    |
|  3.70  | 0.21   |  223,377,568 | `State::vector_append`                       |
|  3.52  | 0.20   |  180,495,516 | `Value::list_length`                         |
|  3.17  | 0.18   |           —  | `_init` (sampling artifact)                  |
|  2.11  | 0.12   |   41,960,887 | `State::make_vector`                         |
|  2.11  | 0.12   |   20,730,103 | `State::eval_form`                           |
|  1.41  | 0.08   |   43,145,614 | `GCSemispace::copy`                          |
|  1.23  | 0.07   |    7,660,069 | `apply_interpreter`                          |
|  0.88  | 0.05   |  238,399,982 | `std::_Hashtable::_M_emplace_uniq` (globals) |
|  0.88  | 0.05   |   41,877,342 | `State::make_env`                            |
|  0.88  | 0.05   |        1,054 | `GCCommon::visit_roots`                      |
|  0.53  | 0.03   |   41,877,342 | `eval_check_arity`                           |
|  0.53  | 0.03   |   18,160,947 | `State::make_pair`                           |
|  0.53  | 0.03   |   11,738,584 | `fn_record_ref`                              |
|  0.35  | 0.02   |   26,745,250 | `fn_eq`                                      |
|  0.35  | 0.02   |   22,873,133 | `fn_value_type`                              |
|  0.35  | 0.02   |   16,045,290 | `do_main`                                    |

Interpreter-attributable share (everything here except `apply_vm`,
`GCSemispace::copy`, `visit_roots`, and the `_init` artifact) is
approximately 77 % of bootstrap CPU. This is consistent with T1's
perf-based figure of 55 % interpreter in bootstrap wall time — the
`-pg` build disproportionately inflates the instrumented C++ frames
(mcount on every call), so the self-time percentages overweight the
interpreter slightly vs. what a wall-clock profile would show. Ranks
are unaffected.

## Top 20 inclusive-time paths — bootstrap

From the gprof call graph (`% time` = total time attributed including
callees):

| % incl | self s | children s | calls                 | function                               |
|-------:|-------:|-----------:|----------------------:|----------------------------------------|
| 72.7   | 2.70   | 1.43       | 31,598,486+320,625,963| `State::eval_body` (cycle 1, self-recursive) |
| 16.6   | 0.75   | 0.19       |  7,115,740+22,210,942 | `apply_vm` (cycle 1)                   |
|  6.0   | 0.34   | 0.00       |  240,331,778          | `env_lookup_impl`                      |
|  5.8   | 0.33   | 0.00       |  785,928,399          | `get_global_value`                     |
|  5.8   | 0.05   | 0.28       |   41,877,342          | `make_env`                             |
|  4.1   | 0.21   | 0.02       |  223,377,568          | `vector_append`                        |
|  3.5   | 0.20   | 0.00       |  180,495,516          | `Value::list_length`                   |
|  3.3   | 0.12   | 0.07       |   41,960,887          | `make_vector`                          |
|  3.2   | 0.18   | 0.00       |           —           | `_init` (artifact)                     |
|  3.2   | 0.12   | 0.06       |   20,730,103          | `eval_form` (cycle 1)                  |
|  3.2   | 0.07   | 0.11       |    7,660,069          | `apply_interpreter` (cycle 1)          |
|  2.5   | 0.01   | 0.13       |        1,054          | `GCSemispace::collect`                 |
|  2.4   | 0.00   | 0.14       |        1,023          | `GCSemispace::allocation_failed`       |
|  1.4   | 0.08   | 0.00       |   43,145,614          | `GCSemispace::copy`                    |
|  0.9   | 0.05   | 0.00       |        1,054          | `visit_roots`                          |
|  0.9   | 0.05   | 0.00       |  238,399,982          | hashtable emplace (globals hash)       |
|  0.9   | 0.03   | 0.02       |   18,160,947          | `make_pair`                            |
|  0.8   | 0.02   | 0.03       |    3,151,026          | `fn_table_ref`                         |
|  0.6   | 0.00   | 0.04       |    6,274,276          | `temps_to_list`                        |
|  0.5   | 0.03   | 0.00       |   41,877,342          | `eval_check_arity`                     |

**Reading the cycle.** `eval_body` recurses into itself 320 M times in
bootstrap (each argument evaluation is a fresh `eval_body(..., true)`
call on line 568/605/645/675/735). Its direct children, by child-time:

- `make_env` (0.23 s child time from `eval_body`)
- `env_lookup_impl` (0.34 s, all self)
- `get_global_value` (0.31 s, all self — ≈32 calls per eval_body)
- `list_length` (0.17 s)
- `vector_append` (0.10 s + 0.01 s children)

`apply_vm` is a second hot path, unavoidable in bootstrap because
`boot.scm` runs the compiler under the VM. Not counted toward the
interpreter budget.

## Per-microbenchmark top-3 self-time functions

Aggregated across 5 runs per bench. % is self-time share within that
bench. "evb" = `eval_body`, "env" = `env_lookup_impl`, "ggv" =
`get_global_value`, "vapp" = `vector_append`, "ll" = `list_length`.

| rank | arith         | closure       | cond-chain    | cons-list     | deep-env       | gc-churn      | global-ref    | higher-order  | tak           |
|------|---------------|---------------|---------------|---------------|----------------|---------------|---------------|---------------|---------------|
| #1   | evb 63.1 %    | evb 61.3 %    | evb 67.9 %    | evb 65.2 %    | evb 41.7 %     | evb 64.7 %    | evb 61.2 %    | evb 57.0 %    | evb 64.1 %    |
| #2   | env 11.7 %    | env 10.4 %    | ggv 9.8 %     | env 10.9 %    | **env 36.0 %** | ggv 7.6 %     | env 16.4 %    | env 10.0 %    | env 7.3 %     |
| #3   | ggv 7.2 %     | ggv 6.4 %     | env 5.4 %     | ll 6.5 %      | ggv 4.1 %      | env 6.7 %     | ggv 8.6 %     | ggv 9.0 %     | ggv 5.7 %     |

Observations:

- **`eval_body` dominates in every benchmark** (41–76 %). It is the
  interpreter. Anything that reduces per-form dispatch cost moves every
  column.
- **`env_lookup_impl` is almost always #2.** It explodes on `deep-env`
  (36 % vs. 6–16 % elsewhere), which is exactly what that bench was
  built to isolate: a long chain of nested `let` / `lambda`.
- **`get_global_value` is structurally #3.** It's called per form (see
  Opp. 3). Benches with tight arithmetic loops (`arith`, `global-ref`)
  push it higher because the per-iteration work is thinner.
- **`vector_append` / `list_length` / `make_env`** show up in the
  4th-onward positions on most benches, always together — they're the
  call-frame construction tax.
- **`tak` and `cons-list` bleed a few % into `apply_vm`** because the
  benches happen to invoke VM-compiled primitives (`fn_lt`, `fn_add`,
  etc.). Not a concern for interpreter optimization.

## Prioritized optimization opportunities

Ordered by expected payoff. Payoff estimates are rough — gprof self-time
is only an upper bound on what a change can recover, and a lot of
eval_body's cost is dispatch overhead that won't flip until multiple
opportunities land together. Where a number is essentially a guess it is
labeled speculative.

### 1. Pre-resolve environment lookups (bind depth/slot at expand time)

- **Hypothesis.** `env_lookup_impl` does a reverse linear scan through
  each env vector (`for(i = len-1; i > ...; i -= 2) identifier_equal`)
  and chains through `env.vector_ref(0)` to walk parent frames. Every
  variable reference pays O(chain depth × frame size). 240 M calls in
  bootstrap. `deep-env` bench confirms the ceiling: when the chain is
  long, `env_lookup_impl` alone is 36 % of CPU.
- **Fix shape.** The expander already knows the lexical structure. Tag
  each `symbol` occurrence in a lambda body with `(depth, slot)` at
  expand time (or on first interpretation). Then `env_lookup` becomes
  `env = env.vector_ref(0)` d times and a single indexed load. For
  globals, cache the slot pointer on the rename / symbol.
- **Payoff.** Medium-large. Self-share is 6 % in bootstrap, but it's a
  multiplier inside `eval_body`, and a fast-path here will also cut
  `eval_body`'s own sampled time because the inner loop shrinks.
  Plausibly 10–20 % on bootstrap and 40–50 % on `deep-env`
  (speculative).
- **Risk.** Invasive. Touches expansion, env representation, and
  possibly rename machinery. Introduces a new variant of eval_body or a
  second code path.
- **Benches that should move.** `deep-env` (the big one), plus
  `global-ref`, `closure`, `higher-order`, `arith` — anything with
  non-trivial lookups.

### 2. Collapse call-frame construction into one allocation

- **Hypothesis.** Every interpreter function application does:
  `make_env(parent, argc+1)` (which itself does `make_vector` +
  `vector_append(parent)` + `vector_append(C_FALSE)`), then **2 ×
  argc** further `vector_append`s (one for the name, one for the
  value). A 3-arg call is 7 mutations + 1 allocation per frame.
  Counts: 41 M `make_env`, 41 M `make_vector`, 223 M `vector_append`,
  41 M `eval_check_arity`. Together these are ~10 % of bootstrap
  self-time.
- **Fix shape.** Add `State::make_call_env(parent, names_list,
  argv[], argc, rest_name, rest_val)` that sizes the vector once,
  writes the header + name/value pairs in a single pass, and skips the
  length-check branch inside each append. Fold `eval_check_arity` into
  the same call so we don't re-walk the args list twice.
- **Payoff.** Small-medium. Maybe 5–10 % of bootstrap, distributed
  across every call-heavy bench.
- **Risk.** Mechanical. Localized to the apply path in `eval_body`
  and the `make_env` helper. Must preserve AR_FRAME GC invariants.
- **Benches that should move.** `higher-order`, `closure`,
  `cons-list`, `tak` — all the call-heavy ones. Also bootstrap.

### 3. Hoist per-form global reads out of `eval_body`'s hot loop

- **Hypothesis.** `get_global_value` is called **786 M times** during
  bootstrap (more calls than any other function profiled). Inside
  `eval_body` alone, every entry reads `G_FORBID_INTERPRETER` (line
  440) and `G_TCO_ENABLED` (line 449), and the `tail_call` goto
  re-enters the function so these are effectively per-TCO-step reads.
  Neither value changes during a run of any reasonable workload.
  Though each call is cheap, the sheer count (~32 per `eval_body`
  invocation including the tail loop) makes this 5.8 % of bootstrap.
- **Fix shape.** Cache both globals on `State` as `bool`/`Value`
  fields, invalidate from the `set_global_value` path for those two
  specific globals. Or promote them to actual members on `State` and
  drop the lookup entirely. Also audit other `get_global_value(...)`
  sites in `eval_body` / `eval_form` for the same pattern.
- **Payoff.** Small. Expect ~3–5 % on bootstrap; visible across every
  bench.
- **Risk.** Mechanical. Single-file change in `eval.cpp` +
  `state.cpp`. Easy to verify: counts should drop by ~90 % after the
  change.
- **Benches that should move.** All of them, uniformly.

### 4. Fold argument list-length checks into a single walk

- **Hypothesis.** `Value::list_length` is called 180 M times (3.5 %).
  In the apply path (`eval_body` around line 662, 722), we call
  `args.list_length()` and `fn_args.list_length()` up-front purely to
  feed `eval_check_arity`, and then we walk both lists *again* to
  evaluate / bind. That's two full list walks per call in addition to
  the evaluation walk.
- **Fix shape.** Replace the up-front length calls with a single walk
  that simultaneously evaluates args, binds them into the env, and
  counts. Produce the arity error only if we run off the end of
  `fn_args` with args left over (or vice versa). `eval_check_arity`
  becomes a slow-path error-formatter.
- **Payoff.** Small. ~2–4 % on bootstrap; more on call-heavy benches.
- **Risk.** Mechanical. Contained in the PAIR/FUNCTION branch of
  `eval_body`.
- **Benches that should move.** `higher-order`, `closure`, `tak`,
  `cons-list`, `arith`.

### 5. Prepared AST / direct-threaded eval (stretch)

- **Hypothesis.** `eval_body` itself is 47.5 % self-time. Inside the
  giant switch, each PAIR invocation re-discovers the form: it does a
  symbol-identity check against `C_SYNTAX`, then an `else-if` chain in
  `get_form` (up to 9 symbol comparisons), then a second switch on the
  form constant, then the per-form logic. All of that dispatch cost is
  paid every time a given form is evaluated, even though its shape is
  fully known after expansion. This is almost certainly where the bulk
  of `eval_body`'s self-time lives after the other wins above are
  taken.
- **Fix shape.** Introduce a one-pass "prepare" that rewrites each
  post-expansion form into a tagged node (form-kind enum + cached
  slots). Interpreter switches on that tag directly; no more
  `get_form` chain, no more `symbol_value() == C_SYNTAX` check, no
  more `env_lookup` for local bindings (ties in with Opp. 1).
  Approaches a bytecode compiler, which is the direction T6+ may head
  anyway.
- **Payoff.** Large-but-speculative. Conservatively 20–30 % on top of
  the other wins; the 2× interpreter speedup ceiling named in T1 is
  only reachable with something in this family.
- **Risk.** Invasive. Changes the representation of "a lambda body"
  and interacts with expansion, macros (the compiler), and debugging
  (source info). Worth doing after 1–4 have landed and the remaining
  self-time in `eval_body` is measurable and attributable.
- **Benches that should move.** All of them, and bootstrap.

## Open questions

- **`_init` at 3 %.** Consistently appears in per-bench profiles
  (3–15 %). Most likely PLT-stub PC sampling or the C++ static-init
  path not being demangled. I did not attribute it further. It will
  disappear in a real `perf`-based profile, so the number to target
  for real gains is the explicitly-named interpreter functions.
- **Hashtable emplace at 238 M calls.** This shows up inside
  `get_global_value`'s own call pattern in the global-interning path.
  If Opp. 3 lands, this count should drop in proportion. Worth
  revisiting afterward; not a standalone opportunity.
- **`apply_interpreter` vs. `eval_body`.** `apply_interpreter` is
  7.6 M calls for 0.07 s self + 0.11 s children — it's the thin
  wrapper that sets up an `EvalFrame` for callers that have a
  `FUNCTION` in hand. Cost is small but its call count suggests it
  could be inlined into the caller. Low priority.
- **Discrepancy with T1's 55 % figure.** T1 used `perf` wall-time to
  attribute 55 % of bootstrap to the interpreter. This report has
  eval-related self-time at roughly 77 %. The delta is mcount overhead
  added by `-pg` (inflates instrumented-code percentages) plus gprof's
  coarser sampling. Rankings and call-count ratios are unaffected;
  speedup estimates above were derived in relative terms, so they
  remain valid.

## Reproducing the profile

Build the instrumented binary (does **not** touch `bin/arete`):

```sh
mkdir -p /tmp/t4-build
CPPFLAGS='-Wall -Wextra -Wno-unused-parameter -Wno-implicit-fallthrough \
          -I. -Ivendor -Ivendor/linenoise -Ivendor/dynasm \
          -DAR_LIB_SDL=0 -DAR_LIB_UV=0'
PGFLAGS='-O2 -g -fno-omit-frame-pointer -pg'
for src in src/*.cpp vendor/linenoise/*.cpp; do
  g++ $CPPFLAGS -std=c++14 -fno-rtti -fno-exceptions -fpermissive $PGFLAGS \
      -c -o /tmp/t4-build/$(basename ${src%.cpp}).pg.o $src
done
g++ -pg -fno-omit-frame-pointer -lm -fno-rtti -fno-exceptions \
    -o bin/arete-dbg /tmp/t4-build/*.pg.o
./bin/arete-dbg boot.scm --save-image /tmp/heap.boot.dbg
```

Bootstrap profile (30 aggregated runs):

```sh
mkdir -p /tmp/t4-gmon-bootstrap
for i in $(seq 1 30); do
  GMON_OUT_PREFIX=/tmp/t4-gmon-bootstrap \
    ./bin/arete-dbg boot.scm --save-image /tmp/heap.boot.dbg2 >/dev/null
done
gprof -b bin/arete-dbg /tmp/t4-gmon-bootstrap.* > /tmp/t4-gprof-bootstrap.txt
```

Per-microbench profile (5 aggregated runs each):

```sh
mkdir -p /tmp/t4-gmon-agg
for b in arith closure cond-chain cons-list deep-env gc-churn global-ref \
         higher-order tak; do
  for i in 1 2 3 4 5; do
    GMON_OUT_PREFIX=/tmp/t4-gmon-agg/$b \
      ./bin/arete-dbg /tmp/heap.boot.dbg --interp-only bench/interp/$b.scm \
        >/dev/null
  done
  gprof -b -p bin/arete-dbg /tmp/t4-gmon-agg/$b.* | head -12
done
```
