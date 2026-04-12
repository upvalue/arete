# Benchmarks: ecraven

Current status of [ecraven/r7rs-benchmarks](https://github.com/ecraven/r7rs-benchmarks)
(vendored at `vendor/r7rs-benchmarks`) running under Arete via
`utils/run-r7rs-benchmarks.sh` / `make r7rs-bench`. For how the ecraven
suite fits with the other performance signals Arete tracks, see
[[Benchmarking]].

All 57 benchmarks were executed with `CPU_LIMIT=30` (per-run soft CPU
limit, in seconds). Raw log: `results.Arete` in the repo root.

## Summary

|    outcome    | count |
| ------------- | ----- |
| PASS          | 26    |
| TIMEOUT (30s) | 11    |
| CRASH         | 19    |
| INCORRECT     |  1    |
| **total**     | **57**|

## Passing (26)

Timings are in wall-clock seconds, single run. These are functional
results; they are **not** meant to be compared against the published
ecraven numbers, because our 30s cap clips the run sizes before Arete
gets to finish larger inputs on some benchmarks.

| benchmark | time (s) | benchmark | time (s) |
| --------- | -------- | --------- | -------- |
| array1    | 26.0     | mbrot     | 14.0     |
| browse    |  4.0     | mazefun   | 21.0     |
| deriv     | 26.0     | nboyer    | 15.0     |
| destruc   | 20.0     | nucleic   | 14.0     |
| diviter   | 21.0     | parsing   | 16.0     |
| divrec    | 27.0     | pnpoly    | 23.0     |
| earley    | 20.0     | primes    |  8.0     |
| fft       |  5.0     | read1     |  0.0     |
| fib       | 28.0     | sboyer    | 15.0     |
| fibc      | 16.0     | string    |  0.0     |
| fibfp     |  6.0     | sum1      |  0.0     |
| sumfp     | 10.0     | tak       | 12.0     |
| triangl   | 20.0     | wc        | 13.0     |

## Timeouts at CPU_LIMIT=30s (11)

These ran to the 30-second CPU cap and were SIGKILLed. Most are legitimately
CPU-bound under Arete's interpreter/VM (no native compilation for these
shapes). Raising `CPU_LIMIT` is expected to let them complete, but Arete
is currently orders of magnitude slower on them than a compiled Scheme.

- `puzzle`, `takl`, `ntakl`, `cpstak` — Gabriel suite, deep recursion
  and/or heavy closure allocation.
- `sum` — tight numeric loop over 200,000 iterations × 10,000 reps.
- `nqueens`, `peval`, `conform`, `lattice` — "other" suite, list- and
  closure-heavy.
- `mperm` — GC-pressure benchmark.
- `equal` — synthetic, exercises `equal?` on large shared structure
  (and Arete's `equal?` does not handle shared structure; see README).

## Incorrect (1)

- `simplex` — runs to completion but returns `#f` where the harness
  expects a specific numeric result. Likely a numerical precision or
  missing-primitive issue inside the benchmark body; not diagnosed.

## Crashes (19), grouped by cause

### Missing R7RS procedures (11)

Arete's standard library does not yet implement these. Each failure is
an immediate `reference to undefined variable` at first use.

| benchmark  | missing procedure     |
| ---------- | --------------------- |
| compiler   | `exact-integer?`      |
| matrix     | `exact-integer?`      |
| maze       | `exact-integer?`      |
| pi         | `exact-integer-sqrt`  |
| chudnovsky | `exact-integer-sqrt`  |
| scheme     | `rational?`           |
| gcbench    | `define-record-type`  |
| cat        | `delete-file`         |
| ray        | `delete-file`         |
| tail       | `read-line`           |
| ack        | (hits `RECURSION-LIMIT 1500` — non-tail recursive call depth cap, not a missing procedure but a runtime ceiling) |

Shimming any of these in `utils/r7rs-bench-prelude.scm` would let the
affected benchmarks start; we haven't done so because the brief was to
avoid pretending Arete supports features it doesn't.

### Reader / syntax (4)

- `mbrotZ` — reader rejects the complex literal `-1.0-0.5i`
  ("Reader error: invalid numeric syntax"). Arete has no complex
  number support.
- `graphs`, `slatex`, `bv2string` — the expander rejects the benchmark
  source with "duplicate argument name" during `expand-lambda` /
  `expand-delayed`. These benchmarks use a pattern (likely inside
  `let-values` / `define-values` / record macros) that our expander
  treats as a lambda with repeated formal params. This is an Arete
  expander limitation, not a real semantic error in the source.

### Runtime VM errors (2)

- `ctak` — crashes inside the call/cc-heavy inner loop with
  `< expects all numeric arguments but argument 2 is a constant`.
  This looks like a VM invariant violation around continuation
  invocation — a raw internal constant tag is leaking into an arithmetic
  primitive. Worth investigating; may be related to Arete's one-shot
  continuation emulation of `call/cc`.
- `quicksort` — similar shape:
  `vm primitive + expected a fixnum or flonum as argument 2 but got constant`.
  Same category as `ctak`.

### Segfaults (2)

- `paraffins` — `SIGSEGV (core dumped)` shortly after start.
- `dynamic` — same.

Both terminate the arete process before it can print a Scheme-level
trace, so we don't have a line-level diagnosis yet. Reproduce with:

```
CPU_LIMIT=30 utils/run-r7rs-benchmarks.sh paraffins
CPU_LIMIT=30 utils/run-r7rs-benchmarks.sh dynamic
```

## Reproducing

```
make heap.boot
CPU_LIMIT=30 make r7rs-bench BENCH=all    # full suite, ~30 min upper bound
CPU_LIMIT=30 make r7rs-bench BENCH=fib    # single benchmark
utils/run-r7rs-benchmarks.sh gabriel      # by group
python3 utils/benchmark-report.py series --output-dir scratch/bench --r7rs fib --r7rs tak --arete boot
```

Results are appended to `results.Arete`. Per-benchmark CSV lines are
emitted with the prefix `+!CSVLINE!+arete,<name>,<time-or-status>` so
they can be extracted for further processing.

If you need machine-readable output in a dedicated folder instead of the
legacy append-only root log, prefer `utils/benchmark-report.py r7rs ...`
or the mixed `series` command above. Those write `r7rs.json` containing
the parsed rows and aggregate summary.
