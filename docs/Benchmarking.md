# Benchmarking

Three complementary signals. Consult more than one before concluding a
change helped or hurt.

| Signal                          | Answers                                              |
| ------------------------------- | ---------------------------------------------------- |
| **Bootstrap time**              | Is the whole stack still healthy end-to-end?         |
| **ecraven benchmarks**          | How does Arete do on standard Scheme workloads?     |
| **Interpreter microbenchmarks** | How does a change affect the tree-walking eval loop? |
| **Perf reports**                | Where inside a single run is time being spent?       |

## Bootstrap time

`make heap.boot` reports its own elapsed time:

```
IMG heap.boot bin/arete boot.scm --save-image heap.boot
;; bootstrapped!
;; 109 ms elapsed
;; wrote image: heap.boot 905kb
```

This exercises reader, expander, interpreter, compiler, VM, image
serializer, and GC on a realistic allocation-heavy workload. It is the
cheapest regression check available — run it before and after any
non-trivial perf change:

```
rm -f heap.boot && make heap.boot 2>&1 | tail -3
```

Best of 3–5 runs; typical is ~100–150 ms on x86-64 Linux. Also watch the
image size — it shifts when heap shape changes.

## ecraven benchmarks

Full pass/fail table, per-benchmark failure causes, and reproduction
commands: [[Benchmarks ecraven]].

```
make heap.boot
CPU_LIMIT=30 make r7rs-bench BENCH=fib        # single
CPU_LIMIT=30 make r7rs-bench BENCH=gabriel    # group
CPU_LIMIT=30 make r7rs-bench BENCH=all        # full suite (~30 min)
utils/run-r7rs-benchmarks.sh fib ack tak      # arbitrary set
```

Groups: `gabriel num kvw io other gc synth perf all`. `perf` is our
curated runtime-focused subset (see `utils/run-r7rs-benchmarks.sh` for
what and why).

Output is appended to `results.Arete`. Per-benchmark lines are grep-able:

```
+!CSVLINE!+arete,<name>,<seconds|CRASHED|ULIMITKILLED|INCORRECT>
```

Caveats when interpreting:

- `CPU_LIMIT=30` clips larger inputs — raise it to compare against
  upstream ecraven numbers.
- Wall-clock times include parse/expand/compile/run. A reader
  regression can look like a VM regression. Cross-check with a perf
  report.
- Single runs are noisy; median of 3–5 under ~10% effect sizes.

## Interpreter microbenchmarks

A small curated set of single-axis benchmarks under `bench/interp/`,
each run through `--interp-only` so the tree-walker in `eval.cpp` is
the only thing being measured (no bytecode compile, no VM). Use these
when you're touching the interpreter — bootstrap and ecraven mix too
many subsystems to isolate a regression there.

```
make heap.boot
utils/run-interp-bench.sh                  # all, best of 3
utils/run-interp-bench.sh arith tak        # pick by name
RUNS=5 utils/run-interp-bench.sh           # more samples
RESULTS=/tmp/before.csv utils/run-interp-bench.sh
```

Each benchmark self-reports its elapsed time as

```
+!CSVLINE!+arete-interp,<name>,<seconds>
```

and the driver prints a summary table at the end. Full suite runs in
roughly 20 s on x86-64 Linux.

| Benchmark      | Isolates                                                    |
| -------------- | ----------------------------------------------------------- |
| `arith`        | tight tail loop of `+ - * =` — arithmetic dispatch          |
| `global-ref`   | hot references to top-level bindings — global lookup path   |
| `deep-env`     | references through 6 nested `let` frames — env-chain walk   |
| `cons-list`    | build + drop length-200 lists — pair alloc + car/cdr        |
| `closure`      | `make-adder` inside the loop — closure creation per call    |
| `higher-order` | fold over 500-element list via user lambda — repeat apply   |
| `cond-chain`   | 9-clause `cond` in a tight loop — special-form dispatch     |
| `tak`          | scaled-down Takeuchi — non-tail recursion + int arith       |
| `gc-churn`     | allocate/discard 40-cell lists — nursery throughput         |

The full ecraven suite is not generally tractable under `--interp-only`
(huge iteration counts, deep recursion); see [[Benchmarks ecraven]].
Cherry-picked cases can be usable at reduced inputs — fib(30) finishes
in ≈0.6 s under pure interpretation here, vs fib(40) as the upstream
ecraven input.

## Perf reports

Field reference: [[Performance Reports]]. Implementation and how to add
counters: [[Performance Reports Implementation]].

```
bin/arete heap.boot --perf-report /tmp/before.json --eval '<expr>'
# change, rebuild
bin/arete heap.boot --perf-report /tmp/after.json  --eval '<expr>'
diff <(jq . /tmp/before.json) <(jq . /tmp/after.json)
```

Splits wall time into interpreter / VM / GC with allocation counts and
pause distributions. Use for *ratios* between runs taken with the flag
on; the flag itself adds per-transition overhead, so measure absolute
numbers with `time bin/arete ...`.

Pass `--interp-only` (after the `heap.boot` image) to clear the
installed bytecode compiler so loaded files run under the tree-walking
interpreter — useful for isolating eval.cpp time on higher-level
workloads.

## Workflow

1. Baseline: bootstrap (best of 5), a few relevant ecraven benchmarks,
   perf-report for bootstrap.
2. Change.
3. Re-measure all three.
4. Check for regressions across the board — not just wins on your
   target. A benchmark win that shows up as "less time in VM, more time
   in GC" is usually cost-shifting, not a speedup.
5. Record the numbers in the commit message.
