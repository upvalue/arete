# Benchmarking

Three complementary signals. Consult more than one before concluding a
change helped or hurt.

| Signal                   | Answers                                              |
| ------------------------ | ---------------------------------------------------- |
| **Bootstrap time**       | Is the whole stack still healthy end-to-end?         |
| **ecraven benchmarks**   | How does Arete do on standard Scheme workloads?      |
| **Perf reports**         | Where inside a single run is time being spent?       |

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
