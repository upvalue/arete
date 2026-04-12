# Benchmarks: Arete Workloads

Project-native workloads tracked by the static benchmark report pipeline.
For where these fit alongside the ecraven suite, interpreter
microbenchmarks, and perf reports, see [[Benchmarking]].

## Workloads

| workload | command | purpose |
| --- | --- | --- |
| `boot` | `bin/arete boot.scm` | Normal end-to-end bootstrap path. |
| `bootstrap-and-psyntax` | `bin/arete bootstrap-and-psyntax.scm` | Bootstrap plus the psyntax stress case described in [[Psyntax]]. |

Both workloads run from the repository root and are timed externally by
the benchmark runner. The report records the best wall-clock time across
`RUNS` successful executions. If any run crashes or hits the CPU limit,
that workload is marked as failed for the report.

## Running

```sh
make bin/arete
make bench-report-arete
make bench-report-arete BENCH=boot
RUNS=5 CPU_LIMIT=600 python3 utils/benchmark-report.py arete
python3 utils/benchmark-report.py series --output-dir scratch/bench --arete boot --arete bootstrap-and-psyntax
```

The generated report lives at `web/benchmarks/reports/arete.html`, with
matching `.log` and `.json` sidecars.

When invoked through `series`, the Arete workload artifacts are written
to the chosen output directory as `arete.html`, `arete.log`, and
`arete.json`, with `manifest.json` at the directory root describing the
full mixed run.

## Interpretation

- `boot` is the cheapest broad health check: reader, expander,
  compiler/bootstrap path, VM, serializer, and GC all run in one shot.
- `bootstrap-and-psyntax` is slower and noisier, but it stresses the
  expander and runtime on a much larger macro workload.
- Compare both before and after changes that touch expansion, module
  setup, `apply`, or VM call machinery. A change that helps one and
  hurts the other is usually shifting cost rather than removing it.
