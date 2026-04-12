# Performance Reports Implementation

How the subsystem documented in [[Performance Reports]] is wired up.

## Components

- `PerfStats` (on `State`) — counters and exclusive-time accumulators.
- `PerfScope` RAII — records entry/exit of a component, bumps an optional
  call counter.
- `GCPauseTimer` — scoped timer for `collect()`; always on.
- `State::write_perf_report(std::ostream&)` — serializes JSON.
- `State::perf_report_enabled` / `perf_report_path` — set by `--perf-report`
  in `enter_cli`. An RAII guard in `enter_cli` writes the report on every exit
  path.

## Time slicing

`PerfStats::switch_to(c)` credits the elapsed delta since the last transition
to the currently-active component, then flips the component. `PerfScope`
restores the previous component in its destructor. Entering the same
component is idempotent — no switch, no `chrono` call. Always call
`PerfStats::flush()` before reading `time_us` (the report does this).

GC time is tracked separately via `GCPauseTimer` and is not part of the
time-slicing state, which is why GC overlaps with interpreter/VM time.

## Instrumented entry points

| Site | Component | Counter |
| --- | --- | --- |
| `State::apply` (`src/eval.cpp`) | — | `apply_calls` |
| `apply_interpreter` (`src/eval.cpp`) | `PERF_INTERP` | `interpreter_calls` |
| `eval_body` inline apply path (`src/eval.cpp`) | — | `interpreter_calls` |
| `eval_list` (`src/eval.cpp`) | `PERF_INTERP` | — |
| `apply_vm` (`src/vm.cpp`) | `PERF_VM` | `vm_calls` |
| `GCSemispace::collect` (`src/gc.cpp`) | (independent timer) | `collections` |

## Adding a new counter or component

1. Extend `enum PerfComponent` and `PERF_N` in `arete.hpp` if adding a new
   time-slicing component. For a pure counter, just add a `size_t` field to
   `PerfStats`.
2. Guard increments with `if(perf_report_enabled)` so non-report runs stay
   zero-overhead.
3. To time-slice a region:
   `PerfScope scope(state, PERF_YOURS, &state.perf.your_counter);`
   Pass `nullptr` for the counter if you only want time-slicing.
4. Serialize the new field in `State::write_perf_report` in `src/gc.cpp`.
5. Update [[Performance Reports]]'s field table.

## Overhead

- Non-report runs: one branch per instrumented site, nothing else.
- Report runs: `steady_clock::now()` (~30–50 ns on Linux) per component
  transition, plus counter increments. Noticeable on call-heavy workloads; use
  it for comparisons, not absolute measurements.
- GC timing is always on. Its cost is negligible next to a collection.
