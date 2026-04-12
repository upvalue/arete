# Performance Reports

Emit a JSON performance report at shutdown.

```sh
bin/arete heap.boot --perf-report -           --eval '<expr>'   # report to stderr
bin/arete heap.boot --perf-report /tmp/p.json --eval '<expr>'   # report to file
```

Reports are always written, including on error exits.

## Report fields

Times are microseconds; sizes are bytes.

| Field | Meaning |
| --- | --- |
| `wall_time_us` | Time since `State` construction. |
| `gc.collections` | `collect()` invocations. |
| `gc.allocations` | Objects allocated through the GC. |
| `gc.collect_time_us` | Cumulative time in collection. |
| `gc.longest_pause_us` | Worst single collection. |
| `gc.time_in_gc_pct` | `collect_time_us / wall_time_us`. |
| `gc.heap_size_bytes` | Current semispace size. |
| `gc.in_use_bytes` | Bytes allocated in the active semispace right now. |
| `gc.live_bytes_after_last_collection` | Survivors from the last collect. |
| `interpreter.calls` | FUNCTION applications (both `apply_interpreter` and inline paths). |
| `interpreter.exclusive_time_us` | Time the interpreter was the active component. |
| `vm.calls` | Entries to `apply_vm`. Tail-call loops do not re-enter, so this is small. |
| `vm.exclusive_time_us` | Time the VM was the active component. |
| `vm.stack_slots_used` / `stack_slots_capacity` | VM value-stack. |
| `apply_calls` | Entries to generic `State::apply` (lower bound on call count). |
| `idle_time_us` | Wall time not attributed to interpreter or VM — reader, CLI, C builtins. |
| `native_code_bytes` | Reserved for JIT/native blocks. |

Interpreter and VM times are **exclusive**: VM time called from the interpreter
is credited only to the VM. GC time is tracked independently and overlaps —
don't expect the three to sum to 100%.

## A/B comparisons

Run the same workload before and after a change with identical flags. The
`--perf-report` overhead (a `steady_clock::now()` per component transition) is
present in both runs and cancels out in ratios. For absolute numbers, compare
runs with the flag off via `time`.

For implementation details and instructions for adding new counters, see
[[Performance Reports Implementation]].
