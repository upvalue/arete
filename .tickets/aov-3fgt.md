---
id: aov-3fgt
status: closed
deps: []
links: []
created: 2026-04-12T20:01:18Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm, profiling]
---
# T13: Profile VM hotspot distribution on fib benchmark


## Notes

**2026-04-12T20:01:27Z**

Goal: get machine-level hotspots for the VM using fib(40) style workload to guide the next optimization experiment. Baseline perf-report shows fib spends 28.184s of 28.187s in the VM with zero GC.

**2026-04-12T20:06:51Z**

Attempted perf record on fib benchmark after installing perf, but kernel denied perf_event_open with EPERM. Investigating perf_event_paranoid / required kernel permissions before profiling can proceed.

**2026-04-12T20:12:26Z**

Closed without profile artifact. perf was installed, and host perf_event_paranoid was reduced to 1, but this container still lacks capabilities (CapEff=0) and perf_event_open remained EPERM even for task-clock. Proceeded with source-driven VM experiments instead.
