---
id: aobi-0gxa
status: closed
deps: []
links: []
created: 2026-04-12T17:40:16Z
type: chore
priority: 2
assignee: Phil
tags: [interp-opt, measurement]
---
# T1: Establish bootstrap and microbenchmark baselines

Collected baseline numbers for the interpreter-optimization project so subsequent changes could be compared against a stable reference.

Bootstrap wall (best-of-5, no perf flag): 110/112/116 ms (min/median/max). heap.boot = 905 kb.

Bootstrap perf split (with --perf-report, wall 251ms total): interpreter 55.5% (139ms, 3.82M calls), VM 8.7% (22ms), GC 3.5%, balance reader/CLI/idle.

Headline conclusion: the base interpreter really is the hot component during bootstrap. Upper bound on this project was roughly a 27% bootstrap cut, corresponding to a 2x interpreter speedup.

Environment: g++ 15.2.0, -O2 -g3 -std=c++14 -fno-rtti -fno-exceptions, Linux 6.12.74 x86_64, HEAD 1d9e57e at the time of measurement.

Closed. No code change.

