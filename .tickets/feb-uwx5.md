---
id: feb-uwx5
status: open
deps: []
links: []
created: 2026-04-12T16:35:23Z
type: bug
priority: 3
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Diagnose SIGSEGV in paraffins and dynamic

paraffins and dynamic both segfault shortly after start with no Scheme-level trace. Reproduce: CPU_LIMIT=30 utils/run-r7rs-benchmarks.sh paraffins / dynamic. Needs a debug build (bin/arete launched under gdb with heap.boot + the assembled /tmp/arete-r7rs-benchmarks/<bench>.scm) to get a C-level backtrace. Not understood yet.

## Acceptance Criteria

C-level backtrace captured for each; root cause identified or narrowed to a specific subsystem (GC, reader, VM).

