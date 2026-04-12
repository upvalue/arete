---
id: aog-f4ib
status: closed
deps: []
links: []
created: 2026-04-12T19:45:45Z
type: task
priority: 1
assignee: Phil
parent: aog-3dp0
tags: [gc, performance]
---
# GC baseline and first-copy-loop experiment

Establish GC-focused baseline with Arete workloads, interpreter GC microbenchmarks, and available R7RS allocation-heavy benchmarks; then try a first semispace collector optimization focused on allocation/copy-loop overhead.

## Notes

**2026-04-12T19:54:19Z**

Experiment ticket paused pending archived benchmark baseline from aog-2w51. No code change retained; baseline should be measured against the unmodified collector.

**2026-04-12T19:58:39Z**

Superseded by aog-lgjr. This ticket bundled baseline setup with a first collector tweak before the GC campaign workflow was established. Work is continuing under the more specific child ticket aog-lgjr against the archived baseline in scratch/2026-04-12-gc-baseline.
