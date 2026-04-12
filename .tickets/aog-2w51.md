---
id: aog-2w51
status: closed
deps: []
links: []
created: 2026-04-12T19:50:04Z
type: task
priority: 1
assignee: Phil
parent: aog-3dp0
tags: [gc, measurement]
---
# GC benchmark baseline via benchmark runner

Run the selected GC benchmark suite through utils/benchmark-report.py series and save artifacts under scratch/2026-04-12-gc-baseline so later experiment tickets compare against a fixed archived baseline.

## Notes

**2026-04-12T19:54:40Z**

Archived baseline completed under scratch/2026-04-12-gc-baseline.
Artifacts:
- manifest: scratch/2026-04-12-gc-baseline/manifest.json
- r7rs report/json/log: scratch/2026-04-12-gc-baseline/r7rs.html, scratch/2026-04-12-gc-baseline/r7rs.json, scratch/2026-04-12-gc-baseline/r7rs.log
- arete report/json/log: scratch/2026-04-12-gc-baseline/arete.html, scratch/2026-04-12-gc-baseline/arete.json, scratch/2026-04-12-gc-baseline/arete.log
Headline results:
- Arete workloads: boot 0.241798s, bootstrap-and-psyntax 1.051705s
- R7RS passes: nboyer 15.0s, earley 20.0s, destruc 21.0s, mperm 36.0s, peval 43.0s, nqueens 63.0s
- Known failure preserved in baseline: gcbench CRASHED due to missing define-record-type
