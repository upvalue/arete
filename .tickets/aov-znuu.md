---
id: aov-znuu
status: closed
deps: []
links: []
created: 2026-04-12T20:09:03Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# T14: Cache RECURSION-LIMIT on State for VM entry checks


## Notes

**2026-04-12T20:09:10Z**

Baseline north-star before change: boot=0.234s, bootstrap-and-psyntax=0.984s, fib=27.0s, tak=12.0s, earley=20.0s, nboyer=15.0s, peval=42.0s, gcbench=CRASHED. Perf: fib wall 28.1868s, VM 28.1844s, vm.calls=1,655,880,570; apply_vm currently fetches G_RECURSION_LIMIT through globals on every entry.

**2026-04-12T20:12:26Z**

Experiment result: keep. Cached G_RECURSION_LIMIT on State and switched apply_vm recursion check to use the cached field. Full baseline comparison: boot 0.234487s -> 0.238299s (+1.6%), bootstrap-and-psyntax 0.983762s -> 0.978386s (-0.5%), fib 27.0s -> 26.0s (-3.7%), tak 12.0s -> 12.0s (same coarse bucket, wall 12.819s -> 12.376s), earley 20.0s -> 20.0s (same coarse bucket), nboyer 15.0s -> 15.0s, peval 42.0s -> 42.0s, gcbench still CRASHED. Perf-report: fib wall 28.186823s -> 26.468537s (~6.1% faster), VM exclusive 28.184449s -> 26.466860s; boot wall 255.863ms -> 250.485ms (~2.1% faster), VM exclusive 23.598ms -> 23.022ms. Code size / complexity cost is minimal, so this is a good keep.
