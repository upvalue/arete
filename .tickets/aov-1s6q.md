---
id: aov-1s6q
status: closed
deps: []
links: []
created: 2026-04-12T19:51:28Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm, jit]
---
# T11: Evaluate re-enabling hot VMFunction native compilation


## Notes

**2026-04-12T19:51:41Z**

Baseline north-star: boot=0.234s, bootstrap-and-psyntax=0.984s, fib=27.0s, tak=12.0s, earley=20.0s, nboyer=15.0s, peval=42.0s, gcbench=CRASHED. Perf: boot VM 23.6ms/255.9ms (9.2%), fib VM 28.184s/28.187s (99.99%).

**2026-04-12T19:58:20Z**

Experiment results: re-enabled hot VMFunction native compilation in src/vm.cpp (#if 0 -> #if 1), rebuilt, and benchmarked fib/tak/earley plus boot. Results: fib 27.0s -> 9.0s, tak 13.0s -> 6.0s, earley 20.0s -> TIMEOUT at 60s CPU limit; with CPU_LIMIT=120 it completed in 66.0s. boot 0.235286s -> 0.295538s. Reverted the change because the regression on earley/boot outweighed the wins on fib/tak.

**2026-04-12T20:01:07Z**

Experiment result: not kept. Re-enabled existing hot VMFunction native compilation threshold. Results: fib 27.0s -> 9.0s, tak 13.0s -> 6.0s, but earley regressed from 20.0s to ULIMITKILLED at CPU_LIMIT=60 and to 66.0s at CPU_LIMIT=120; boot regressed from 0.235286s to 0.295538s. Strongly workload-specific and net-negative on the chosen north-star suite.
