---
id: aobi-tp6u
status: closed
deps: [aobi-0gxa]
links: []
created: 2026-04-12T17:40:24Z
type: feature
priority: 2
assignee: Phil
external-ref: commit:0c92bb8
tags: [interp-opt, cli]
---
# T2: Add --interp-only CLI flag to bypass VM

Added --interp-only CLI flag so code loaded after it (including --eval expressions) runs entirely through eval.cpp's tree-walker instead of being compiled to bytecode and executed on the VM. Enables benchmarking the interpreter in isolation.

Mechanism: when the flag is encountered in CLI argument processing, set G_COMPILER to C_UNDEFINED. eval_list already routes through the tree-walker when the compiler global is undefined, so no changes to eval.cpp or boot.scm.

Must appear AFTER the heap.boot image (or after boot.scm on fresh boot) because bootstrap reinstalls the compiler.

Smoke test (50k-element cons loop):
- without flag: vm 1662us (52.8%), interpreter 0us
- with flag:    vm 101us (0.48%), interpreter 19333us (92.8%)

Default behavior unchanged. Bootstrap still 153ms (with --perf-report), heap.boot still 905kb.

Commit 0c92bb8 on arete-opt-base-interp.

