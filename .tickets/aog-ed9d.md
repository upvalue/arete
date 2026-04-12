---
id: aog-ed9d
status: closed
deps: []
links: []
created: 2026-04-12T20:12:26Z
type: task
priority: 1
assignee: Phil
parent: aog-3dp0
tags: [gc, benchmarking, crash, expander]
---
# Investigate earley crash triggered by unused top-level define-syntax

Document and investigate a crash where adding an otherwise-unused top-level define-syntax form to the benchmark prelude causes the earley R7RS benchmark to segfault at runtime. Control run without the macro passes (~20s); a trivial unused macro in the prelude crashes earley; the gcbench-specific define-record-type shim appears safe when injected only for gcbench. Suspect areas include define-transformer!/macro_env installation in scheme/expand.scm, macro recompilation in scheme/compiler.scm, and GC rooting/forwarding of macro-related objects in src/gc.cpp.


## Notes

**2026-04-12T20:16:28Z**

Reproduction summary:
- earley passes when assembled without any extra top-level macro in the shared benchmark prelude (~20s)
- a trivial unused top-level define-syntax in the shared prelude can make earley segfault
- the gcbench-specific define-record-type shim is stable when injected only for gcbench
Likely suspect areas from code inspection: define-transformer!/macro_env installation in scheme/expand.scm, macro recompilation in scheme/compiler.scm, and GC rooting/forwarding of macro-related objects in src/gc.cpp.

**2026-04-12T20:58:45Z**

Tracked the issue to bootstrap recompilation dropping macro flag metadata. recompile-function now preserves procedural and identifier macro bits independently, and added tests/bootstrap/recompile-macro-bits.scm plus a bootstrap test suite entry to lock this in. Focused verification: python3 utils/run-tests.py bootstrap.
