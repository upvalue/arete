---
id: feb-c9vd
status: closed
deps: []
links: []
created: 2026-04-12T16:34:51Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Add delete-file builtin or shim for cat/ray

Benchmarks cat and ray crash with 'reference to undefined variable delete-file'. Arete already exposes open-output-file and read-char as builtins, so file I/O infrastructure is present. Likely a one-line C builtin in src/files.cpp calling unlink(2). If that is undesirable, a pure-Scheme shim using a rename-to-nonexistent-path trick is also possible but ugly. Unblocks 2 benchmarks.

## Acceptance Criteria

cat and ray each PASS or TIMEOUT under CPU_LIMIT=30.


## Notes

**2026-04-12T16:47:49Z**

Implemented as a C builtin in src/platform.cpp using std::remove(3). tail, cat, ray all now PASS (27s, 11s, 13s).
