---
id: are-bco0
status: closed
deps: []
links: []
created: 2026-04-18T20:08:26Z
type: task
priority: 2
assignee: Phil
tags: [perf, compiler, autoresearch]
---
# Exp 1: hash-table constant dedup

Autoresearch experiment: replace the linear-scan eq?-based dedup in scheme/compiler.scm register-constant with a hash-table-backed lookup for hashable constants (symbols, fixnums, strings, booleans). Unhashable values keep the linear fallback. Tried on peval/browse/nboyer benchmarks; result was a wash. Revert committed.


## Notes

**2026-04-18T20:08:43Z**

Autoresearch Experiment 1 result: wash — reverted.

Change (commit 311c829, reverted in 8bf6a09):
- Added an 18th OpenFn record field `constants-table` (a `make-table`).
- In register-constant: for symbol/fixnum/string/boolean constants,
  look up via table-ref and fall through to table-set! on insert.
  Other types (pairs, vectors, procedures, chars, flonums) keep the
  existing linear scan to preserve eq? semantics, since Arete tables
  key by equal? and only accept hashable types.

Benchmark comparison vs scratch/compiler-baseline-2026-04-18-peval.json
(CPU limit 120s, 5% noise floor):

  browse:2000   2.045s vs 2.042s   +3 ms   +0.15%   WITHIN_NOISE
  nboyer:5:1   10.522s vs 10.495s  +27 ms  +0.26%   WITHIN_NOISE
  peval:2000   33.782s vs 33.749s  +33 ms  +0.10%   WITHIN_NOISE
  geomean: +0.17%

Bootstrap time (`bin/arete boot.scm --save-image`): ~112-115 ms either
way, so even the compile-time side has no visible improvement.

Why it didn't matter: the constant pool per function in these
benchmarks is small enough that O(n) linear scan with integer equality
is already faster than a hashtable lookup (hashing + chain walk +
allocation on insert). The dominant register-constant caller is
global-get/global-set on symbols; those pools are bounded by the
number of distinct globals in one function, which is small.

Observations for the next agent:
- register-constant is unlikely to be a fruitful compile-time target
  unless compile-time itself becomes a benchmark; even there,
  bootstrap is already ~115ms total.
- For peval runtime (the north star, 33.7s), nothing the compiler
  dedupes is on the hot path during execution.
- Fruitful directions likely lie elsewhere: reducing generated
  bytecode (fewer instructions per hot op), better inlining,
  tightening the VM dispatch, or attacking peval-specific patterns
  (tail calls, closure allocation, eq? chains).
- Mechanical observation: Arete's table-ref uses equal? and refuses
  non-hashable keys, so any future hashtable substitution for eq?
  lookups needs the same fallback structure — or a new "eq?-table"
  primitive that hashes by pointer bits.
