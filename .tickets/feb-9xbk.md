---
id: feb-9xbk
status: closed
deps: []
links: []
created: 2026-04-12T16:35:18Z
type: bug
priority: 3
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# Investigate VM constant-tag leak in ctak and quicksort

ctak crashes inside its call/cc-heavy inner loop with '< expects all numeric arguments but argument 2 is a constant'. quicksort has the same shape: 'vm primitive + expected a fixnum or flonum as argument 2 but got constant'. Looks like a raw internal constant tag is leaking into an arithmetic primitive — a VM invariant violation, possibly related to Arete's one-shot continuation emulation of call/cc for ctak. Needs VM-level investigation in src/vm.cpp and the call/cc path. Not understood yet.

## Acceptance Criteria

Root cause identified. ctak and quicksort either PASS, TIMEOUT, or the bug is localized enough to have its own precise repro.


## Notes

**2026-04-12T16:48:23Z**

Also affects matrix (crashes with 'vm primitive car expected a pair as its argument but got constant' after exact-integer? shim lets it progress) and compiler (crashes with 'function vector-ref expected argument 0 to be of type vector but got constant' after the string->number wrapper lets it progress). All four crashes have the same shape: an internal constant tag leaking into a primitive that expected a concrete value. Now affecting: ctak, quicksort, matrix, compiler.

**2026-04-12T17:04:42Z**

Root cause of the 'constant' tag leak was a let* expansion bug in scheme/syntax.scm:269 that expanded (let* ((x e) ...) body) into (let ((x unspecified) ...) (set! x e) ... body), so e was evaluated in a scope where x already shadowed any outer same-named binding with the placeholder constant #<unspecified>. Any RHS that referenced the name it was binding (e.g. (let* ((i (+ i 1))) i)) read unspecified instead of the outer i, and the primitive complained 'got constant'. Fix: rewrite let* as nested single-binding lets. Also discovered and fixed a separate call/1cc bug (scheme/library.scm:300) that discarded the thunk's normal return value. Results: quicksort, ctak, and slatex PASS; matrix and compiler progress past the tag-leak point and hit different, unrelated bugs (closure leaking into vector-ref / integer?) filed separately.
