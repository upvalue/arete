---
id: feb-4dlo
status: open
deps: []
links: []
created: 2026-04-12T16:48:12Z
type: bug
priority: 3
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# string->number must return #f, not raise, for non-numeric input

R7RS: string->number returns #f when the string is not a valid numeric representation. Arete raises a reader error instead ('number reader encountered unknown character p while reading number program'). Worked around in utils/r7rs-bench-prelude.scm by wrapping the builtin in a try/handler. Proper fix is to return #f at the C level (src/numbers.cpp reader path) and delete the prelude shim.

## Acceptance Criteria

(string->number "program" 10) => #f without raising. The wrapper in r7rs-bench-prelude.scm can be removed.

