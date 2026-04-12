---
id: feb-gh8l
status: closed
deps: []
links: []
created: 2026-04-12T16:48:09Z
type: bug
priority: 2
assignee: Phil
parent: feb-sjow
tags: [ecraven-benchmarks]
---
# string<? and friends compare by length, not lexicographically

Arete's string<? violates R7RS: (string<? "ab" "b") returns #f, but R7RS requires lexicographic char-by-char comparison, so the correct answer is #t (since #\a < #\b). Verified interactively. The scheme benchmark (an interpreter) sorts a list of spelled-out numbers using string<? inside its sandbox; the wrong comparison returns a wrongly-ordered list and the benchmark is flagged INCORRECT despite reaching completion. Fix is in Arete's string primitives (src/strings.cpp or wherever string<? lives).

## Acceptance Criteria

(string<? "ab" "b") => #t. scheme benchmark PASSes (modulo CPU_LIMIT).


## Notes

**2026-04-12T17:04:48Z**

Fixed. scheme/library.scm:191 defined string<? as (< (string-sum a) (string-sum b)) — comparing the sum of character codes, not lexicographic order. Rewrote to do proper R7RS char-by-char comparison. scheme benchmark now runs to completion and is correct (TIMEOUTs only under tight CPU_LIMIT — performance, not breakage).
