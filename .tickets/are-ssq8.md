---
id: are-ssq8
status: closed
deps: []
links: []
created: 2026-04-18T21:08:37Z
type: task
priority: 2
assignee: Phil
tags: [perf, compiler, autoresearch]
---
# Exp 3c: OP_JUMP_IF_FIXNUM fusion

Extend exp 3 / exp 3b's null?+jump and pair?+jump fusion to fixnum?. Adds OP_JUMP_IF_FIXNUM / OP_JUMP_IF_NOT_FIXNUM opcodes that test top-of-stack for the fixnum low-tag bit (bits & 1), pop, and branch. compile-if detects (if (fixnum? X) ...) and (if (not (fixnum? X)) ...) via if-primitive-call-match and emits a single fused opcode instead of (fixnum?; jump-when-pop). Wired through interpreter (vm.cpp), shared native-VM dispatch (vm-native-x64.cpp.dasc + eligibility in vm-native.cpp), and JIT (compile-x64.cpp.dasc).


## Notes

**2026-04-18T21:09:33Z**

Files + lines touched:
  arete.hpp                   +8 (OP_JUMP_IF_NOT_FIXNUM=40, OP_JUMP_IF_FIXNUM=41)
  scheme/compiler.scm        +17 (insn-list, stack-effects, compile-if cond arms + if-primitive-call-match for 'fixnum?)
  src/vm.cpp                 +25 (dispatch table entries + two VM_CASE handlers using Value::fixnump())
  src/vm-native-x64.cpp.dasc +25 (dispatch_table size bumped, DISPATCH_OPS entries, two DynASM stubs: test rdx,1; jnz/jz ->dispatch; jumpToOperandTarget rax)
  src/vm-native.cpp           +5 (eligibility switch + supported-opcode switch)
  src/compile-x64.cpp.dasc   +24 (label-scan case + JIT codegen: popStackTop rax; test rax,1; jnz/jz =>lbl)

Fusion sites: 2 during bootstrap, 0 in peval, 0 in browse, 0 in nboyer.
This is the root cause of the wash: fixnum? predicates just don't show up in
these benchmark hot paths, even though fixnum itself is ubiquitous (integer?
dispatches to it, + and < with fixnum args skip the boolean). The literal
pattern (if (fixnum? X) ...) is rare in hand-written Scheme.

Benchmark runs vs scratch/compiler-baseline-2026-04-18-peval.json (baseline
predates exp 3 & exp 3b so deltas mostly reflect those):
  exp 3c (this commit):
    run 1 geomean -6.79%  browse -8.52%  nboyer -8.94%  peval -2.79%
    run 2 geomean -6.79%  browse -9.99%  nboyer -7.33%  peval -2.92%
    run 3 geomean -6.56%  browse -7.39%  nboyer -8.76%  peval -3.46%
    run 4 geomean -6.68%  browse -8.28%  nboyer -8.29%  peval -3.40%
  exp 3b-only (stash + HEAD rebuild, same baseline):
    run 1 geomean -5.02%  browse -7.10%  nboyer -5.62%  peval -2.27%
    run 2 geomean -6.10%  browse -10.63% nboyer -5.00%  peval -2.49%

Delta attributable to exp 3c is ~1%pt of geomean, well within the run-to-run
variance of exp 3b alone (-5% to -7%). With zero fusion sites in all three
benchmarks, there is no mechanism for exp 3c to affect their hot paths. The
apparent improvement is noise.

Decision: WASH. Commit 0211280, reverted in 0988e4e.

Next-agent observation: type-check fusion is saturating. null?/pair? were
the big wins because list-walk and tree-walk code constantly hits them at
base cases; fixnum? just doesn't appear in those idioms. Remaining plausible
targets (symbol?, string?, vector?, char?, eof-object?, zero?) will fire even
less. The category is played out.

peval's stubborn -2 to -3% non-movement: peval is a partial evaluator and
its hot loop is very likely env lookup / substitution / closure dispatch
rather than type-test branching. Promising directions: (1) perf-report a
single peval run to find the hot function and inspect its bytecode; (2)
target apply-tail cost or closure-call cost, since peval recurses heavily
over closures; (3) consider specializing (eq? X 'SYM) where SYM is a known
interned symbol — peval almost certainly does this constantly when dispatching
on primitive names. A peek at peval.scm's main case analysis (ecraven
vendor/src/peval.scm ~line 380+) shows tons of (case) over symbols, which
after expansion becomes chains of (eq? X 'fn-name); that's the hot path,
not (if (type? X) ...).
