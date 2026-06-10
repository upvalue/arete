---
id: are-vfam
status: closed
deps: []
links: []
created: 2026-05-09T01:50:21Z
type: task
priority: 1
assignee: Phil
tags: [perf, vm]
---
# Slim apply_vm per-call prologue

On peval (r7rs-benchmarks) arete is 1.84x slower than femtolisp (30.6s vs 16.6s); on nboyer 1.37x (9.49s vs 6.95s). perf-report attributes 99.99% of wall to the VM, only 2.2% to GC. 322M apply calls / 30.6s = ~95 ns/apply on arete vs ~52 ns on femtolisp; 43 ns x 322M = ~14s, matching the gap.

Bottleneck is the per-call work in apply_vm (src/vm.cpp:159-260). Specific known-suspect items, all carrying TODOs or noted weaknesses in-tree:

  1. memset of the unused frame at vm.cpp:243 — has `// TODO: Necessary?` in source. Same loop is replicated in the native dispatch core at src/vm-native-x64.cpp.dasc:1167-1173 (apply_zero_loop). Either eliminate (audit dispatch to confirm no opcode reads uninitialized slots), or scope to the box slots only (which the GC needs initialized).
  2. VM2_RESTORE pointer recompute at vm.cpp:143-150 has `// TODO: Way to do this without division?` — caches sff_offset + frame layout instead of recomputing.
  3. AR_FRAME push of f.fn / f.closure (vm.cpp:196) — both are reachable through the value stack already; consider dropping the per-call handle.
  4. Stack-growth + recursion-limit checks (vm.cpp:176-194) are two separate branches; can be folded.
  5. VMFrame2 ctor/dtor includes 3 AR_ASSERTs and a per-box scan in the destructor (vm.cpp:88-118) — verify they're stripped in release builds; the dtor's box close-over loop runs even when box_count==0.

Average opcodes-per-call on peval is 774M/322M = 2.4 — many calls are tiny, so per-call overhead dominates. Each ns shaved is ~322ms on peval.

Mirror any C++ change into the asm path (src/vm-native-x64.cpp.dasc) so both the native fast path and the C-fallback path benefit.

Prior closed work that touched this area but didn't close the gap: aov-qnia, aov-zqc6, aov-3jwl, aov-vg03, aov-kn1t.

Acceptance: measurable improvement on peval and nboyer (geomean target -10% or better) with full r7rs-benchmarks regression check. Update the perf-report VM/GC split numbers in the closing note.


## Notes

**2026-06-10T04:26:20Z**

Closed by the flat calling convention rewrite (PLAN.md 'Flat VM Calling Convention', scratch/flatcall/). apply_vm no longer recurses through C for Scheme->Scheme calls: explicit VMCallFrame records (offsets only — the VM2_RESTORE division is gone), zero-copy args-in-place, memmove tail calls (state.temps round-trip deleted from direct tail sites), and frame init reduced to box slots + unsupplied locals (the '// TODO: Necessary?' memset is gone; GC safety now comes from the vm_stack high-water invariant, see docs/VM Calling Convention.md). VMFrame2, its dtor box-scan, and the per-call AR_FRAME are deleted.

Numbers (x86-64, NATIVE_VM_DEFAULT=0, best-of-5, scratch/flatcall/baseline/{native0,flat-native0}): peval 16.86->13.40 (-20.5%), nboyer 9.99->8.11 (-18.9%) — acceptance was geomean -10%. Full perf-subset deltas: fib -19.7, tak -24.1, browse -24.0, destruc -10.0, mazefun -12.6. callrate microbench 16.96->13.49 ns/call. VM/GC split (perf reports, identical 411.9M vm_calls): peval vm 100%/gc 1.6%->2.2%, nboyer gc 12.9%->15.9% — absolute GC time flat, not cost-shifting. boot -0.7%, psyntax -10.1%.

Post-change profile of fib/callrate: memset/memcpy/memmove GONE; 98.6% apply_vm, flat, dispatch-bound (top insn 2.3%). Remaining per-call cost is opcode dispatch — that is are-8qgc territory. Bonus: ack/takl/cpstak/ctak now complete (default RECURSION-LIMIT raised to 100K flat / 10K native builds); also fixed set-top-level-value! never syncing the cached recursion_limit (the bench prelude's 75000 silently stayed 1500).
