---
id: are-vfam
status: open
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

