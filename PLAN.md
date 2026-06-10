# Variadic callees in the native-VM asm fast paths

Goal: eliminate the last large C-fallback bucket in the asm dispatch core —
variadic VM functions called with extra args. On peval:2000 that is 33.2M
non-tail calls (`apply-c-argc-mismatch` in native-vm-stats) plus an unknown
share of the 97.5M `apply-tail-c` tail fallbacks. peval is the only r7rs
benchmark where the asm core still trails the flat C++ core (14.38 vs 13.40);
this traffic is why. Everything else already wins (see
scratch/asmcall/NOTES.md, Phase 4). Predecessor plan: "Native VM: Flat Calls
in Asm" (this file at commit de08445).

## Work mode (read this first)

Per Phil: **do not take measurements between sub-steps.** No baselines, no
per-change timing, no instrumentation passes, no diagnosis loops. Implement
the whole change, then run the correctness battery once at the end, then
take ONE speed measurement (peval best-of-3 + one native-vm-stats histogram
run). If a gate fails, fix and re-run that gate; don't expand scope.

## Context you need (all verified 2026-06-10, don't re-derive)

- Build: `make bin/arete` (x86-64, NATIVE_VM_DEFAULT=1), bootstrap with
  `bin/arete boot.scm`. The dispatch core is src/vm-native-x64.cpp.dasc
  (DynASM; the generated .cpp is untracked). Semantic reference is
  `apply_vm` in src/vm.cpp; pinned reference binary `bin/arete-flat-native0`.
- Variadic VMFunctions have `min_arity == max_arity == F` (the fixed param
  count) plus `VMFUNCTION_VARIABLE_ARITY_BIT`. That is why they land in the
  argc-mismatch bucket: the fast-path checks compare argc to min BEFORE
  testing the variadic bit (`apply-c-arity-range` is 0 on peval; the 344
  `apply-c-variadic` hits are variadic functions called with exactly F args).
- The rest param lives at `locals[F]` (= `locals[max_arity]`); it is a
  normal local, counted in local_count.
- `OP_ARGV_REST` in the asm core is a **no-op** (search `->op_argv_rest`):
  the rest list is built before dispatch, in apply_native_vm's C prologue
  ("Variable-arity prologue", src/vm-native-x64.cpp.dasc ~line 3058, which
  mirrors apply_vm's OP_ARGV_REST at src/vm.cpp ~1069). The fast paths can
  therefore build the rest list at frame entry and leave the opcode alone.
- The boxed-callee work (commit de08445) added exactly the pattern to copy:
  a cold shared continuation `->apply_enter_boxes` that runs after the frame
  is fully committed, calls a C helper (`native_vm_alloc_frame_boxes`),
  then `restoreAfterGcHelperCall` + reloads ARGC from sScratch0 (ARGC is
  r10, caller-saved, and OP_ARGC_EQ/GTE still need it post-entry). LOCALS
  and STACK need no post-helper fixup: the GC never moves vm_stack.
- There are SIX call fast paths: op_apply / op_apply_local / op_apply_global
  and their _tail variants. The boxed-callee diff touched all six the same
  way; this change does too. Read that diff first:
  `git show de08445 -- src/vm-native-x64.cpp.dasc`.

## Design

At each fast path, the callee checks become:

  - min != max            -> bail (unchanged, arity_range)
  - VARIABLE_ARITY bit set:
      argc <  F (=min)    -> bail to the C slow path (it raises the proper
                             arity exception; keeps error behavior identical
                             with zero asm work)
      argc >= F           -> take the fast path, variadic flavor
  - else (fixed arity):
      argc != F           -> bail (unchanged, argc_mismatch)

Variadic flavor of frame entry (delta vs the existing path):

1. **extent is unchanged.** Zero-copy puts the args (including extras) at
   [frame_base, frame_base+argc), which is inside the caller's eval region,
   already below vm_stack_used — extras are GC-scannable for free. Same for
   the tail variant after the arg slide (the slide loop is argc-driven
   already).
2. **Skip the inline zero loop for variadic callees.** The current count
   `local_count + box_count - argc` goes NEGATIVE when argc > local_count +
   box_count and the loop decrements toward it — this is the one place that
   corrupts memory if missed. Let the helper do all slot init beyond the
   args instead.
3. After the frame is fully committed (record pushed/recycled, registers and
   stack slots set — the point where the box-count test sits today), jump to
   a new cold continuation:

     |->apply_enter_variadic:
     | mov sScratch0, ARGC
     | mov rdi, STATE
     | mov rsi, ARGC
     | mov64 rax, (ptrdiff_t) &native_vm_enter_variadic
     | call rax
     | restoreAfterGcHelperCall
     | mov ARGC, sScratch0
     | jmp ->dispatch

   The existing box test chains: test VARIABLE_ARITY first, jnz
   ->apply_enter_variadic (whose helper also handles boxes); else the
   existing box_count test -> ->apply_enter_boxes.
4. C helper, next to native_vm_alloc_frame_boxes:

     static void native_vm_enter_variadic(State* state, size_t argc)

   Derive vfn + frame_base from the top vm_frames record (the record roots
   the closure; re-derive vfn after every allocation — the GC may move it).
   With F = vfn->max_arity and fi = local_count + box_count:
     - rest list: mirror the session prologue (dasc ~3058) / apply_vm's
       OP_ARGV_REST. If argc == F, rest = C_NIL; else temps.clear(), insert
       vm_stack[frame_base+F .. frame_base+argc), rest =
       state->temps_to_list() (temps are GC roots; the source slots stay
       scannable in vm_stack throughout). Store rest at
       vm_stack[frame_base + F].
     - zero vm_stack[frame_base + F + 1 .. frame_base + fi) — everything the
       skipped inline loop would have initialized, including slots where
       extras sat. (Slots beyond fi need nothing: the eval region is
       write-before-read, and anything below the high-water mark is already
       scannable.)
     - if box_count: run the same loop as native_vm_alloc_frame_boxes (call
       it or share the body).
   Rest-then-boxes matches the session prologue; apply_vm does boxes first —
   the orders are equivalent (a Box only records its backing slot index), so
   match the session prologue since you're mirroring its rest code anyway.
5. **Tail variants:** identical delta. Caller box close, record recycle, arg
   slide, frames_lost++ all stay; skip the zero loop and take the
   continuation when the variadic bit is set.
6. ARGC must be the ORIGINAL argc when the callee's OP_ARGC_GTE runs — hence
   the sScratch0 save/reload above (same as ->apply_enter_boxes).

Not in scope: variadic calls with argc < F (slow path raises), CFunction
calls, the `apply` builtin's list-flattening path, arity-range callees
(bucket is 0 on peval), eligibility changes (OP_ARGV_REST is already
eligible).

## Danger spots (each cost a debug cycle in past phases)

- The negative zero-count (design point 2). Audit all six paths.
- DynASM edits: several sequences are textually identical across the six
  paths — when search/replacing, verify the match count is exactly what you
  expect (non-tail uses r10 + [rsp+32/40/48]; tail uses VFN + [rsp+56] too).
- After any C helper that can GC: re-derive VFN from the traced *vfn_ref at
  [rsp], rebase CP/sCodeBase — `restoreAfterGcHelperCall` does all of it.
  Never cache a heap pointer across the call.
- state->temps is clobbered by other helpers: clear-then-fill-then-consume
  inside the helper only.
- The [rsp+32..56] scratch slots are live during entry (fargc / args_off /
  fn / callee-vfn spills) and only dead once `jmp ->dispatch` is reachable —
  the continuation may use sScratch0 freely at that point, nothing earlier.
- Run the differential's `applyfn` case early if confused: it exercises
  variadic through `apply` with zero and multiple extras.

## Verification (once, at the end)

Correctness battery (all must pass; reference = bin/arete-flat-native0):
1. `make bin/arete && ./bin/arete boot.scm`
2. `python3 utils/run-tests.py` (121/121) and `tests/test-semispace`
   (build via `make tests/test-semispace`)
3. `./bin/arete bootstrap-and-psyntax.scm` (fresh boot — do NOT pass
   heap.boot first; exits 0)
4. `bash scratch/flatcall/differential.sh ./bin/arete ./bin/arete-flat-native0`
   -> ALL OK (covers traces, RECURSION-LIMIT, variadic `applyfn`)
5. GC stress, flag AFTER the image:
   `./bin/arete heap.boot --debug-gc <script>` vs the same under
   bin/arete-flat-native0, byte-identical output. Scripts were
   /tmp/gc-stress.scm + /tmp/gc-stress2.scm; if gone, recreate: a loop of
   variadic calls at varying argc (0, 1, many extras) in non-tail AND tail
   position, closures over rest params, and an exception thrown from a deep
   variadic tail chain.
6. Add to the stress the new code's hardest case, which nothing existing
   covers: variadic + boxed combined, e.g.
   `(define (f a . rest) (lambda () (set! a (cons a rest)) a))` in a loop.
7. r7rs subset result validation (harness self-checks):
   `ARETE=$PWD/bin/arete HEAP=$PWD/heap.boot TEMP=/tmp/r7 RESULTS=/tmp/r7.log
    utils/run-r7rs-benchmarks.sh fib tak peval nboyer browse destruc mazefun`

Speed (single measurement, after correctness is green):
- peval best-of-3: `cd vendor/r7rs-benchmarks && <arete> <heap.boot>
  /tmp/arete-r7rs-p1/peval.scm < inputs/peval.input` (that prepared copy has
  the harness exit removed; if gone, step 7's harness reports times too).
- One histogram run: a script that loads peval.scm then prints
  `(native-vm-stats)` (pattern was /tmp/peval-stats.scm; recreate if gone).

Success: `apply-c-argc-mismatch` drops from 33.2M to ~0 and peval beats
14.38s. Parity with flat-C (13.40) or better is the hoped-for outcome; if
peval improves without reaching parity, ship anyway and record the number —
do not start a diagnosis loop.

Wrap-up: append final numbers to scratch/asmcall/NOTES.md, save the
cumulative diff as scratch/asmcall/phase5-variadic.patch, commit. Background
ticket are-8qgc is closed — a one-line `ticket add-note are-8qgc` pointing
at the commit is enough.
