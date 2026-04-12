---
id: aobi-yqnf
status: closed
deps: [aobi-f4zz, aobi-vg03]
links: []
created: 2026-04-12T17:42:00Z
type: feature
priority: 3
assignee: Phil
external-ref: commit:8ae1d9a
tags: [interp-opt, optimization, aborted]
---
# T5: Pre-resolve env lookups to (depth, slot) — aborted

Attempted to pre-resolve env lookups by tagging symbol occurrences inside lambda bodies with (depth, slot) via a new LOCAL_REF heap type and lazy self-rewriting in eval_body's SYMBOL case. Correct (81/81 tests) but aborted for two compounding reasons:

1) Base interpreter is thrown away once bootstrap switches to the VM. Invasive changes that thread a new heap type through GC, image serializer, expander, and compiler are paying tax on throwaway code.
2) Final measurement didn't meet commit criteria anyway: bootstrap +4% regression, deep-env -23% (target was >=25%), three microbenches regressed 6-9%, aggregate -1.8% (target -5%).

Implementation retained on branch opt/t5-env-lookup as a record; not merged.

Full retrospective landed on arete-opt-base-interp (commit 8ae1d9a) — see below for the key reusable lessons.

DESIGN: LOCAL_REF { name, depth, slot } heap type. eval_body's SYMBOL case walks env chain once, writes resolved (depth, slot) back into the car of the containing pair. From then on the LOCAL_REF fast path: d parent hops via vector_ref(0) + one indexed load. Globals cached via sentinel depth LOCAL_REF_GLOBAL = 0xFFFFFFFFu. Required expander + compiler unwrap hooks and GC/image serializer updates for the new pointer-bearing type.

KEY LESSONS:
- AST mutation has a long blast radius in a self-bootstrapping system. pull-up-bootstraps feeds interpreted Function.body back into the compiler, and expand also re-walks bodies. Any AST-rewrite must either carry the original name so consumers can unwrap, or guarantee no downstream consumer re-reads the mutated tree. Option 1 was pursued; unwrap then has to be taught to every walker, and the heap type to GC + image. Tax is real.
- The hot spot env_lookup_impl=36% on deep-env is a genuine signal, but on the base interpreter that cost amortizes over one-shot bootstrap. The 30%+ win lives in the VM-resident lookup path, where depth/slot can be baked into bytecode without downstream consumers reinterpreting.
- Caching globals is necessary, not optional: first pass regressed global-ref +13% because prelookup walked chain for globals, then eval_body walked it again via env_lookup. Sentinel-depth fix removed the double walk.
- A 5% regression threshold on individual microbenches is tight. Three benches regressed 6-9% from what should be a pure win; suspects are the per-SYMBOL branch and prelookup call. A future attempt should lift prelookup out of the arg-eval loop and hoist LOCAL_REF dispatch above the type switch, or fold it into the existing SYMBOL case with no extra branch on the cold path.

UNATTEMPTED:
- T9 (first-interpretation prepare-pass): same class of problem — any prepare-pass that mutates/decorates the AST in the base interpreter has to teach every downstream walker. Deferred on the same grounds.
- Hoisting prelookup out of the four arg-eval sites to lambda-body first-entry. Would reduce per-arg overhead but wouldn't fix bootstrap regression.
- Tighter LOCAL_REF layout (packing depth+slot into one immediate to skip heap alloc). Would cut GC pressure but not solve downstream-consumer tax.

