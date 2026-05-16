# Rust Runtime Port Feasibility

This note studies whether Arete's current C++ runtime can be ported to Rust
while dropping the template JIT, native VM, SDL, libuv, and other non-core
features at first. The short answer is yes, but the first useful Rust port
should target semantic compatibility and bootstrapping, not C++ ABI or heap
image compatibility.

Related context:

- [[Benchmarking]]
- [[Performance Reports]]
- Tickets `aog-3dp0`, `aog-il5a`, `are-vfam`, and `are-8qgc`
- External GC/runtime crates checked on 2026-05-16:
  [gc-arena](https://docs.rs/crate/gc-arena/latest),
  [safe-gc](https://docs.rs/safe-gc/latest/safe_gc/),
  [MMTk](https://www.mmtk.io/),
  [MMTk porting guide](https://docs.mmtk.io/portingguide/howto/nogc.html),
  [boa_gc](https://docs.rs/boa_gc/latest/boa_gc/),
  [shredder](https://docs.rs/shredder/latest/shredder/),
  [zerogc](https://docs.rs/zerogc/latest/zerogc/),
  [rudo-gc](https://docs.rs/crate/rudo-gc/0.8.8)

## Current Runtime Shape

The native runtime is small enough to port deliberately:

- `src/*.cpp`: about 19k lines, including the JIT and native VM
- `arete.hpp`: about 2.9k lines
- boot Scheme in `scheme/*.scm`, `boot.scm`, `compile.scm`: about 4.4k lines
- current tests: `python3 utils/run-tests.py` passes 100/100 in this checkout
- low-level doctest runtime/GC tests: `tests/test-semispace` passes 18/18
- temporary bootstrap image: `bin/arete boot.scm --save-image /tmp/arete-feasibility.heap.boot`
  succeeds and produced a 947kb image in this checkout

The runtime boundary is concentrated in:

- `arete.hpp`: tagged `Value`, heap object layouts, `State`, GC root protocol,
  and VM opcode numbers
- `src/gc.cpp`: moving semispace collector, root tracing, finalizers, perf
  counters
- `src/eval.cpp`: tree-walking interpreter used by early bootstrap and macro
  execution
- `src/builtins.cpp`, `src/numbers.cpp`, `src/files.cpp`, `src/table.cpp`,
  `src/strings.cpp`, `src/platform.cpp`: core host functions
- `src/reader.cpp`, `src/writer.cpp`: syntax input/output
- `src/vm.cpp`: bytecode VM, the runtime target of the self-hosted compiler
- `src/image.cpp`: heap image save/load

The current value representation is C-like and layout-sensitive:

- fixnums use low bit `1`
- constants use low bits `10`, with `#f` as zero
- heap pointers are aligned and use low bits `00`
- heap object headers contain type, flags, mark/forwarding state, and a small
  header integer
- the collector moves objects and updates every known slot

The compiler produces `VMFunction` objects through `OpenFn->procedure`, copying
Scheme-side instruction vectors into a `Bytevector` of `size_t` opcodes. There
are currently 56 VM opcodes. The native VM and template JIT are optional
accelerators; the bytecode VM in `src/vm.cpp` is the correctness target.

There are about 177 core `AR_DEFUN` host bindings when excluding the JIT,
native VM, SDL, and libuv groups, and 212 total with those optional groups.

## Feasible Initial Scope

The first Rust milestone should be:

1. Implement the same logical `Value` universe: constants, fixnums, flonums,
   strings, chars, bytevectors, symbols, renames, pairs, vectors, tables,
   records, exceptions, interpreted functions, C/Rust host functions,
   VMFunctions, closures, boxes, and ports.
2. Port `State` boot and builtins needed by `boot.scm` through compiler
   installation.
3. Port the reader, writer, tree-walking interpreter, and core host functions.
4. Port `OpenFn->procedure` and `apply_vm`.
5. Run `boot.scm`, save a Rust-native image, reload it, and pass the existing
   Scheme test runner.

This can drop:

- `src/compile-x64.cpp.dasc` and `src/compile-x64.cpp`
- `src/vm-native-x64.cpp.dasc` and generated native dispatch
- native VM builtins and stats
- SDL/libuv initially
- Emscripten/distilled build paths initially

The current `heap.boot` file format should not be a first milestone. It writes
raw C++ heap layouts with pointer-size and object-layout assumptions. A Rust
port can bootstrap from `boot.scm` and write a Rust-native image. Compatibility
with existing C++ images is possible only if the Rust port preserves exact
object layout and pointer update semantics, which would fight most ecosystem
GC choices.

## GC Options

There is no low-risk "drop in a high performance GC crate and keep the rest of
Arete unchanged" path. The hard part is not allocation; it is root discipline,
tracing `Value` precisely, preserving fast `Value` operations, and handling the
VM stack/closures/boxes.

### gc-arena

`gc-arena` is the best-aligned existing Rust VM crate. It was built for
garbage-collected language runtimes, is used by Ruffle and Piccolo, provides
zero-cost pointer-sized `Gc` handles, and uses an incremental mark-sweep
collector optimized for low pause time.

Fit:

- good for a single-threaded language runtime
- safe root model through a single arena root
- procedural tracing support
- no conservative stack scanning

Mismatch:

- non-moving collector, so it will not preserve Arete's compaction behavior
- no collection inside `Arena::mutate`; Arete's current VM assumes allocation
  can collect at many points
- `Gc<'gc, T>` lifetimes would reach through most runtime APIs
- likely needs a stepper-style VM or a root object containing all VM state

Recommendation: worth a focused prototype, but only if the Rust VM is designed
around `gc-arena` from the start. Do not try to mechanically translate
`AR_FRAME` into `gc-arena`.

### safe-gc

`safe-gc` has a very attractive correctness story: no unsafe code, zero
dependencies, heterogeneous heaps, explicit roots, and cycle collection. Its
own documentation says it is not a particularly high-performance collector.

Fit:

- good for a correctness-first prototype
- explicit roots are easier to reason about than conservative stack scanning
- heterogenous heap support maps naturally to Scheme object variants

Mismatch:

- not a likely final performance answer
- current Arete performance work is already VM-call dominated on some
  workloads, so a slow GC cannot be ignored for allocation-heavy code

Recommendation: useful as a spike backend for validating a Rust object model,
but not as the committed runtime backend unless benchmarks say it is good
enough.

### MMTk

MMTk is the serious high-performance option. It is a runtime integration
framework, not a plug-in smart pointer crate. Its project page describes it as
active and experimental, and the porting guide requires a binding that defines
the VM object model, slots, scanning, collection hooks, finalization/reference
glue, mutators, and memory slices.

Fit:

- best long-term route if Arete needs a mature collector family
- explicitly meant for language runtimes
- can support moving/copying collectors through a runtime binding

Mismatch:

- integrating MMTk is a runtime project inside the runtime project
- the binding burden is larger than Arete's current collector in the first
  porting phase
- it should be evaluated after the Rust object model and VM are stable

Recommendation: defer. Keep the Rust heap/slot abstraction explicit enough
that an MMTk backend can be tried later.

### Other Crates

`boa_gc` is a mark-sweep collector extracted from Boa's JavaScript engine. It
is plausible, but it is coupled to Boa's design assumptions and does not look
as directly suited to an independent Scheme VM as `gc-arena`.

`shredder` offers concurrent collection and ergonomic smart pointers. It is
interesting for general cyclic Rust graphs, but less attractive for a small
single-threaded VM where predictable roots, safepoints, and low overhead matter.

`zerogc` advertises explicit safepoints, moving collector support, and
zero-overhead handles, but the crate is young. Treat it as research input, not
the first backend.

`rudo-gc` advertises a high-performance generational non-moving collector with
conservative stack scanning. It is also very new, and its docs call out sharp
edge cases such as storing `Gc` pointers in ordinary `Vec`s outside the
GC-managed object graph. It is not a conservative choice for Arete's core
runtime yet.

## Recommended Heap Strategy

Use an abstraction boundary from day one:

```text
Value -> HeapRef/ObjectId -> Heap backend
State roots -> Heap::collect()
TraceObject -> visit outgoing Value slots
```

Then test two backends early:

- a minimal Arete-specific Rust mark-sweep or semispace collector with explicit
  root sets, built to match current semantics and make the port straightforward
- a `gc-arena` prototype for the same object subset, to see whether its arena
  root model fits the VM without contorting the code

Pick the backend after it can run a meaningful subset: reader, cons/vector,
symbol interning, simple eval, and enough `boot.scm` to load
`scheme/expand.scm`. If `gc-arena` forces a stepper architecture that makes the
VM awkward, prefer the Arete-specific collector first and revisit MMTk later.

Avoid conservative stack scanning for the first port. Arete's current tagged
values and VM stack are explicit enough that precise roots are practical, and
conservative false positives would complicate correctness and memory behavior.

## Source-Level Compatibility Reorientation

If exact runtime concepts are a non-goal, this becomes a cleaner project:
write a source-compatible Arete Scheme in Rust, using the C++ runtime only as a
behavior oracle and benchmark baseline. Do not preserve `Value` bits, heap
object layout, bytecode encoding, `heap.boot`, `AR_FRAME`, native-code hooks,
or the current VM stack protocol unless they independently prove useful.

The compatibility contract should move up to:

- reader syntax and source-location behavior observable in errors
- printed output for supported data
- core special forms and macro/module behavior
- public builtins used by `boot.scm`, `scheme/*.scm`, examples, and tests
- `eq?`, `eqv?`, `equal?`, mutation, symbol interning, and object identity
  semantics at the Scheme level
- ability to load the existing Scheme bootstrap sources without translating
  them

This changes several design choices.

`Value` can be a Rust-native enum rather than a tagged word:

```text
Value =
  Bool | Nil | Undefined | Eof | Fixnum(i64) | Flonum(f64) | Char(char)
  | Symbol(SymbolId)
  | Object(Gc<Object>)

Object =
  Pair | Vector | String | Bytevector | Table | Record | Function
  | VMFunction | Closure | Box | Rename | Exception | Port | ...
```

That is slower than a carefully packed tagged word in the abstract, but it is
much easier to implement correctly, easier to fuzz, and still leaves room for a
later packed representation once the compatibility suite is green. Scheme-level
identity can come from interned symbol IDs plus `Gc` allocation identity; it
does not require exposing raw addresses.

The heap can be non-moving at first. Source compatibility does not require
compaction, so `gc-arena`, `safe-gc`, or an Arete-specific mark-sweep collector
become more attractive. This also simplifies ports, closures, tables, and Rust
host functions because object addresses/handles are stable across collection.
The tradeoff is possible fragmentation and less pressure to prove moving-root
correctness early.

The VM can change format. The compiler can still be the existing
`scheme/compiler.scm`, but `OpenFn->procedure` could translate its instruction
vector into a Rust enum bytecode or a compact Rust-owned instruction array.
Opcode numbers only need compatibility at the `OpenFn->procedure` boundary;
the in-memory VM representation can be whatever is easiest to execute and
debug.

The image format should definitely change. If the implementation is
source-compatible only, the right image is a Rust-native serialized snapshot or
no image at all until bootstrap works. Rebuilding from `boot.scm` is slower but
keeps the first milestone honest.

The development sequence should be contract-first:

1. Define a compatibility runner that compares Rust Arete against C++ Arete on
   `.scm` inputs: stdout, stderr class, exit status, and optionally normalized
   source locations.
2. Port the reader/writer and pass `tests/reader` by snapshot comparison.
3. Implement a minimal evaluator and enough builtins to pass `tests/preboot`.
4. Load existing `scheme/expand.scm` and pass expander tests.
5. Load existing compiler sources and translate `OpenFn->procedure` into the
   new Rust VM representation.
6. Only after that, choose whether the Rust VM needs tagged values, direct
   threading, specialization, or a different GC.

The major benefit is that large parts of the C++ runtime stop being design
constraints. The major risk is semantic drift: without exact runtime concepts,
the compatibility harness has to become much stronger. Every implementation
shortcut should be judged against the existing tests plus differential runs
against the C++ binary.

## Ecosystem To Use

The port should still lean on Rust libraries outside the GC core:

- `clap` for CLI parsing instead of hand-rolled argument handling
- `rustyline` for REPL line editing instead of vendored linenoise
- `hashbrown` or `indexmap` for tables and deterministic test-friendly maps
  where order matters
- `serde` with a deliberate binary format for Rust-native images
- `thiserror` or `miette` for structured runtime/read/expand error plumbing
- `insta` for reader/writer/compiler snapshot tests
- `proptest` for reader and optimizer differential tests
- `criterion` for microbenchmarks equivalent to `bench/interp/`

Do not outsource the Scheme reader/parser until the existing syntax behavior is
well documented. Arete's reader carries source-location and syntax quirks that
are more important than parser-combinator reuse.

## Main Risks

Rooting and safepoints are the largest correctness risk. The C++ runtime uses
`AR_FRAME`, handles, globals, temps, the symbol table, native frames, protected
argument arrays, and the VM stack. A Rust port needs one root story that covers
all of these without relying on accidental Rust stack liveness.

The image format is a portability risk. Reusing C++ images ties Rust to C++
layout. A Rust-native image format avoids that and is the pragmatic path.

The VM stack and mutable-capture boxes are a semantic risk. Boxes can refer to
open VM stack slots and later close over a copied value. This behavior must be
ported with tests before trusting closure-heavy compiler output.

Host builtins are a volume risk. The core set is not conceptually hard, but it
is broad enough that mechanical porting and snapshot coverage matter.

Performance expectations should be staged. Existing tickets show that some
real workloads are VM-call dominated, not GC dominated. Rust will not
automatically fix that. A clean Rust bytecode VM may be easier to optimize, but
the same apply/frame/arity/closure costs must be measured.

## Milestones

1. Rust crate skeleton with `Value`, object enum, explicit heap trait, symbols,
   pairs, vectors, strings, and a tiny printer.
2. Reader and writer snapshot parity for `tests/reader`.
3. Core `State` boot with globals and a minimal builtin registry.
4. Tree-walking interpreter enough to pass `tests/preboot`.
5. Load `scheme/expand.scm`, `scheme/syntax.scm`, and `scheme/types.scm`;
   pass expander tests.
6. Implement records, tables, exceptions, ports, and remaining core builtins.
7. Port `OpenFn->procedure` and `apply_vm`; compile and run compiler tests.
8. Bootstrap full `boot.scm`; save and reload a Rust-native image.
9. Run `python3 utils/run-tests.py` equivalent and `tests/test-semispace`
   equivalent.
10. Benchmark against C++ on `boot`, `bootstrap-and-psyntax`, `fib`, `tak`,
    `nboyer`, `peval`, and `gcbench`.

## Recommendation

A Rust port is feasible and should be easier to maintain than the current C++
runtime once it is through the bootstrap barrier. The feasible path is not a
line-for-line rewrite with a third-party GC swapped underneath. It is a staged
runtime port with an explicit heap/root abstraction, a correctness-first
bytecode VM, and a Rust-native image format.

Use `gc-arena` as the first ecosystem GC prototype and keep MMTk as the
long-term high-performance option. Keep an Arete-specific collector available
as the fallback if the arena/lifetime model makes the VM unnatural. Drop the
template JIT, native VM, SDL, libuv, and C++ heap-image compatibility until the
Rust runtime can bootstrap and pass the existing test matrix.
