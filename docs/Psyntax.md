# Psyntax

Psyntax in this repository is a bootstrap experiment around Kent Dybvig's `syntax-case` expander.
The goal is not that Arete permanently replaces its own expander with psyntax today. The useful part of this experiment is that it loads a large preprocessed macro system into an already-booted Arete image and asks it to expand its own source. That makes psyntax a good stress test for the bootstrap path, expander, runtime, and VM call machinery.

Related files:

- [[README]] for the normal bootstrap flow.
- `bootstrap-and-psyntax.scm` is the top-level entry point for this experiment.
- `tests/psyntax/prelude.scm` defines a small compatibility layer that psyntax expects.
- `tests/psyntax/load.scm` loads the compatibility prelude and `psyntax.pp`.
- `tests/psyntax/psyntax.ss` is the source being expanded.
- `tests/psyntax/psyntax.pp` is the preprocessed form that gets loaded into Arete.
- Ticket `fp-x309` tracks the specific stabilization work that made this path run again.

## Why It Is Useful

Psyntax is a better stress test than a small macro example because it pushes several parts of the system at once:

- it loads a substantial preprocessed Scheme program on top of a live bootstrapped image
- it exercises expansion on code that is itself a macro expander
- it drives a call-heavy runtime path with lots of list processing and `apply`
- it gives a compact command that can reveal regressions outside the normal `make heap.boot` flow

For that reason, psyntax is worth keeping around as both a debugging target and a benchmark-style smoke test.

## What The Bootstrap Script Does

`bootstrap-and-psyntax.scm` currently does this:

1. Loads `boot.scm`, which brings up Arete's native expander and module setup.
2. Disables undefined-variable compiler warnings.
3. Loads `tests/psyntax/load.scm`, which in turn loads the psyntax prelude and `psyntax.pp`.
4. Reads `tests/psyntax/psyntax.ss` and runs `sc-expand` over each form.

So the current experiment is: "boot Arete, install psyntax's runtime pieces into that booted image, then ask psyntax to expand its own source."

## Running It

From the repository root:

```sh
make bin/arete
./bin/arete bootstrap-and-psyntax.scm
```

If you want the wider bootstrap image first, the normal project flow is still:

```sh
make heap.boot
```

That is not required for the direct psyntax attempt above, but it is useful context because `boot.scm` is the same bootstrap path the main system uses.

Another useful smoke test is the direct full-file expansion path:

```sh
./bin/arete boot.scm \
  --eval "(set-top-level-value! 'COMPILER-WARN-UNDEFINED #f)" \
  --eval "(load \"tests/psyntax/load.scm\")" \
  --eval "(begin (for-each1 sc-expand (slurp-file \"tests/psyntax/psyntax.ss\")) (print 'all-ok))"
```

This is handy when you want the smaller reproducer without printing the full expanded output.

## Interpreting The Result

`bootstrap-and-psyntax.scm` prints the fully expanded top-level forms from `tests/psyntax/psyntax.ss`. The output is large; the important property is that the run completes cleanly.

The direct `for-each1 sc-expand` smoke test should print:

```text
all-ok
```

If this path breaks, the failure is often interesting even when the ordinary bootstrap still works, because psyntax tends to stress deeper corners of expansion and procedure application.

## When To Use It

- Use `make heap.boot` when you want the normal end-to-end bootstrap check.
- Use the psyntax path when you want a stronger expander/runtime stress case with a small reproduction command.
- Use both if you are touching expansion, module setup, `apply`, tail calls, or VM closure/upvalue handling.
