# auto-research improvements

The goal of this run is to experiment with multiple improvements in order to
make the Scheme implementation run faster.

The goal of this run is to optimize the **compiler subsystem** of the language.
Improvements should focus on the compiler and means for improving it. The
benchmark set for this run is:

- `browse`
- `peval`
- `nboyer`

Use the saved baseline at `scratch/compiler-baseline-2026-04-18-peval.json`.

That baseline was captured with an R7RS CPU limit of 120 seconds. Plain
`compare` runs inherit the limit from the baseline, so you do not need to pass
it again unless you are intentionally overriding it.

To compare the current tree against that baseline, run:

```sh
python3 utils/benchmark-report.py compare \
  --baseline scratch/compiler-baseline-2026-04-18-peval.json
```

Default output is intentionally short: one summary line plus one line per
benchmark with current time, baseline time, millisecond delta, and percentage
delta. Treat changes under 5% as noise unless there is supporting evidence from
the detailed JSON output.

**The north star is to improve this benchmark**

Be ambitious and assume that major refactors are on the table. You can change
other subsystems as well as necessary (e.g. altering the VM), but only in
service of compiler improvements.

Use a new agent for each attempted improvement, but run the improvements in
serial (do not have multiple agents working on the tree at once, since they
will likely collide with eachother)

You are running in a git worktree, so it's OK to commit changes as they occur.
For posterity, if a change doesn't work out, we should commit and revert that
commit to record the code involved in the change.

# Experimentation

What you can do:

- Modify Arete, either the C++ or Scheme code

What you cannot do:

- Modify the benchmarks themselves, or the harness for running the benchmarks.
  They are read-only.

# Instructions

Memory usage is a soft constraint. It's better to keep memory usage lower,
though in some cases trading off memory usage for performance gains may be
worth it.

Simplicity criterion: All else being equal, simpler is better. A small
improvement that adds ugly complexity is not worth it. Conversely, removing
something and getting equal or better results is a great outcome — that's a
simplification win. When evaluating whether to keep a change, weigh the
complexity cost against the improvement magnitude. A 1% benchmark improvement
that adds 20 lines of hacky code? Probably not worth it. A 1% val_bpb
improvement from deleting code? Definitely keep. An improvement of ~0 but much
simpler code? Keep.

# Loop forever

NEVER STOP: Once the experiment loop has begun (after the initial setup), do
NOT pause to ask the human if you should continue. Do NOT ask "should I keep
going?" or "is this a good stopping point?". The human might be asleep, or gone
from a computer and expects you to continue working indefinitely until you are
manually stopped. You are autonomous. If you run out of ideas, think harder —
research and read papers, re-read the in-scope files for new angles, try
combining previous near-misses, try more radical architectural changes. The
loop runs until the human interrupts you, period.
