# auto-research improvements

The goal of this run is to experiment with multiple improvements in order to
make the Scheme implementation run faster.

The goal of this run is to optimize the **virtual machine subsystem** of the
language. Improvements should focus on the portable virtual machine and means
for improving it. Focus on making *global* improvements - do not target
specific benchmarks, but instead reason through improvements that are likely to
improve the overall system's performance.

Be ambitious and assume that major refactors are on the table.

Use subagents for each attempted improvement. For each attempted improvement,
if a ticket does not already exist, create one. Make sure the ticket is closed
with a detail of the results at the end of a subagent run. 

You are running in a git worktree, so it's OK to commit changes as they occur.
For posterity, if a change doesn't work out, we should commit and revert that
commit to record the code involved in the change.

# Before starting

Analyze the source code, and agree with the user on a set of benchmarks to
target for this, and an initial set of experiments to perform. Run the
benchmarks to get a baseline set of measurements and treat this as the north star.

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
 
# Run summary (2026-04-12)

Benchmarks used as the north star:

- Arete workloads: `boot`, `bootstrap-and-psyntax`
- R7RS subset: `tak`, `earley`, `destruc`, `nboyer`
- Perf reports: `boot`, `tak`

Baseline retained for comparison:

- `boot`: `236.714ms`
- `bootstrap-and-psyntax`: `1.024648s`
- `tak`: `12.434s`
- `destruc`: `20.936s`
- `nboyer`: `15.555s`
- `earley`: `CRASHED`

Experiments attempted in this run:

- `aov-0gte` VM fast path for simple function entry
  - rejected, committed and reverted
  - slower on `boot` and `tak`
- `aov-md9k` lazy VM upvalue allocation
  - rejected, committed and reverted
  - `tak` regressed and bootstrap validation became unstable
- `aov-dm18` outline cold VM arithmetic slow paths
  - rejected, committed and reverted
  - `tak` regressed, `bootstrap-and-psyntax` roughly flat
- `aov-kn1t` recycle VM frames on tail calls
  - broader version showed some benchmark wins but failed additional validation
  - safer version fixed the `boot --perf-report` crash but regressed `boot`, `bootstrap-and-psyntax`, and `tak`
  - rejected, committed as `211682f` and reverted as `906b7d3`

Net result:

- No VM optimization from this run was kept.
- The easy local VM changes are mostly exhausted.
- The most credible future directions still look structural:
  - a correctness-proofed tail-call frame-reuse design
  - bytecode/immediate-format redesign rather than a narrow word-size tweak
  - reconsidering selective native compilation only if the campaign is willing to trade simplicity for speed
