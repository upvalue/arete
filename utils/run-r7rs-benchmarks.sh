#!/usr/bin/env bash
#
# run-r7rs-benchmarks.sh - run the ecraven/r7rs-benchmarks suite under Arete.
#
# This is a standalone driver for vendor/r7rs-benchmarks that avoids
# modifying the vendored tree. It concatenates our prelude/postlude with
# the benchmark source (mirroring the layout the vendored `bench` script
# expects), runs it under bin/arete with heap.boot loaded, and tees the
# output to results.Arete in the repo root.
#
# Usage:
#   utils/run-r7rs-benchmarks.sh <benchmark> [benchmark ...]
#   utils/run-r7rs-benchmarks.sh all
#   utils/run-r7rs-benchmarks.sh gabriel | kvw | num | io | other | gc | synth
#   utils/run-r7rs-benchmarks.sh perf
#
# Environment overrides:
#   ARETE      - path to arete binary (default: $ROOT/bin/arete)
#   HEAP       - path to heap image   (default: $ROOT/heap.boot)
#   CPU_LIMIT  - ulimit -t per run in seconds (default: 300)
#   TEMP       - where assembled benchmark sources are written
#                (default: /tmp/arete-r7rs-benchmarks)
#   RESULTS    - output results file (default: $ROOT/results.Arete)
#
# Exit status is always 0 if scaffolding ran; individual benchmark
# failures are logged (many benchmarks are expected to fail due to
# missing functions).

set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
BENCH_DIR="$ROOT/vendor/r7rs-benchmarks"
SRC="$BENCH_DIR/src"
INPUTS="$BENCH_DIR/inputs"

ARETE="${ARETE:-$ROOT/bin/arete}"
HEAP="${HEAP:-$ROOT/heap.boot}"
PRELUDE="$HERE/r7rs-bench-prelude.scm"
POSTLUDE="$HERE/r7rs-bench-postlude.scm"
CPU_LIMIT="${CPU_LIMIT:-300}"
TEMP="${TEMP:-/tmp/arete-r7rs-benchmarks}"
RESULTS="${RESULTS:-$ROOT/results.Arete}"

# These lists mirror the groupings in vendor/r7rs-benchmarks/bench.
GABRIEL_BENCHMARKS="browse deriv destruc diviter divrec puzzle triangl tak takl ntakl cpstak ctak"
NUM_BENCHMARKS="fib fibc fibfp sum sumfp fft mbrot mbrotZ nucleic pi pnpoly ray simplex"
KVW_BENCHMARKS="ack array1 string sum1 cat tail wc"
IO_BENCHMARKS="read1"
OTHER_BENCHMARKS="compiler conform dynamic earley graphs lattice matrix maze mazefun nqueens paraffins parsing peval primes quicksort scheme slatex chudnovsky"
GC_BENCHMARKS="nboyer sboyer gcbench mperm"
SYNTH_BENCHMARKS="equal bv2string"
ALL_BENCHMARKS="$GABRIEL_BENCHMARKS $NUM_BENCHMARKS $KVW_BENCHMARKS $IO_BENCHMARKS $OTHER_BENCHMARKS $GC_BENCHMARKS $SYNTH_BENCHMARKS"

# Curated runtime-performance suite. This is our own grouping (not from the
# upstream bench script) chosen to exercise distinct runtime subsystems
# without being dominated by floating-point or bignum numeric kernels.
#
#   gcbench  - Boehm/Ellis GC benchmark: balanced trees of varying lifetimes.
#              Primary collector stress test.
#   nboyer   - Boyer-Moore rewriter. Heavy cons + symbol allocation over
#              symbolic terms; "realistic" symbolic workload.
#   earley   - Feeley's Earley parser. Closure- and list-heavy; stresses
#              minor GC and write barriers without being numeric.
#   destruc  - Destructive list ops (set-cdr!). Isolates mutator / write
#              barrier cost on pairs.
#   cpstak   - CPS-transformed tak. First-class procedures + tail calls;
#              isolates call/return and closure creation.
#   ctak     - tak via call/cc. Exercises the continuation implementation
#              specifically.
#   peval    - Feeley's partial evaluator. Medium-sized realistic program;
#              mix of recursion, lists, closures.
#   nqueens  - Classic N-queens search. Small, list-heavy backtracking;
#              cheap sanity check that allocation + recursion are healthy.
PERF_BENCHMARKS="gcbench nboyer earley destruc cpstak ctak peval nqueens"

usage() {
    cat >&2 <<EOF
Usage: $0 <benchmark> [benchmark ...]
       $0 all | gabriel | num | kvw | io | other | gc | synth | perf

Runs ecraven/r7rs-benchmarks under Arete. Results are appended to
$RESULTS. Many benchmarks are expected to fail due to missing functions.
EOF
    exit 1
}

if [ "$#" -lt 1 ]; then usage; fi

# Expand group names into benchmark lists.
benchmarks=""
for arg in "$@"; do
    case "$arg" in
        all)     benchmarks="$benchmarks $ALL_BENCHMARKS" ;;
        gabriel) benchmarks="$benchmarks $GABRIEL_BENCHMARKS" ;;
        num)     benchmarks="$benchmarks $NUM_BENCHMARKS" ;;
        kvw)     benchmarks="$benchmarks $KVW_BENCHMARKS" ;;
        io)      benchmarks="$benchmarks $IO_BENCHMARKS" ;;
        other)   benchmarks="$benchmarks $OTHER_BENCHMARKS" ;;
        gc)      benchmarks="$benchmarks $GC_BENCHMARKS" ;;
        synth)   benchmarks="$benchmarks $SYNTH_BENCHMARKS" ;;
        perf)    benchmarks="$benchmarks $PERF_BENCHMARKS" ;;
        *)       benchmarks="$benchmarks $arg" ;;
    esac
done

if [ ! -x "$ARETE" ]; then
    echo "arete binary not found or not executable: $ARETE" >&2
    echo "(run 'make bin/arete' first, or set ARETE=...)" >&2
    exit 2
fi

if [ ! -f "$HEAP" ]; then
    echo "heap image not found: $HEAP" >&2
    echo "(run 'make heap.boot' first, or set HEAP=...)" >&2
    exit 2
fi

mkdir -p "$TEMP"

{
    echo
    echo '****************************'
    echo "Benchmarking Arete on $(date) under $(uname -a)"
} >> "$RESULTS"

run_one() {
    local bench="$1"
    local src="$SRC/$bench.scm"
    local inp="$INPUTS/$bench.input"
    local tgt="$TEMP/$bench.scm"

    if [ ! -f "$src" ]; then
        echo "+!CSVLINE!+arete,$bench,MISSINGSOURCE"
        return
    fi

    # Assemble: <our prelude> <bench src> <common.scm> <our postlude> <common-postlude.scm>
    # Order matches what vendor/r7rs-benchmarks/bench's make_src_code does,
    # substituting our out-of-tree prelude/postlude for src/Arete-*.scm.
    cat "$PRELUDE" "$src" "$SRC/common.scm" "$POSTLUDE" "$SRC/common-postlude.scm" > "$tgt"

    echo
    echo "Testing $bench under Arete"
    ulimit -t "$CPU_LIMIT"
    local status
    # Run from the vendored benchmark root so benchmarks that open
    # relative paths like "inputs/bib" or "outputs/ray.output" resolve
    # the same way they do under the upstream `bench` script.
    mkdir -p "$BENCH_DIR/outputs"
    if [ -f "$inp" ]; then
        ( cd "$BENCH_DIR" && time "$ARETE" "$HEAP" "$tgt" < "$inp" )
        status=$?
    else
        ( cd "$BENCH_DIR" && time "$ARETE" "$HEAP" "$tgt" < /dev/null )
        status=$?
    fi
    if [ "$status" -eq 137 ] || [ "$status" -eq 152 ] || [ "$status" -eq 124 ]; then
        echo "+!CSVLINE!+arete,$bench,ULIMITKILLED"
    elif [ "$status" -ne 0 ]; then
        echo "+!CSVLINE!+arete,$bench,CRASHED"
    fi
}

for bench in $benchmarks; do
    { run_one "$bench"; } 2>&1 | tee -a "$RESULTS"
done
