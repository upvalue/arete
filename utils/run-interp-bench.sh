#!/usr/bin/env bash
#
# run-interp-bench.sh - run the interpreter microbenchmark suite.
#
# Each benchmark in bench/interp/*.scm is run under the tree-walking
# interpreter (bin/arete heap.boot --interp-only <file>), best-of-3 by
# self-reported elapsed time (the benchmark prints
# "+!CSVLINE!+arete-interp,<name>,<seconds>"). The driver reports each
# CSV line verbatim and then a summary table.
#
# Usage:
#   utils/run-interp-bench.sh                 # run all
#   utils/run-interp-bench.sh arith tak       # subset by bench name
#
# Environment overrides:
#   ARETE       path to arete binary (default: $ROOT/bin/arete)
#   HEAP        path to heap image   (default: $ROOT/heap.boot)
#   RUNS        runs per benchmark   (default: 3)
#   CPU_LIMIT   ulimit -t per run    (default: 30)
#   BENCH_DIR   benchmark directory  (default: $ROOT/bench/interp)
#   RESULTS     append CSV to file   (default: unset — stdout only)
#
# Exit status: 0 if every benchmark produced a CSV line, non-zero otherwise.

set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"

ARETE="${ARETE:-$ROOT/bin/arete}"
HEAP="${HEAP:-$ROOT/heap.boot}"
BENCH_DIR="${BENCH_DIR:-$ROOT/bench/interp}"
RUNS="${RUNS:-3}"
CPU_LIMIT="${CPU_LIMIT:-30}"
RESULTS="${RESULTS:-}"

if [ ! -x "$ARETE" ]; then
    echo "arete binary not found or not executable: $ARETE" >&2
    exit 2
fi
if [ ! -f "$HEAP" ]; then
    echo "heap image not found: $HEAP (run 'make heap.boot')" >&2
    exit 2
fi
if [ ! -d "$BENCH_DIR" ]; then
    echo "bench dir not found: $BENCH_DIR" >&2
    exit 2
fi

# Collect benchmark files: either all of $BENCH_DIR/*.scm or the names
# passed on the command line (without the .scm suffix).
benches=()
if [ "$#" -eq 0 ]; then
    for f in "$BENCH_DIR"/*.scm; do
        [ -f "$f" ] && benches+=("$f")
    done
else
    for arg in "$@"; do
        f="$BENCH_DIR/$arg.scm"
        if [ ! -f "$f" ]; then
            echo "no such benchmark: $arg (expected $f)" >&2
            exit 2
        fi
        benches+=("$f")
    done
fi

if [ "${#benches[@]}" -eq 0 ]; then
    echo "no benchmarks found in $BENCH_DIR" >&2
    exit 2
fi

ulimit -t "$CPU_LIMIT"

# min prints the smallest float from its argument list, or "ERR" if none given.
min() {
    local best=""
    for v in "$@"; do
        if [ -z "$best" ]; then
            best="$v"
        else
            best="$(awk -v a="$best" -v b="$v" 'BEGIN { print (b<a)?b:a }')"
        fi
    done
    if [ -z "$best" ]; then echo "ERR"; else echo "$best"; fi
}

emit() {
    if [ -n "$RESULTS" ]; then echo "$1" | tee -a "$RESULTS"; else echo "$1"; fi
}

emit "# interpreter microbenchmarks ($(date))"
emit "# arete=$ARETE heap=$HEAP runs=$RUNS cpu_limit=$CPU_LIMIT"

declare -a summary_names
declare -a summary_bests
rc=0

for bench in "${benches[@]}"; do
    name="$(basename "$bench" .scm)"
    times=()
    for i in $(seq 1 "$RUNS"); do
        # The benchmark prints its own "+!CSVLINE!+arete-interp,<name>,<secs>".
        out="$("$ARETE" "$HEAP" --interp-only "$bench" 2>&1)"
        line="$(printf '%s\n' "$out" | grep '^+!CSVLINE!+arete-interp,' | tail -1)"
        if [ -z "$line" ]; then
            emit "+!CSVLINE!+arete-interp,$name,CRASHED"
            echo "---- $name run $i output:" >&2
            printf '%s\n' "$out" >&2
            rc=1
            continue
        fi
        emit "$line"
        t="$(printf '%s' "$line" | awk -F, '{print $NF}')"
        times+=("$t")
    done
    if [ "${#times[@]}" -gt 0 ]; then
        best="$(min "${times[@]}")"
    else
        best="ERR"
    fi
    summary_names+=("$name")
    summary_bests+=("$best")
done

emit ""
emit "summary (best of $RUNS, seconds):"
total=0
for idx in "${!summary_names[@]}"; do
    n="${summary_names[$idx]}"
    b="${summary_bests[$idx]}"
    printf -v line "  %-14s %s" "$n" "$b"
    emit "$line"
    if [ "$b" != "ERR" ]; then
        total="$(awk -v a="$total" -v b="$b" 'BEGIN { printf "%.3f", a+b }')"
    fi
done
emit "  ---"
emit "  total         $total"

exit "$rc"
