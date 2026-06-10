#!/usr/bin/env bash
#
# run-callrate.sh - measure VM ns/call via bench/vm/callrate.scm.
#
# Protocol: RUNS plain runs (no perf instrumentation), best-of by the
# benchmark's self-reported wall time; then one --perf-report run to read
# the exact vm.calls count. Reports ns/call = best_wall / vm_calls.
# (vm.calls includes a few hundred image-load calls; the benchmark itself
# performs ~18.5M, so the error is < 0.01%.)
#
# Environment overrides:
#   ARETE   path to arete binary (default: $ROOT/bin/arete)
#   HEAP    path to heap image   (default: $ROOT/heap.boot)
#   RUNS    plain runs           (default: 5)

set -eu

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"

ARETE="${ARETE:-$ROOT/bin/arete}"
HEAP="${HEAP:-$ROOT/heap.boot}"
RUNS="${RUNS:-5}"
BENCH="$ROOT/bench/vm/callrate.scm"

best=""
for i in $(seq 1 "$RUNS"); do
    line="$("$ARETE" "$HEAP" "$BENCH" | grep '^+!CSVLINE!+arete-vm,callrate,' | tail -1)"
    t="$(printf '%s' "$line" | awk -F, '{print $NF}')"
    echo "run $i: ${t}s"
    if [ -z "$best" ] || awk -v a="$best" -v b="$t" 'BEGIN { exit !(b<a) }'; then
        best="$t"
    fi
done

perf_json="$(mktemp /tmp/callrate-perf-XXXXXX.json)"
"$ARETE" "$HEAP" --perf-report "$perf_json" "$BENCH" >/dev/null
vm_calls="$(python3 -c "import json,sys; print(json.load(open('$perf_json'))['vm']['calls'])")"
rm -f "$perf_json"

# Exact benchmark call count: T(34) = 2*fib(35) - 1 (see callrate.scm).
# vm.calls only counts apply_vm entries — on NATIVE_VM_DEFAULT=1 builds the
# native dispatch core handles most calls without entering apply_vm, so
# ns/call(vm) is only meaningful when vm_calls is close to expected.
EXPECTED_CALLS=18454929

ns_analytic="$(awk -v w="$best" -v c="$EXPECTED_CALLS" 'BEGIN { printf "%.2f", (w * 1e9) / c }')"
ns_vm="$(awk -v w="$best" -v c="$vm_calls" 'BEGIN { printf "%.2f", (w * 1e9) / c }')"
echo "+!CSVLINE!+arete-vm,callrate-best,$best"
echo "vm_calls (apply_vm entries): $vm_calls  (expected calls: $EXPECTED_CALLS)"
echo "ns/call (wall / expected):   $ns_analytic"
echo "ns/call (wall / vm_calls):   $ns_vm"
