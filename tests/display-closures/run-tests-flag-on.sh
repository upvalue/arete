#!/bin/bash
# Run preboot + compiler tests with COMPILER-DISPLAY-CLOSURES on. Compares
# each test's stdout (minus `arete:` and `;;` debug lines) to its .exp file.
#
# This exercises display-closure emission on programs the existing compiler
# already compiles correctly with the flag off. Any divergence is a bug.

set -u

cd "$(dirname "$0")/../.."

pass=0
fail=0
failures=()

ansi_strip='s/\x1b\[[0-9;]*m//g'

# Preboot tests — each is a standalone Scheme program, no boot.scm needed.
for t in tests/preboot/*.scm; do
  exp="${t%.scm}.exp"
  [ -f "$exp" ] || continue
  actual=$(./bin/arete --set COMPILER-DISPLAY-CLOSURES '"#t"' "$t" 2>&1 \
    | sed "$ansi_strip" \
    | grep -vE '^(arete:|;;)')
  expected=$(cat "$exp")
  if [ "$actual" = "$expected" ]; then
    pass=$((pass+1))
  else
    fail=$((fail+1))
    failures+=("$t")
  fi
done

# Compiler tests — run under --set BOOT-STAGE 3 + boot.scm via compiler-test.scm.
for t in tests/compiler/*.scm; do
  exp="${t%.scm}.exp"
  [ -f "$exp" ] || continue
  actual=$(./bin/arete --set BOOT-STAGE 3 --set COMPILER-DISPLAY-CLOSURES '"#t"' \
    boot.scm --set compiler-test-file "\"$t\"" tests/compiler-test.scm 2>&1 \
    | sed "$ansi_strip" \
    | grep -vE '^(arete:|;;)')
  expected=$(cat "$exp")
  if [ "$actual" = "$expected" ]; then
    pass=$((pass+1))
  else
    fail=$((fail+1))
    failures+=("$t")
  fi
done

echo "pass: $pass"
echo "fail: $fail"
if [ $fail -gt 0 ]; then
  echo "FAILURES:"
  for f in "${failures[@]}"; do
    echo "  $f"
  done
  exit 1
fi
