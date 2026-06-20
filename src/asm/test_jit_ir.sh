#!/bin/bash
# End-to-end smoke tests for the IR -> native JIT path (aarch64 & x86_64).
#
# Each test compiles a HolyC program with `hcc -jit`, captures stdout
# and exit code, and compares them against expected values.  Run from
# anywhere; the script locates hcc relative to the project root.
#
# Usage: ./test_jit_ir.sh [path-to-hcc]
#
# Exits non-zero if any case fails.

set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
HCC="${1:-$HERE/../../hcc}"

if [[ ! -x "$HCC" ]]; then
    echo "hcc not found at $HCC (build with -DHCC_ENABLE_JIT=ON)"
    exit 1
fi

case "$(uname -sm)" in
    "Darwin arm64"|"Linux aarch64"|"Darwin x86_64"|"Linux x86_64") ;;
    *) echo "JIT only supports aarch64/x86_64 hosts; skipping"; exit 0 ;;
esac

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0

run_case() {
    local name="$1" src="$2" want_out="$3" want_rc="$4"
    local f="$TMP/$name.HC"
    printf '%s' "$src" > "$f"
    local out rc
    out="$("$HCC" -jit "$f" 2>&1)"
    rc=$?
    if [[ "$out" == "$want_out" && "$rc" == "$want_rc" ]]; then
        printf "PASSED: %s\n" "$name"
        PASS=$((PASS+1))
    else
        printf "FAILED: %s\n  want: out=%q rc=%s\n  got:  out=%q rc=%s\n" \
            "$name" "$want_out" "$want_rc" "$out" "$rc"
        FAIL=$((FAIL+1))
    fi
}

# Hello world: tests string literal globals, variadic printf shim, U0 main.
run_case "hello" \
    'U0 Main() { "hello\n"; }' \
    'hello' 0

# Arithmetic, locals, frame layout, variadic printf with format spec.
run_case "arith_print" \
    'U0 Main()
{
  I64 a = 7;
  I64 b = 6;
  "%d\n", a * b;
}' \
    '42' 0

# Recursive function exercising call-into-self, conditional branch,
# epilogue-from-IR_RET.
run_case "fact5" \
    'I64 fact(I64 n) { if (n <= 1) return 1; return n * fact(n - 1); }
U0 Main() { "%d\n", fact(5); }' \
    '120' 0

# Loop + accumulator: exercises CMP_BR, BR, JMP, and phi materialisation.
run_case "sum_to_100" \
    'I64 sum(I64 n)
{
  I64 t = 0;
  for (I64 i = 1; i <= n; i++) t = t + i;
  return t;
}
U0 Main() { "%d\n", sum(100); }' \
    '5050' 0

# Multiple integer args (x0..x4), tests inter-function call argument
# marshalling.
run_case "args5" \
    'I64 add5(I64 a, I64 b, I64 c, I64 d, I64 e) { return a + b + c + d + e; }
U0 Main() { "%d\n", add5(1, 2, 3, 4, 5); }' \
    '15' 0

printf "\n%d passed, %d failed\n" "$PASS" "$FAIL"
if [[ "$FAIL" -gt 0 ]]; then exit 1; fi
