#!/bin/sh
# Test runner for hcc.
#
# A shell script rather than a program compiled by hcc: a harness written
# in the language under test turns a compiler regression into a total
# failure with no signal about what broke, and run.HC can only ever exit
# 0, so nothing calling it could tell a pass from a failure.
#
# Usage:
#   ./run.sh                    run every test, AOT
#   ./run.sh --jit              run every test through the JIT
#   ./run.sh 19_switch.HC       run one test
#   ./run.sh --cflags='-O'      pass extra flags to every compile
#
# A test passes when it compiles, runs, exits 0, and prints no line
# containing "FAILED" - most tests report through PrintResult and then
# exit 0 regardless, so the exit status alone proves nothing.
#
# libtos is built from the tree into build/test-prefix, so the suite tests
# what was just compiled rather than whatever is in /usr/local. Nothing
# needs installing first.

set -u

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
HCC="${HCC:-$ROOT/hcc}"
BUILD="${HCC_BUILD:-$ROOT/build}"
PREFIX="$BUILD/test-prefix"

MODE=aot
CFLAGS=""
FILTER=""

for arg in "$@"; do
    case "$arg" in
        --jit)      MODE=jit ;;
        --cflags=*) CFLAGS="${arg#--cflags=}" ;;
        -*)         echo "unknown option: $arg" >&2; exit 2 ;;
        *)          FILTER="$arg" ;;
    esac
done

if [ ! -x "$HCC" ]; then
    echo "error: $HCC not built - run 'make' first" >&2
    exit 1
fi

mkdir -p "$PREFIX/include" "$PREFIX/lib" "$BUILD/test-lib"
cp "$ROOT/src/holyc-lib/tos.HH" "$PREFIX/include/tos.HH"
(cd "$BUILD/test-lib" &&
    "$HCC" -fPIC -lib tos --install-dir="$PREFIX" \
           "$ROOT/src/holyc-lib/all.HC") > "$BUILD/test-lib/build.log" 2>&1
if [ $? -ne 0 ]; then
    echo "error: failed to build libtos" >&2
    cat "$BUILD/test-lib/build.log" >&2
    exit 1
fi

# 60_link and 64_sret_x8 run the compiler themselves.
HCC_TEST_FLAGS="--install-dir=$PREFIX"
LD_LIBRARY_PATH="$PREFIX/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
DYLD_LIBRARY_PATH="$PREFIX/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
export HCC_TEST_FLAGS LD_LIBRARY_PATH DYLD_LIBRARY_PATH

cd "$ROOT/src/tests" || exit 1

# The suite runs code built by a compiler that may be wrong, so a test
# that spins or hangs has to fail on its own rather than take the run
# with it. macOS has no timeout(1), which is what the CPU limit is for.
HCC_TEST_CPU=${HCC_TEST_CPU:-120}
HCC_TEST_WALL=${HCC_TEST_WALL:-180}
TIMEOUT=""
if command -v timeout >/dev/null 2>&1; then
    TIMEOUT="timeout -k 5 $HCC_TEST_WALL"
fi

guarded() {
    (
        ulimit -t "$HCC_TEST_CPU" 2>/dev/null
        # shellcheck disable=SC2086
        exec $TIMEOUT "$@"
    )
}

OUT=$(mktemp -d "${TMPDIR:-/tmp}/hcc-tests.XXXXXX") || exit 1
trap 'rm -rf "$OUT"' EXIT

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0;0m'

passed=0
failed=0
failures=""

fail() { # name reason
    failed=$((failed + 1))
    failures="$failures $1"
    printf "${RED}FAIL${RESET} %s: %s\n" "$1" "$2"
    sed 's/^/    /' "$OUT/log" | tail -20
}

if [ -n "$FILTER" ]; then
    tests="$FILTER"
else
    tests=$(ls [0-9]*.HC repro_*.HC conditions.HC 2>/dev/null | sort -n)
fi

for test in $tests; do
    # Needs sqlite3, which is opt-in: see HCC_LINK_SQLITE3.
    if [ -z "$FILTER" ] && [ "$test" = 32_sql.HC ]; then
        continue
    fi

    if [ "$MODE" = jit ]; then
        # shellcheck disable=SC2086
        guarded "$HCC" $CFLAGS --install-dir="$PREFIX" -jit "$test" \
            > "$OUT/log" 2>&1
        rc=$?
        if [ $rc -ne 0 ]; then fail "$test" "exit $rc"; continue; fi
    else
        # shellcheck disable=SC2086
        guarded "$HCC" $CFLAGS --install-dir="$PREFIX" "$test" -o "$OUT/a" \
            > "$OUT/log" 2>&1
        rc=$?
        if [ $rc -ne 0 ]; then fail "$test" "compile, exit $rc"; continue; fi
        guarded "$OUT/a" > "$OUT/log" 2>&1
        rc=$?
        if [ $rc -ne 0 ]; then fail "$test" "exit $rc"; continue; fi
    fi

    if grep -aq FAILED "$OUT/log"; then
        fail "$test" "reported FAILED"
        continue
    fi
    passed=$((passed + 1))
done

printf "${GREEN}%d passed${RESET}, " "$passed"
if [ "$failed" -ne 0 ]; then
    printf "${RED}%d failed${RESET}:%s\n" "$failed" "$failures"
    exit 1
fi
printf "0 failed\n"
