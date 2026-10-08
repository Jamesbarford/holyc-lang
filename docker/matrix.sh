#!/usr/bin/env bash
# Runs inside a docker/test.sh container: every build.sh configuration in
# turn, with a PASS/FAIL line per step. Used by `docker/test.sh --full`.
set -u

steps=0
failed=0
hello="$(mktemp -d)"
printf 'U0 Main() { I64 x = 6 * 7; "hello %%d\\n", x; }\n' > "$hello/hello.HC"

# Test groups that failed; the HolyC runners exit 0 even on failure.
test_failures() { grep -c 'FAILED\|Failed to compile' "$1" || true; }

# step <name> <command...>: run it, fail on non-zero exit or failed tests.
step() {
    local name="$1" log status=0 bad
    shift
    log="$(mktemp)"
    printf '\n##### %s\n' "$name"
    "$@" >"$log" 2>&1 || status=$?
    cat "$log"
    bad=$(test_failures "$log")
    steps=$((steps + 1))
    if [ "$status" = 0 ] && [ "$bad" = 0 ]; then
        printf '##### RESULT PASS %s\n' "$name"
    else
        printf '##### RESULT FAIL %s (exit %s, %s failed test groups)\n' "$name" "$status" "$bad"
        failed=$((failed + 1))
    fi
}

# Compile + run hello.HC with the given hcc, AOT and JIT.
hello_works() {
    local hcc="$1"; shift
    "$hcc" "$@" "$hello/hello.HC" -o "$hello/hello" &&
    [ "$("$hello/hello")" = "hello 42" ] &&
    [ "$("$hcc" "$@" -jit "$hello/hello.HC")" = "hello 42" ]
}

# Is libtos in build/prefix linked against sqlite3?
libtos_has_sqlite() { readelf -d build/prefix/lib/libtos.so* | grep -q sqlite; }

export hello
export -f hello_works libtos_has_sqlite

step "gcc release: build + all tests"          ./build.sh --clean --test
step "gcc release: hello AOT + JIT"            hello_works ./hcc --install-dir=build/prefix
step "gcc release: libtos has no sqlite"       bash -c '! libtos_has_sqlite'

step "sqlite: build + all tests"               ./build.sh --sqlite --test
step "sqlite: libtos links sqlite3"            libtos_has_sqlite
step "sqlite: hello AOT + JIT"                 hello_works ./hcc --install-dir=build/prefix
step "sqlite: plain rebuild turns it off"      bash -c "./build.sh && ! grep -q '^HCC_LINK_SQLITE3' build/CMakeCache.txt && ! libtos_has_sqlite"

step "debug (ASan): build + all tests"         ./build.sh --debug --test
step "debug (ASan): hello AOT + JIT"           env ASAN_OPTIONS=detect_leaks=0 bash -c 'hello_works ./hcc --install-dir=build/prefix'

step "clang release: build + all tests"        env CC=clang ./build.sh --test
step "clang release: compiler is clang"        grep -q '^CMAKE_C_COMPILER:.*clang' build/CMakeCache.txt
step "clang release: hello AOT + JIT"          hello_works ./hcc --install-dir=build/prefix

step "install: --install to \$HOME/prefix"    env INSTALL_PREFIX="$HOME/prefix" ./build.sh --install
step "install: files in place"                 bash -c "[ -x $HOME/prefix/bin/hcc ] && [ -f $HOME/prefix/include/tos.HH ] && ls $HOME/prefix/lib | grep -q libtos"
step "install: installed hcc, no --install-dir" hello_works "$HOME/prefix/bin/hcc"

printf '\n##### SUMMARY %s steps, %s failed\n' "$steps" "$failed"
[ "$failed" = 0 ]
