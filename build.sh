#!/usr/bin/env bash
# Build the whole thing: the hcc compiler (with JIT), the libtos standard
# library, and optionally run the test suites.
#
# Everything is built into a local prefix (./build/prefix) so nothing in
# /usr/local is touched or relied upon.
#
# Usage: ./build.sh [--debug] [--sqlite] [--test] [--install] [--clean]
#   --debug    Debug build instead of Release
#   --sqlite   Link libtos against sqlite3
#   --test     Run unit, JIT and LSP tests after building
#   --install  Also install hcc + libtos into $INSTALL_PREFIX (default
#              /usr/local), using sudo for the copy only if needed
#   --clean    Remove ./build and ./hcc before building
#
# Run as your normal user, not with sudo.
#
# Env overrides: CC, JOBS, INSTALL_PREFIX
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$ROOT/build"
PREFIX="$BUILD_DIR/prefix"
CC="${CC:-gcc}"
JOBS="${JOBS:-$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)}"
INSTALL_PREFIX="${INSTALL_PREFIX:-/usr/local}"
# hcc always writes its intermediate assembly here (see ASM_TMP_FILE in
# src/main.c), so it has to be ours to overwrite.
ASM_TMP_FILE=/tmp/holyc-asm.s

BUILD_TYPE=Release
SQLITE=0
RUN_TESTS=0
DO_INSTALL=0
DO_CLEAN=0

usage() { awk 'NR > 1 && /^#/ { sub(/^# ?/, ""); print; next } NR > 1 { exit }' "$0"; }

for arg in "$@"; do
    case "$arg" in
        --debug)   BUILD_TYPE=Debug ;;
        --sqlite)  SQLITE=1 ;;
        --test)    RUN_TESTS=1 ;;
        --install) DO_INSTALL=1 ;;
        --clean)   DO_CLEAN=1 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "unknown option: $arg (see --help)" >&2; exit 1 ;;
    esac
done

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }
die()  { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

# Building as root via sudo leaves root-owned files in the tree that break
# every later normal build, so only the install copy is ever elevated.
if [ "$(id -u)" = 0 ] && [ -n "${SUDO_USER:-}" ]; then
    die "don't run build.sh with sudo. Run it as yourself; --install will
       ask for sudo only to copy files into $INSTALL_PREFIX."
fi

for tool in cmake make "$CC"; do
    command -v "$tool" >/dev/null || die "missing required tool: $tool"
done

# Files left behind by an earlier root build can't be overwritten.
uid="$(id -u)"
if [ "$uid" != 0 ]; then
    foreign="$(find "$BUILD_DIR" "$ROOT/hcc" "$ROOT/src/holyc-lib" "$ROOT/src/tests" \
        ! -user "$uid" 2>/dev/null | head -5 || true)"
    if [ -n "$foreign" ]; then
        die "some build files are owned by another user (left over from a sudo build?):
$foreign
Fix with:  sudo chown -R $(id -un) \"$BUILD_DIR\" \"$ROOT/hcc\" \"$ROOT/src\""
    fi
    if [ -n "$(find "$ASM_TMP_FILE" ! -user "$uid" 2>/dev/null)" ]; then
        die "$ASM_TMP_FILE belongs to another user and hcc can't overwrite it.
Fix with:  sudo rm -f $ASM_TMP_FILE"
    fi
fi

# Switching compilers on an existing build dir leaves stale objects that
# fail to link, so start over when CC changes.
cached_cc="$(sed -n 's/^CMAKE_C_COMPILER:[A-Z]*=//p' "$BUILD_DIR/CMakeCache.txt" 2>/dev/null || true)"
if [ -n "$cached_cc" ] && [ "$cached_cc" != "$(command -v "$CC")" ] && [ "$DO_CLEAN" = 0 ]; then
    echo "compiler changed ($cached_cc -> $CC), doing a clean build"
    DO_CLEAN=1
fi

# Debug builds use AddressSanitizer, whose leak checker makes hcc exit
# non-zero on every compile.
if [ "$BUILD_TYPE" = Debug ]; then
    export ASAN_OPTIONS="${ASAN_OPTIONS:-detect_leaks=0}"
fi

if [ "$DO_CLEAN" = 1 ]; then
    step "Cleaning"
    rm -rf "$BUILD_DIR" "$ROOT/hcc"
fi

step "Configuring hcc ($BUILD_TYPE, CC=$CC)"
cmake_args=(
    -S "$ROOT/src" -B "$BUILD_DIR" -G 'Unix Makefiles'
    -DCMAKE_C_COMPILER="$CC"
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE"
    -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX"
    -DCMAKE_C_FLAGS='-Wextra -Wall -Wpedantic'
    -DCMAKE_EXPORT_COMPILE_COMMANDS=on
    -DHCC_ENABLE_JIT=on
)
# CMakeLists checks `DEFINED HCC_LINK_SQLITE3`, so turning it off means
# removing it from the cache rather than setting it to OFF.
if [ "$SQLITE" = 1 ]; then
    cmake_args+=(-DHCC_LINK_SQLITE3=1)
else
    cmake_args+=(-UHCC_LINK_SQLITE3)
fi
cmake "${cmake_args[@]}"

step "Building hcc (-j$JOBS)"
make -C "$BUILD_DIR" -j"$JOBS"
HCC="$ROOT/hcc"
[ -x "$HCC" ] || die "hcc binary not produced at $HCC"

step "Building libtos into $PREFIX"
rm -rf "$PREFIX"
mkdir -p "$PREFIX/include" "$PREFIX/lib"
cp "$ROOT/src/holyc-lib/tos.HH" "$PREFIX/include/tos.HH"
lib_args=(-fPIC -lib tos --install-dir="$PREFIX")
[ "$SQLITE" = 1 ] && lib_args=(-DHCC_LINK_SQLITE3 "${lib_args[@]}")
(cd "$ROOT/src/holyc-lib" && "$HCC" "${lib_args[@]}" ./all.HC)

if [ "$RUN_TESTS" = 1 ]; then
    # The test runners otherwise compile each test against /usr/local.
    export HCC_INSTALL_DIR="$PREFIX"

    step "Running unit tests"
    (cd "$ROOT/src/tests" && "$HCC" --install-dir="$PREFIX" ./run.HC -o test-runner && ./test-runner)

    step "Running JIT unit tests"
    (cd "$ROOT/src/tests" && "$HCC" --install-dir="$PREFIX" ./run_jit.HC -o test-runner-jit && ./test-runner-jit)

    step "Running LSP tests"
    (cd "$ROOT/src/tests/lsp" && "$HCC" --install-dir="$PREFIX" ./run_lsp_tests.HC -o lsp-test-runner && ./lsp-test-runner)
fi

if [ "$DO_INSTALL" = 1 ]; then
    step "Installing into $INSTALL_PREFIX"
    # Copy what was already built rather than `make install`, which re-runs
    # hcc (as root, under sudo) inside the source tree.
    as=()
    for d in "$INSTALL_PREFIX" "$INSTALL_PREFIX/bin" "$INSTALL_PREFIX/include" "$INSTALL_PREFIX/lib"; do
        while [ ! -e "$d" ]; do d="$(dirname "$d")"; done
        if [ ! -w "$d" ] && [ "$uid" != 0 ]; then
            as=(sudo)
            echo "$d is not writable, using sudo"
            break
        fi
    done
    ${as[@]+"${as[@]}"} mkdir -p "$INSTALL_PREFIX/bin" "$INSTALL_PREFIX/include" "$INSTALL_PREFIX/lib"
    ${as[@]+"${as[@]}"} cp -f "$HCC" "$INSTALL_PREFIX/bin/hcc"
    ${as[@]+"${as[@]}"} cp -f "$PREFIX/include/tos.HH" "$INSTALL_PREFIX/include/tos.HH"
    for f in "$PREFIX"/lib/*; do
        ${as[@]+"${as[@]}"} cp -fPR "$f" "$INSTALL_PREFIX/lib/"
    done
fi

step "Done"
echo "compiler: $HCC"
echo "libtos:   $PREFIX/lib"
if [ "$DO_INSTALL" = 1 ]; then
    echo "installed into $INSTALL_PREFIX - compile with: hcc file.HC -o out"
else
    echo "compile with: $HCC --install-dir=$PREFIX file.HC -o out"
fi
if [ "$BUILD_TYPE" = Debug ]; then
    echo "debug hcc is built with AddressSanitizer: run it with ASAN_OPTIONS=detect_leaks=0"
fi
