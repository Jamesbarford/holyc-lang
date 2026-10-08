#!/usr/bin/env bash
# Build and test hcc in lightweight containers for several CPU architectures.
#
# Usage: docker/test.sh [--full] [--verbose] [arch...] [-- build.sh args]
#   --full     run every configuration (gcc, sqlite, debug/ASan, clang,
#              install), see docker/matrix.sh
#   --verbose  also stream the logs to the terminal, prefixed by arch
#   arch       x86_64 or arm64 (default: both)
#   args       passed to build.sh in the container (default: --test)
#
# Examples:
#   docker/test.sh                   # both archs, build + tests
#   docker/test.sh --full            # both archs, every configuration
#   docker/test.sh arm64             # just arm64
#   docker/test.sh x86_64 -- --sqlite --test
#
# All archs run in parallel, logs go to docker/logs/<arch>.log. The
# working tree (including uncommitted changes, minus ignored files) is
# copied into each container, so the host checkout is never modified.
#
# Archs that differ from the host run under QEMU. On Debian/Ubuntu:
#   sudo apt install qemu-user-static
# Docker Desktop on macOS already includes it.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_DIR="$ROOT/docker/logs"
ALL_ARCHS="x86_64 arm64"

die() { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }
usage() { awk 'NR > 1 && /^#/ { sub(/^# ?/, ""); print; next } NR > 1 { exit }' "$0"; }

archs=()
args=()
full=0
verbose=0
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help) usage; exit 0 ;;
        --full) full=1 ;;
        -v|--verbose) verbose=1 ;;
        --) shift; args=("$@"); break ;;
        x86_64|arm64) archs+=("$1") ;;
        *) die "unknown arch: $1 (expected one of: $ALL_ARCHS)" ;;
    esac
    shift
done
[ ${#archs[@]} -gt 0 ] || archs=($ALL_ARCHS)
[ ${#args[@]} -gt 0 ] || args=(--test)

command -v docker >/dev/null || die "docker is not installed"
docker info >/dev/null 2>&1 || die "can't reach the docker daemon (is it running, and are you in the docker group?)"
command -v git >/dev/null || die "git is needed to collect the source files"

mkdir -p "$LOG_DIR"

# Tracked and untracked files, skipping ignored ones and deleted tracked files.
source_tar() {
    (cd "$ROOT" && git ls-files -z --cached --others --exclude-standard |
        while IFS= read -r -d '' f; do [ -e "$f" ] && printf '%s\0' "$f"; done |
        tar --null -T - -cf -)
}

# Docker's name for each arch.
platform() { if [ "$1" = x86_64 ]; then echo linux/amd64; else echo "linux/$1"; fi; }

if [ "$full" = 1 ]; then
    cmd=(bash docker/matrix.sh)
else
    cmd=(./build.sh --clean "${args[@]}")
fi

run_arch() {
    local arch="$1" start=$SECONDS status=0
    {
        docker build --platform "$(platform "$arch")" -t "holyc-test:$arch" "$ROOT/docker" &&
        source_tar | docker run --rm -i --platform "$(platform "$arch")" "holyc-test:$arch" \
            bash -c 'tar -xf - && echo "arch: $(uname -m)" && "$@"' _ "${cmd[@]}"
    } || status=$?
    echo "elapsed: $((SECONDS - start))s"
    echo "exit status: $status"
}

# Prefix each line with the arch so parallel output stays readable.
prefix() {
    local colour=36
    [ "$1" = arm64 ] && colour=35
    awk -v p="$(printf '\033[%sm[%s]\033[0m ' "$colour" "$1")" '{ print p $0; fflush() }'
}

printf 'Running %s on: %s\n' "${cmd[*]}" "${archs[*]}"
pids=()
for arch in "${archs[@]}"; do
    if [ "$verbose" = 1 ]; then
        run_arch "$arch" 2>&1 | tee "$LOG_DIR/$arch.log" | prefix "$arch" &
    else
        run_arch "$arch" >"$LOG_DIR/$arch.log" 2>&1 &
    fi
    pids+=($!)
    printf '  %-7s started, log: %s\n' "$arch" "${LOG_DIR#$ROOT/}/$arch.log"
done

# Test groups that failed. Counted from the log because the HolyC test
# runners exit 0 even when a test fails.
# In --full mode, matrix.sh prints a RESULT line per step instead.
failed_groups() {
    if [ "$full" = 1 ]; then grep -c '^##### RESULT FAIL' "$1" || true
    else grep -c 'FAILED\|Failed to compile' "$1" || true; fi
}

summarise() {
    local log="$1" passed lsp
    if [ "$full" = 1 ]; then
        echo "$(grep -c '^##### RESULT PASS' "$log" || true) steps passed, $(failed_groups "$log") failed"
        grep '^##### RESULT FAIL' "$log" | sed 's/^##### RESULT FAIL /         ✗ /' || true
        return
    fi
    passed=$(grep -c 'PASSED' "$log" || true)
    lsp=$(grep -o 'lsp: [0-9]*/[0-9]* passed' "$log" | tail -1 || true)
    echo "$passed passed, $(failed_groups "$log") failed${lsp:+, $lsp}"
}

failures=0
echo
for i in "${!archs[@]}"; do
    arch="${archs[$i]}"
    log="$LOG_DIR/$arch.log"
    wait "${pids[$i]}" || true
    status=$(sed -n 's/^exit status: //p' "$log" | tail -1)
    elapsed=$(sed -n 's/^elapsed: //p' "$log" | tail -1)
    if [ "$status" = 0 ] && [ "$(failed_groups "$log")" = 0 ]; then
        result=$'\033[32mok\033[0m  '
    else
        result=$'\033[31mFAIL\033[0m'
        failures=$((failures + 1))
    fi
    printf '%s %-7s %6s  %s\n' "$result" "$arch" "$elapsed" "$(summarise "$log")"
    if [ "$full" = 1 ] && ! grep -q '^##### SUMMARY' "$log"; then
        echo "         matrix didn't finish, see the log"
    fi
    if grep -q 'exec format error' "$log"; then
        echo "       can't run $arch binaries: install QEMU (sudo apt install qemu-user-static)"
    fi
done

[ "$failures" = 0 ] || { echo; echo "see ${LOG_DIR#$ROOT/}/<arch>.log for details"; exit 1; }
