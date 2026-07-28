#!/bin/sh
# Install hcc + libtos from a release archive.
#
# Mirrors what `hcc -lib tos` does at the end of a source build
# (hccLibInit in src/main.c), namely:
#   <prefix>/bin/hcc
#   <prefix>/include/tos.HH
#   <prefix>/lib/libtos.a
#   <prefix>/lib/libtos.so.0.0.1   (Linux)
#   <prefix>/lib/libtos.0.0.1.dylib (macOS)
#
# Two deliberate details, both load-bearing:
#
#  1. No unversioned libtos.so / libtos.dylib is installed. AOT links
#     with `-ltos` and MUST resolve to the static archive: libtos is
#     built -Bsymbolic, so a dynamically linked executable gets a copy
#     relocation for exported data (e.g. the `Fs` exception object)
#     while libtos keeps writing its own copy - two `Fs` objects, and
#     the first `throw` null-derefs. With no unversioned shared object
#     beside it, `-ltos` finds the archive.
#
#  2. The shared library is installed under its VERSIONED name, which
#     is what jitLoadLibtos (src/jit-common.c) probes for and what lets
#     -jit and -repl find the stdlib. It is not a name `-ltos` will
#     ever pick, so (1) is preserved.

set -eu

PREFIX=/usr/local
UNINSTALL=0

usage() {
    cat <<EOF
Usage: ./install.sh [--prefix=DIR] [--uninstall]

  --prefix=DIR   Install root (default: /usr/local)
  --uninstall    Remove a previous install from --prefix
  --help         Show this message
EOF
}

for arg in "$@"; do
    case "$arg" in
        --prefix=*)  PREFIX=${arg#--prefix=} ;;
        --uninstall) UNINSTALL=1 ;;
        --help|-h)   usage; exit 0 ;;
        *) echo "install.sh: unknown option '$arg'" >&2; usage >&2; exit 2 ;;
    esac
done

SRC=$(cd -- "$(dirname -- "$0")" && pwd)

# Work out which platform's archive this is from the payload itself
# rather than from uname - the archive is the authority on what it
# contains.
if [ -f "$SRC/lib/libtos.so.0.0.1" ]; then
    SHLIB=libtos.so.0.0.1
elif [ -f "$SRC/lib/libtos.0.0.1.dylib" ]; then
    SHLIB=libtos.0.0.1.dylib
elif [ "$UNINSTALL" -eq 1 ]; then
    SHLIB=
else
    echo "install.sh: no libtos shared library in $SRC/lib - is this archive complete?" >&2
    exit 1
fi

if [ "$UNINSTALL" -eq 1 ]; then
    for f in "$PREFIX/bin/hcc" \
             "$PREFIX/include/tos.HH" \
             "$PREFIX/lib/libtos.a" \
             "$PREFIX/lib/libtos.so.0.0.1" \
             "$PREFIX/lib/libtos.0.0.1.dylib"; do
        if [ -e "$f" ]; then
            rm -f "$f" && echo "removed $f"
        fi
    done
    exit 0
fi

# Fail early with a useful message rather than part-way through with a
# permission error on one of five copies.
for d in bin include lib; do
    if ! mkdir -p "$PREFIX/$d" 2>/dev/null; then
        echo "install.sh: cannot create $PREFIX/$d - try: sudo ./install.sh --prefix=$PREFIX" >&2
        exit 1
    fi
    if [ ! -w "$PREFIX/$d" ]; then
        echo "install.sh: $PREFIX/$d is not writable - try: sudo ./install.sh --prefix=$PREFIX" >&2
        exit 1
    fi
done

install_file() {
    src=$1 dst=$2 mode=$3
    cp -f "$src" "$dst"
    chmod "$mode" "$dst"
    echo "installed $dst"
}

install_file "$SRC/hcc"             "$PREFIX/bin/hcc"           755
install_file "$SRC/include/tos.HH"  "$PREFIX/include/tos.HH"    644
install_file "$SRC/lib/libtos.a"    "$PREFIX/lib/libtos.a"      644
install_file "$SRC/lib/$SHLIB"      "$PREFIX/lib/$SHLIB"        755

# Downloaded archives carry com.apple.quarantine, which makes the
# binary unlaunchable until Gatekeeper is placated. Clearing it here
# saves a confusing "cannot be opened" dialog.
if [ "$SHLIB" != "${SHLIB%.dylib}" ] && command -v xattr >/dev/null 2>&1; then
    xattr -d com.apple.quarantine "$PREFIX/bin/hcc" 2>/dev/null || true
    xattr -d com.apple.quarantine "$PREFIX/lib/$SHLIB" 2>/dev/null || true
fi

echo
echo "hcc installed to $PREFIX"

# The binary bakes INSTALL_PREFIX in at compile time (release builds
# use /usr/local) and looks for <prefix>/include/tos.HH there. A
# non-default prefix therefore needs --install-dir on every call.
if [ "$PREFIX" != "/usr/local" ]; then
    echo
    echo "NOTE: this build defaults to /usr/local. Because you installed to"
    echo "      $PREFIX, pass --install-dir=$PREFIX to every hcc invocation:"
    echo "        hcc --install-dir=$PREFIX ./hello.HC -o hello"
fi

case ":$PATH:" in
    *":$PREFIX/bin:"*) ;;
    *) echo
       echo "NOTE: $PREFIX/bin is not on your PATH." ;;
esac
