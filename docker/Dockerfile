# Minimal build environment for hcc; the platform (x86_64 or arm64) is
# picked by `docker build --platform`, see docker/test.sh.
FROM debian:trixie-slim

# g++ only because src/CMakeLists.txt's project() doesn't restrict itself to C.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        gcc g++ clang libc6-dev make cmake libsqlite3-dev binutils \
    && rm -rf /var/lib/apt/lists/*

# Build as a normal user, like build.sh expects.
RUN useradd -m hcc
USER hcc
WORKDIR /home/hcc/src
