FROM gcc:14

RUN apt-get update -qq && \
    apt-get install -y -qq cmake libsqlite3-dev dos2unix clang > /dev/null 2>&1 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /holyc
COPY . .

RUN find src/holyc-lib src/tests -type f \( -name '*.HC' -o -name '*.HH' \) -exec dos2unix {} + 2>/dev/null && \
    cmake -S ./src -B ./build -G 'Unix Makefiles' \
      -DCMAKE_C_COMPILER=gcc \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=/usr/local \
      -DHCC_LINK_SQLITE3=1 \
      -DCMAKE_C_FLAGS='-Wextra -Wall -Wpedantic' && \
    make -C ./build -j$(nproc) && \
    make -C ./build install

WORKDIR /code
ENTRYPOINT ["hcc"]
CMD ["--help"]
