cmake_minimum_required(VERSION 3.10)

project(HolyC)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_C_STANDARD_REQUIRED True)
set(CMAKE_C_STANDARD 11)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ../)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build (Debug or Release)" FORCE)
endif()

add_definitions(-DINSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}")

# Add debugging symbols for Debug builds
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g")
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_definitions(DEBUG)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address")
endif()

if (HCC_LINK_SQLITE3)
    add_compile_definitions(HCC_LINK_SQLITE3)
endif()

execute_process(
  COMMAND git rev-parse main
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE HCC_GIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

add_compile_definitions(HCC_GIT_HASH="${HCC_GIT_HASH}")

# Optionally, add more flags or options depending on the build type
# For example, optimization flags for Release builds:
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O2")

set(SOURCES
    aostr.c
    arena.c
    ast.c
    cctrl.c
    cfg-print.c
    cfg.c
    cli.c
    containers.c
    compile.c
    ir.c
    ir-types.c
    lexer.c
    list.c
    main.c
    memory.c
    mempool.c
    parser.c
    prsasm.c
    prslib.c
    prsutil.c
    transpiler.c
    x86.c
)

set(HEADERS 
    aostr.h
    arena.h
    ast.h
    cctrl.h
    cfg-print.h
    cfg.h
    cli.h
    compile.h
    config.h
    containers.h
    ir.h
    ir-types.h
    lexer.h
    list.h
    memory.h
    mempool.h
    parser.h
    prsasm.h
    prslib.h
    prsutil.h
    util.h
    transpiler.h
    version.h
    x86.h
)

add_executable(hcc ${SOURCES} ${HEADERS})

# Custom target to build libtos.a using hcc
add_custom_target(libtos ALL
    COMMAND $<TARGET_FILE:hcc> -lib tos all.HC
    COMMAND ${CMAKE_COMMAND} -E copy_if_different tos.a ${CMAKE_BINARY_DIR}/lib/libtos.a
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/holyc-lib
    DEPENDS hcc
    COMMENT "Building libtos.a using HolyC compiler"
)
# Ensure build directory exists for library copy
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

set(HCC_PATH "${CMAKE_INSTALL_PREFIX}/bin/hcc")

# Command for unit tests
add_custom_target(unit-test
    COMMAND ${CMAKE_COMMAND} -E echo "Running unit tests..."
    COMMAND ${HCC_PATH} -o test-runner ${CMAKE_SOURCE_DIR}/tests/run.HC
    COMMAND ${CMAKE_SOURCE_DIR}/tests/test-runner
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tests
    COMMENT "Building and running unit tests"
    USES_TERMINAL
)

target_link_libraries(hcc m)

# Install the executable
install(TARGETS hcc DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)

# Install libtos.a built by custom target
install(FILES ${CMAKE_BINARY_DIR}/lib/libtos.a DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

# Install the header file
install(FILES ${CMAKE_SOURCE_DIR}/holyc-lib/tos.HH DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

# Custom installation step to run `hcc` during install to create libtos
# This creates libtos.a and libtos.so in the holyc-lib directory
if (DEFINED HCC_LINK_SQLITE3)
    install(CODE "execute_process(COMMAND \"${HCC_PATH}\" -DHCC_LINK_SQLITE3 -lib tos ${CMAKE_SOURCE_DIR}/holyc-lib/all.HC WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/holyc-lib)")
else()
    install(CODE "execute_process(COMMAND \"${HCC_PATH}\" -lib tos ${CMAKE_SOURCE_DIR}/holyc-lib/all.HC WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/holyc-lib)")
endif()

# Install the generated libtos library files
# Note: The actual filenames depend on the platform (libtos.a and libtos.so on Linux, libtos.dylib on macOS)
install(CODE "
    if(EXISTS \"${CMAKE_SOURCE_DIR}/holyc-lib/tos.a\")
        file(INSTALL \"${CMAKE_SOURCE_DIR}/holyc-lib/tos.a\" DESTINATION \"${CMAKE_INSTALL_PREFIX}/lib\" RENAME \"libtos.a\")
    endif()
    if(EXISTS \"${CMAKE_SOURCE_DIR}/holyc-lib/tos.so\")
        file(INSTALL \"${CMAKE_SOURCE_DIR}/holyc-lib/tos.so\" DESTINATION \"${CMAKE_INSTALL_PREFIX}/lib\" RENAME \"libtos.so\")
    endif()
    if(EXISTS \"${CMAKE_SOURCE_DIR}/holyc-lib/tos.dylib\")
        file(INSTALL \"${CMAKE_SOURCE_DIR}/holyc-lib/tos.dylib\" DESTINATION \"${CMAKE_INSTALL_PREFIX}/lib\" RENAME \"libtos.dylib\")
    endif()
")
