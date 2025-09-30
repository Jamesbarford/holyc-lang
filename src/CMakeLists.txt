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

# Function to build clean libtos without duplicate symbols
function(build_clean_libtos target_name)
    # Only include modules that are truly independent and don't include memory.HC
    set(CLEAN_MODULES 
        memory    # The ONLY module that should define memory functions
        math      # Pure math functions - standalone
        system    # System calls - standalone  
        csv       # CSV parsing - standalone
        dir       # Directory operations - standalone
        stat      # File statistics - standalone
    )
    
    # Create object file build commands only for clean modules
    set(OBJ_COMMANDS "")
    set(OBJ_FILES "")
    
    foreach(module ${CLEAN_MODULES})
        list(APPEND OBJ_COMMANDS 
            COMMAND ${CMAKE_COMMAND} -E echo "Compiling ${module}.HC..."
            COMMAND $<TARGET_FILE:hcc> -obj -o ${module}.o ${module}.HC || echo "Failed to compile ${module}.HC"
        )
        list(APPEND OBJ_FILES ${module}.o)
    endforeach()
    
    # Create the custom target
    add_custom_target(${target_name} ALL
        COMMAND ${CMAKE_COMMAND} -E echo "Building minimal clean libtos.a..."
        ${OBJ_COMMANDS}
        COMMAND ar rcs tos.a ${OBJ_FILES}
        COMMAND ${CMAKE_COMMAND} -E copy_if_different tos.a ${CMAKE_BINARY_DIR}/lib/libtos.a
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/holyc-lib
        DEPENDS hcc
        COMMENT "Building minimal clean libtos.a using modular HolyC compilation"
    )
endfunction()

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

# Build clean libtos library without duplicate symbols
build_clean_libtos(libtos)
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
install(TARGETS hcc DESTINATION bin)

# Install libtos.a built by custom target  
install(FILES ${CMAKE_BINARY_DIR}/lib/libtos.a DESTINATION lib)

# Install the header file
install(FILES ${CMAKE_SOURCE_DIR}/holyc-lib/tos.HH DESTINATION include)

# Ensure libtos is built before installation
install(CODE "
    message(STATUS \"Installing HolyC compiler and libtos library...\")
    message(STATUS \"hcc -> ${CMAKE_INSTALL_PREFIX}/bin/hcc\")
    message(STATUS \"libtos.a -> ${CMAKE_INSTALL_PREFIX}/lib/libtos.a\") 
    message(STATUS \"tos.HH -> ${CMAKE_INSTALL_PREFIX}/include/tos.HH\")
")
