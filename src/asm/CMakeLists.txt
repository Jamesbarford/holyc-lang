set(TASM_SOURCES
    asm.c
    asm_enc.c
    asm_jit.c
    dis_arm64.c
    dis_x86_64.c
    enc_arm64.c
    enc_x86_64.c
    target.c
    tasm_debug.c
    tasm_util.c
)

set(TASM_HEADERS
    asm.h
    asm_enc.h
    asm_jit.h
    dis_arm64.h
    dis_x86_64.h
    enc_arm64.h
    enc_x86_64.h
    target.h
    tasm_util.h
)

add_library(tasm STATIC ${TASM_SOURCES} ${TASM_HEADERS})

target_include_directories(tasm PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

find_package(Threads REQUIRED)
target_link_libraries(tasm PUBLIC Threads::Threads)
