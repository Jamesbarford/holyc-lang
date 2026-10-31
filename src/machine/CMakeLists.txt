set(TASM_SOURCES
    asm.c
    asm_enc.c
    asm_jit.c
    dis_arm64.c
    dis_x86_64.c
    elf.c
    enc_arm64.c
    enc_x86_64.c
    macho.c
    memory.c
    mir.c
    mir_arm64.c
    mir_x86_64.c
    obj.c
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
    elf.h
    enc_arm64.h
    enc_x86_64.h
    macho.h
    memory.h
    mir.h
    obj.h
    target.h
    tasm_util.h
)

add_library(tasm STATIC ${TASM_SOURCES} ${TASM_HEADERS})

# Position independent: `libhcc_shared` links this archive in, and a
# non-PIC object cannot go into a shared library on Linux.
set_target_properties(tasm PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Absolute paths handed up so `libhcc` can compile these in directly
# rather than link the archive. An embedder should need one library, and
# a static archive only contributes the members something already
# references - most of the encoder is reached through tables and
# function pointers, so a plain link drops it.
set(TASM_SOURCES_ABS "")
foreach(src ${TASM_SOURCES})
    list(APPEND TASM_SOURCES_ABS "${CMAKE_CURRENT_SOURCE_DIR}/${src}")
endforeach()
set(TASM_SOURCES_ABS ${TASM_SOURCES_ABS} PARENT_SCOPE)

target_include_directories(tasm PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

find_package(Threads REQUIRED)
target_link_libraries(tasm PUBLIC Threads::Threads)
