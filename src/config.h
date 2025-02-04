#ifndef CONFIG_H
#define CONFIG_H

#if defined(__APPLE__) && defined(MAC_OS_X_VERSION_10_6)
#define OS_STR "apple"
#define IS_MACOS 1
#endif
#if defined(__APPLE__) && defined(MAC_OS_X_VERSION_10_6) || \
        defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#define IS_BSD 1
#define IS_LINUX 0
#elif defined(__linux__)
#define OS_STR "Linux"
#define IS_LINUX 1
#define IS_BSD 0
#endif

#if defined(__x86_64__)
#define IS_X86_64 1
#define IS_ARM_64 0
#define ARCH_STR "x86_64"
#elif defined (__aarch64__) || defined(__ARM_ARCH) || defined(__ARM_ARCH_64)
#define IS_X86_64 0
#define IS_ARM_64 1
#define ARCH_STR "aarch64"
#endif

#ifndef __GNUC__
    #define __attribute__(x)  // Define as empty if not using GCC/Clang
#endif

#define __noreturn __attribute__((noreturn))

#endif // !CONFIG_H
