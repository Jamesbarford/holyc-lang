#ifndef CONFIG_H
#define CONFIG_H

#if defined(__APPLE__) && defined(MAC_OS_X_VERSION_10_6) || \
        defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#define IS_BSD (1)
#elif defined(__linux__)
#define IS_LINUX (1)
#endif

#endif // !CONFIG_H
