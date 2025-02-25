#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#ifdef _WIN32
    #include <io.h>
    #include <windows.h>
#endif


#include "config.h"
#include "memory.h"
#include "util.h"

char *ioReadWholeFile(char *file_name, unsigned long *_len) {
    int fd = open(file_name, O_RDONLY, 0644);
    if (fd == -1) {
        loggerPanic("Failed to open file: '%s' - %s\n", file_name,
                strerror(errno));
    }

    unsigned long len = lseek(fd,0,SEEK_END);
    lseek(fd,0,SEEK_SET);

    char *buffer = (char *)xmalloc((sizeof(char)*len)+1);

    unsigned long size = 0;
    unsigned long rbytes = 0;
    while ((rbytes = read(fd,buffer,len)) != 0) {
        size += rbytes;
    }

    if (size != len) {
        loggerPanic("Failed to read whole file size=%zu != len=%zu\n",size,len);
    }

    *_len = len;
    buffer[len] = '\0';
    close(fd);
    return buffer;
}

int ioWriteToFileDescriptor(int fd, int sync, char *buffer, unsigned long len) {
    ssize_t written = 0;
    size_t towrite = len;
    char *ptr = buffer;
    while (towrite > 0) {
        written = write(fd,ptr,towrite);
        if (written < 0) {
            if (written == EINTR) {
                continue;
            }
            close(fd);
            loggerPanic("Failed to write %s\n", strerror(errno));
        }
        towrite -= written;
        ptr += written;
    }

    if (sync) {
#ifdef _WIN32
        HANDLE file_handle = (HANDLE)_get_osfhandle(fd);
        FlushFileBuffers(file_handle);
#else
        fsync(fd);
#endif    
    }
    close(fd);
    return 1;

}

static int ioWriteFileInternal(char *file_name, int sync, char *buffer,
                               unsigned long len)
{
    int fd = open(file_name,O_RDWR|O_TRUNC|O_CREAT,0644);

    if (fd == -1) {
        loggerPanic("Failed to create file for intermediary assembly: %s\n",
                strerror(errno));
    }
    return ioWriteToFileDescriptor(fd,sync,buffer,len);
} 

int ioWriteFile(char *file_name, char *buffer, unsigned long len) {
    return ioWriteFileInternal(file_name,0,buffer,len);
}

int ioWriteAndSyncFile(char *file_name, char *buffer, unsigned long len) {
    return ioWriteFileInternal(file_name,1,buffer,len);
}
