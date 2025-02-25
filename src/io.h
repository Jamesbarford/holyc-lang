#ifndef IO_H__
#define IO_H__

char *ioReadWholeFile(char *file_name, unsigned long *_len);
int ioWriteFile(char *file_name, char *buffer, unsigned long len);
int ioWriteAndSyncFile(char *file_name, char *buffer, unsigned long len);
int ioWriteToFileDescriptor(int fd, int sync, char *buffer, unsigned long len);

#endif
