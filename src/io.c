#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

int main(void) {
    int flags = FD_CLOEXEC;

    printf("#define F_GETOWN   0x%x\n",F_GETOWN );
    printf("#define F_SETOWN   0x%x\n",F_SETOWN );
    printf("#define F_GETLK    0x%x\n",F_GETLK );
    printf("#define F_SETLK    0x%x\n",F_SETLK );
    printf("#define F_SETLKW   0x%x\n",F_SETLKW );




    printf("#define O_CREAT      0x%x\n",O_CREAT);
    printf("#define O_TRUNC      0x%x\n",O_TRUNC);
    printf("#define O_EXCL       0x%x\n",O_EXCL);
    printf("#define O_NONBLOCK   0x%x\n",O_NONBLOCK);
    printf("#define O_APPEND     0x%x\n",O_APPEND);




    int fd = open("./file.txt",flags,0664);
    printf("flags: dec=%d hex=0x%X num=%d\n",flags,flags,0100);
    if (fd == -1) {
        fprintf(stderr,"Failed to open file: %s\n",
            strerror(errno));
    }
}
