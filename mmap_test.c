#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>

#define CHUNK 2097152

int main(int argc, char **argv)
{
    time_t start, end;
    start = clock();
    unsigned char *f;
    unsigned char buffer[CHUNK];
    int i;
    int fd = open(argv[1], O_RDONLY);
    struct stat s;
    if(fstat(fd, &s) < 0)
    {
        perror("fstat");
        exit(1);
    }
    int size = s.st_size;

    f = (char *)mmap(0, size, PROT_READ|PROT_EXEC, MAP_PRIVATE, fd, 0);


    for(i = 0; i < CHUNK; i++)
    {
        buffer[i] = f[i];
        printf("%c", buffer[i]);
    }
    // f += CHUNK;
    // for(i = 0; i < CHUNK; i++)
    // {
    //     buffer[i] = f[i];
    //     printf("%c", buffer[i]);
    // }
    munmap(f, size);
    close(fd);
    end = clock();
    double diff = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("time = %f\n", diff);

    return 0;
}