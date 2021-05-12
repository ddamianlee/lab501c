#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include <libpmem.h>
#include <libpmemobj.h>

#define CHUNK 2097152


int main(int argc, char **argv)
{
    time_t start, end;
    start = clock();

    char *pmemaddr;
    size_t mapped_len;
    int is_pmem;
    int i;
    unsigned char buffer[CHUNK];
    
    if((pmemaddr = pmem_map_file(argv[1], 0, PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
    {
        perror("pmem_map_file");
        exit(1);
    }

    printf("is_pmem = %d\n", is_pmem);
    printf("size = %ld\n", mapped_len);
    
    for(i = 0; i < CHUNK; i++)
    {
        buffer[i] = pmemaddr[i];
       // printf("%c", buffer[i]);
    }
    // pmemaddr += CHUNK;
    // for(i = 0; i < CHUNK; i++)
    // {
    //     buffer[i] = pmemaddr[i];
    //     printf("%c", buffer[i]);
    // }
    
    pmem_unmap(pmemaddr, mapped_len);
    printf("\n");

    end = clock();
    double diff = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("time = %f\n", diff);

    return 0;
}
