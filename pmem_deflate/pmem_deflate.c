#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include "zlib.h"
#include "deflate.h"
#include <libpmemobj.h>
#include <libpmem.h>

//#include "layout.h"




#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define CHUNK 2097152



struct InOutbuffer
{   
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];
    unsigned have;
};


static void
do_copy_to_pmem(char *pmemaddr, int fd, off_t len)
{
	char buf[CHUNK];
	int cc;

	/* copy the file, saving the last flush step to the end */
	while ((cc = read(fd, buf, CHUNK) > 0)) {
		//for(int i = 0; i < 100; i++)
            //printf("%c", buf[i]);
        pmem_memcpy_nodrain(pmemaddr, buf, cc);
		pmemaddr += cc;
	}

	if (cc < 0) {
		perror("read");
		exit(1);
	}

	/* perform final flush step */
	pmem_drain();
    printf("flush to pmem!\n");
}

/*
 * do_copy_to_non_pmem -- copy to a non-pmem memory mapped file
 */
static void
do_copy_to_non_pmem(char *addr, int fd, off_t len)
{
	char *startaddr = addr;
	char buf[CHUNK];
	int cc;

	/* copy the file, saving the last flush step to the end */
	while ((cc = read(fd, buf, CHUNK)) > 0) {
		//for(int i = 0; i < 100; i++)
            //printf("%c", buf[i]);
        memcpy(addr, buf, cc);
		addr += cc;
	}
	if (cc == -1) {
		perror("read");
		exit(1);
	}

	/* flush it */
	if (pmem_msync(startaddr, len) < 0) {
		perror("pmem_msync");
		exit(1);
	}
    printf("flush to non pmem!\n");
}


/* Compress from file source to file dest until EOF on source.
   def() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_STREAM_ERROR if an invalid compression
   level is supplied, Z_VERSION_ERROR if the version of zlib.h and the
   version of the library linked do not match, or Z_ERRNO if there is
   an error reading or writing the files. */
int def(PMEMobjpool *pop, char *src, char *pmemfile, int level)
{
    int ret, flush;

    int fd;  
    // if((fd = open("/tmp", O_TMPFILE | O_RDWR | O_EXCL , S_IWUSR | S_IRUSR)) < 0)
    //     perror("open error"); 
    if((fd = open(pmemfile, O_CREAT | O_RDWR | O_EXCL , S_IRWXU | S_IWUSR | S_IRUSR)) < 0)
        perror("open error");                    
    //FILE *fp;                   /* temporary file pointer */
    //fp = tmpfile();
    
    /* pmem file parameters */
    char *src_pmemaddr;         /* src file pointer */
    //char *out_pmemaddr;             /* output file pointer */
    int is_pmem;                /* pmem_map_file arguments */
    size_t mapped_len_src;
    //size_t mapped_len_out;
    int i;
    size_t maplen;
    
    /* map the src pmem file */
    if((src_pmemaddr = pmem_map_file(src, 0, PMEM_FILE_EXCL, 0666, &mapped_len_src, &is_pmem)) == NULL)
    {
        perror("pmem_map_file");
        exit(1);
    }
    printf("is source file mapped in pmem? = %d\n", is_pmem);
    maplen = mapped_len_src;

    /* set the root */
    TOID(struct myroot) root = POBJ_ROOT(pop, struct myroot);
    TOID(struct z_stream) strm = D_RO(root)->strm;
    POBJ_ALLOC(pop, &strm, struct z_stream, sizeof(struct z_stream), NULL, NULL);
    pmemobj_persist(pop, D_RW(strm), sizeof(*D_RW(strm)));
    // TX_BEGIN(pop)
    // {
    //     strm = TX_NEW(struct z_stream);

    // } TX_END
    
    ret = deflateInit(pop, strm, level);
    if (ret != Z_OK)
        return ret;
    TOID(struct InOutbuffer) io = D_RO(root)->io;
    POBJ_ALLOC(pop, &io, struct InOutbuffer, sizeof(struct InOutbuffer), NULL, NULL);
    pmemobj_persist(pop, D_RW(io), sizeof(*D_RW(io)));
    /* compress until end of file */
        do 
        {
            int input_len = 0;          /* input file length counter */
            if(maplen > 0)
            {
                if(maplen < CHUNK)
                {
                    pmemobj_memcpy_persist(pop, D_RW(io)->in, src_pmemaddr, maplen);
                    input_len = maplen;
                    maplen = 0;
                }
                else
                {
                    pmemobj_memcpy_persist(pop, D_RW(io)->in, src_pmemaddr, CHUNK);
                    maplen -= CHUNK;
                    input_len = CHUNK;
                    src_pmemaddr += CHUNK;
                }    
            }
            D_RW(strm)->avail_in = input_len;
            flush = (D_RO(strm)->avail_in == 0) ? Z_FINISH : Z_NO_FLUSH;
            D_RW(strm)->next_in = D_RW(io)->in;

            /* run deflate() on input until output buffer not full, finish
            compression if all of source has been read in */
            do 
            {
                D_RW(strm)->avail_out = CHUNK;
                D_RW(strm)->next_out = D_RW(io)->out;
                ret = deflate(pop, strm, flush);    /* no bad return value */
                assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
                D_RW(io)->have = CHUNK - D_RO(strm)->avail_out;
                if ((write(fd, D_RO(io)->out, D_RO(io)->have)) < 0) 
                {
                    (void)deflateEnd(strm);
                    return Z_ERRNO;
                }
                
            } while (D_RO(strm)->avail_out == 0);
            assert(D_RO(strm)->avail_in == 0);     /* all input will be used */

        /* done when last data in file processed */
        } while (flush != Z_FINISH);
        assert(ret == Z_STREAM_END);        /* stream will be complete */

        /* clean up and return */
        (void)deflateEnd(strm);  
    
    
    // TX_BEGIN(pop)
    // {
    //     TX_ADD(root);
    //     TOID(struct InOutbuffer) io = TX_NEW(struct InOutbuffer); 
        
    // } TX_END
    
    // TX_BEGIN(pop)
    // {
    //     TX_ADD(root);
    //     TX_FREE(D_RW(root)->io);
    //     TX_FREE(D_RW(root)->strm);
    //     D_RW(root)->io = TOID_NULL(struct InOutbuffer);
    // } TX_END

    /* turn the temp file to file discriptor and get the size */
    // fseek(fp, SEEK_SET, 0);
    // fd = fileno(fp);
    // struct stat buf;
    // if(fstat(fd, &buf) < 0)
    // {
    //     perror("fstat");
    //     exit(1);
    // }    
    
    // /* map the output file to pmem */
    // if((out_pmemaddr = pmem_map_file(pmemfile, buf.st_size, PMEM_FILE_CREATE|PMEM_FILE_EXCL, 0666, &mapped_len_out, &is_pmem)) == NULL)
    // {
    //     perror("pmem_map_file");
    //     exit(1);
    // }
    // printf("is_pmem = %d\n", is_pmem);  /* check is_pmem is 0 or 1 */

    // /* determine if range is true pmem, call appropriate copy routine */
	// if (is_pmem)
	//     do_copy_to_pmem(out_pmemaddr, fd, buf.st_size);
	// else
	//  	do_copy_to_non_pmem(out_pmemaddr, fd, buf.st_size);
    POBJ_FREE(&io);
    POBJ_FREE(&strm);
    pmemobj_close(pop);
    if(close(fd) < 0)
    {
        perror("close");
        exit(1);
    }
    pmem_unmap(src_pmemaddr, mapped_len_src);
    //pmem_unmap(out_pmemaddr, mapped_len_out);
    //fclose(fp);
    
    
    return Z_OK;
    
}

/* Decompress from file source to file dest until stream ends or EOF.
   inf() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_DATA_ERROR if the deflate data is
   invalid or incomplete, Z_VERSION_ERROR if the version of zlib.h and
   the version of the library linked do not match, or Z_ERRNO if there
   is an error reading or writing the files. */
int inf(PMEMobjpool *pop, char *src, char *pmemfile)
{
    int ret;
    //TOID(struct z_stream) strm;
    int fd;
    FILE *fp;
    fp = tmpfile();
    /* pmem file parameters */
    char *src_pmemaddr;         /* src file pointer */
    char *pmemaddr;             /* output file pointer */
    int is_pmem;                /* pmem_map_file arguments */
    size_t mapped_len;
    int i;
    size_t maplen;
 
     /* map the src pmem file */
    if((src_pmemaddr = pmem_map_file(src, 0, PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
    {
        perror("pmem_map_file");
        exit(1);
    }
    printf("is source file pmem-file? = %d\n", is_pmem);
    maplen = mapped_len;

    
    
    /* set the root */
    TOID(struct myroot) root = POBJ_ROOT(pop, struct myroot);
    TOID(struct z_stream) strm = D_RW(root)->strm;
    

    TX_BEGIN(pop)
    {
        strm = TX_NEW(struct z_stream);
        /* allocate deflate state */
        // D_RW(strm)->zalloc = Z_NULL;
        // D_RW(strm)->zfree = Z_NULL;
        // D_RW(strm)->opaque = Z_NULL;
        D_RW(strm)->avail_in = 0;
        D_RW(strm)->next_in = Z_NULL;    
    } TX_END

    ret = inflateInit(pop, strm);
    if (ret != Z_OK)
        return ret;



    /* decompress until deflate stream ends or end of file */
    /* set the root */
    //TOID(struct myroot) root = POBJ_ROOT(pop, struct myroot);
    TX_BEGIN(pop)
    {
        TX_ADD(root);
        TOID(struct InOutbuffer) io = TX_NEW(struct InOutbuffer);
        do {
            int input_len = 0;          /* input file length counter */
            if(maplen > 0)
            {
                if(maplen < CHUNK)
                {
                    TX_MEMCPY(D_RW(io)->in, src_pmemaddr, maplen);
                    input_len = maplen;
                    maplen = 0;
                }
                else
                {
                    TX_MEMCPY(D_RW(io)->in, src_pmemaddr, CHUNK);
                    maplen -= CHUNK;
                    input_len = CHUNK;
                    src_pmemaddr += CHUNK;
                }    
            }
            D_RW(strm)->avail_in = input_len;
            D_RW(strm)->next_in = D_RW(io)->in;
        /* run inflate() on input until output buffer not full */
        do {
            D_RW(strm)->avail_out = CHUNK;
            D_RW(strm)->next_out = D_RW(io)->out;
            ret = inflate(pop, strm, Z_NO_FLUSH);
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            switch (ret) {
            case Z_NEED_DICT:
                ret = Z_DATA_ERROR;     /* and fall through */
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                (void)inflateEnd(strm);
                return ret;
            }
            D_RW(io)->have = CHUNK - D_RO(strm)->avail_out;
            if (fwrite(D_RO(io)->out, 1, D_RO(io)->have, fp) != D_RO(io)->have) 
                {
                    (void)inflateEnd(strm);
                    return Z_ERRNO;
                }
            } while (D_RO(strm)->avail_out == 0);

        /* done when inflate() says it's done */
        } while (ret != Z_STREAM_END);
        /* clean up and return */
        (void)inflateEnd(strm);
    } TX_END
    
    TX_BEGIN(pop)
    {
        TX_ADD(root);
        TX_FREE(D_RW(root)->io);
        TX_FREE(D_RW(root)->strm);
        D_RW(root)->io = TOID_NULL(struct InOutbuffer);
    } TX_END
    
     /* turn the temp file to file discriptor and get the size */   
    fseek(fp, SEEK_SET, 0);
    fd = fileno(fp);
    struct stat buf;
    if(fstat(fd, &buf) < 0)
    {
        perror("fstat");
        exit(1);
    }    
    if((pmemaddr = pmem_map_file(pmemfile, buf.st_size, PMEM_FILE_CREATE|PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
    {
        perror("pmem_map_file");
        exit(1);
    }
    
    /* determine if range is true pmem, call appropriate copy routine */
	if (is_pmem)
	    do_copy_to_pmem(pmemaddr, fp, buf.st_size);
	else
	 	do_copy_to_non_pmem(pmemaddr, fp, buf.st_size);

    pmem_unmap(pmemaddr, mapped_len);
    fclose(fp);
    pmemobj_close(pop);
    //close(srcfd);
    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

/* report a zlib or i/o error */
void zerr(int ret)
{
    fputs("odeflate: ", stderr);
    switch (ret) {
    case Z_ERRNO:
        if (ferror(stdin))
            fputs("error reading stdin\n", stderr);
        if (ferror(stdout))
            fputs("error writing stdout\n", stderr);
        break;
    case Z_STREAM_ERROR:
        fputs("invalid compression level\n", stderr);
        break;
    case Z_DATA_ERROR:
        fputs("invalid or incomplete deflate data\n", stderr);
        break;
    case Z_MEM_ERROR:
        fputs("out of memory\n", stderr);
        break;
    case Z_VERSION_ERROR:
        fputs("zlib version mismatch!\n", stderr);
    }
}

/* compress or decompress */
int main(int argc, char **argv)
{
    // time_t start, end;
    // start = clock();
    int ret;
    int level;
    /* do compression if arguments = 5 */
    if (argc == 5)
    {
        /* create or open a memory pool */
        PMEMobjpool *pop;
        if((pop = pmemobj_create(argv[3], POBJ_LAYOUT_NAME(pmem_deflate), 104857600, 0666)) == NULL)
        {
            perror("pmemobj_create");
            return 1;
        }
        char *outfile = argv[2];    /* output file path */
        level = atoi(argv[4]);
        printf("level = %d\n", level);
        ret = def(pop, argv[1], outfile, level);
        if (ret != Z_OK)
            zerr(ret);
        return ret;

    }

    /* do decompression if -d specified */
    else if (argc == 5 && strcmp(argv[1], "-d") == 0) 
    {
        /* create or open a memory pool */
        PMEMobjpool *pop;
        if((pop = pmemobj_create(argv[4], POBJ_LAYOUT_NAME(pmem_deflate), 104857600, 0666)) == NULL)
        {
            perror("pmemobj_create");
            return 1;
        }
        char *outfile = argv[3];    /* output file path */   
        ret = inf(pop, argv[2], outfile);
        if (ret != Z_OK)
            zerr(ret);
        return ret;
        return 0;
    }

    /* otherwise, report usage */
    else {
        fputs("pdeflate usage: pdeflate [-d] srcfile outfile mempool\n", stderr);
        return 1;
    }
}
