/* zpipe.c: example of proper use of zlib's inflate() and deflate()
   Not copyrighted -- provided to the public domain
   Version 1.4  11 December 2005  Mark Adler */

/* Version history:
   1.0  30 Oct 2004  First version
   1.1   8 Nov 2004  Add void casting for unused return values
                     Use switch statement for inflate() return values
   1.2   9 Nov 2004  Add assertions to document zlib guarantees
   1.3   6 Apr 2005  Remove incorrect assertion in inf()
   1.4  11 Dec 2005  Add hack to avoid MSDOS end-of-line conversions
                     Avoid some compiler warnings for input and output buffers
 */

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
#include <libpmemobj.h>
#include <libpmem.h>
//#include "layout.h"

POBJ_LAYOUT_BEGIN(test);
POBJ_LAYOUT_ROOT(test, struct myroot);
//POBJ_LAYOUT_TOID(test, struct z_stream_s);
//POBJ_LAYOUT_TOID(test, struct internal_state);
POBJ_LAYOUT_TOID(test, struct InOutbuffer);
POBJ_LAYOUT_END(test);


#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define CHUNK 16384

/*
 * pmem struct
 */
struct myroot
{
    //TOID(struct z_stream_s) z_stream_s;
    //TOID(struct internal_state) internal_state;
    TOID(struct InOutbuffer) io;
};

struct InOutbuffer
{   
    char in[CHUNK];
    char out[CHUNK];
    unsigned have;
};


static void
do_copy_to_pmem(char *pmemaddr, FILE *dest, off_t len)
{
	char buf[CHUNK];
	int cc;

	/* copy the file, saving the last flush step to the end */
	while ((cc = fread(buf, CHUNK, 1, dest)) > 0) {
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
do_copy_to_non_pmem(char *addr, FILE *dest, off_t len)
{
	char *startaddr = addr;
	char buf[CHUNK];
	int cc;

	/* copy the file, saving the last flush step to the end */
	while ((cc = fread(buf, CHUNK, 1, dest)) > 0) {
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
int def(PMEMobjpool *pop, char *pmemfile, FILE *source, FILE *dest, int level)
{
    int ret, flush;
    //unsigned have;
    z_stream strm;
    //struct InOutbuffer *io;
    //unsigned char in[CHUNK];
    //unsigned char out[CHUNK];
    
    /* pmem file parameters */
    int fd;
    char *pmemaddr;
    int is_pmem;
    size_t mapped_len;
    int cc;
   
    /* allocate deflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    //pmemobj_persist(pop, &strm, sizeof(strm));

    ret = deflateInit(&strm, level);
    if (ret != Z_OK)
        return ret;
    
    /* set the root */
    TOID(struct myroot) root = POBJ_ROOT(pop, struct myroot);
    TX_BEGIN(pop)
    {
        TX_ADD(root);
        TOID(struct InOutbuffer) io = TX_NEW(struct InOutbuffer);
        /* compress until end of file */
        do 
        {
            strm.avail_in = fread(D_RW(io)->in, 1, CHUNK, source);
            if (ferror(source)) 
            {
                (void)deflateEnd(&strm);
                return Z_ERRNO;
            }
            flush = feof(source) ? Z_FINISH : Z_NO_FLUSH;
            strm.next_in = D_RO(io)->in;

            /* run deflate() on input until output buffer not full, finish
            compression if all of source has been read in */
        do 
        {
            strm.avail_out = CHUNK;
            strm.next_out = D_RO(io)->out;
            ret = deflate(&strm, flush);    /* no bad return value */
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            D_RW(io)->have = CHUNK - strm.avail_out;
            if (fwrite(D_RO(io)->out, 1, D_RO(io)->have, dest) != D_RO(io)->have || ferror(dest)) 
            {
                (void)deflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);
        assert(strm.avail_in == 0);     /* all input will be used */

        /* done when last data in file processed */
        } while (flush != Z_FINISH);
        assert(ret == Z_STREAM_END);        /* stream will be complete */

        /* clean up and return */
        (void)deflateEnd(&strm);
    } TX_END
    
    TX_BEGIN(pop)
    {
        TX_ADD(root);
        TX_FREE(D_RW(root)->io);
        D_RW(root)->io = TOID_NULL(struct InOutbuffer);
    } TX_END

 
    fseek(dest, SEEK_SET, 0);
    fd = fileno(dest);
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
		do_copy_to_pmem(pmemaddr, dest, buf.st_size);
	else
		do_copy_to_non_pmem(pmemaddr, dest, buf.st_size);




    close(fd);
    pmem_unmap(pmemaddr, mapped_len);
    pmemobj_close(pop);
    return Z_OK;
    
}

/* Decompress from file source to file dest until stream ends or EOF.
   inf() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_DATA_ERROR if the deflate data is
   invalid or incomplete, Z_VERSION_ERROR if the version of zlib.h and
   the version of the library linked do not match, or Z_ERRNO if there
   is an error reading or writing the files. */
int inf(FILE *source, FILE *dest)
{
    int ret;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];

    /* allocate inflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;
    ret = inflateInit(&strm);
    if (ret != Z_OK)
        return ret;

    /* decompress until deflate stream ends or end of file */
    do {
        strm.avail_in = fread(in, 1, CHUNK, source);
        if (ferror(source)) {
            (void)inflateEnd(&strm);
            return Z_ERRNO;
        }
        if (strm.avail_in == 0)
            break;
        strm.next_in = in;

        /* run inflate() on input until output buffer not full */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = inflate(&strm, Z_NO_FLUSH);
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            switch (ret) {
            case Z_NEED_DICT:
                ret = Z_DATA_ERROR;     /* and fall through */
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                (void)inflateEnd(&strm);
                return ret;
            }
            have = CHUNK - strm.avail_out;
            if (fwrite(out, 1, have, dest) != have || ferror(dest)) {
                (void)inflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);

        /* done when inflate() says it's done */
    } while (ret != Z_STREAM_END);

    /* clean up and return */
    (void)inflateEnd(&strm);
    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

/* report a zlib or i/o error */
void zerr(int ret)
{
    fputs("zpipe: ", stderr);
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
    time_t start, end;
    start = clock();
    int ret;
    
    FILE *fpIn = fopen(argv[1], "rb+");
    FILE *fpOut = fopen(argv[2], "wb+");
    char *pmemfile = argv[3];
    /* avoid end-of-line conversions */
    //SET_BINARY_MODE(stdin);
    //SET_BINARY_MODE(stdout);

    PMEMobjpool *pop = pmemobj_create(argv[4], POBJ_LAYOUT_NAME(test), PMEMOBJ_MIN_POOL, 0666);
    if (pop == NULL)
    {
        perror("pmemobj_create");
        return 1;
    }
    
    /* do compression if arguments = 3 */
    if (argc == 5 && pop != NULL)
    {
        ret = def(pop, pmemfile, fpIn, fpOut, Z_DEFAULT_COMPRESSION);
        if (ret != Z_OK)
            zerr(ret);
        return ret;

    }

    /* do decompression if -d specified */
    else if (argc == 2 && strcmp(argv[1], "-d") == 0) {
        ret = inf(stdin, stdout);
        if (ret != Z_OK)
            zerr(ret);
        return ret;
    }

    /* otherwise, report usage */
    else {
        fputs("pdeflate usage: pdeflate [-d] srcfile outfile pmemfile mempool\n", stderr);
        return 1;
    }
}
