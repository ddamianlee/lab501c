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
#include "zlib.h"
#include <time.h>
#include <libpmem.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define CHUNK 16384

static void
do_copy_to_pmem(char *pmemaddr, FILE *fp, off_t len)
{
	char buf[CHUNK];
	int cc;

	/* copy the file, saving the last flush step to the end */
	while ((cc = fread(buf, 1, CHUNK, fp)) > 0) {
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
do_copy_to_non_pmem(char *addr, FILE *fp, off_t len)
{
	char *startaddr = addr;
	char buf[CHUNK];
	int cc;

	/* copy the file, saving the last flush step to the end */
	while ((cc = fread(buf, 1, CHUNK, fp)) > 0) {
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
int def(char *source, char *dest, size_t mapped_len, int level)
{
    int ret, flush;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];

    /* pmem file parameters */
    char *src_pmemaddr;         /* src file pointer */
    char *pmemaddr;             /* output file pointer */
    int is_pmem;                /* pmem_map_file arguments */
    //size_t mapped_len;
    int i;
    size_t maplen;

    int fd;                     /* temporary file discriptor */
    FILE *fp;                   /* temporary file pointer */
    fp = tmpfile();

    /* map the src pmem file */
    // if((src_pmemaddr = pmem_map_file(source, 0, PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
    // {
    //     perror("pmem_map_file");
    //     exit(1);
    // }
    maplen = mapped_len;
    printf("is source file pmem-file? = %d\n", is_pmem);

    /* allocate deflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    ret = deflateInit(&strm, level);
    if (ret != Z_OK)
        return ret;

    /* compress until end of file */
    do {
            int input_len = 0;          /* input file length counter */
            if(maplen > 0)
            {
                if(maplen < CHUNK)
                {
                    memcpy(in, source, maplen);
                    input_len = maplen;
                    maplen = 0;
                }
                else
                {
                    memcpy(in, source, CHUNK);
                    maplen -= CHUNK;
                    input_len = CHUNK;
                    source += CHUNK;
                }    
            }
            strm.avail_in = input_len;
            flush = strm.avail_in == 0 ? Z_FINISH : Z_NO_FLUSH;
            strm.next_in = in;

        /* run deflate() on input until output buffer not full, finish
           compression if all of source has been read in */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = deflate(&strm, flush);    /* no bad return value */
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            have = CHUNK - strm.avail_out;
            if (fwrite(out, 1, have, fp) != have || ferror(fp)) {
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

    /* turn the temp file to file discriptor and get the size */
    fseek(fp, SEEK_SET, 0);
    fd = fileno(fp);
    struct stat buf;
    if(fstat(fd, &buf) < 0)
    {
        perror("fstat");
        exit(1);
    }    
    
    /* map the output file to pmem */
    if((pmemaddr = pmem_map_file(dest, buf.st_size, PMEM_FILE_CREATE|PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
    {
        perror("pmem_map_file");
        exit(1);
    }
    printf("is_pmem = %d\n", is_pmem);  /* check is_pmem is 0 or 1 */

    /* determine if range is true pmem, call appropriate copy routine */
	if (is_pmem)
	    do_copy_to_pmem(pmemaddr, fp, buf.st_size);
	else
	 	do_copy_to_non_pmem(pmemaddr, fp, buf.st_size);
    
    pmem_unmap(pmemaddr, mapped_len);
    fclose(fp);
    return Z_OK;
}

/* Decompress from file source to file dest until stream ends or EOF.
   inf() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_DATA_ERROR if the deflate data is
   invalid or incomplete, Z_VERSION_ERROR if the version of zlib.h and
   the version of the library linked do not match, or Z_ERRNO if there
   is an error reading or writing the files. */
int inf(char *source, char *dest, size_t mapped_len)
{
    int ret;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];

    int fd;
    FILE *fp;
    fp = tmpfile();
    
    /* pmem file parameters */
    char *src_pmemaddr;         /* src file pointer */
    char *pmemaddr;             /* output file pointer */
    int is_pmem;                /* pmem_map_file arguments */
    //size_t mapped_len;
    int i;
    size_t maplen;

    /* map the src pmem file */
    // if((src_pmemaddr = pmem_map_file(source, 0, PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
    // {
    //     perror("pmem_map_file");
    //     exit(1);
    // }
    printf("is source file pmem-file? = %d\n", is_pmem);
    maplen = mapped_len;

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
        int input_len = 0;          /* input file length counter */
        if(maplen > 0)
        {
            if(maplen < CHUNK)
            {
                memcpy(in, source, maplen);
                input_len = maplen;
                maplen = 0;
            }
            else
            {
                memcpy(in, source, CHUNK);
                maplen -= CHUNK;
                input_len = CHUNK;
                source += CHUNK;
            }    
        }
        strm.avail_in = input_len;
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
            if (fwrite(out, 1, have, fp) != have || ferror(fp)) {
                (void)inflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);

        /* done when inflate() says it's done */
    } while (ret != Z_STREAM_END);

    /* clean up and return */
    (void)inflateEnd(&strm);

    /* turn the temp file to file discriptor and get the size */   
    fseek(fp, SEEK_SET, 0);
    fd = fileno(fp);
    struct stat buf;
    if(fstat(fd, &buf) < 0)
    {
        perror("fstat");
        exit(1);
    }    
    if((pmemaddr = pmem_map_file(dest, buf.st_size, PMEM_FILE_CREATE|PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
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

/* compress or decompress from stdin to stdout */
int main(int argc, char **argv)
{
    int ret;
    int srcfd;
    char *startaddr;
    struct stat stbuf;
    char *srcpmemaddr;
    int is_pmem;
    char buf[4096];
    int cc;
    size_t mapped_len;

    /* do compression if arguments = 4 */
    if (argc == 4)
    {
        if((srcfd = open(argv[1], O_RDONLY)) < 0)
        {
            perror(argv[1]);
            exit(1);
        }
        if(fstat(srcfd, &stbuf) < 0)
        {
            perror("fstat");
            exit(1);
        }
        if((srcpmemaddr = pmem_map_file(argv[2], stbuf.st_size, PMEM_FILE_CREATE|PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
        {
            perror("pmem_map_file");
            exit(1);
        }
        if(is_pmem)
        {
            startaddr = srcpmemaddr;
            while((cc = read(srcfd, buf, 4096)) > 0)
            {
                pmem_memcpy_nodrain(srcpmemaddr, buf, cc);
                srcpmemaddr += cc;
            }
            if (cc < 0)
            {
                perror("read");
                exit(1);
            }
            pmem_drain();
        }
        else
        {
            startaddr = srcpmemaddr;
            while((cc = read(srcfd, buf, 4096)) > 0)
            {
                memcpy(srcpmemaddr, buf, cc);
                srcpmemaddr += cc;
            }
            if (cc < 0)
            {
                perror("read");
                exit(1);
            }
            if(pmem_msync(startaddr, stbuf.st_size) < 0)
            {
                perror("pmem_msync");
                exit(1);
            }

        }
        ret = def(startaddr, argv[3], mapped_len, Z_DEFAULT_COMPRESSION);
        if (ret != Z_OK)
            zerr(ret);
        pmem_unmap(srcpmemaddr, mapped_len);
        return ret;

    }

    /* do decompression if -d specified */
    else if (argc == 5 && strcmp(argv[1], "-d") == 0) 
    {
        if((srcfd = open(argv[2], O_RDONLY)) < 0)
        {
            perror(argv[1]);
            exit(1);
        }
        if(fstat(srcfd, &stbuf) < 0)
        {
            perror("fstat");
            exit(1);
        }
        if((srcpmemaddr = pmem_map_file(argv[3], stbuf.st_size, PMEM_FILE_CREATE|PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
        {
            perror("pmem_map_file");
            exit(1);
        }
        if(is_pmem)
        {
            startaddr = srcpmemaddr;
            while((cc = read(srcfd, buf, 4096)) > 0)
            {
                pmem_memcpy_nodrain(srcpmemaddr, buf, cc);
                srcpmemaddr += cc;
            }
            if (cc < 0)
            {
                perror("read");
                exit(1);
            }
            pmem_drain();
        }
        else
        {
            startaddr = srcpmemaddr;
            while((cc = read(srcfd, buf, 4096)) > 0)
            {
                memcpy(srcpmemaddr, buf, cc);
                srcpmemaddr += cc;
            }
            if (cc < 0)
            {
                perror("read");
                exit(1);
            }
            if(pmem_msync(startaddr, stbuf.st_size) < 0)
            {
                perror("pmem_msync");
                exit(1);
            }

        }
        ret = inf(startaddr, argv[4], mapped_len);
        if (ret != Z_OK)
            zerr(ret);
        pmem_unmap(srcpmemaddr, mapped_len);
        return ret;
    }

    /* otherwise, report usage */
    else {
        fputs("zpipev2 usage: zpipe [-d] srcfile outfile \n", stderr);
        return 1;
    }
}
