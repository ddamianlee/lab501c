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

POBJ_LAYOUT_BEGIN(pmem_deflate);
POBJ_LAYOUT_ROOT(pmem_deflate, struct myroot);
POBJ_LAYOUT_TOID(pmem_deflate, struct z_stream_s);
POBJ_LAYOUT_TOID(pmem_deflate, struct internal_state);
POBJ_LAYOUT_TOID(pmem_deflate, struct InOutbuffer);
POBJ_LAYOUT_TOID(pmem_deflate, struct static_tree_desc_s);
POBJ_LAYOUT_TOID(pmem_deflate, struct tree_desc_s);
POBJ_LAYOUT_TOID(pmem_deflate, struct ct_data_s);
POBJ_LAYOUT_END(pmem_deflate);


#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define CHUNK 2097152

/*
 * pmem struct
 */
struct myroot
{
    TOID(struct z_stream_s) z_stream_s;
    TOID(struct internal_state) internal_state;
    TOID(struct InOutbuffer) io;
    TOID(struct static_tree_desc_s) static_tree_desc;

};

struct InOutbuffer
{   
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];
    unsigned have;
};


static void
do_copy_to_pmem(char *pmemaddr, FILE *fp, off_t len)
{
	char buf[CHUNK];
	int cc;

	/* copy the file, saving the last flush step to the end */
	while ((cc = fread(buf, CHUNK, 1, fp)) > 0) {
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
	while ((cc = fread(buf, CHUNK, 1, fp)) > 0) {
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
    z_stream strm;
    int fd;                     /* temporary file discriptor */
    FILE *fp;                   /* temporary file pointer */
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
    printf("is_pmem = %d\n", is_pmem);
    maplen = mapped_len;

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
            int input_len = 0;          /* input file length counter */
            if(maplen > 0)
            {
                if(maplen < CHUNK)
                {
                    strncpy(D_RW(io)->in, src_pmemaddr, maplen);
                    input_len = maplen;
                    maplen = 0;
                }
                else
                {
                    strncpy(D_RW(io)->in, src_pmemaddr, CHUNK);
                    maplen -= CHUNK;
                    input_len = CHUNK;
                    src_pmemaddr += CHUNK;
                }    
            }
            strm.avail_in = input_len;
            flush = (strm.avail_in == 0) ? Z_FINISH : Z_NO_FLUSH;
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
                if (fwrite(D_RO(io)->out, 1, D_RO(io)->have, fp) != D_RO(io)->have) 
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
    if((pmemaddr = pmem_map_file(pmemfile, buf.st_size, PMEM_FILE_CREATE|PMEM_FILE_EXCL, 0666, &mapped_len, &is_pmem)) == NULL)
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
    pmemobj_close(pop);
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
    z_stream strm;
    int fd;
    FILE *fp;
    fp = tmpfile();
    char *pmemaddr;
    int is_pmem;
    size_t mapped_len;
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
    /* set the root */
    TOID(struct myroot) root = POBJ_ROOT(pop, struct myroot);
    TX_BEGIN(pop)
    {
        TX_ADD(root);
        TOID(struct InOutbuffer) io = TX_NEW(struct InOutbuffer);
        do {
        //strm.avail_in = read(srcfd, D_RW(io)->in, CHUNK);
        if (strm.avail_in == 1) {
            (void)inflateEnd(&strm);
            return Z_ERRNO;
        }
        if (strm.avail_in == 0)
            break;
        strm.next_in = D_RW(io)->in;

        /* run inflate() on input until output buffer not full */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = D_RO(io)->out;
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
            D_RW(io)->have = CHUNK - strm.avail_out;
            if (fwrite(D_RO(io)->out, 1, D_RO(io)->have, fp) != D_RO(io)->have) {
                (void)inflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);

        /* done when inflate() says it's done */
    } while (ret != Z_STREAM_END);
    } TX_END
    
    TX_BEGIN(pop)
    {
        TX_ADD(root);
        TX_FREE(D_RW(root)->io);
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
    /* clean up and return */
    (void)inflateEnd(&strm);
    //close(srcfd);
    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

/* report a zlib or i/o error */
void zerr(int ret)
{
    fputs("pdeflate: ", stderr);
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
    char *outfile = argv[2];    /* output file path */   

    /* create a memory pool */
    PMEMobjpool *pop = pmemobj_create(argv[3], POBJ_LAYOUT_NAME(pmem_deflate), 3221225472, 0666);
    if (pop == NULL)
    {
        perror("pmemobj_create");
        return 1;
    }
    
    /* do compression if arguments = 4 */
    if (argc == 4 && pop != NULL)
    {
        ret = def(pop, argv[1], outfile, Z_DEFAULT_COMPRESSION);
        if (ret != Z_OK)
            zerr(ret);
        return ret;

    }

    /* do decompression if -d specified */
    else if (argc == 5 && strcmp(argv[1], "-d") == 0) {
        ret = inf(pop, argv[1], outfile);
        if (ret != Z_OK)
            zerr(ret);
        return ret;
    }

    /* otherwise, report usage */
    else {
        fputs("pdeflate usage: pdeflate [-d] srcfile outfile mempool\n", stderr);
        return 1;
    }
}
