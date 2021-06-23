#include "deflate.h"
#include <stdio.h>
//#include <libpmemobj.h>
//#include "zlib.h"

/* ===========================================================================
 * Local data
 */

#define NIL 0
/* Tail of hash chains */

#ifndef TOO_FAR
#  define TOO_FAR 4096
#endif
/* Matches of length 3 are discarded if their distance exceeds TOO_FAR */


/* ===========================================================================
 *  Function prototypes.
 */
typedef enum {
    need_more,      /* block not completed, need more input or more output */
    block_done,     /* block flush performed */
    finish_started, /* finish started, need only more output at next deflate */
    finish_done     /* finish done, accept no more input or output */
} block_state;

typedef block_state (*compress_func) OF((PMEMobjpool *pop, deflate_state *s, int flush));

/* Compression function. Returns the block state after the call. */
local int deflateStateCheck      OF((z_streamp strm));
local void flush_pending         OF((PMEMobjpool *pop, z_streamp strm));
local block_state deflate_stored OF((PMEMobjpool *pop, deflate_state *s, int flush));
local block_state deflate_fast   OF((PMEMobjpool *pop, deflate_state *s, int flush));
local block_state deflate_slow   OF((PMEMobjpool *pop, deflate_state *s, int flush));
local void lm_init               OF((PMEMobjpool *pop, deflate_state *s));
local void putShortMSB           OF((deflate_state *s, uInt b));
local void slide_hash            OF((deflate_state *s));
local void fill_window           OF((PMEMobjpool *pop, deflate_state *s));
local unsigned read_buf          OF((PMEMobjpool *pop, z_streamp strm, Byte *buf, unsigned size));
local uInt longest_match         OF((deflate_state *s, IPos cur_match));
local block_state deflate_rle    OF((PMEMobjpool *pop, deflate_state *s, int flush));
local block_state deflate_huff   OF((PMEMobjpool *pop, deflate_state *s, int flush));

/* Values for max_lazy_match, good_match and max_chain_length, depending on
 * the desired pack level (0..9). The values given below have been tuned to
 * exclude worst case performance for pathological files. Better values may be
 * found for specific files.
 */
typedef struct config_s {
   ush good_length; /* reduce lazy search above this match length */
   ush max_lazy;    /* do not perform lazy search above this match length */
   ush nice_length; /* quit search above this match length */
   ush max_chain;
   compress_func func;
} config;


local const config configuration_table[10] = {
/*      good lazy nice chain */
/* 0 */ {0,    0,  0,    0, deflate_stored},  /* store only */
/* 1 */ {4,    4,  8,    4, deflate_fast}, /* max speed, no lazy matches */
/* 2 */ {4,    5, 16,    8, deflate_fast},
/* 3 */ {4,    6, 32,   32, deflate_fast},

/* 4 */ {4,    4, 16,   16, deflate_slow},  /* lazy matches */
/* 5 */ {8,   16, 32,   32, deflate_slow},
/* 6 */ {8,   16, 128, 128, deflate_slow},
/* 7 */ {8,   32, 128, 256, deflate_slow},
/* 8 */ {32, 128, 258, 1024, deflate_slow},
/* 9 */ {32, 258, 258, 4096, deflate_slow}}; /* max compression */


/* Note: the deflate() code requires max_lazy >= MIN_MATCH and max_chain >= 4
 * For deflate_fast() (levels <= 3) good is ignored and lazy has a different
 * meaning.
 */

/* rank Z_BLOCK between Z_NO_FLUSH and Z_PARTIAL_FLUSH */
#define RANK(f) (((f) * 2) - ((f) > 4 ? 9 : 0))

/* ===========================================================================
 * Update a hash value with the given input byte
 * IN  assertion: all calls to UPDATE_HASH are made with consecutive input
 *    characters, so that a running hash key can be computed from the previous
 *    key instead of complete recalculation each time.
 */
#define UPDATE_HASH(s,h,c) (h = (((h) << s->hash_shift) ^ (c)) & s->hash_mask)


/* ===========================================================================
 * Insert string str in the dictionary and set match_head to the previous head
 * of the hash chain (the most recent string with same hash key). Return
 * the previous length of the hash chain.
 * If this file is compiled with -DFASTEST, the compression level is forced
 * to 1, and no hash chains are maintained.
 * IN  assertion: all calls to INSERT_STRING are made with consecutive input
 *    characters and the first MIN_MATCH bytes of str are valid (except for
 *    the last MIN_MATCH-1 bytes of the input file).
 */
#define INSERT_STRING(s, str, match_head) \
   (UPDATE_HASH(s, s->ins_h, D_RW(s->window)[(str) + (MIN_MATCH-1)]), \
    match_head = D_RW(s->prev)[(str) & s->w_mask] = D_RO(s->head)[s->ins_h], \
    D_RW(s->head)[s->ins_h] = (Pos)(str))



/* ===========================================================================
 * Initialize the hash table (avoiding 64K overflow for 16 bit systems).
 * prev[] will be initialized on the fly.
 */
// #define CLEAR_HASH(s) \
//     s->head[s->hash_size-1] = NIL; \
//     zmemzero((Bytef *)s->head, (unsigned)(s->hash_size-1)*sizeof(*s->head));

// #define CLEAR_HASH(s) \
//     ws->head[rs->hash_size-1] = NIL; \
//     zmemzero((Bytef *)s->head, (unsigned)(s->hash_size-1)*sizeof(*s->head));
//     pmemobj_memset_persist(pop, (Bytef *)ws->head, 0, (unsigned)(ws->hash_size-1)*sizeof(rs->head));



/* =========================================================================
 * Check for a valid deflate stream state. Return 0 if ok, 1 if not.
 */
local int deflateStateCheck (strm)
    z_streamp strm;
{
    //const struct z_stream *rstrm = D_RO(strm);
    deflate_state *s;
    
    if (strm == Z_NULL ||
        strm->zalloc == (alloc_func)0 || strm->zfree == (free_func)0)
        return 1;
    s = strm->state;
    //const struct deflate_state *rs = D_RO(s);
    // if(TOID_IS_NULL(s))
    //     printf("null");

    if (s == Z_NULL || s->strm != strm || (s->status != INIT_STATE &&
                                           s->status != EXTRA_STATE &&
                                           s->status != NAME_STATE &&
                                           s->status != COMMENT_STATE &&
                                           s->status != HCRC_STATE &&
                                           s->status != BUSY_STATE &&
                                           s->status != FINISH_STATE))
        return 1;
    return 0;
}

/* ========================================================================= */
int ZEXPORT deflateInit_(pop, strm, level, version, stream_size)
    z_streamp strm;
    PMEMobjpool *pop;
    int level;
    const char *version;
    int stream_size;
{
    return deflateInit2_(pop, strm, level, Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL,
                         Z_DEFAULT_STRATEGY, version, stream_size);
    /* To do: ignore strm->next_in if we use it as window */
}
/* ========================================================================= */
int ZEXPORT deflateInit2_(pop, strm, level, method, windowBits, memLevel, strategy,
                  version, stream_size)
    PMEMobjpool *pop;
    z_streamp strm;
    int  level;
    int  method;
    int  windowBits;
    int  memLevel;
    int  strategy;
    const char *version;
    int stream_size;
{
    deflate_state *s;
    int wrap = 1;
    static const char my_version[] = ZLIB_VERSION;

    /* We overlay pending_buf and d_buf+l_buf. This works since the average
     * output size for (length,distance) codes is <= 24 bits.
     */
    //TOID(ush) overlay;

    if (version == Z_NULL || version[0] != my_version[0] ||
        stream_size != sizeof(z_stream)) {
        return Z_VERSION_ERROR;
    }

    // if(TOID_IS_NULL(strm))
    // {
    //     printf("struct z_stream is NULL");
    //     return Z_STREAM_ERROR;
    // } 
    
    // struct z_stream *wstrm = D_RW(strm);
    // const struct z_stream *rstrm = D_RO(strm);

    strm->msg = Z_NULL;
       if (strm->zalloc == (alloc_func)0) {
#ifdef Z_SOLO
        return Z_STREAM_ERROR;
#else
        strm->zalloc = zcalloc;
        strm->opaque = (voidpf)0;
#endif
    }
    if (strm->zfree == (free_func)0)
#ifdef Z_SOLO
        return Z_STREAM_ERROR;
#else
        strm->zfree = zcfree;
#endif

    if (level == Z_DEFAULT_COMPRESSION) level = 6;

    if (windowBits < 0) { /* suppress zlib wrapper */
        wrap = 0;
        windowBits = -windowBits;
    }

    if (memLevel < 1 || memLevel > MAX_MEM_LEVEL || method != Z_DEFLATED ||
        windowBits < 8 || windowBits > 15 || level < 0 || level > 9 ||
        strategy < 0 || strategy > Z_FIXED || (windowBits == 8 && wrap != 1)) {
        return Z_STREAM_ERROR;
    }
    if (windowBits == 8) windowBits = 9;  /* until 256-byte window bug fixed */
    
    
    /* deflate_state allocation */
    // TOID(struct deflate_state) s;
    // if(POBJ_ALLOC(pop, &s, struct deflate_state, sizeof(struct deflate_state), NULL, NULL))
    // {
    //     fprintf(stderr, "deflate_state alloc failed: %s\n", pmemobj_errormsg());
    //     abort();
    // }
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    s = (deflate_state *) ZALLOC(strm, 1, sizeof(deflate_state));

    strm->state = (struct internal_state FAR *)s;
    s->strm = strm;
    s->status = INIT_STATE;     /* to pass state test in deflateReset() */

    s->wrap = wrap;
    s->gzhead = Z_NULL;
    s->w_bits = (uInt)windowBits;
    s->w_size = 1 << s->w_bits;
    s->w_mask = s->w_size - 1;

    s->hash_bits = (uInt)memLevel + 7;
    s->hash_size = 1 << s->hash_bits;
    s->hash_mask = s->hash_size - 1;
    s->hash_shift =  ((s->hash_bits+MIN_MATCH-1)/MIN_MATCH);

    // ws->window = (Bytef *) ZALLOC(rstrm, rs->w_size, 2*sizeof(Byte));
    // ws->prev   = (Posf *)  ZALLOC(rstrm, rs->w_size, sizeof(Pos));
    // ws->head   = (Posf *)  ZALLOC(rstrm, rs->hash_size, sizeof(Pos));
    
    s->high_water = 0;      /* nothing written to s->window yet */
    s->lit_bufsize = 1 << (memLevel + 6); /* 16K elements by default */

    if(sizeof(uInt) > 2)
    {
        if((POBJ_ALLOC(pop, &s->window, Byte, s->w_size * (2*sizeof(Byte)), NULL, NULL)) || 
        (POBJ_ALLOC(pop, &s->prev, ush, s->w_size * sizeof(Pos), NULL, NULL)) || 
        (POBJ_ALLOC(pop, &s->head, ush, s->hash_size * sizeof(Pos), NULL, NULL)))
        {
            fprintf(stderr, "deflate_state alloc failed: %s\n", pmemobj_errormsg());
            abort();
        }
        pmemobj_persist(pop, D_RW(s->window), sizeof(*D_RW(s->window)));
        pmemobj_persist(pop, D_RW(s->prev), sizeof(*D_RW(s->prev)));
        pmemobj_persist(pop, D_RW(s->head), sizeof(*D_RW(s->head)));
        
    }
    else
    {
        if((POBJ_ZALLOC(pop, &s->window, Byte, s->w_size * (2*sizeof(Byte)))) || 
        (POBJ_ZALLOC(pop, &s->prev, ush, s->w_size * sizeof(Pos))) || 
        (POBJ_ZALLOC(pop, &s->head, ush, s->hash_size * sizeof(Pos))))
        {
            fprintf(stderr, "deflate_state alloc failed: %s\n", pmemobj_errormsg());
            abort();
        }
        pmemobj_persist(pop, D_RW(s->window), sizeof(*D_RW(s->window)));
        pmemobj_persist(pop, D_RW(s->prev), sizeof(*D_RW(s->prev)));
        pmemobj_persist(pop, D_RW(s->head), sizeof(*D_RW(s->head)));     
    }
    
    POBJ_ALLOC(pop, &s->overlay, ush, s->lit_bufsize * (sizeof(ush)+2), NULL, NULL);
    pmemobj_persist(pop, D_RW(s->overlay), sizeof(*D_RW(s->overlay)));
    //POBJ_ALLOC(pop, &ws->pending_buf, Byte, rs->lit_bufsize * (sizeof(ush)+2), NULL, NULL);
    //pmemobj_persist(pop, D_RW(ws->pending_buf), sizeof(*(D_RW(ws->pending_buf))));
    //overlay = (ushf *) ZALLOC(rstrm, rs->lit_bufsize, sizeof(ush)+2);
    
    s->pending_buf = (uch *)D_RW(s->overlay);
    s->pending_buf_size = (ulg)s->lit_bufsize * (sizeof(ush)+2L);


    if (D_RO(s->window) == Z_NULL || D_RO(s->prev) == Z_NULL || D_RO(s->head) == Z_NULL ||
          s->pending_buf == Z_NULL) 
    {
        s->status = FINISH_STATE;
        strm->msg = ERR_MSG(Z_MEM_ERROR);
        deflateEnd (strm);
        return Z_MEM_ERROR;
    }
    s->d_buf = D_RW(s->overlay) + s->lit_bufsize/sizeof(ush);
    s->l_buf = s->pending_buf + (1+sizeof(ush))*(s->lit_bufsize);

    s->level = level;
    s->strategy = strategy;
    s->method = (Byte)method;

    // pmemobj_persist(pop, s, sizeof(*ws));
    // pmemobj_persist(pop, strm, sizeof(*wstrm));
 
    return deflateReset(pop, strm);
}
/* ========================================================================= */
int ZEXPORT deflateResetKeep (pop, strm)
    PMEMobjpool *pop;
    z_streamp strm;
{
    deflate_state *s;

    if (deflateStateCheck(strm)) {
        return Z_STREAM_ERROR;
    }
    // struct z_stream *wstrm = D_RW(strm);
    // const struct z_stream *rstrm = D_RO(strm);
    strm->total_in = strm->total_out = 0;
    strm->msg = Z_NULL; /* use zfree if we ever allocate msg dynamically */
    strm->data_type = Z_UNKNOWN;

    s = strm->state;
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    s->pending = 0;
    s->pending_out = s->pending_buf;

    if (s->wrap < 0) {
        s->wrap = -(s->wrap); /* was made negative by deflate(..., Z_FINISH); */
    }
    s->status = s->wrap ? INIT_STATE : BUSY_STATE;
    strm->adler = adler32(0L, Z_NULL, 0);
    s->last_flush = Z_NO_FLUSH;

    _tr_init(pop, s);
    return Z_OK;
}
/* ========================================================================= */
int ZEXPORT deflateReset (pop, strm)
    PMEMobjpool *pop;
    z_streamp strm;
{
    int ret;
    ret = deflateResetKeep(pop, strm);
    if (ret == Z_OK)
        lm_init(pop, strm->state);
    return ret;
}
/* ========================================================================= */
int ZEXPORT deflateEnd (strm)
    z_streamp strm;
{
    int status;
    deflate_state *s = strm->state;
    //struct deflate_state *ws = D_RW(s);
    //const struct deflate_state *rs = D_RO(s);
    if (deflateStateCheck(strm)) return Z_STREAM_ERROR;

    status = strm->state->status;
    /* Deallocate tree space */
    //POBJ_FREE(&ws->dyn_ltree);
    //POBJ_FREE(&ws->l_desc);
    //POBJ_FREE(&ws->dyn_dtree);
    //POBJ_FREE(&ws->d_desc);
    //POBJ_FREE(&ws->bl_tree);
    //POBJ_FREE(&ws->bl_desc);

    /* Deallocate in reverse order of allocations: */
    POBJ_FREE(&s->overlay);
    POBJ_FREE(&s->head);
    POBJ_FREE(&s->prev);
    POBJ_FREE(&s->window);
    //POBJ_FREE(&D_RW(strm)->state);
    ZFREE(strm, strm->state);

    return status == BUSY_STATE ? Z_DATA_ERROR : Z_OK;
}
/* ===========================================================================
 * Initialize the "longest match" routines for a new zlib stream
 */
local void lm_init (pop, s)
    PMEMobjpool *pop;
    deflate_state *s;
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    s->window_size = (ulg)2L*(s->w_size);

    /* CLEAR_HASH(s) */
    D_RW(s->head)[s->hash_size-1] = NIL; 
    pmemobj_memset_persist(pop, (Bytef *)D_RW(s->head), 0, (unsigned)(s->hash_size-1)*sizeof(*D_RO(s->head)));

    /* Set the default configuration parameters:
     */
    s->max_lazy_match   = configuration_table[s->level].max_lazy;
    s->good_match       = configuration_table[s->level].good_length;
    s->nice_match       = configuration_table[s->level].nice_length;
    s->max_chain_length = configuration_table[s->level].max_chain;

    s->strstart = 0;
    s->block_start = 0L;
    s->lookahead = 0;
    s->insert = 0;
    s->match_length = s->prev_length = MIN_MATCH-1;
    s->match_available = 0;
    s->ins_h = 0;
}
/* ===========================================================================
 * Slide the hash table when sliding the window down (could be avoided with 32
 * bit values at the expense of memory usage). We slide even when level == 0 to
 * keep the hash table consistent if we switch back to level > 0 later.
 */
local void slide_hash(s)
    deflate_state *s;
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    unsigned n, m;
    Posf *p;
    uInt wsize = s->w_size;

    n = s->hash_size;
    p = &D_RW(s->head)[n];
    do {
        m = *--p;
        *p = (Pos)(m >= wsize ? m - wsize : NIL);
    } while (--n);
    n = wsize;
#ifndef FASTEST
    p = &D_RW(s->prev)[n];
    do {
        m = *--p;
        *p = (Pos)(m >= wsize ? m - wsize : NIL);
        /* If n is not on any hash chain, prev[n] is garbage but
         * its value will never be used.
         */
    } while (--n);
#endif
}
/* ===========================================================================
 * Read a new buffer from the current input stream, update the adler32
 * and total number of bytes read.  All deflate() input goes through
 * this function so some applications may wish to modify it to avoid
 * allocating a large strm->next_in buffer and copying from it.
 * (See also flush_pending()).
 */
local unsigned read_buf(pop, strm, buf, size)
    PMEMobjpool *pop;
    z_streamp strm;
    Byte *buf;
    unsigned size;
{
    // struct z_stream *wstrm = D_RW(strm);
    // const struct z_stream *rstrm = D_RO(strm);
    unsigned len = strm->avail_in;

    if (len > size) len = size;
    if (len == 0) return 0;

    strm->avail_in  -= len;

    pmemobj_memcpy_persist(pop, buf, strm->next_in, len);
    if (strm->state->wrap == 1) {
        strm->adler = adler32(strm->adler, buf, len);
    }

    strm->next_in  += len;
    strm->total_in += len;

    return len;
}


/* ===========================================================================
 * Fill the window when the lookahead becomes insufficient.
 * Updates strstart and lookahead.
 *
 * IN assertion: lookahead < MIN_LOOKAHEAD
 * OUT assertions: strstart <= window_size-MIN_LOOKAHEAD
 *    At least one byte has been read, or avail_in == 0; reads are
 *    performed for at least two bytes (required for the zip translate_eol
 *    option -- not supported here).
 */
local void fill_window(pop, s)
    PMEMobjpool *pop;
    deflate_state *s;
{
    // const struct deflate_state *rs = D_RO(s);
    // struct deflate_state *ws = D_RW(s);

    z_streamp strm = s->strm;
    //struct z_stream *wstrm = D_RW(strm);
    // const struct z_stream *rstrm = D_RO(strm);
    
    unsigned n;
    unsigned more;    /* Amount of free space at the end of the window. */
    uInt wsize = s->w_size;

    Assert(rs->lookahead < MIN_LOOKAHEAD, "already enough lookahead");

    do {
        more = (unsigned)(s->window_size -(ulg)s->lookahead -(ulg)s->strstart);

        /* Deal with !@#$% 64K limit: */
        if (sizeof(int) <= 2) {
            if (more == 0 && s->strstart == 0 && s->lookahead == 0) {
                more = wsize;

            } else if (more == (unsigned)(-1)) {
                /* Very unlikely, but possible on 16 bit machine if
                 * strstart == 0 && lookahead == 1 (input done a byte at time)
                 */
                more--;
            }
        }

        /* If the window is almost full and there is insufficient lookahead,
         * move the upper half to the lower one to make room in the upper half.
         */
        if (s->strstart >= wsize+MAX_DIST(s)) {
            pmemobj_memcpy_persist(pop, D_RW(s->window), D_RO(s->window) + wsize, (unsigned)wsize - more);
            s->match_start -= wsize;
            s->strstart    -= wsize; /* we now have strstart >= MAX_DIST */
            s->block_start -= (long) wsize;
            slide_hash(s);
            more += wsize;
        }
        if (strm->avail_in == 0) break;

        /* If there was no sliding:
         *    strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
         *    more == window_size - lookahead - strstart
         * => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
         * => more >= window_size - 2*WSIZE + 2
         * In the BIG_MEM or MMAP case (not yet supported),
         *   window_size == input_size + MIN_LOOKAHEAD  &&
         *   strstart + s->lookahead <= input_size => more >= MIN_LOOKAHEAD.
         * Otherwise, window_size == 2*WSIZE so more >= 2.
         * If there was sliding, more >= WSIZE. So in all cases, more >= 2.
         */
        Assert(more >= 2, "more < 2");

        n = read_buf(pop, s->strm, D_RW(s->window) + s->strstart + s->lookahead, more);
        s->lookahead += n;

        /* Initialize the hash value now that we have some input: */
        if (s->lookahead + s->insert >= MIN_MATCH) {
            uInt str = s->strstart - s->insert;
            s->ins_h = D_RO(s->window)[str];
            UPDATE_HASH(s, s->ins_h, D_RW(s->window)[str + 1]);
#if MIN_MATCH != 3
            Call UPDATE_HASH() MIN_MATCH-3 more times
#endif
            while (s->insert) {
                UPDATE_HASH(s, s->ins_h, D_RW(s->window)[str + MIN_MATCH-1]);
#ifndef FASTEST
                D_RW(s->prev)[str & s->w_mask] = D_RO(s->head)[s->ins_h];
#endif
                D_RW(s->head)[s->ins_h] = (Pos)str;
                str++;
                s->insert--;
                if (s->lookahead + s->insert < MIN_MATCH)
                    break;
            }
        }
        /* If the whole input has less than MIN_MATCH bytes, ins_h is garbage,
         * but this is not important since only literal bytes will be emitted.
         */

    } while (s->lookahead < MIN_LOOKAHEAD && strm->avail_in != 0);

    /* If the WIN_INIT bytes after the end of the current data have never been
     * written, then zero those bytes in order to avoid memory check reports of
     * the use of uninitialized (or uninitialised as Julian writes) bytes by
     * the longest match routines.  Update the high water mark for the next
     * time through here.  WIN_INIT is set to MAX_MATCH since the longest match
     * routines allow scanning to strstart + MAX_MATCH, ignoring lookahead.
     */
    if (s->high_water < s->window_size) {
        ulg curr = s->strstart + (ulg)(s->lookahead);
        ulg init;

        if (s->high_water < curr) {
            /* Previous high water mark below current data -- zero WIN_INIT
             * bytes or up to end of window, whichever is less.
             */
            init = s->window_size - curr;
            if (init > WIN_INIT)
                init = WIN_INIT;
            pmemobj_memset_persist(pop, D_RW(s->window) + curr, 0, (unsigned)init);
            s->high_water = curr + init;
        }
        else if (s->high_water < (ulg)curr + WIN_INIT) {
            /* High water mark at or above current data, but below current data
             * plus WIN_INIT -- zero out to current data plus WIN_INIT, or up
             * to end of window, whichever is less.
             */
            init = (ulg)curr + WIN_INIT - s->high_water;
            if (init > s->window_size - s->high_water)
                init = s->window_size - s->high_water;
            pmemobj_memset_persist(pop, D_RW(s->window) + (s->high_water), 0, (unsigned)init);
            s->high_water += init;
        }
    }

    Assert((ulg)rs->strstart <= rs->window_size - MIN_LOOKAHEAD,
           "not enough room for search");
}
/* =========================================================================
 * Put a short in the pending buffer. The 16-bit value is put in MSB order.
 * IN assertion: the stream state is correct and there is enough room in
 * pending_buf.
 */
local void putShortMSB (s, b)
    deflate_state *s;
    uInt b;
{
    put_byte(s, (Byte)(b >> 8));
    put_byte(s, (Byte)(b & 0xff));
}
/* =========================================================================
 * Flush as much pending output as possible. All deflate() output, except for
 * some deflate_stored() output, goes through this function so some
 * applications may wish to modify it to avoid allocating a large
 * strm->next_out buffer and copying into it. (See also read_buf()).
 */
local void flush_pending(pop, strm)
    PMEMobjpool *pop;
    z_stream *strm;
{
    // struct z_stream *wstrm = D_RW(strm);
    // const struct z_stream *rstrm = D_RO(strm);
    unsigned len;
    deflate_state *s = strm->state;
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    
    _tr_flush_bits(s);
    len = s->pending;
    if (len > strm->avail_out) len = strm->avail_out;
    if (len == 0) return;

    pmemobj_memcpy_persist(pop, strm->next_out, s->pending_out, len);
    strm->next_out  += len;
    s->pending_out  += len;
    strm->total_out += len;
    strm->avail_out -= len;
    s->pending      -= len;
    if (s->pending == 0) {
        s->pending_out = s->pending_buf;
    }
}

/* ===========================================================================
 * Set match_start to the longest match starting at the given string and
 * return its length. Matches shorter or equal to prev_length are discarded,
 * in which case the result is equal to prev_length and match_start is
 * garbage.
 * IN assertions: cur_match is the head of the hash chain for the current
 *   string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1
 * OUT assertion: the match length is not greater than s->lookahead.
 */

local uInt longest_match(s, cur_match)
    deflate_state *s;
    IPos cur_match;                             /* current match */
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    unsigned chain_length = s->max_chain_length;/* max hash chain length */
    register Bytef *scan = D_RW(s->window) + s->strstart; /* current string */
    register Bytef *match;                      /* matched string */
    register int len;                           /* length of current match */
    int best_len = (int)s->prev_length;         /* best match length so far */
    int nice_match = s->nice_match;             /* stop if match long enough */
    IPos limit = s->strstart > (IPos)MAX_DIST(s) ?
        s->strstart - (IPos)MAX_DIST(s) : NIL;
    /* Stop when cur_match becomes <= limit. To simplify the code,
     * we prevent matches with the string of window index 0.
     */
    Posf *prev = D_RW(s->prev);
    uInt wmask = s->w_mask;

#ifdef UNALIGNED_OK
    /* Compare two bytes at a time. Note: this is not always beneficial.
     * Try with and without -DUNALIGNED_OK to check.
     */
    register Bytef *strend = s->window + s->strstart + MAX_MATCH - 1;
    register ush scan_start = *(ushf*)scan;
    register ush scan_end   = *(ushf*)(scan+best_len-1);
#else
    register Bytef *strend = D_RW(s->window) + s->strstart + MAX_MATCH;
    register Byte scan_end1  = scan[best_len-1];
    register Byte scan_end   = scan[best_len];
#endif

    /* The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
     * It is easy to get rid of this optimization if necessary.
     */
    Assert(rs->hash_bits >= 8 && MAX_MATCH == 258, "Code too clever");

    /* Do not waste too much time if we already have a good match: */
    if (s->prev_length >= s->good_match) {
        chain_length >>= 2;
    }
    /* Do not look for matches beyond the end of the input. This is necessary
     * to make deflate deterministic.
     */
    if ((uInt)nice_match > s->lookahead) nice_match = (int)s->lookahead;

    Assert((ulg)rs->strstart <= rs->window_size-MIN_LOOKAHEAD, "need lookahead");

    do {
        Assert(cur_match < rs->strstart, "no future");
        match = D_RW(s->window) + cur_match;

        /* Skip to next match if the match length cannot increase
         * or if the match length is less than 2.  Note that the checks below
         * for insufficient lookahead only occur occasionally for performance
         * reasons.  Therefore uninitialized memory will be accessed, and
         * conditional jumps will be made that depend on those values.
         * However the length of the match is limited to the lookahead, so
         * the output of deflate is not affected by the uninitialized values.
         */
#if (defined(UNALIGNED_OK) && MAX_MATCH == 258)
        /* This code assumes sizeof(unsigned short) == 2. Do not use
         * UNALIGNED_OK if your compiler uses a different size.
         */
        if (*(ushf*)(match+best_len-1) != scan_end ||
            *(ushf*)match != scan_start) continue;

        /* It is not necessary to compare scan[2] and match[2] since they are
         * always equal when the other bytes match, given that the hash keys
         * are equal and that HASH_BITS >= 8. Compare 2 bytes at a time at
         * strstart+3, +5, ... up to strstart+257. We check for insufficient
         * lookahead only every 4th comparison; the 128th check will be made
         * at strstart+257. If MAX_MATCH-2 is not a multiple of 8, it is
         * necessary to put more guard bytes at the end of the window, or
         * to check more often for insufficient lookahead.
         */
        Assert(scan[2] == match[2], "scan[2]?");
        scan++, match++;
        do {
        } while (*(ushf*)(scan+=2) == *(ushf*)(match+=2) &&
                 *(ushf*)(scan+=2) == *(ushf*)(match+=2) &&
                 *(ushf*)(scan+=2) == *(ushf*)(match+=2) &&
                 *(ushf*)(scan+=2) == *(ushf*)(match+=2) &&
                 scan < strend);
        /* The funny "do {}" generates better code on most compilers */

        /* Here, scan <= window+strstart+257 */
        Assert(scan <= s->window+(unsigned)(s->window_size-1), "wild scan");
        if (*scan == *match) scan++;

        len = (MAX_MATCH - 1) - (int)(strend-scan);
        scan = strend - (MAX_MATCH-1);

#else /* UNALIGNED_OK */

        if (match[best_len]   != scan_end  ||
            match[best_len-1] != scan_end1 ||
            *match            != *scan     ||
            *++match          != scan[1])      continue;

        /* The check at best_len-1 can be removed because it will be made
         * again later. (This heuristic is not always a win.)
         * It is not necessary to compare scan[2] and match[2] since they
         * are always equal when the other bytes match, given that
         * the hash keys are equal and that HASH_BITS >= 8.
         */
        scan += 2, match++;
        Assert(*scan == *match, "match[2]?");

        /* We check for insufficient lookahead only every 8th comparison;
         * the 256th check will be made at strstart+258.
         */
        do {
        } while (*++scan == *++match && *++scan == *++match &&
                 *++scan == *++match && *++scan == *++match &&
                 *++scan == *++match && *++scan == *++match &&
                 *++scan == *++match && *++scan == *++match &&
                 scan < strend);

        Assert(scan <= s->window+(unsigned)(s->window_size-1), "wild scan");

        len = MAX_MATCH - (int)(strend - scan);
        scan = strend - MAX_MATCH;

#endif /* UNALIGNED_OK */

        if (len > best_len) {
            s->match_start = cur_match;
            best_len = len;
            if (len >= nice_match) break;
#ifdef UNALIGNED_OK
            scan_end = *(ushf*)(scan+best_len-1);
#else
            scan_end1  = scan[best_len-1];
            scan_end   = scan[best_len];
#endif
        }
    } while ((cur_match = prev[cur_match & wmask]) > limit
             && --chain_length != 0);

    if ((uInt)best_len <= s->lookahead) return (uInt)best_len;
    return s->lookahead;
}

/* ========================================================================= */
int ZEXPORT deflate (pop, strm, flush)
    PMEMobjpool *pop;
    z_streamp strm;
    int flush;
{
    // struct z_stream *wstrm = D_RW(strm);
    // const struct z_stream *rstrm = D_RO(strm);
    int old_flush; /* value of flush param for previous deflate call */
    

    if (deflateStateCheck(strm) || flush > Z_BLOCK || flush < 0) {
        return Z_STREAM_ERROR;
    }
    deflate_state *s;
    s = strm->state;
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);


    if (strm->next_out == Z_NULL ||
        (strm->avail_in != 0 && strm->next_in == Z_NULL) ||
        (s->status == FINISH_STATE && flush != Z_FINISH)) {
        ERR_RETURN(strm, Z_STREAM_ERROR);
    }
    if (strm->avail_out == 0) ERR_RETURN(strm, Z_BUF_ERROR);

    old_flush = s->last_flush;
    s->last_flush = flush;

    /* Flush as much pending output as possible */
    if (s->pending != 0) {
        flush_pending(pop, strm);
        if (strm->avail_out == 0) {
            /* Since avail_out is 0, deflate will be called again with
             * more output space, but possibly with both pending and
             * avail_in equal to zero. There won't be anything to do,
             * but this is not an error situation so make sure we
             * return OK instead of BUF_ERROR at next call of deflate:
             */
            s->last_flush = -1;
            return Z_OK;
        }

    /* Make sure there is something to do and avoid duplicate consecutive
     * flushes. For repeated and useless calls with Z_FINISH, we keep
     * returning Z_STREAM_END instead of Z_BUF_ERROR.
     */
    } else if (strm->avail_in == 0 && RANK(flush) <= RANK(old_flush) &&
               flush != Z_FINISH) {
        ERR_RETURN(strm, Z_BUF_ERROR);
    }

    /* User must not provide more input after the first FINISH: */
    if (s->status == FINISH_STATE && strm->avail_in != 0) {
        ERR_RETURN(strm, Z_BUF_ERROR);
    }

    /* Write the header */
    if (s->status == INIT_STATE) {
        /* zlib header */
        uInt header = (Z_DEFLATED + ((s->w_bits-8)<<4)) << 8;
        uInt level_flags;

        if (s->strategy >= Z_HUFFMAN_ONLY || s->level < 2)
            level_flags = 0;
        else if (s->level < 6)
            level_flags = 1;
        else if (s->level == 6)
            level_flags = 2;
        else
            level_flags = 3;
        header |= (level_flags << 6);
        if (s->strstart != 0) header |= PRESET_DICT;
        header += 31 - (header % 31);

        putShortMSB(s, header);

        /* Save the adler32 of the preset dictionary: */
        if (s->strstart != 0) {
            putShortMSB(s, (uInt)(strm->adler >> 16));
            putShortMSB(s, (uInt)(strm->adler & 0xffff));
        }
        strm->adler = adler32(0L, Z_NULL, 0);
        s->status = BUSY_STATE;

        /* Compression must start with an empty pending buffer */
        flush_pending(pop, strm);
        if (s->pending != 0) {
            s->last_flush = -1;
            return Z_OK;
        }
    }
    /* ------------------------------------------------
    /* Start a new block or continue the current one.
     * ------------------------------------------------*/
    if (strm->avail_in != 0 || s->lookahead != 0 ||
        (flush != Z_NO_FLUSH && s->status != FINISH_STATE)) {
        block_state bstate;
        bstate = s->level == 0 ? deflate_stored(pop, s, flush) :
                 s->strategy == Z_HUFFMAN_ONLY ? deflate_huff(pop, s, flush) :
                 s->strategy == Z_RLE ? deflate_rle(pop, s, flush) :
                 (*(configuration_table[s->level].func))(pop, s, flush);

        if (bstate == finish_started || bstate == finish_done) {
            s->status = FINISH_STATE;
        }
        if (bstate == need_more || bstate == finish_started) {
            if (strm->avail_out == 0) {
                s->last_flush = -1; /* avoid BUF_ERROR next call, see above */
            }
            return Z_OK;
            /* If flush != Z_NO_FLUSH && avail_out == 0, the next call
             * of deflate should use the same flush parameter to make sure
             * that the flush is complete. So we don't have to output an
             * empty block here, this will be done at next call. This also
             * ensures that for a very small output buffer, we emit at most
             * one empty block.
             */
        }
        if (bstate == block_done) {
            if (flush == Z_PARTIAL_FLUSH) {
                _tr_align(s);
            } else if (flush != Z_BLOCK) { /* FULL_FLUSH or SYNC_FLUSH */
                _tr_stored_block(pop, s, (char*)0, 0L, 0);
                /* For a full flush, this empty block will be recognized
                 * as a special marker by inflate_sync().
                 */
                if (flush == Z_FULL_FLUSH) {
                    /* CLEAR_HASH(s) */            /* forget history */
                    D_RW(s->head)[s->hash_size-1] = NIL;
                    pmemobj_memset_persist(pop, (Bytef *)D_RW(s->head), 0, (unsigned)(s->hash_size-1)*sizeof(*D_RO(s->head)));
                    
                    if (s->lookahead == 0) {
                        s->strstart = 0;
                        s->block_start = 0L;
                        s->insert = 0;
                    }
                }
            }
            flush_pending(pop, strm);
            if (strm->avail_out == 0) {
              s->last_flush = -1; /* avoid BUF_ERROR at next call, see above */
              return Z_OK;
            }
        }
    }

    if (flush != Z_FINISH) return Z_OK;
    if (s->wrap <= 0) return Z_STREAM_END;

    /* Write the trailer */
    {
        putShortMSB(s, (uInt)(strm->adler >> 16));
        putShortMSB(s, (uInt)(strm->adler & 0xffff));
    }
    flush_pending(pop, strm);
    /* If avail_out is zero, the application will call deflate again
     * to flush the rest.
     */
    if (s->wrap > 0) s->wrap = -(s->wrap); /* write the trailer only once! */
    return s->pending != 0 ? Z_OK : Z_STREAM_END;
}
/* ===========================================================================
 * Flush the current block, with given end-of-file flag.
 * IN assertion: strstart is set to the end of the current match.
 */
#define FLUSH_BLOCK_ONLY(s, last) { \
   _tr_flush_block(pop, s, (s->block_start >= 0L ? \
                   (charf *)&(D_RW(s->window)[(unsigned)s->block_start]) : \
                   (charf *)Z_NULL), \
                (ulg)((long)s->strstart - s->block_start), \
                (last)); \
   s->block_start = s->strstart; \
   flush_pending(pop, s->strm); \
   Tracev((stderr,"[FLUSH]")); \
}

/* Same but force premature exit if necessary. */
#define FLUSH_BLOCK(s, last) { \
   FLUSH_BLOCK_ONLY(s, last); \
   if (s->strm->avail_out == 0) return (last) ? finish_started : need_more; \
}

/* Maximum stored block length in deflate format (not including header). */
#define MAX_STORED 65535

/* Minimum of a and b. */
#define MIN(a, b) ((a) > (b) ? (b) : (a))
/* ===========================================================================
 * Same as above, but achieves better compression. We use a lazy
 * evaluation for matches: a match is finally adopted only if there is
 * no better match at the next window position.
 */
local block_state deflate_slow(pop, s, flush)
    PMEMobjpool *pop;
    deflate_state *s;
    int flush;
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    IPos hash_head;          /* head of hash chain */
    int bflush;              /* set if current block must be flushed */

    /* Process the input block. */
    for (;;) {
        /* Make sure that we always have enough lookahead, except
         * at the end of the input file. We need MAX_MATCH bytes
         * for the next match, plus MIN_MATCH bytes to insert the
         * string following the next match.
         */
        if (s->lookahead < MIN_LOOKAHEAD) {
            fill_window(pop, s);
            if (s->lookahead < MIN_LOOKAHEAD && flush == Z_NO_FLUSH) {
                return need_more;
            }
            if (s->lookahead == 0) break; /* flush the current block */
        }

        /* Insert the string window[strstart .. strstart+2] in the
         * dictionary, and set hash_head to the head of the hash chain:
         */
        hash_head = NIL;
        if (s->lookahead >= MIN_MATCH) {
            INSERT_STRING(s, s->strstart, hash_head);
        }

        /* Find the longest match, discarding those <= prev_length.
         */
        s->prev_length = s->match_length, s->prev_match = s->match_start;
        s->match_length = MIN_MATCH-1;

        if (hash_head != NIL && s->prev_length < s->max_lazy_match &&
            s->strstart - hash_head <= MAX_DIST(s)) {
            /* To simplify the code, we prevent matches with the string
             * of window index 0 (in particular we have to avoid a match
             * of the string with itself at the start of the input file).
             */
            s->match_length = longest_match (s, hash_head);
            /* longest_match() sets match_start */

            if (s->match_length <= 5 && (s->strategy == Z_FILTERED
#if TOO_FAR <= 32767
                || (s->match_length == MIN_MATCH &&
                    s->strstart - s->match_start > TOO_FAR)
#endif
                )) {

                /* If prev_match is also MIN_MATCH, match_start is garbage
                 * but we will ignore the current match anyway.
                 */
                s->match_length = MIN_MATCH-1;
            }
        }
        /* If there was a match at the previous step and the current
         * match is not better, output the previous match:
         */
        if (s->prev_length >= MIN_MATCH && s->match_length <= s->prev_length) {
            uInt max_insert = s->strstart + s->lookahead - MIN_MATCH;
            /* Do not insert strings in hash table beyond this. */

            //check_match(s, s->strstart-1, s->prev_match, s->prev_length);

            _tr_tally_dist(s, s->strstart -1 - s->prev_match,
                           s->prev_length - MIN_MATCH, bflush);
            /* Insert in hash table all strings up to the end of the match.
             * strstart-1 and strstart are already inserted. If there is not
             * enough lookahead, the last two strings are not inserted in
             * the hash table.
             */
            s->lookahead -= (s->prev_length)-1;
            s->prev_length -= 2;
            do {
                if (++s->strstart <= max_insert) {
                    INSERT_STRING(s, s->strstart, hash_head);
                }
            } while (--s->prev_length != 0);
            s->match_available = 0;
            s->match_length = MIN_MATCH-1;
            s->strstart++;

            if (bflush) FLUSH_BLOCK(s, 0);

        } else if (s->match_available) {
            /* If there was no match at the previous position, output a
             * single literal. If there was a match but the current match
             * is longer, truncate the previous match to a single literal.
             */
            //Tracevv((stderr,"%c", rs->window[rs->strstart-1]));
            _tr_tally_lit(s, D_RW(s->window)[s->strstart-1], bflush);
            if (bflush) {
                FLUSH_BLOCK_ONLY(s, 0);
            }
            s->strstart++;
            s->lookahead--;
            if (s->strm->avail_out == 0) return need_more;
        } else {
            /* There is no previous match to compare with, wait for
             * the next step to decide.
             */
            s->match_available = 1;
            s->strstart++;
            s->lookahead--;
        }
    }
    Assert (flush != Z_NO_FLUSH, "no flush?");
    if (s->match_available) {
        //Tracevv((stderr,"%c", rs->window[rs->strstart-1]));
        _tr_tally_lit(s, D_RW(s->window)[s->strstart-1], bflush);
        s->match_available = 0;
    }
    s->insert = s->strstart < MIN_MATCH-1 ? s->strstart : MIN_MATCH-1;
    if (flush == Z_FINISH) {
        FLUSH_BLOCK(s, 1);
        return finish_done;
    }
    if (s->last_lit)
        FLUSH_BLOCK(s, 0);
    return block_done;
}
/* =========================================================================
 * Copy the source state to the destination state.
 * To simplify the source, this is not supported for 16-bit MSDOS (which
 * doesn't have enough memory anyway to duplicate compression states).
 */
// int ZEXPORT deflateCopy (dest, source)
//     TOID(struct z_stream) dest;
//     TOID(struct z_stream) source;
// {
// #ifdef MAXSEG_64K
//     return Z_STREAM_ERROR;
// #else
//     deflate_state *ds;
//     deflate_state *ss;
//     ushf *overlay;


//     if (deflateStateCheck(source) || dest == Z_NULL) {
//         return Z_STREAM_ERROR;
//     }

//     ss = source->state;

//     zmemcpy((voidpf)dest, (voidpf)source, sizeof(z_stream));

//     ds = (deflate_state *) ZALLOC(dest, 1, sizeof(deflate_state));
//     if (ds == Z_NULL) return Z_MEM_ERROR;
//     dest->state = (struct internal_state FAR *) ds;
//     zmemcpy((voidpf)ds, (voidpf)ss, sizeof(deflate_state));
//     ds->strm = dest;

//     ds->window = (Bytef *) ZALLOC(dest, ds->w_size, 2*sizeof(Byte));
//     ds->prev   = (Posf *)  ZALLOC(dest, ds->w_size, sizeof(Pos));
//     ds->head   = (Posf *)  ZALLOC(dest, ds->hash_size, sizeof(Pos));
//     overlay = (ushf *) ZALLOC(dest, ds->lit_bufsize, sizeof(ush)+2);
//     ds->pending_buf = (uchf *) overlay;

//     if (ds->window == Z_NULL || ds->prev == Z_NULL || ds->head == Z_NULL ||
//         ds->pending_buf == Z_NULL) {
//         deflateEnd (dest);
//         return Z_MEM_ERROR;
//     }
//     /* following zmemcpy do not work for 16-bit MSDOS */
//     zmemcpy(ds->window, ss->window, ds->w_size * 2 * sizeof(Byte));
//     zmemcpy((voidpf)ds->prev, (voidpf)ss->prev, ds->w_size * sizeof(Pos));
//     zmemcpy((voidpf)ds->head, (voidpf)ss->head, ds->hash_size * sizeof(Pos));
//     zmemcpy(ds->pending_buf, ss->pending_buf, (uInt)ds->pending_buf_size);

//     ds->pending_out = ds->pending_buf + (ss->pending_out - ss->pending_buf);
//     ds->d_buf = overlay + ds->lit_bufsize/sizeof(ush);
//     ds->l_buf = ds->pending_buf + (1+sizeof(ush))*ds->lit_bufsize;

//     ds->l_desc.dyn_tree = ds->dyn_ltree;
//     ds->d_desc.dyn_tree = ds->dyn_dtree;
//     ds->bl_desc.dyn_tree = ds->bl_tree;

//     return Z_OK;
// #endif /* MAXSEG_64K */
// }

/* ===========================================================================
 * Copy without compression as much as possible from the input stream, return
 * the current block state.
 *
 * In case deflateParams() is used to later switch to a non-zero compression
 * level, s->matches (otherwise unused when storing) keeps track of the number
 * of hash table slides to perform. If s->matches is 1, then one hash table
 * slide will be done when switching. If s->matches is 2, the maximum value
 * allowed here, then the hash table will be cleared, since two or more slides
 * is the same as a clear.
 *
 * deflate_stored() is written to minimize the number of times an input byte is
 * copied. It is most efficient with large input and output buffers, which
 * maximizes the opportunites to have a single copy from next_in to next_out.
 */
local block_state deflate_stored(pop, s, flush)
    PMEMobjpool *pop;
    deflate_state *s;
    int flush;
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    /* Smallest worthy block size when not flushing or finishing. By default
     * this is 32K. This can be as small as 507 bytes for memLevel == 1. For
     * large input and output buffers, the stored block size will be larger.
     */
    unsigned min_block = MIN(s->pending_buf_size - 5, s->w_size);

    /* Copy as many min_block or larger stored blocks directly to next_out as
     * possible. If flushing, copy the remaining available input to next_out as
     * stored blocks, if there is enough space.
     */
    unsigned len, left, have, last = 0;
    unsigned used = s->strm->avail_in;
    do {
        /* Set len to the maximum size block that we can copy directly with the
         * available input data and output space. Set left to how much of that
         * would be copied from what's left in the window.
         */
        len = MAX_STORED;       /* maximum deflate stored block length */
        have = (s->bi_valid + 42) >> 3;         /* number of header bytes */
        if (s->strm->avail_out < have)          /* need room for header */
            break;
            /* maximum stored block length that will fit in avail_out: */
        have = s->strm->avail_out - have;
        left = s->strstart - s->block_start;    /* bytes left in window */
        if (len > (ulg)left + s->strm->avail_in)
            len = left + s->strm->avail_in;     /* limit len to the input */
        if (len > have)
            len = have;                         /* limit len to the output */

        /* If the stored block would be less than min_block in length, or if
         * unable to copy all of the available input when flushing, then try
         * copying to the window and the pending buffer instead. Also don't
         * write an empty block when flushing -- deflate() does that.
         */
        if (len < min_block && ((len == 0 && flush != Z_FINISH) ||
                                flush == Z_NO_FLUSH ||
                                len != left + s->strm->avail_in))
            break;

        /* Make a dummy stored block in pending to get the header bytes,
         * including any pending bits. This also updates the debugging counts.
         */
        last = flush == Z_FINISH && len == left + s->strm->avail_in ? 1 : 0;
        _tr_stored_block(pop, s, (char *)0, 0L, last);

        /* Replace the lengths in the dummy stored block with len. */
        s->pending_buf[s->pending - 4] = len;
        s->pending_buf[s->pending - 3] = len >> 8;
        s->pending_buf[s->pending - 2] = ~len;
        s->pending_buf[s->pending - 1] = ~len >> 8;

        /* Write the stored block header bytes. */
        flush_pending(pop, s->strm);

#ifdef ZLIB_DEBUG
        /* Update debugging counts for the data about to be copied. */
        s->compressed_len += len << 3;
        s->bits_sent += len << 3;
#endif

        /* Copy uncompressed bytes from the window to next_out. */
        if (left) {
            if (left > len)
                left = len;
            pmemobj_memcpy_persist(pop, s->strm->next_out, D_RW(s->window) + s->block_start, left);
            s->strm->next_out += left;
            s->strm->avail_out -= left;
            s->strm->total_out += left;
            s->block_start += left;
            len -= left;
        }

        /* Copy uncompressed bytes directly from next_in to next_out, updating
         * the check value.
         */
        if (len) {
            read_buf(pop, s->strm, s->strm->next_out, len);
            s->strm->next_out += len;
            s->strm->avail_out -= len;
            s->strm->total_out += len;
        }
    } while (last == 0);

    /* Update the sliding window with the last s->w_size bytes of the copied
     * data, or append all of the copied data to the existing window if less
     * than s->w_size bytes were copied. Also update the number of bytes to
     * insert in the hash tables, in the event that deflateParams() switches to
     * a non-zero compression level.
     */
    used -= s->strm->avail_in;      /* number of input bytes directly copied */
    if (used) {
        /* If any input was used, then no unused input remains in the window,
         * therefore s->block_start == s->strstart.
         */
        if (used >= s->w_size) {    /* supplant the previous history */
            s->matches = 2;         /* clear hash */
            memcpy(D_RW(s->window), s->strm->next_in - s->w_size, s->w_size);
            s->strstart = s->w_size;
        }
        else {
            if (s->window_size - s->strstart <= used) {
                /* Slide the window down. */
                s->strstart -= s->w_size;
                pmemobj_memcpy_persist(pop, D_RW(s->window), D_RW(s->window) + s->w_size, s->strstart);
                if (s->matches < 2)
                    s->matches++;   /* add a pending slide_hash() */
            }
            pmemobj_memcpy_persist(pop, D_RW(s->window) + s->strstart, s->strm->next_in - used, used);
            s->strstart += used;
        }
        s->block_start = s->strstart;
        s->insert += MIN(used, s->w_size - s->insert);
    }
    if (s->high_water < s->strstart)
        s->high_water = s->strstart;

    /* If the last block was written to next_out, then done. */
    if (last)
        return finish_done;

    /* If flushing and all input has been consumed, then done. */
    if (flush != Z_NO_FLUSH && flush != Z_FINISH &&
        s->strm->avail_in == 0 && (long)s->strstart == s->block_start)
        return block_done;

    /* Fill the window with any remaining input. */
    have = s->window_size - s->strstart - 1;
    if (s->strm->avail_in > have && s->block_start >= (long)s->w_size) {
        /* Slide the window down. */
        s->block_start -= s->w_size;
        s->strstart -= s->w_size;
        pmemobj_memcpy_persist(pop, D_RW(s->window), D_RW(s->window) + s->w_size, s->strstart);
        if (s->matches < 2)
            s->matches++;           /* add a pending slide_hash() */
        have += s->w_size;          /* more space now */
    }
    if (have > s->strm->avail_in)
        have = s->strm->avail_in;
    if (have) {
        read_buf(pop, s->strm, D_RW(s->window) + s->strstart, have);
        s->strstart += have;
    }
    if (s->high_water < s->strstart)
        s->high_water = s->strstart;

    /* There was not enough avail_out to write a complete worthy or flushed
     * stored block to next_out. Write a stored block to pending instead, if we
     * have enough input for a worthy block, or if flushing and there is enough
     * room for the remaining input as a stored block in the pending buffer.
     */
    have = (s->bi_valid + 42) >> 3;         /* number of header bytes */
        /* maximum stored block length that will fit in pending: */
    have = MIN(s->pending_buf_size - have, MAX_STORED);
    min_block = MIN(have, s->w_size);
    left = s->strstart - s->block_start;
    if (left >= min_block ||
        ((left || flush == Z_FINISH) && flush != Z_NO_FLUSH &&
         s->strm->avail_in == 0 && left <= have)) {
        len = MIN(left, have);
        last = flush == Z_FINISH && s->strm->avail_in == 0 &&
               len == left ? 1 : 0;
        _tr_stored_block(pop, s, (charf *)D_RO(s->window) + s->block_start, len, last);
        s->block_start += len;
        flush_pending(pop, s->strm);
    }

    /* We've done all we can with the available input and output. */
    return last ? finish_started : need_more;
}

/* ===========================================================================
 * Compress as much as possible from the input stream, return the current
 * block state.
 * This function does not perform lazy evaluation of matches and inserts
 * new strings in the dictionary only for unmatched strings or for short
 * matches. It is used only for the fast compression options.
 */
local block_state deflate_fast(pop, s, flush)
    PMEMobjpool *pop;
    deflate_state *s;
    int flush;
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    IPos hash_head;       /* head of the hash chain */
    int bflush;           /* set if current block must be flushed */

    for (;;) {
        /* Make sure that we always have enough lookahead, except
         * at the end of the input file. We need MAX_MATCH bytes
         * for the next match, plus MIN_MATCH bytes to insert the
         * string following the next match.
         */
        if (s->lookahead < MIN_LOOKAHEAD) {
            fill_window(pop, s);
            if (s->lookahead < MIN_LOOKAHEAD && flush == Z_NO_FLUSH) {
                return need_more;
            }
            if (s->lookahead == 0) break; /* flush the current block */
        }

        /* Insert the string window[strstart .. strstart+2] in the
         * dictionary, and set hash_head to the head of the hash chain:
         */
        hash_head = NIL;
        if (s->lookahead >= MIN_MATCH) {
            INSERT_STRING(s, s->strstart, hash_head);
        }

        /* Find the longest match, discarding those <= prev_length.
         * At this point we have always match_length < MIN_MATCH
         */
        if (hash_head != NIL && s->strstart - hash_head <= MAX_DIST(s)) {
            /* To simplify the code, we prevent matches with the string
             * of window index 0 (in particular we have to avoid a match
             * of the string with itself at the start of the input file).
             */
            s->match_length = longest_match (s, hash_head);
            /* longest_match() sets match_start */
        }
        if (s->match_length >= MIN_MATCH) {
            //check_match(s, rs->strstart, rs->match_start, rs->match_length);

            _tr_tally_dist(s, s->strstart - s->match_start,
                           s->match_length - MIN_MATCH, bflush);

            s->lookahead -= s->match_length;

            /* Insert new strings in the hash table only if the match length
             * is not too large. This saves time but degrades compression.
             */
#ifndef FASTEST
            if (s->match_length <= s->max_insert_length &&
                s->lookahead >= MIN_MATCH) {
                s->match_length--; /* string at strstart already in table */
                do {
                    s->strstart++;
                    INSERT_STRING(s, s->strstart, hash_head);
                    /* strstart never exceeds WSIZE-MAX_MATCH, so there are
                     * always MIN_MATCH bytes ahead.
                     */
                } while (--s->match_length != 0);
                s->strstart++;
            } else
#endif
            {
                s->strstart += s->match_length;
                s->match_length = 0;
                s->ins_h = D_RO(s->window)[s->strstart];
                UPDATE_HASH(s, s->ins_h, D_RO(s->window)[s->strstart+1]);
#if MIN_MATCH != 3
                Call UPDATE_HASH() MIN_MATCH-3 more times
#endif
                /* If lookahead < MIN_MATCH, ins_h is garbage, but it does not
                 * matter since it will be recomputed at next deflate call.
                 */
            }
        } else {
            /* No match, output a literal byte */
            //Tracevv((stderr,"%c", s->window[s->strstart]));
            _tr_tally_lit (s, D_RO(s->window)[s->strstart], bflush);
            s->lookahead--;
            s->strstart++;
        }
        if (bflush) FLUSH_BLOCK(s, 0);
    }
    s->insert = s->strstart < MIN_MATCH-1 ? s->strstart : MIN_MATCH-1;
    if (flush == Z_FINISH) {
        FLUSH_BLOCK(s, 1);
        return finish_done;
    }
    if (s->last_lit)
        FLUSH_BLOCK(s, 0);
    return block_done;
}

/* ===========================================================================
 * For Z_RLE, simply look for runs of bytes, generate matches only of distance
 * one.  Do not maintain a hash table.  (It will be regenerated if this run of
 * deflate switches away from Z_RLE.)
 */
local block_state deflate_rle(pop, s, flush)
    PMEMobjpool *pop;
    deflate_state *s;
    int flush;
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    int bflush;             /* set if current block must be flushed */
    uInt prev;              /* byte at distance one to match */
    Bytef *scan, *strend;   /* scan goes up to strend for length of run */

    for (;;) {
        /* Make sure that we always have enough lookahead, except
         * at the end of the input file. We need MAX_MATCH bytes
         * for the longest run, plus one for the unrolled loop.
         */
        if (s->lookahead <= MAX_MATCH) {
            fill_window(pop, s);
            if (s->lookahead <= MAX_MATCH && flush == Z_NO_FLUSH) {
                return need_more;
            }
            if (s->lookahead == 0) break; /* flush the current block */
        }

        /* See how many times the previous byte repeats */
        s->match_length = 0;
        if (s->lookahead >= MIN_MATCH && s->strstart > 0) {
            scan = D_RW(s->window) + s->strstart - 1;
            prev = *scan;
            if (prev == *++scan && prev == *++scan && prev == *++scan) {
                strend = D_RW(s->window) + s->strstart + MAX_MATCH;
                do {
                } while (prev == *++scan && prev == *++scan &&
                         prev == *++scan && prev == *++scan &&
                         prev == *++scan && prev == *++scan &&
                         prev == *++scan && prev == *++scan &&
                         scan < strend);
                s->match_length = MAX_MATCH - (uInt)(strend - scan);
                if (s->match_length > s->lookahead)
                    s->match_length = s->lookahead;
            }
            Assert(scan <= s->window+(uInt)(s->window_size-1), "wild scan");
        }

        /* Emit match if have run of MIN_MATCH or longer, else emit literal */
        if (s->match_length >= MIN_MATCH) {
            //check_match(s, rs->strstart, rs->strstart - 1, rs->match_length);

            _tr_tally_dist(s, 1, s->match_length - MIN_MATCH, bflush);

            s->lookahead -= s->match_length;
            s->strstart += s->match_length;
            s->match_length = 0;
        } else {
            /* No match, output a literal byte */
            //Tracevv((stderr,"%c", s->window[s->strstart]));
            _tr_tally_lit (s, D_RO(s->window)[s->strstart], bflush);
            s->lookahead--;
            s->strstart++;
        }
        if (bflush) FLUSH_BLOCK(s, 0);
    }
    s->insert = 0;
    if (flush == Z_FINISH) {
        FLUSH_BLOCK(s, 1);
        return finish_done;
    }
    if (s->last_lit)
        FLUSH_BLOCK(s, 0);
    return block_done;
}

/* ===========================================================================
 * For Z_HUFFMAN_ONLY, do not look for matches.  Do not maintain a hash table.
 * (It will be regenerated if this run of deflate switches away from Huffman.)
 */
local block_state deflate_huff(pop, s, flush)
    PMEMobjpool *pop;
    deflate_state *s;
    int flush;
{
    // struct deflate_state *ws = D_RW(s);
    // const struct deflate_state *rs = D_RO(s);
    int bflush;             /* set if current block must be flushed */

    for (;;) {
        /* Make sure that we have a literal to write. */
        if (s->lookahead == 0) {
            fill_window(pop, s);
            if (s->lookahead == 0) {
                if (flush == Z_NO_FLUSH)
                    return need_more;
                break;      /* flush the current block */
            }
        }

        /* Output a literal byte */
        s->match_length = 0;
        Tracevv((stderr,"%c", s->window[s->strstart]));
        _tr_tally_lit (s, D_RO(s->window)[s->strstart], bflush);
        s->lookahead--;
        s->strstart++;
        if (bflush) FLUSH_BLOCK(s, 0);
    }
    s->insert = 0;
    if (flush == Z_FINISH) {
        FLUSH_BLOCK(s, 1);
        return finish_done;
    }
    if (s->last_lit)
        FLUSH_BLOCK(s, 0);
    return block_done;
}
