/* deflate.h -- internal compression state
 * Copyright (C) 1995-2016 Jean-loup Gailly
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

/* WARNING: this file should *not* be used by applications. It is
   part of the implementation of the compression library and is
   subject to change. Applications should only use zlib.h.
 */

/* @(#) $Id$ */

#ifndef DEFLATE_H
#define DEFLATE_H

#include "zutil.h"

/* define NO_GZIP when compiling if you want to disable gzip header and
   trailer creation by deflate().  NO_GZIP would be used to avoid linking in
   the crc code when it is not needed.  For shared libraries, gzip encoding
   should be left enabled. */
#ifndef NO_GZIP
#  define GZIP
#endif

/* ===========================================================================
 * Internal compression state.
 */

#define LENGTH_CODES 29
/* number of length codes, not counting the special END_BLOCK code */

#define LITERALS  256
/* number of literal bytes 0..255 */

#define L_CODES (LITERALS+1+LENGTH_CODES)
/* number of Literal or Length codes, including the END_BLOCK code */

#define D_CODES   30
/* number of distance codes */

#define BL_CODES  19
/* number of codes used to transfer the bit lengths */

#define HEAP_SIZE (2*L_CODES+1)
/* maximum heap size */

#define MAX_BITS 15
/* All codes must not exceed MAX_BITS bits */

#define Buf_size 16
/* size of bit buffer in bi_buf */

#define INIT_STATE    42    /* zlib header -> BUSY_STATE */
#ifdef GZIP
#  define GZIP_STATE  57    /* gzip header -> BUSY_STATE | EXTRA_STATE */
#endif
#define EXTRA_STATE   69    /* gzip extra block -> NAME_STATE */
#define NAME_STATE    73    /* gzip file name -> COMMENT_STATE */
#define COMMENT_STATE 91    /* gzip comment -> HCRC_STATE */
#define HCRC_STATE   103    /* gzip header CRC -> BUSY_STATE */
#define BUSY_STATE   113    /* deflate -> FINISH_STATE */
#define FINISH_STATE 666    /* stream complete */
/* Stream status */

#define DIST_CODE_LEN  512

/* Data structure describing a single value and its code string. */
struct ct_data {
    union {
        ush  freq;       /* frequency count */
        ush  code;       /* bit string */
    } fc;
    union {
        ush  dad;        /* father node in Huffman tree */
        ush  len;        /* length of bit string */
    } dl;
};

#define Freq fc.freq
#define Code fc.code
#define Dad  dl.dad
#define Len  dl.len

//typedef struct static_tree_desc_s  static_tree_desc;

struct tree_desc {
    struct ct_data *dyn_tree;           /* the dynamic tree */
    int     max_code;                         /* largest code with non zero frequency */
    struct static_tree_desc *stat_desc; /* the corresponding static tree */
};

typedef ush Pos;
typedef Pos FAR Posf;
typedef unsigned IPos;

/* A Pos is an index in the character window. We use short instead of int to
 * save space in the various tables. IPos is used only for parameter passing.
 */

struct deflate_state {
    TOID(struct z_stream) strm;      /* pointer back to this zlib stream */
    int   status;                    /* as the name implies */
    TOID(ush) overlay;
    Byte  *pending_buf;          /* output still pending */
    ulg   pending_buf_size;          /* size of pending_buf */
    Byte  *pending_out;          /* next pending byte to output to the stream */
    ulg   pending;                   /* nb of bytes in the pending buffer */
    int   wrap;                      /* bit 0 true for zlib, bit 1 true for gzip */
    gz_headerp  gzhead;              /* gzip header information to write */
    ulg   gzindex;                   /* where in extra, name, or comment */
    Byte  method;                    /* can only be DEFLATED */
    int   last_flush;                /* value of flush param for previous deflate call */

                /* used by deflate.c: */

    uInt  w_size;        /* LZ77 window size (32K by default) */
    uInt  w_bits;        /* log2(w_size)  (8..16) */
    uInt  w_mask;        /* w_size - 1 */
    
    Byte *window;
    //TOID(Byte) window;
    /* Sliding window. Input bytes are read into the second half of the window,
     * and move to the first half later to keep a dictionary of at least wSize
     * bytes. With this organization, matches are limited to a distance of
     * wSize-MAX_MATCH bytes, but this ensures that IO is always
     * performed with a length multiple of the block size. Also, it limits
     * the window size to 64K, which is quite useful on MSDOS.
     * To do: use the user input buffer as sliding window.
     */

    ulg window_size;
    /* Actual size of window: 2*wSize, except when the user input buffer
     * is directly used as sliding window.
     */
    
    ush *prev;
    //TOID(ush) prev;
    /* Link to older string with same hash index. To limit the size of this
     * array to 64K, this link is maintained only for the last 32K strings.
     * An index in this array is thus a window index modulo 32K.
     */
    ush *head;
    //TOID(ush) head; /* Heads of the hash chains or NIL. */
    uInt  ins_h;          /* hash index of string to be inserted */
    uInt  hash_size;      /* number of elements in hash table */
    uInt  hash_bits;      /* log2(hash_size) */
    uInt  hash_mask;      /* hash_size-1 */

    uInt  hash_shift;
    /* Number of bits by which ins_h must be shifted at each input
     * step. It must be such that after MIN_MATCH steps, the oldest
     * byte no longer takes part in the hash key, that is:
     *   hash_shift * MIN_MATCH >= hash_bits
     */

    long block_start;
    /* Window position at the beginning of the current output block. Gets
     * negative when the window is moved backwards.
     */

    uInt match_length;           /* length of best match */
    IPos prev_match;             /* previous match */
    int match_available;         /* set if previous match exists */
    uInt strstart;               /* start of string to insert */
    uInt match_start;            /* start of matching string */
    uInt lookahead;              /* number of valid bytes ahead in window */

    uInt prev_length;
    /* Length of the best match at previous step. Matches not greater than this
     * are discarded. This is used in the lazy match evaluation.
     */

    uInt max_chain_length;
    /* To speed up deflation, hash chains are never searched beyond this
     * length.  A higher limit improves compression ratio but degrades the
     * speed.
     */

    uInt max_lazy_match;
    /* Attempt to find a better match only when the current match is strictly
     * smaller than this value. This mechanism is used only for compression
     * levels >= 4.
     */
#   define max_insert_length  max_lazy_match
    /* Insert new strings in the hash table only if the match length is not
     * greater than this length. This saves time but degrades compression.
     * max_insert_length is used only for compression levels <= 3.
     */

    int level;    /* compression level (1..9) */
    int strategy; /* favor or force Huffman coding*/

    uInt good_match;
    /* Use a faster search when the previous match is longer than this */

    int nice_match; /* Stop searching when current match exceeds this */

                /* used by trees.c: */
    /* Didn't use ct_data typedef below to suppress compiler warning */

    struct ct_data dyn_ltree[HEAP_SIZE];  /* literal and length tree */
    struct ct_data dyn_dtree[2*D_CODES+1];/* distance tree */
    struct ct_data bl_tree[2*BL_CODES+1]; /* Huffman tree for bit lengths */

              
    struct tree_desc l_desc;              /* desc. for literal tree */         
    struct tree_desc d_desc;              /* desc. for distance tree */             
    struct tree_desc bl_desc;             /* desc. for bit length tree */

    ush bl_count[MAX_BITS+1];
    /* number of codes at each bit length for an optimal tree */

    int heap[2*L_CODES+1];      /* heap used to build the Huffman trees */
    int heap_len;               /* number of elements in the heap */
    int heap_max;               /* element of largest frequency */
    /* The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
     * The same heap array is used to build all trees.
     */

    uch depth[2*L_CODES+1];
    /* Depth of each subtree used as tie breaker for trees of equal frequency
     */

    uchf *l_buf;          /* buffer for literals or lengths */

    uInt  lit_bufsize;
    /* Size of match buffer for literals/lengths.  There are 4 reasons for
     * limiting lit_bufsize to 64K:
     *   - frequencies can be kept in 16 bit counters
     *   - if compression is not successful for the first block, all input
     *     data is still in the window so we can still emit a stored block even
     *     when input comes from standard input.  (This can also be done for
     *     all blocks if lit_bufsize is not greater than 32K.)
     *   - if compression is not successful for a file smaller than 64K, we can
     *     even emit a stored file instead of a stored block (saving 5 bytes).
     *     This is applicable only for zip (not gzip or zlib).
     *   - creating new Huffman trees less frequently may not provide fast
     *     adaptation to changes in the input data statistics. (Take for
     *     example a binary file with poorly compressible code followed by
     *     a highly compressible string table.) Smaller buffer sizes give
     *     fast adaptation but have of course the overhead of transmitting
     *     trees more frequently.
     *   - I can't count above 4
     */

    uInt last_lit;      /* running index in l_buf */

    ushf *d_buf;
    /* Buffer for distances. To simplify the code, d_buf and l_buf have
     * the same number of elements. To use different lengths, an extra flag
     * array would be necessary.
     */

    ulg opt_len;        /* bit length of current block with optimal trees */
    ulg static_len;     /* bit length of current block with static trees */
    uInt matches;       /* number of string matches in current block */
    uInt insert;        /* bytes at end of window left to insert */

#ifdef ZLIB_DEBUG
    ulg compressed_len; /* total bit length of compressed file mod 2^32 */
    ulg bits_sent;      /* bit length of compressed data sent mod 2^32 */
#endif

    ush bi_buf;
    /* Output buffer. bits are inserted starting at the bottom (least
     * significant bits).
     */
    int bi_valid;
    /* Number of valid bits in bi_buf.  All bits above the last valid bit
     * are always zero.
     */

    ulg high_water;
    /* High water mark offset in window for initialized bytes -- bytes above
     * this are set to zero in order to avoid memory check warnings when
     * longest match routines access bytes past the input.  This is then
     * updated to the new high water mark.
     */

    // struct ct_data static_ltree[L_CODES+2];
    // struct ct_data static_dtree[D_CODES];
    // uch _dist_code[DIST_CODE_LEN];
    // uch _length_code[MAX_MATCH-MIN_MATCH+1];
    // int base_length[LENGTH_CODES];
    // int base_dist[D_CODES];
};

/* Output a byte on the stream.
 * IN assertion: there is enough room in pending_buf.
 */
#define put_byte(s, c) {D_RW(s)->pending_buf[D_RW(s)->pending++] = (Bytef)(c);}


#define MIN_LOOKAHEAD (MAX_MATCH+MIN_MATCH+1)
/* Minimum amount of lookahead, except at the end of the input file.
 * See deflate.c for comments about the MIN_MATCH+1.
 */

#define MAX_DIST(s)  ((D_RO(s)->w_size)-MIN_LOOKAHEAD)
/* In order to simplify the code, particularly on 16 bit machines, match
 * distances are limited to MAX_DIST instead of WSIZE.
 */

#define WIN_INIT MAX_MATCH
/* Number of bytes after end of data in window to initialize in order to avoid
   memory checker errors from longest match routines */

        /* in trees.c */
void ZLIB_INTERNAL _tr_init OF((PMEMobjpool *pop, TOID(struct deflate_state) s));
int ZLIB_INTERNAL _tr_tally OF((TOID(struct deflate_state) s, unsigned dist, unsigned lc));
void ZLIB_INTERNAL _tr_flush_block OF((PMEMobjpool *pop, TOID(struct deflate_state) s, charf *buf,
                        ulg stored_len, int last));
void ZLIB_INTERNAL _tr_flush_bits OF((TOID(struct deflate_state) s));
void ZLIB_INTERNAL _tr_align OF((TOID(struct deflate_state) s));
void ZLIB_INTERNAL _tr_stored_block OF((PMEMobjpool *pop, TOID (struct deflate_state) s, charf *buf,
                        ulg stored_len, int last));

#define d_code(dist) \
   ((dist) < 256 ? _dist_code[dist] : _dist_code[256+((dist)>>7)])
/* Mapping from a distance to a distance code. dist is the distance - 1 and
 * must not have side effects. _dist_code[256] and _dist_code[257] are never
 * used.
 */

#ifndef ZLIB_DEBUG
/* Inline versions of _tr_tally for speed: */

#if defined(GEN_TREES_H) || !defined(STDC)
  extern uch ZLIB_INTERNAL _length_code[];
  extern uch ZLIB_INTERNAL _dist_code[];
#else
  extern const uch ZLIB_INTERNAL _length_code[];
  extern const uch ZLIB_INTERNAL _dist_code[];
#endif

# define _tr_tally_lit(s, c, flush) \
  { uch cc = (c); \
    ws->d_buf[rs->last_lit] = 0; \
    ws->l_buf[ws->last_lit++] = cc; \
    ws->dyn_ltree[cc].Freq++; \
    flush = (ws->last_lit == rs->lit_bufsize-1); \
   }
# define _tr_tally_dist(s, distance, length, flush) \
  { uch len = (uch)(length); \
    ush dist = (ush)(distance); \
    ws->d_buf[rs->last_lit] = dist; \
    ws->l_buf[ws->last_lit++] = len; \
    dist--; \
    ws->dyn_ltree[_length_code[len]+LITERALS+1].Freq++; \
    ws->dyn_dtree[d_code(dist)].Freq++; \
    flush = (ws->last_lit == rs->lit_bufsize-1); \
  }
#else
# define _tr_tally_lit(s, c, flush) flush = _tr_tally(s, 0, c)
# define _tr_tally_dist(s, distance, length, flush) \
              flush = _tr_tally(s, distance, length)
#endif

#endif /* DEFLATE_H */