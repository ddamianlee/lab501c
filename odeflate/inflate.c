/* inflate.c -- zlib decompression
 * Copyright (C) 1995-2016 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

/*
 * Change history:
 *
 * 1.2.beta0    24 Nov 2002
 * - First version -- complete rewrite of inflate to simplify code, avoid
 *   creation of window when not needed, minimize use of window when it is
 *   needed, make inffast.c even faster, implement gzip decoding, and to
 *   improve code readability and style over the previous zlib inflate code
 *
 * 1.2.beta1    25 Nov 2002
 * - Use pointers for available input and output checking in inffast.c
 * - Remove input and output counters in inffast.c
 * - Change inffast.c entry and loop from avail_in >= 7 to >= 6
 * - Remove unnecessary second byte pull from length extra in inffast.c
 * - Unroll direct copy to three copies per loop in inffast.c
 *
 * 1.2.beta2    4 Dec 2002
 * - Change external routine names to reduce potential conflicts
 * - Correct filename to inffixed.h for fixed tables in inflate.c
 * - Make hbuf[] unsigned char to match parameter type in inflate.c
 * - Change strm->next_out[-state->offset] to *(strm->next_out - state->offset)
 *   to avoid negation problem on Alphas (64 bit) in inflate.c
 *
 * 1.2.beta3    22 Dec 2002
 * - Add comments on state->bits assertion in inffast.c
 * - Add comments on op field in inftrees.h
 * - Fix bug in reuse of allocated window after inflateReset()
 * - Remove bit fields--back to byte structure for speed
 * - Remove distance extra == 0 check in inflate_fast()--only helps for lengths
 * - Change post-increments to pre-increments in inflate_fast(), PPC biased?
 * - Add compile time option, POSTINC, to use post-increments instead (Intel?)
 * - Make MATCH copy in inflate() much faster for when inflate_fast() not used
 * - Use local copies of stream next and avail values, as well as local bit
 *   buffer and bit count in inflate()--for speed when inflate_fast() not used
 *
 * 1.2.beta4    1 Jan 2003
 * - Split ptr - 257 statements in inflate_table() to avoid compiler warnings
 * - Move a comment on output buffer sizes from inffast.c to inflate.c
 * - Add comments in inffast.c to introduce the inflate_fast() routine
 * - Rearrange window copies in inflate_fast() for speed and simplification
 * - Unroll last copy for window match in inflate_fast()
 * - Use local copies of window variables in inflate_fast() for speed
 * - Pull out common wnext == 0 case for speed in inflate_fast()
 * - Make op and len in inflate_fast() unsigned for consistency
 * - Add FAR to lcode and dcode declarations in inflate_fast()
 * - Simplified bad distance check in inflate_fast()
 * - Added inflateBackInit(), inflateBack(), and inflateBackEnd() in new
 *   source file infback.c to provide a call-back interface to inflate for
 *   programs like gzip and unzip -- uses window as output buffer to avoid
 *   window copying
 *
 * 1.2.beta5    1 Jan 2003
 * - Improved inflateBack() interface to allow the caller to provide initial
 *   input in strm.
 * - Fixed stored blocks bug in inflateBack()
 *
 * 1.2.beta6    4 Jan 2003
 * - Added comments in inffast.c on effectiveness of POSTINC
 * - Typecasting all around to reduce compiler warnings
 * - Changed loops from while (1) or do {} while (1) to for (;;), again to
 *   make compilers happy
 * - Changed type of window in inflateBackInit() to unsigned char *
 *
 * 1.2.beta7    27 Jan 2003
 * - Changed many types to unsigned or unsigned short to avoid warnings
 * - Added inflateCopy() function
 *
 * 1.2.0        9 Mar 2003
 * - Changed inflateBack() interface to provide separate opaque descriptors
 *   for the in() and out() functions
 * - Changed inflateBack() argument and in_func typedef to swap the length
 *   and buffer address return values for the input function
 * - Check next_in and next_out for Z_NULL on entry to inflate()
 *
 * The history for versions after 1.2.0 are in ChangeLog in zlib distribution.
 */

#include "zutil.h"
#include "inftrees.h"
#include "inflate.h"
#include "inffast.h"
#include <errno.h>
#include <stdio.h>

#ifdef MAKEFIXED
#  ifndef BUILDFIXED
#    define BUILDFIXED
#  endif
#endif

/* function prototypes */
local int inflateStateCheck OF((TOID(struct z_stream) strm));
local void fixedtables OF((TOID(struct inflate_state)state));
local int updatewindow OF((PMEMobjpool *pop, TOID(struct z_stream) strm, const unsigned char FAR *end,
                           unsigned copy));
#ifdef BUILDFIXED
   void makefixed OF((void));
#endif
local unsigned syncsearch OF((unsigned FAR *have, const unsigned char FAR *buf,
                              unsigned len));

local int inflateStateCheck(strm)
TOID(struct z_stream) strm;
{
    //struct inflate_state FAR *state;
    TOID(struct inflate_state) state;
    if (TOID_IS_NULL(strm) ||
        D_RO(strm)->zalloc == (alloc_func)0 || D_RO(strm)->zfree == (free_func)0)
        return 1;
    state = D_RO(strm)->istate;
    if (TOID_IS_NULL(state) || !TOID_EQUALS(D_RO(state)->strm, strm) ||
        D_RO(state)->mode < HEAD || D_RO(state)->mode > SYNC)
        return 1;
    return 0;
}

int ZEXPORT inflateResetKeep(pop, strm)
PMEMobjpool *pop;
TOID(struct z_stream) strm;
{
    //struct inflate_state FAR *state;
    TOID(struct inflate_state) state;
    if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
    state = D_RW(strm)->istate;
    D_RW(strm)->total_in = D_RW(strm)->total_out = D_RW(state)->total = 0;
    D_RW(strm)->msg = Z_NULL;
    if (D_RO(state)->wrap)        /* to support ill-conceived Java test suite */
        D_RW(strm)->adler = D_RO(state)->wrap & 1;
    D_RW(state)->mode = HEAD;
    D_RW(state)->last = 0;
    D_RW(state)->havedict = 0;
    D_RW(state)->dmax = 32768U;
    D_RW(state)->head = Z_NULL;
    D_RW(state)->hold = 0;
    D_RW(state)->bits = 0;
    D_RW(state)->lencode = D_RW(state)->distcode = D_RW(state)->next = D_RW(state)->codes;
    D_RW(state)->sane = 1;
    D_RW(state)->back = -1;
    pmemobj_persist(pop, D_RW(strm), sizeof(*D_RW(strm)));
    pmemobj_persist(pop, D_RW(state), sizeof(*D_RW(state)));
    Tracev((stderr, "inflate: reset\n"));
    return Z_OK;
}

int ZEXPORT inflateReset(pop, strm)
PMEMobjpool *pop;
TOID(struct z_stream) strm;
{
    //struct inflate_state FAR *state;
    TOID(struct inflate_state) state;
    if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
    state.oid = D_RW(strm)->istate.oid;
    D_RW(state)->wsize = 0;
    D_RW(state)->whave = 0;
    D_RW(state)->wnext = 0;
    pmemobj_persist(pop, D_RW(strm), sizeof(*D_RW(strm)));
    pmemobj_persist(pop, D_RW(state), sizeof(*D_RW(state)));
    return inflateResetKeep(pop, strm);
}

int ZEXPORT inflateReset2(pop, strm, windowBits)
PMEMobjpool *pop;
TOID(struct z_stream) strm;
int windowBits;
{
    int wrap;
    //struct inflate_state FAR *state;
    TOID(struct inflate_state) state;
    
    /* get the state */
    if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
    //state = (struct inflate_state FAR *)strm->state;
    state = D_RW(strm)->istate;
    
    /* extract wrap request from windowBits parameter */
    if (windowBits < 0) {
        wrap = 0;
        windowBits = -windowBits;
    }
    else {
        wrap = (windowBits >> 4) + 5;
#ifdef GUNZIP
        if (windowBits < 48)
            windowBits &= 15;
#endif
    }

    /* set number of window bits, free window if different */
    if (windowBits && (windowBits < 8 || windowBits > 15))
        return Z_STREAM_ERROR;
    if (D_RO(state)->window != Z_NULL && D_RO(state)->wbits != (unsigned)windowBits) {
        ZFREE(D_RW(strm), D_RW(state)->window);

        D_RW(state)->window = Z_NULL;
    }

    /* update state and reset the rest of it */
    D_RW(state)->wrap = wrap;
    D_RW(state)->wbits = (unsigned)windowBits;
    pmemobj_persist(pop, D_RW(strm), sizeof(*D_RW(strm)));
    pmemobj_persist(pop, D_RW(state), sizeof(*D_RW(state)));
    return inflateReset(pop, strm);
}

int ZEXPORT inflateInit2_(pop, strm, windowBits, version, stream_size)
PMEMobjpool *pop;
TOID(struct z_stream) strm;
int windowBits;
const char *version;
int stream_size;
{
    int ret;
    //struct inflate_state FAR *state;

    if (version == Z_NULL || version[0] != ZLIB_VERSION[0] ||
        stream_size != (int)(sizeof(struct z_stream)))
        return Z_VERSION_ERROR;
    //if (strm == Z_NULL) return Z_STREAM_ERROR;
    if(TOID_IS_NULL(strm))
    {
        printf("struct z_stream is NULL");
        return Z_STREAM_ERROR;
    } 
    D_RW(strm)->msg = Z_NULL;                 /* in case we return an error */
    if (D_RW(strm)->zalloc == (alloc_func)0) {
#ifdef Z_SOLO
        return Z_STREAM_ERROR;
#else
        D_RW(strm)->zalloc = zcalloc;
        D_RW(strm)->opaque = (voidpf)0;
#endif
    }
    if (D_RW(strm)->zfree == (free_func)0)
#ifdef Z_SOLO
        return Z_STREAM_ERROR;
#else
        D_RW(strm)->zfree = zcfree;
#endif
    // state = (struct inflate_state FAR *)
    //         ZALLOC(strm, 1, sizeof(struct inflate_state));
    // if (state == Z_NULL) return Z_MEM_ERROR;
    // Tracev((stderr, "inflate: allocated\n"));
    
    TOID(struct inflate_state) state;
    if(POBJ_ALLOC(pop, &state, struct inflate_state, sizeof(struct inflate_state), NULL, NULL))
    {
        fprintf(stderr, "deflate_state alloc failed: %s\n", pmemobj_errormsg());
        abort();
    }
    
    
    
    D_RW(strm)->istate = state;
    D_RW(state)->strm = strm;
    D_RW(state)->window = Z_NULL;
    //pmemobj_alloc(pop, D_RW(state)->window, 0, 20, NULL, NULL);
    D_RW(state)->mode = HEAD;     /* to pass state test in inflateReset2() */
    ret = inflateReset2(pop, strm, windowBits);
    if (ret != Z_OK) {
        //ZFREE(strm, state);
        POBJ_FREE(&state);
        //D_RW(strm)->istate =  NULL;
        TOID_NULL(struct inflate_state);
    }
    pmemobj_persist(pop, D_RW(strm), sizeof(*D_RW(strm)));
    pmemobj_persist(pop, D_RW(state), sizeof(*D_RW(state)));
    return ret;
}

int ZEXPORT inflateInit_(pop, strm, version, stream_size)
PMEMobjpool *pop;
TOID(struct z_stream) strm;
const char *version;
int stream_size;
{
    return inflateInit2_(pop, strm, DEF_WBITS, version, stream_size);
}

int ZEXPORT inflatePrime(strm, bits, value)
TOID(struct z_stream) strm;
int bits;
int value;
{
    //struct inflate_state FAR *state;
    TOID(struct inflate_state) state;
    if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
    state = D_RW(strm)->istate;
    if (bits < 0) {
        D_RW(state)->hold = 0;
        D_RW(state)->bits = 0;
        return Z_OK;
    }
    if (bits > 16 || D_RO(state)->bits + (uInt)bits > 32) return Z_STREAM_ERROR;
    value &= (1L << bits) - 1;
    D_RW(state)->hold += (unsigned)value << D_RO(state)->bits;
    D_RW(state)->bits += (uInt)bits;
    return Z_OK;
}

/*
   Return state with length and distance decoding tables and index sizes set to
   fixed code decoding.  Normally this returns fixed tables from inffixed.h.
   If BUILDFIXED is defined, then instead this routine builds the tables the
   first time it's called, and returns those tables the first time and
   thereafter.  This reduces the size of the code by about 2K bytes, in
   exchange for a little execution time.  However, BUILDFIXED should not be
   used for threaded applications, since the rewriting of the tables and virgin
   may not be thread-safe.
 */
local void fixedtables(state)
TOID(struct inflate_state) state;
{
#ifdef BUILDFIXED
    static int virgin = 1;
    static code *lenfix, *distfix;
    static code fixed[544];

    /* build fixed huffman tables if first call (may not be thread safe) */
    if (virgin) {
        unsigned sym, bits;
        static code *next;

        /* literal/length table */
        sym = 0;
        while (sym < 144) state->lens[sym++] = 8;
        while (sym < 256) state->lens[sym++] = 9;
        while (sym < 280) state->lens[sym++] = 7;
        while (sym < 288) state->lens[sym++] = 8;
        next = fixed;
        lenfix = next;
        bits = 9;
        inflate_table(LENS, state->lens, 288, &(next), &(bits), state->work);

        /* distance table */
        sym = 0;
        while (sym < 32) state->lens[sym++] = 5;
        distfix = next;
        bits = 5;
        inflate_table(DISTS, state->lens, 32, &(next), &(bits), state->work);

        /* do this just once */
        virgin = 0;
    }
#else /* !BUILDFIXED */
#   include "inffixed.h"
#endif /* BUILDFIXED */
    D_RW(state)->lencode = lenfix;
    D_RW(state)->lenbits = 9;
    D_RW(state)->distcode = distfix;
    D_RW(state)->distbits = 5;
}

#ifdef MAKEFIXED
#include <stdio.h>

/*
   Write out the inffixed.h that is #include'd above.  Defining MAKEFIXED also
   defines BUILDFIXED, so the tables are built on the fly.  makefixed() writes
   those tables to stdout, which would be piped to inffixed.h.  A small program
   can simply call makefixed to do this:

    void makefixed(void);

    int main(void)
    {
        makefixed();
        return 0;
    }

   Then that can be linked with zlib built with MAKEFIXED defined and run:

    a.out > inffixed.h
 */
void makefixed()
{
    unsigned low, size;
    struct inflate_state state;

    fixedtables(&state);
    puts("    /* inffixed.h -- table for decoding fixed codes");
    puts("     * Generated automatically by makefixed().");
    puts("     */");
    puts("");
    puts("    /* WARNING: this file should *not* be used by applications.");
    puts("       It is part of the implementation of this library and is");
    puts("       subject to change. Applications should only use zlib.h.");
    puts("     */");
    puts("");
    size = 1U << 9;
    printf("    static const code lenfix[%u] = {", size);
    low = 0;
    for (;;) {
        if ((low % 7) == 0) printf("\n        ");
        printf("{%u,%u,%d}", (low & 127) == 99 ? 64 : state.lencode[low].op,
               state.lencode[low].bits, state.lencode[low].val);
        if (++low == size) break;
        putchar(',');
    }
    puts("\n    };");
    size = 1U << 5;
    printf("\n    static const code distfix[%u] = {", size);
    low = 0;
    for (;;) {
        if ((low % 6) == 0) printf("\n        ");
        printf("{%u,%u,%d}", state.distcode[low].op, state.distcode[low].bits,
               state.distcode[low].val);
        if (++low == size) break;
        putchar(',');
    }
    puts("\n    };");
}
#endif /* MAKEFIXED */

/*
   Update the window with the last wsize (normally 32K) bytes written before
   returning.  If window does not exist yet, create it.  This is only called
   when a window is already in use, or when output has been written during this
   inflate call, but the end of the deflate stream has not been reached yet.
   It is also called to create a window for dictionary data when a dictionary
   is loaded.

   Providing output buffers larger than 32K to inflate() should provide a speed
   advantage, since only the last 32K of output is copied to the sliding window
   upon return from inflate(), and since all distances after the first 32K of
   output will fall in the output data, making match copies simpler and faster.
   The advantage may be dependent on the size of the processor's data caches.
 */
local int updatewindow(pop, strm, end, copy)
PMEMobjpool *pop;
TOID (struct z_stream) strm;
const Bytef *end;
unsigned copy;
{
    //struct inflate_state ZFREEFAR *state;
    TOID(struct inflate_state) state;
    unsigned dist;

    state = D_RW(strm)->istate;
    struct inflate_state *wstate = D_RW(state);
    const struct inflate_state *rstate = D_RO(state);
    //struct inflate_state *rstate = rstate;
    
    //TOID(Byte) windowp;
    if(POBJ_ALLOC(pop, &D_RW(strm)->windowp, Byte, 1U << rstate->wbits*sizeof(unsigned char), NULL, NULL))
    {
        printf("window allocation wrong");
        exit(1);
    }
    /* if it hasn't been done already, allocate space for the window */
    if (rstate->window == Z_NULL) {
        // wstate->window = (unsigned char FAR *)
        //                  ZALLOC(D_RW(strm), 1U << rstate->wbits,
        //                         sizeof(unsigned char));
        // if(pmemobj_alloc(pop, wstate->window, 1U << rstate->wbits*sizeof(unsigned char), NULL, NULL, NULL))
        // {
        //     printf("window allocation wrong");
        //     exit(1);
        // }
        wstate->window = D_RW(D_RW(strm)->windowp);
        if (rstate->window == Z_NULL) return 1;
    }

    /* if window not in use yet, initialize */
    if (rstate->wsize == 0) {
        wstate->wsize = 1U << rstate->wbits;
        wstate->wnext = 0;
        wstate->whave = 0;
    }

    /* copy state->wsize or less output bytes into the circular window */
    if (copy >= rstate->wsize) {
        pmemobj_memcpy_persist(pop, wstate->window, end - wstate->wsize, wstate->wsize);
        wstate->wnext = 0;
        wstate->whave = rstate->wsize;
    }
    else {
        dist = rstate->wsize - rstate->wnext;
        if (dist > copy) dist = copy;
        pmemobj_memcpy_persist(pop, wstate->window + rstate->wnext, end - copy, dist);
        copy -= dist;
        if (copy) {
            pmemobj_memcpy_persist(pop, wstate->window, end - copy, copy);
            wstate->wnext = copy;
            wstate->whave = rstate->wsize;
        }
        else {
            wstate->wnext += dist;
            if (rstate->wnext == rstate->wsize) wstate->wnext = 0;
            if (rstate->whave < rstate->wsize) wstate->whave += dist;
        }
    }
    // pmemobj_persist(pop, D_RW(strm), sizeofof(*D_RW(strm)));
    // pmemobj_persist(pop, D_RW(state), sizeofof(*D_RW(state)));
    return 0;
}

/* Macros for inflate(): */

/* check function to use adler32() for zlib or crc32() for gzip */
// #ifdef GUNZIP
// #  define UPDATE(check, buf, len) \
//     (rstate->flags ? crc32(check, buf, len) : adler32(check, buf, len))
// #else
#  define UPDATE(check, buf, len) adler32(check, buf, len)
// #endif

/* check macros for header crc */
// #ifdef GUNZIP
// #  define CRC2(check, word) \
//     do { \
//         hbuf[0] = (unsigned char)(word); \
//         hbuf[1] = (unsigned char)((word) >> 8); \
//         check = crc32(check, hbuf, 2); \
//     } while (0)

// #  define CRC4(check, word) \
//     do { \
//         hbuf[0] = (unsigned char)(word); \
//         hbuf[1] = (unsigned char)((word) >> 8); \
//         hbuf[2] = (unsigned char)((word) >> 16); \
//         hbuf[3] = (unsigned char)((word) >> 24); \
//         check = crc32(check, hbuf, 4); \
//     } while (0)
// #endif

/* Load registers with state in inflate() for speed */
#define LOAD() \
    do { \
        put = wstrm->next_out; \
        *D_RW(left_) = rstrm->avail_out; \
        next = wstrm->next_in; \
        *D_RW(have_) = rstrm->avail_in; \
        *D_RW(hold_) = rstate->hold; \
        *D_RW(bits_) = rstate->bits; \
    } while (0)

/* Restore state from registers in inflate() */
#define RESTORE() \
    do { \
        wstrm->next_out = put; \
        wstrm->avail_out = *left; \
        wstrm->next_in = next; \
        wstrm->avail_in = *have; \
        wstate->hold = *hold; \
        wstate->bits = *bits; \
    } while (0)

/* Clear the input bit accumulator */
#define INITBITS() \
    do { \
        *hold = 0; \
        *bits = 0; \
    } while (0)

/* Get a byte of input into the bit accumulator, or return from inflate()
   if there is no input available. */
#define PULLBYTE() \
    do { \
        if (*have == 0) goto inf_leave; \
        (*have)--; \
        *hold += (unsigned long)(*next++) << *bits; \
        *bits += 8; \
    } while (0)

/* Assure that there are at least n bits in the bit accumulator.  If there is
   not enough available input to do that, then return from inflate(). */
#define NEEDBITS(n) \
    do { \
        while (*bits < (unsigned)(n)) \
            PULLBYTE(); \
    } while (0)

/* Return the low n bits of the bit accumulator (n < 16) */
#define BITS(n) \
    ((unsigned)(*hold) & ((1U << (n)) - 1))

/* Remove n bits from the bit accumulator */
#define DROPBITS(n) \
    do { \
        *hold >>= (n); \
        *bits -= (unsigned)(n); \
    } while (0)

/* Remove zero to seven bits as needed to go to a byte boundary */
#define BYTEBITS() \
    do { \
        *hold >>= *bits & 7; \
        *bits -= *bits & 7; \
    } while (0)

/*
   inflate() uses a state machine to process as much input data and generate as
   much output data as possible before returning.  The state machine is
   structured roughly as follows:

    for (;;) switch (state) {
    ...
    case STATEn:
        if (not enough input data or output space to make progress)
            return;
        ... make progress ...
        state = STATEm;
        break;
    ...
    }

   so when inflate() is called again, the same case is attempted again, and
   if the appropriate resources are provided, the machine proceeds to the
   next state.  The NEEDBITS() macro is usually the way the state evaluates
   whether it can proceed or should return.  NEEDBITS() does the return if
   the requested bits are not available.  The typical use of the BITS macros
   is:

        NEEDBITS(n);
        ... do something with BITS(n) ...
        DROPBITS(n);

   where NEEDBITS(n) either returns from inflate() if there isn't enough
   input left to load n bits into the accumulator, or it continues.  BITS(n)
   gives the low n bits in the accumulator.  When done, DROPBITS(n) drops
   the low n bits off the accumulator.  INITBITS() clears the accumulator
   and sets the number of available bits to zero.  BYTEBITS() discards just
   enough bits to put the accumulator on a byte boundary.  After BYTEBITS()
   and a NEEDBITS(8), then BITS(8) would return the next byte in the stream.

   NEEDBITS(n) uses PULLBYTE() to get an available byte of input, or to return
   if there is no input available.  The decoding of variable length codes uses
   PULLBYTE() directly in order to pull just enough bytes to decode the next
   code, and no more.

   Some states loop until they get enough input, making sure that enough
   state information is maintained to continue the loop where it left off
   if NEEDBITS() returns in the loop.  For example, want, need, and keep
   would all have to actually be part of the saved state in case NEEDBITS()
   returns:

    case STATEw:
        while (want < need) {
            NEEDBITS(n);
            keep[want++] = BITS(n);
            DROPBITS(n);
        }
        state = STATEx;
    case STATEx:

   As shown above, if the next state is also the next case, then the break
   is omitted.

   A state may also return if there is not enough output space available to
   complete that state.  Those states are copying stored data, writing a
   literal byte, and copying a matching string.

   When returning, a "goto inf_leave" is used to update the total counters,
   update the check value, and determine whether any progress has been made
   during that inflate() call in order to return the proper return code.
   Progress is defined as a change in either strm->avail_in or strm->avail_out.
   When there is a window, goto inf_leave will update the window with the last
   output written.  If a goto inf_leave occurs in the middle of decompression
   and there is no window currently, goto inf_leave will create one and copy
   output to the window for the next call of inflate().

   In this implementation, the flush parameter of inflate() only affects the
   return code (per zlib.h).  inflate() always writes as much as possible to
   strm->next_out, given the space available and the provided input--the effect
   documented in zlib.h of Z_SYNC_FLUSH.  Furthermore, inflate() always defers
   the allocation of and copying into a sliding window until necessary, which
   provides the effect documented in zlib.h for Z_FINISH when the entire input
   stream available.  So the only thing the flush parameter actually does is:
   when flush is set to Z_FINISH, inflate() cannot return Z_OK.  Instead it
   will return Z_BUF_ERROR if it has not reached the end of the stream.
 */

int ZEXPORT inflate(pop, strm, flush)
PMEMobjpool *pop;
TOID(struct z_stream) strm;
int flush;
{
    //struct inflate_state FAR *state;
    TOID(struct inflate_state) state;
    z_const unsigned char FAR *next;    /* next input */
    unsigned char *put;     /* next output */
    // unsigned have, left;        /* available input and output */
    // unsigned long hold;         /* bit buffer */
    // unsigned bits;              /* bits in bit buffer */
    // unsigned in, out;           /* save starting available input and output */
    // unsigned copy;              /* number of stored or match bytes to copy */
    unsigned char *from;    /* where to copy match bytes from */
    code here;                  /* current decoding table entry */
    code last;                  /* parent table entry */
    // unsigned len;               /* length to copy for repeats, bits to drop */
    // int ret;                    /* return code */

    // TOID(Byte)  next;    /* next input */
    // TOID(Byte)  put;     /* next output */
    TOID(uint) have_, left_;        /* available input and output */
    TOID(ulong) hold_;         /* bit buffer */
    TOID(uint) bits_;              /* bits in bit buffer */
    TOID(uint) in_, out_;           /* save starting available input and output */
    TOID(uint) copy_;              /* number of stored or match bytes to copy */
    // TOID(Byte) from;    /* where to copy match bytes from */
    // TOID(code) here;                  /* current decoding table entry */
    // TOID(code) last;                  /* parent table entry */
    TOID(uint) len_;               /* length to copy for repeats, bits to drop */
    int ret;                    /* return code */

    POBJ_ALLOC(pop, &have_, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &left_, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &hold_, ulong, sizeof(ulong), NULL, NULL);
    POBJ_ALLOC(pop, &bits_, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &in_, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &out_, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &copy_, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &len_, uint, sizeof(uint), NULL, NULL);
    
    unsigned *have = D_RW(have_);
    // const unsigned *have = D_RO(have);
    unsigned *left = D_RW(left_);
    // const unsigned left = D_RO(left);
    unsigned long *hold = D_RW(hold_);
    // const unsigned long hold = D_RO(hold);
    unsigned *bits = D_RW(bits_);
    // const unsigned bits = D_RO(bits);
    unsigned *in = D_RW(in_);
    // const unsigned in = D_RO(in);
    unsigned *out = D_RW(out_);
    // const unsigned out = D_RO(out);
    unsigned *copy = D_RW(copy_);
    // const unsigned copy = D_RO(copy);
    unsigned *len = D_RW(len_);
    // const unsigned len = D_RO(len);

    state = D_RW(strm)->istate;
    struct inflate_state *wstate = D_RW(state);
    const struct inflate_state *rstate = D_RO(state);
    struct z_stream *wstrm = D_RW(strm);
    const struct z_stream *rstrm = D_RO(strm);

#ifdef GUNZIP
    unsigned char hbuf[4];      /* buffer for gzip header crc calculation */
#endif
    static const unsigned short order[19] = /* permutation of code lengths */
        {16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};

    if (inflateStateCheck(strm) || rstrm->next_out == Z_NULL ||
        (rstrm->next_in == Z_NULL && rstrm->avail_in != 0))
        return Z_STREAM_ERROR;

    
    //inflate_mode mode = D_RW(state)->mode;
    //char *msg = D_RW(strm)->msg;
    //int nlen = D_RW(state)->nlen;

    if (rstate->mode == TYPE) wstate->mode = TYPEDO;      /* skip check */
    LOAD();
    *in = *have;
    *out = *left;
    ret = Z_OK;

    for (;;)
        switch (rstate->mode) {
        case HEAD:
            if (rstate->wrap == 0) {
                wstate->mode = TYPEDO;
                break;
            }
            NEEDBITS(16);

            if (((BITS(8) << 8) + ((*hold) >> 8)) % 31) {
                wstrm->msg = (char *)"incorrect header check";
                wstate->mode = BAD;
                break;
            }
            if (BITS(4) != Z_DEFLATED) {
                wstrm->msg = (char *)"unknown compression method";
                wstate->mode = BAD;
                break;
            }
            DROPBITS(4);
            (*len) = BITS(4) + 8;
            if (rstate->wbits == 0)
                wstate->wbits = (*len);
            if ((*len) > 15 || (*len) > rstate->wbits) {
                wstrm->msg = (char *)"invalid window size";
                wstate->mode = BAD;
                break;
            }
            wstate->dmax = 1U << (*len);
            Tracev((stderr, "inflate:   zlib header ok\n"));
            wstrm->adler = wstate->check = adler32(0L, Z_NULL, 0);
            wstate->mode = (*hold) & 0x200 ? DICTID : TYPE;
            INITBITS();
            break;

        case DICTID:
            NEEDBITS(32);
            wstrm->adler = wstate->check = ZSWAP32((*hold));
            INITBITS();
            wstate->mode = DICT;
        case DICT:
            if (rstate->havedict == 0) {
                RESTORE();
                return Z_NEED_DICT;
            }
            wstrm->adler = wstate->check = adler32(0L, Z_NULL, 0);
            wstate->mode = TYPE;
        case TYPE:
            if (flush == Z_BLOCK || flush == Z_TREES) goto inf_leave;
        case TYPEDO:
            if (rstate->last) {
                BYTEBITS();
                wstate->mode = CHECK;
                break;
            }
            NEEDBITS(3);
            wstate->last = BITS(1);
            DROPBITS(1);
            switch (BITS(2)) {
            case 0:                             /* stored block */
                Tracev((stderr, "inflate:     stored block%s\n",
                        state->last ? " (last)" : ""));
                wstate->mode = STORED;
                break;
            case 1:                             /* fixed block */
                fixedtables(state);
                Tracev((stderr, "inflate:     fixed codes block%s\n",
                        state->last ? " (last)" : ""));
                wstate->mode = LEN_;             /* decode codes */
                if (flush == Z_TREES) {
                    DROPBITS(2);
                    goto inf_leave;
                }
                break;
            case 2:                             /* dynamic block */
                Tracev((stderr, "inflate:     dynamic codes block%s\n",
                        state->last ? " (last)" : ""));
                wstate->mode = TABLE;
                break;
            case 3:
                wstrm->msg = (char *)"invalid block type";
                wstate->mode = BAD;
            }
            DROPBITS(2);
            break;
        case STORED:
            BYTEBITS();                         /* go to byte boundary */
            NEEDBITS(32);
            if (((*hold) & 0xffff) != (((*hold) >> 16) ^ 0xffff)) {
                wstrm->msg = (char *)"invalid stored block lengths";
                wstate->mode = BAD;
                break;
            }
            wstate->length = (unsigned)((*hold)) & 0xffff;
            Tracev((stderr, "inflate:       stored length %u\n",
                    state->length));
            INITBITS();
            wstate->mode = COPY_;
            if (flush == Z_TREES) goto inf_leave;
        case COPY_:
            wstate->mode = COPY;
        case COPY:
            ((*copy)) = rstate->length;
            if ((*copy)) {
                if ((*copy) > (*have)) (*copy) = (*have);
                if ((*copy) > (*left)) (*copy) = (*left);
                if ((*copy) == 0) goto inf_leave;
                //zmemcpy(put, next, copy);
                pmemobj_memcpy_persist(pop, put, next, (*copy));
                (*have) -= (*copy);
                next += (*copy);
                (*left) -= (*copy);
                put += (*copy);
                wstate->length -= (*copy);
                break;
            }
            Tracev((stderr, "inflate:       stored end\n"));
            wstate->mode = TYPE;
            break;
        case TABLE:
            NEEDBITS(14);
            wstate->nlen = BITS(5) + 257;
            DROPBITS(5);
            wstate->ndist = BITS(5) + 1;
            DROPBITS(5);
            wstate->ncode = BITS(4) + 4;
            DROPBITS(4);
#ifndef PKZIP_BUG_WORKAROUND
            if (rstate->nlen > 286 || rstate->ndist > 30) {
                wstrm->msg = (char *)"too many length or distance symbols";
                wstate->mode = BAD;
                break;
            }
#endif
            Tracev((stderr, "inflate:       table sizes ok\n"));
            wstate->have = 0;
            wstate->mode = LENLENS;
        case LENLENS:
            while (rstate->have < rstate->ncode) {
                NEEDBITS(3);
                wstate->lens[order[wstate->have++]] = (unsigned short)BITS(3);
                DROPBITS(3);
            }
            while (rstate->have < 19)
                wstate->lens[order[wstate->have++]] = 0;
            wstate->next = wstate->codes;
            wstate->lencode = (const code FAR *)(wstate->next);
            wstate->lenbits = 7;
            ret = inflate_table(pop, CODES, wstate->lens, 19, &(wstate->next),
                                &(wstate->lenbits), wstate->work);
            if (ret) {
                wstrm->msg = (char *)"invalid code lengths set";
                wstate->mode = BAD;
                break;
            }
            Tracev((stderr, "inflate:       code lengths ok\n"));
            wstate->have = 0;
            wstate->mode = CODELENS;
        case CODELENS:
            while (rstate->have < rstate->nlen + rstate->ndist) {
                for (;;) {
                    here = rstate->lencode[BITS(rstate->lenbits)];
                    if ((unsigned)(here.bits) <= (*bits)) break;
                    PULLBYTE();
                }
                if (here.val < 16) {
                    DROPBITS(here.bits);
                    wstate->lens[wstate->have++] = here.val;
                }
                else {
                    if (here.val == 16) {
                        NEEDBITS(here.bits + 2);
                        DROPBITS(here.bits);
                        if (rstate->have == 0) {
                            wstrm->msg = (char *)"invalid bit length repeat";
                            wstate->mode = BAD;
                            break;
                        }
                        (*len) = rstate->lens[rstate->have - 1];
                        (*copy) = 3 + BITS(2);
                        DROPBITS(2);
                    }
                    else if (here.val == 17) {
                        NEEDBITS(here.bits + 3);
                        DROPBITS(here.bits);
                        (*len) = 0;
                        (*copy) = 3 + BITS(3);
                        DROPBITS(3);
                    }
                    else {
                        NEEDBITS(here.bits + 7);
                        DROPBITS(here.bits);
                        (*len) = 0;
                        (*copy) = 11 + BITS(7);
                        DROPBITS(7);
                    }
                    if (rstate->have + (*copy) > rstate->nlen + rstate->ndist) {
                        wstrm->msg = (char *)"invalid bit length repeat";
                        wstate->mode = BAD;
                        break;
                    }
                    while ((*copy)--)
                        wstate->lens[wstate->have++] = (unsigned short)((*len));
                }
            }

            /* handle error breaks in while */
            if (rstate->mode == BAD) break;

            /* check for end-of-block code (better have one) */
            if (rstate->lens[256] == 0) {
                wstrm->msg = (char *)"invalid code -- missing end-of-block";
                wstate->mode = BAD;
                break;
            }

            /* build code tables -- note: do not change the lenbits or distbits
               values here (9 and 6) without reading the comments in inftrees.h
               concerning the ENOUGH constants, which depend on those values */
            wstate->next = D_RW(state)->codes;
            wstate->lencode = (const code FAR *)(wstate->next);
            wstate->lenbits = 9;
            ret = inflate_table(pop, LENS, wstate->lens, wstate->nlen, &(wstate->next),
                                &(wstate->lenbits), wstate->work);
            if (ret) {
                wstrm->msg = (char *)"invalid literal/lengths set";
                wstate->mode = BAD;
                break;
            }
            wstate->distcode = (const code FAR *)(wstate->next);
            wstate->distbits = 6;
            ret = inflate_table(pop, DISTS, wstate->lens + wstate->nlen, wstate->ndist,
                            &(wstate->next), &(wstate->distbits), wstate->work);
            if (ret) {
                wstrm->msg = (char *)"invalid distances set";
                wstate->mode = BAD;
                break;
            }
            Tracev((stderr, "inflate:       codes ok\n"));
            wstate->mode = LEN_;
            if (flush == Z_TREES) goto inf_leave;
        case LEN_:
            wstate->mode = LEN;
        case LEN:
            if ((*have) >= 6 && (*left) >= 258) {
                RESTORE();
                inflate_fast(pop, strm, (*out));
                LOAD();
                if (wstate->mode == TYPE)
                    wstate->back = -1;
                break;
            }
            wstate->back = 0;
            for (;;) {
                here = rstate->lencode[BITS(rstate->lenbits)];
                if ((unsigned)(here.bits) <= (*bits)) break;
                PULLBYTE();
            }
            if (here.op && (here.op & 0xf0) == 0) {
                last = here;
                for (;;) {
                    here = rstate->lencode[last.val +
                            (BITS(last.bits + last.op) >> last.bits)];
                    if ((unsigned)(last.bits + here.bits) <= (*bits)) break;
                    PULLBYTE();
                }
                DROPBITS(last.bits);
                wstate->back += last.bits;
            }
            DROPBITS(here.bits);
            wstate->back += here.bits;
            wstate->length = (unsigned)here.val;
            if ((int)(here.op) == 0) {
                Tracevv((stderr, here.val >= 0x20 && here.val < 0x7f ?
                        "inflate:         literal '%c'\n" :
                        "inflate:         literal 0x%02x\n", here.val));
                wstate->mode = LIT;
                break;
            }
            if (here.op & 32) {
                Tracevv((stderr, "inflate:         end of block\n"));
                wstate->back = -1;
                wstate->mode = TYPE;
                break;
            }
            if (here.op & 64) {
                wstrm->msg = (char *)"invalid literal/length code";
                wstate->mode = BAD;
                break;
            }
            wstate->extra = (unsigned)(here.op) & 15;
            wstate->mode = LENEXT;
        case LENEXT:
            if (rstate->extra) {
                NEEDBITS(wstate->extra);
                wstate->length += BITS(rstate->extra);
                DROPBITS(wstate->extra);
                wstate->back += rstate->extra;
            }
            Tracevv((stderr, "inflate:         length %u\n", state->length));
            wstate->was = rstate->length;
            wstate->mode = DIST;
        case DIST:
            for (;;) {
                here = rstate->distcode[BITS(rstate->distbits)];
                if ((unsigned)(here.bits) <= (*bits)) break;
                PULLBYTE();
            }
            if ((here.op & 0xf0) == 0) {
                last = here;
                for (;;) {
                    here = rstate->distcode[last.val +
                            (BITS(last.bits + last.op) >> last.bits)];
                    if ((unsigned)(last.bits + here.bits) <= (*bits)) break;
                    PULLBYTE();
                }
                DROPBITS(last.bits);
                wstate->back += last.bits;
            }
            DROPBITS(here.bits);
            wstate->back += here.bits;
            if (here.op & 64) {
                wstrm->msg = (char *)"invalid distance code";
                wstate->mode = BAD;
                break;
            }
            wstate->offset = (unsigned)here.val;
            wstate->extra = (unsigned)(here.op) & 15;
            wstate->mode = DISTEXT;
        case DISTEXT:
            if (wstate->extra) {
                NEEDBITS(rstate->extra);
                wstate->offset += BITS(rstate->extra);
                DROPBITS(rstate->extra);
                wstate->back += wstate->extra;
            }
#ifdef INFLATE_STRICT
            if (state->offset > state->dmax) {
                strm->msg = (char *)"invalid distance too far back";
                state->mode = BAD;
                break;
            }
#endif
            Tracevv((stderr, "inflate:         distance %u\n", state->offset));
            wstate->mode = MATCH;
        case MATCH:
            if ((*left) == 0) goto inf_leave;
            (*copy) = (*out) - (*left);
            if (rstate->offset > (*copy)) {         /* copy from window */
                (*copy) = rstate->offset - (*copy);
                if ((*copy) > rstate->whave) {
                    if (rstate->sane) {
                        wstrm->msg = (char *)"invalid distance too far back";
                        wstate->mode = BAD;
                        break;
                    }
#ifdef INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR
                    Trace((stderr, "inflate.c too far\n"));
                    copy -= state->whave;
                    if (copy > state->length) copy = state->length;
                    if (copy > left) copy = left;
                    left -= copy;
                    state->length -= copy;
                    do {
                        *put++ = 0;
                    } while (--copy);
                    if (state->length == 0) state->mode = LEN;
                    break;
#endif
                }
                if ((*copy) > rstate->wnext) {
                    (*copy) -= rstate->wnext;
                    from = rstate->window + (rstate->wsize - (*copy));
                }
                else
                    from = wstate->window + (rstate->wnext - (*copy));
                if ((*copy) > rstate->length) ((*copy)) = rstate->length;
            }
            else {                              /* copy from output */
                from = put - rstate->offset;
                (*copy) = rstate->length;
            }
            if ((*copy) > (*left)) ((*copy)) = (*left);
            (*left) -= (*copy);
            wstate->length -= (*copy);
            do {
                *put++ = *from++;
            } while (--(*copy));
            if (rstate->length == 0) wstate->mode = LEN;
            break;
        case LIT:
            if ((*left) == 0) goto inf_leave;
            *put++ = (unsigned char)(wstate->length);
            (*left)--;
            wstate->mode = LEN;
            break;
        case CHECK:
            if (rstate->wrap) {
                NEEDBITS(32);
                (*out) -= (*left);
                wstrm->total_out += (*out);
                wstate->total += (*out);
                if ((rstate->wrap & 4) && (*out))
                    wstrm->adler = wstate->check =
                        UPDATE(rstate->check, put - (*out), (*out));
                *out = *left;
                if ((rstate->wrap & 4) && (

                     ZSWAP32((*hold))) != rstate->check) {
                    wstrm->msg = (char *)"incorrect data check";
                    wstate->mode = BAD;
                    break;
                }
                INITBITS();
                Tracev((stderr, "inflate:   check matches trailer\n"));
            }

            wstate->mode = DONE;
            pmemobj_persist(pop, D_RW(strm), sizeof(*D_RW(strm)));
            pmemobj_persist(pop, D_RW(state), sizeof(*D_RW(state)));
        case DONE:
            ret = Z_STREAM_END;
            goto inf_leave;
        case BAD:
            ret = Z_DATA_ERROR;
            goto inf_leave;
        case MEM:
            return Z_MEM_ERROR;
        case SYNC:
        default:
            return Z_STREAM_ERROR;
        }

    /*
       Return from inflate(), updating the total counts and the check value.
       If there was no progress during the inflate() call, return a buffer
       error.  Call updatewindow() to create and/or update the window state.
       Note: a memory error from inflate() is non-recoverable.
     */
  inf_leave:
    RESTORE();
    if (rstate->wsize || ((*out) != D_RO(strm)->avail_out && rstate->mode < BAD &&
            (rstate->mode < CHECK || flush != Z_FINISH)))
        if (updatewindow(pop, strm, wstrm->next_out, (*out) - wstrm->avail_out)) {
            wstate->mode = MEM;
            return Z_MEM_ERROR;
        }
    (*in) -= D_RO(strm)->avail_in;
    (*out) -= D_RO(strm)->avail_out;
    wstrm->total_in += (*in);
    wstrm->total_out += (*out);
    wstate->total += (*out);
    if ((rstate->wrap & 4) && (*out))
        wstrm->adler = wstate->check =
            UPDATE(rstate->check, D_RO(strm)->next_out - (*out), (*out));
    wstrm->data_type = (int)rstate->bits + (rstate->last ? 64 : 0) +
                      (rstate->mode == TYPE ? 128 : 0) +
                      (rstate->mode == LEN_ || rstate->mode == COPY_ ? 256 : 0);
    if ((((*in) == 0 && (*out) == 0) || flush == Z_FINISH) && ret == Z_OK)
        ret = Z_BUF_ERROR;
    pmemobj_persist(pop, D_RW(strm), sizeof(*D_RW(strm)));
    pmemobj_persist(pop, D_RW(state), sizeof(*D_RW(state)));
    POBJ_FREE(&have_);
    POBJ_FREE(&left_);
    POBJ_FREE(&hold_);
    POBJ_FREE(&bits_);
    POBJ_FREE(&in_);
    POBJ_FREE(&out_);
    POBJ_FREE(&copy_);
    POBJ_FREE(&len_);

    return ret;
}

int ZEXPORT inflateEnd(strm)
TOID(struct z_stream) strm;
{
    //struct inflate_state FAR *state;
    TOID(struct inflate_state) state;
    if (inflateStateCheck(strm))
        return Z_STREAM_ERROR;
    state = D_RW(strm)->istate;
    //if (D_RO(state)->window != Z_NULL) ZFREE(D_RW(strm), D_RW(state)->window);
    //ZFREE(strm, strm->state);
    POBJ_FREE(&D_RW(strm)->windowp);
    POBJ_FREE(&state);
    POBJ_FREE(&D_RW(strm)->istate);
    //POBJ_FREE(D_RW(strm)->state)
    Tracev((stderr, "inflate: end\n"));
    return Z_OK;
}

// int ZEXPORT inflateGetDictionary(strm, dictionary, dictLength)
// z_streamp strm;
// Bytef *dictionary;
// uInt *dictLength;
// {
//     struct inflate_state FAR *state;

//     /* check state */
//     if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)strm->state;

//     /* copy dictionary */
//     if (state->whave && dictionary != Z_NULL) {
//         zmemcpy(dictionary, state->window + state->wnext,
//                 state->whave - state->wnext);
//         zmemcpy(dictionary + state->whave - state->wnext,
//                 state->window, state->wnext);
//     }
//     if (dictLength != Z_NULL)
//         *dictLength = state->whave;
//     return Z_OK;
// }

// int ZEXPORT inflateSetDictionary(strm, dictionary, dictLength)
// z_streamp strm;
// const Bytef *dictionary;
// uInt dictLength;
// {
//     struct inflate_state FAR *state;
//     unsigned long dictid;
//     int ret;

//     /* check state */
//     if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)strm->state;
//     if (state->wrap != 0 && state->mode != DICT)
//         return Z_STREAM_ERROR;

//     /* check for correct dictionary identifier */
//     if (state->mode == DICT) {
//         dictid = adler32(0L, Z_NULL, 0);
//         dictid = adler32(dictid, dictionary, dictLength);
//         if (dictid != state->check)
//             return Z_DATA_ERROR;
//     }

//     /* copy dictionary to window using updatewindow(), which will amend the
//        existing dictionary if appropriate */
//     ret = updatewindow(strm, dictionary + dictLength, dictLength);
//     if (ret) {
//         state->mode = MEM;
//         return Z_MEM_ERROR;
//     }
//     state->havedict = 1;
//     Tracev((stderr, "inflate:   dictionary set\n"));
//     return Z_OK;
// }

// int ZEXPORT inflateGetHeader(strm, head)
// z_streamp strm;
// gz_headerp head;
// {
//     struct inflate_state FAR *state;

//     /* check state */
//     if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)strm->state;
//     if ((state->wrap & 2) == 0) return Z_STREAM_ERROR;

//     /* save header structure */
//     state->head = head;
//     head->done = 0;
//     return Z_OK;
// }

// /*
//    Search buf[0..len-1] for the pattern: 0, 0, 0xff, 0xff.  Return when found
//    or when out of input.  When called, *have is the number of pattern bytes
//    found in order so far, in 0..3.  On return *have is updated to the new
//    state.  If on return *have equals four, then the pattern was found and the
//    return value is how many bytes were read including the last byte of the
//    pattern.  If *have is less than four, then the pattern has not been found
//    yet and the return value is len.  In the latter case, syncsearch() can be
//    called again with more data and the *have state.  *have is initialized to
//    zero for the first call.
//  */
// local unsigned syncsearch(have, buf, len)
// unsigned FAR *have;
// const unsigned char FAR *buf;
// unsigned len;
// {
//     unsigned got;
//     unsigned next;

//     got = *have;
//     next = 0;
//     while (next < len && got < 4) {
//         if ((int)(buf[next]) == (got < 2 ? 0 : 0xff))
//             got++;
//         else if (buf[next])
//             got = 0;
//         else
//             got = 4 - got;
//         next++;
//     }
//     *have = got;
//     return next;
// }

// int ZEXPORT inflateSync(strm)
// z_streamp strm;
// {
//     unsigned len;               /* number of bytes to look at or looked at */
//     unsigned long in, out;      /* temporary to save total_in and total_out */
//     unsigned char buf[4];       /* to restore bit buffer to byte string */
//     struct inflate_state FAR *state;

//     /* check parameters */
//     if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)strm->state;
//     if (strm->avail_in == 0 && state->bits < 8) return Z_BUF_ERROR;

//     /* if first time, start search in bit buffer */
//     if (state->mode != SYNC) {
//         state->mode = SYNC;
//         state->hold <<= state->bits & 7;
//         state->bits -= state->bits & 7;
//         len = 0;
//         while (state->bits >= 8) {
//             buf[len++] = (unsigned char)(state->hold);
//             state->hold >>= 8;
//             state->bits -= 8;
//         }
//         state->have = 0;
//         syncsearch(&(state->have), buf, len);
//     }

//     /* search available input */
//     len = syncsearch(&(state->have), strm->next_in, strm->avail_in);
//     strm->avail_in -= len;
//     strm->next_in += len;
//     strm->total_in += len;

//     /* return no joy or set up to restart inflate() on a new block */
//     if (state->have != 4) return Z_DATA_ERROR;
//     in = strm->total_in;  out = strm->total_out;
//     inflateReset(strm);
//     strm->total_in = in;  strm->total_out = out;
//     state->mode = TYPE;
//     return Z_OK;
// }

// /*
//    Returns true if inflate is currently at the end of a block generated by
//    Z_SYNC_FLUSH or Z_FULL_FLUSH. This function is used by one PPP
//    implementation to provide an additional safety check. PPP uses
//    Z_SYNC_FLUSH but removes the length bytes of the resulting empty stored
//    block. When decompressing, PPP checks that at the end of input packet,
//    inflate is waiting for these length bytes.
//  */
// int ZEXPORT inflateSyncPoint(strm)
// z_streamp strm;
// {
//     struct inflate_state FAR *state;

//     if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)strm->state;
//     return state->mode == STORED && state->bits == 0;
// }

// int ZEXPORT inflateCopy(dest, source)
// z_streamp dest;
// z_streamp source;
// {
//     struct inflate_state FAR *state;
//     struct inflate_state FAR *copy;
//     unsigned char FAR *window;
//     unsigned wsize;

//     /* check input */
//     if (inflateStateCheck(source) || dest == Z_NULL)
//         return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)source->state;

//     /* allocate space */
//     copy = (struct inflate_state FAR *)
//            ZALLOC(source, 1, sizeof(struct inflate_state));
//     if (copy == Z_NULL) return Z_MEM_ERROR;
//     window = Z_NULL;
//     if (state->window != Z_NULL) {
//         window = (unsigned char FAR *)
//                  ZALLOC(source, 1U << state->wbits, sizeof(unsigned char));
//         if (window == Z_NULL) {
//             ZFREE(source, copy);
//             return Z_MEM_ERROR;
//         }
//     }

//     /* copy state */
//     zmemcpy((voidpf)dest, (voidpf)source, sizeof(z_stream));
//     zmemcpy((voidpf)copy, (voidpf)state, sizeof(struct inflate_state));
//     copy->strm = dest;
//     if (state->lencode >= state->codes &&
//         state->lencode <= state->codes + ENOUGH - 1) {
//         copy->lencode = copy->codes + (state->lencode - state->codes);
//         copy->distcode = copy->codes + (state->distcode - state->codes);
//     }
//     copy->next = copy->codes + (state->next - state->codes);
//     if (window != Z_NULL) {
//         wsize = 1U << state->wbits;
//         zmemcpy(window, state->window, wsize);
//     }
//     copy->window = window;
//     dest->state = (struct internal_state FAR *)copy;
//     return Z_OK;
// }

// int ZEXPORT inflateUndermine(strm, subvert)
// z_streamp strm;
// int subvert;
// {
//     struct inflate_state FAR *state;

//     if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)strm->state;
// #ifdef INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR
//     state->sane = !subvert;
//     return Z_OK;
// #else
//     (void)subvert;
//     state->sane = 1;
//     return Z_DATA_ERROR;
// #endif
// }

// int ZEXPORT inflateValidate(strm, check)
// z_streamp strm;
// int check;
// {
//     struct inflate_state FAR *state;

//     if (inflateStateCheck(strm)) return Z_STREAM_ERROR;
//     state = (struct inflate_state FAR *)strm->state;
//     if (check)
//         state->wrap |= 4;
//     else
//         state->wrap &= ~4;
//     return Z_OK;
// }

// long ZEXPORT inflateMark(strm)
// z_streamp strm;
// {
//     struct inflate_state FAR *state;

//     if (inflateStateCheck(strm))
//         return -(1L << 16);
//     state = (struct inflate_state FAR *)strm->state;
//     return (long)(((unsigned long)((long)state->back)) << 16) +
//         (state->mode == COPY ? state->length :
//             (state->mode == MATCH ? state->was - state->length : 0));
// }

// unsigned long ZEXPORT inflateCodesUsed(strm)
// z_streamp strm;
// {
//     struct inflate_state FAR *state;
//     if (inflateStateCheck(strm)) return (unsigned long)-1;
//     state = (struct inflate_state FAR *)strm->state;
//     return (unsigned long)(state->next - state->codes);
// }
