/* inffast.c -- fast decoding
 * Copyright (C) 1995-2017 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

#include "zutil.h"
#include "inftrees.h"
#include "inflate.h"
#include "inffast.h"

#ifdef ASMINF
#  pragma message("Assembler code may have bugs -- use at your own risk")
#else

/*
   Decode literal, length, and distance codes and write out the resulting
   literal and match bytes until either not enough input or output is
   available, an end-of-block is encountered, or a data error is encountered.
   When large enough input and output buffers are supplied to inflate(), for
   example, a 16K input buffer and a 64K output buffer, more than 95% of the
   inflate execution time is spent in this routine.

   Entry assumptions:

        state->mode == LEN
        strm->avail_in >= 6
        strm->avail_out >= 258
        start >= strm->avail_out
        state->bits < 8

   On return, state->mode is one of:

        LEN -- ran out of enough output space or enough available input
        TYPE -- reached end of block code, inflate() to interpret next block
        BAD -- error in block data

   Notes:

    - The maximum input bits used by a length/distance pair is 15 bits for the
      length code, 5 bits for the length extra, 15 bits for the distance code,
      and 13 bits for the distance extra.  This totals 48 bits, or six bytes.
      Therefore if strm->avail_in >= 6, then there is enough input to avoid
      checking for available input while decoding.

    - The maximum bytes that a single length/distance pair can output is 258
      bytes, which is the maximum length that can be coded.  inflate_fast()
      requires strm->avail_out >= 258 for each loop to avoid checking for
      output space.
 */
void ZLIB_INTERNAL inflate_fast(pop, strm, start)
PMEMobjpool *pop;
TOID(struct z_stream) strm;
unsigned start;         /* inflate()'s starting value for strm->avail_out */
{
    TOID(struct inflate_state) state;
    z_const unsigned char FAR *in;      /* local strm->next_in */
    z_const unsigned char FAR *last;    /* have enough input while in < last */
    unsigned char FAR *out;     /* local strm->next_out */
    unsigned char FAR *beg;     /* inflate()'s initial strm->next_out */
    unsigned char FAR *end;     /* while out < end, enough space available */
#ifdef INFLATE_STRICT
    unsigned dmax;              /* maximum distance from zlib header */
#endif
    // unsigned wsize;             /* window size or zero if not using window */
    // unsigned whave;             /* valid bytes in the window */
    // unsigned wnext;             /* window write index */
    unsigned char FAR *window;  /* allocated sliding window, if wsize != 0 */
    // unsigned long hold;         /* local strm->hold */
    // unsigned bits;              /* local strm->bits */
    // code const FAR *lcode;      /* local strm->lencode */
    // code const FAR *dcode;      /* local strm->distcode */
    // unsigned lmask;             /* mask for first level of length codes */
    // unsigned dmask;             /* mask for first level of distance codes */
    // code here;                  /* retrieved table entry */
    // unsigned op;                /* code bits, operation, extra bits, or */
    //                             /*  window position, window bytes to copy */
    // unsigned len;               /* match length, unused bytes */
    // unsigned dist;              /* match distance */
    unsigned char FAR *from;    /* where to copy match from */

    TOID(uint) wsize;             /* window size or zero if not using window */
    TOID(uint) whave;             /* valid bytes in the window */
    TOID(uint) wnext;             /* window write index */
    //TOID(Byte) window;  /* allocated sliding window, if wsize != 0 */
    TOID(ulong) hold;         /* local strm->hold */
    TOID(uint) bits;              /* local strm->bits */
    code const FAR *lcode;      /* local strm->lencode */
    code const FAR *dcode;      /* local strm->distcode */
    TOID(uint) lmask;             /* mask for first level of length codes */
    TOID(uint) dmask;             /* mask for first level of distance codes */
    code here;                  /* retrieved table entry */
    TOID(uint) op;                /* code bits, operation, extra bits, or */
                                /*  window position, window bytes to copy */
    TOID(uint) len;               /* match length, unused bytes */
    TOID(uint) dist;              /* match distance */
    //TOID(Byte) from;    /* where to copy match from */

    /* copy state to local variables */
    state = D_RW(strm)->istate;
    struct z_stream *wstrm = D_RW(strm);
    const struct z_stream *rstrm = D_RO(strm);
    struct inflate_state *wstate = D_RW(state);
    const struct inflate_state *rstate = D_RO(state);

    in = wstrm->next_in;
    last = in + (rstrm->avail_in - 5);
    out = wstrm->next_out;
    beg = out - (start - rstrm->avail_out);
    end = out + (rstrm->avail_out - 257);
#ifdef INFLATE_STRICT
    dmax = state->dmax;
#endif
    POBJ_ALLOC(pop, &wsize, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &whave, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &wnext, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &hold, ulong, sizeof(ulong), NULL, NULL);
    POBJ_ALLOC(pop, &bits, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &lmask, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &dmask, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &op, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &len, uint, sizeof(uint), NULL, NULL);
    POBJ_ALLOC(pop, &dist, uint, sizeof(uint), NULL, NULL);
    
    *D_RW(wsize) = rstate->wsize;
    *D_RW(whave) = rstate->whave;
    *D_RW(wnext) = rstate->wnext;
    *D_RW(hold) = rstate->hold;
    *D_RW(bits) = rstate->bits;
    lcode = wstate->lencode;
    dcode = wstate->distcode;
    *D_RW(lmask) = (1U << rstate->lenbits) - 1;
    *D_RW(dmask) = (1U << rstate->distbits) - 1;
    

    unsigned *whave_= D_RW(whave);
    const unsigned *whave_c = D_RO(whave);
    unsigned *wsize_ = D_RW(wsize);
    const unsigned *wsize_c = D_RO(wsize);
    unsigned *wnext_ = D_RW(wnext);
    const unsigned *wnext_c = D_RO(wnext);
    unsigned long *hold_ = D_RW(hold);
    const unsigned long *hold_c = D_RO(hold);
    unsigned *bits_ = D_RW(bits);
    const unsigned *bits_c = D_RO(bits);
    unsigned *lmask_ = D_RW(lmask);
    const unsigned *lmask_c = D_RO(lmask);
    unsigned *dmask_ = D_RW(dmask);
    const unsigned *dmask_c = D_RO(dmask);

    unsigned *op_w = D_RW(op);
    const unsigned *op_c = D_RO(op);
    unsigned *lenw = D_RW(len);
    const unsigned *lenc = D_RO(len);
    unsigned *distw = D_RW(dist);
    /* decode literals and length/distances until end-of-block or not enough
       input data or output space */
    do {
        if (*bits_c < 15) {
            *hold_ += (unsigned long)(*in++) << *bits_c;
            *bits_ += 8;
            *hold_ += (unsigned long)(*in++) << *bits_c;
            *bits_ += 8;
        }
        here = lcode[*hold_c & *lmask_c];
      dolen:
        *op_w = (unsigned)(here.bits);
        *hold_ >>= *op_c;
        *bits_ -= *op_c;
        *op_w = (unsigned)(here.op);
        if (*op_c == 0) {                          /* literal */
            Tracevv((stderr, here.val >= 0x20 && here.val < 0x7f ?
                    "inflate:         literal '%c'\n" :
                    "inflate:         literal 0x%02x\n", here.val));
            *out++ = (unsigned char)(here.val);
        }
        else if (*op_c & 16) {                     /* length base */
            *lenw = (unsigned)(here.val);
            *op_w &= 15;                           /* number of extra bits */
            if (*op_w) {
                if (*bits_c < *op_w) {
                    *hold_ += (unsigned long)(*in++) << *bits_c;
                    *bits_ += 8;
                }
                *lenw += (unsigned)*hold_c & ((1U << *op_c) - 1);
                *hold_ >>= *op_c;
                *bits_ -= *op_c;
            }
            Tracevv((stderr, "inflate:         length %u\n", len));
            if (*bits_c < 15) {
                *hold_ += (unsigned long)(*in++) << *bits_c;
                *bits_ += 8;
                *hold_ += (unsigned long)(*in++) << *bits_c;
                *bits_ += 8;
            }
            here = dcode[*hold_c & *lmask_c];
          dodist:
            *op_w = (unsigned)(here.bits);
            *hold_ >>= *op_w;
            *bits_ -= *op_w;
            *op_w = (unsigned)(here.op);
            if (*op_w & 16) {                      /* distance base */
                *distw = (unsigned)(here.val);
                *op_w &= 15;                       /* number of extra bits */
                if (*bits_c < *op_w) {
                    *hold_ += (unsigned long)(*in++) << *bits_c;
                    *bits_ += 8;
                    if (*bits_c < *op_w) {
                        *hold_ += (unsigned long)(*in++) << *bits_c;
                        *bits_ += 8;
                    }
                }
               *distw += (unsigned)*hold_c & ((1U << *op_w) - 1);
#ifdef INFLATE_STRICT
                if (dist > dmax) {
                    strm->msg = (char *)"invalid distance too far back";
                    state->mode = BAD;
                    break;
                }
#endif
                *hold_ >>= *op_w;
                *bits_ -= *op_w;
                Tracevv((stderr, "inflate:         distance %u\n", dist));
                *op_w = (unsigned)(out - beg);     /* max distance in output */
                if (*distw > *op_w) {                /* see if copy from window */
                   *op_w = *distw - *op_w;             /* distance back in window */
                    if (*op_w > *whave_c) {
                        if (rstate->sane) {
                            wstrm->msg =
                                (char *)"invalid distance too far back";
                            wstate->mode = BAD;
                            break;
                        }
#ifdef INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR
                        if (len <=*op_w - whave) {
                            do {
                                *out++ = 0;
                            } while (--len);
                            continue;
                        }
                        len -=*op_w - whave;
                        do {
                            *out++ = 0;
                        } while (--op > whave);
                        if (op == 0) {
                            from = out - dist;
                            do {
                                *out++ = *from++;
                            } while (--len);
                            continue;
                        }
#endif
                    }
                    from = wstate->window;
                    if (*wnext_c == 0) {           /* very common case */
                        from += *wsize_c - *op_w;
                        if (*op_w < *lenw) {         /* some from window */
                            *lenw -= *op_w;
                            do {
                                *out++ = *from++;
                            } while (--*op_w);
                            from = out - *distw;  /* rest from output */
                        }
                    }
                    else if (*wnext_c <*op_w) {      /* wrap around window */
                        from += *wsize_c + *wnext_c -*op_w;
                       *op_w -= *wnext_c;
                        if (*op_w < *lenw) {         /* some from end of window */
                            *lenw -=*op_w;
                            do {
                                *out++ = *from++;
                            } while (--*op_w);
                            from = wstate->window;
                            if (*wnext_c < *lenw) {  /* some from start of window */
                               *op_w = *wnext_c;
                                *lenw -=*op_w;
                                do {
                                    *out++ = *from++;
                                } while (--*op_w);
                                from = out - *distw;      /* rest from output */
                            }
                        }
                    }
                    else {                      /* contiguous in window */
                        from += *wnext_c -*op_w;
                        if (*op_w < *lenw) {         /* some from window */
                            *lenw -= *op_w;
                            do {
                                *out++ = *from++;
                            } while (-- *op_w);
                            from = out - *distw;  /* rest from output */
                        }
                    }
                    while (*lenw > 2) {
                        *out++ = *from++;
                        *out++ = *from++;
                        *out++ = *from++;
                        *lenw -= 3;
                    }
                    if (*lenw) {
                        *out++ = *from++;
                        if (*lenw > 1)
                            *out++ = *from++;
                    }
                }
                else {
                    from = out - *distw;          /* copy direct from output */
                    do {                        /* minimum length is three */
                        *out++ = *from++;
                        *out++ = *from++;
                        *out++ = *from++;
                        *lenw -= 3;
                    } while (*lenw > 2);
                    if (*lenw) {
                        *out++ = *from++;
                        if (*lenw > 1)
                            *out++ = *from++;
                    }
                }
            }
            else if ((*op_w & 64) == 0) {          /* 2nd level distance code */
                here = dcode[here.val + (*hold_c & ((1U <<*op_w) - 1))];
                goto dodist;
            }
            else {
                wstrm->msg = (char *)"invalid distance code";
                wstate->mode = BAD;
                break;
            }
        }
        else if ((*op_w & 64) == 0) {              /* 2nd level length code */
            here = lcode[here.val + (*hold_c & ((1U <<*op_w) - 1))];
            goto dolen;
        }
        else if (*op_w & 32) {                     /* end-of-block */
            Tracevv((stderr, "inflate:         end of block\n"));
            wstate->mode = TYPE;
            break;
        }
        else {
            wstrm->msg = (char *)"invalid literal/length code";
            wstate->mode = BAD;
            break;
        }
    } while (in < last && out < end);

    /* return unused bytes (on entry, bits < 8, so in won't go too far back) */
    *lenw = *bits_c >> 3;
    in -= *lenw;
    *bits_ -= *lenw << 3;
    *hold_ &= (1U << *bits_c) - 1;

    /* update state and return */
    wstrm->next_in = in;
    wstrm->next_out = out;
    wstrm->avail_in = (unsigned)(in < last ? 5 + (last - in) : 5 - (in - last));
    wstrm->avail_out = (unsigned)(out < end ?
                                 257 + (end - out) : 257 - (out - end));
    wstate->hold = *hold_c;
    wstate->bits = *bits_c;
    pmemobj_persist(pop, wstrm, sizeof(*wstrm));
    pmemobj_persist(pop, D_RW(state), sizeof(*D_RW(state)));

    POBJ_FREE(&wsize);
    POBJ_FREE(&whave);
    POBJ_FREE(&wnext);
    POBJ_FREE(&hold);
    POBJ_FREE(&bits);
    POBJ_FREE(&lmask);
    POBJ_FREE(&dmask);
    POBJ_FREE(&op);
    POBJ_FREE(&len);
    POBJ_FREE(&dist);
    return;
}

/*
   inflate_fast() speedups that turned out slower (on a PowerPC G3 750CXe):
   - Using bit fields for code structure
   - Different*op_w definition to avoid & for extra bits (do & for table bits)
   - Three separate decoding do-loops for direct, window, and wnext == 0
   - Special case for distance > 1 copies to do overlapped load and store copy
   - Explicit branch predictions (based on measured branch probabilities)
   - Deferring match copy and interspersed it with decoding subsequent codes
   - Swapping literal/length else
   - Swapping window/direct else
   - Larger unrolled copy loops (three is about right)
   - Moving len -= 3 statement into middle of loop
 */

#endif /* !ASMINF */
