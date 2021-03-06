#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "misc.h"
#include "lz77.h"
#include <libpmemobj.h>
//#include "ex_common.h"
//#include <sys/stat.h>
#include "layout.h"
#define MAXMATCHDIST 32768	       /* maximum backward distance */
#define MAXMATCHLEN 258		       /* maximum length of a match */
#define HASHMAX 2039		       /* one more than max hash value */
#define HASHCHARS 3		       /* how many chars make a hash */
#define MAXLAZY 3		       /* limit of lazy matching */
#define MAXREWIND (HASHCHARS * 2)
#define WINSIZE (MAXMATCHDIST + HASHCHARS + MAXREWIND)
#define NMATCHES (MAXLAZY + MAXLAZY * MAXLAZY)
#define TOO_FAR 4096
#define MAXCHAIN 16
//#define LAZY_MATCHING_DONE 0
//#define LONGEST_MATCH 0
//#define MAXLEN 4954496 

//POBJ_LAYOUT_BEGIN(rstore);
//POBJ_LAYOUT_ROOT(rstore, struct my_root);
//POBJ_LAYOUT_END(rstore);


//struct my_root
//{
//	char r[MAXLEN + 1];
//};

struct LZ77 
{
    /*
     * Administrative data passed in from lz77_new().
     */
    void *ctx;
    void (*literal)(LZ77 *lz, void *ctx, unsigned char c);
    void (*match)(LZ77 *lz, void *ctx, int distance, int len);

    
    unsigned char data[WINSIZE+HASHCHARS];
	int lzlen;
	int datalen;
	int winpos;

    
    int k;

    
    int nvalid;

    
    int rewound;

    
    int hashnext[WINSIZE];

    
    int hashhead[HASHMAX];

    
    int matchnext[WINSIZE];

    
    int matchhead[NMATCHES];

    
    int matchlen[NMATCHES], matchdist[NMATCHES];

    
    int matchlater[HASHCHARS*HASHCHARS];
    int nextindex;

	int prev[WINSIZE];

	unsigned char literals[HASHCHARS * (HASHCHARS+1)];
	//unsigned char currchar[HASHCHARS];
	int matchstart;
	unsigned char result[MAXLEN + 1];
	int resultlen;
};

static int lz77_hash(const unsigned char *data) {
    return (257*data[0] + 263*data[1] + 269*data[2]) % HASHMAX;
}

LZ77 *lz77_new(void (*literal)(void *ctx, unsigned char c),
	       void (*match)(void *ctx, int distance, int len),
	       void *ctx)
{
    LZ77 *lz;
    int i;

    lz = (LZ77 *)malloc(sizeof(LZ77));
    if (!lz)
	return NULL;

    lz->ctx = ctx;
    lz->literal = literal;
    lz->match = match;

    memset(lz->data, 0, sizeof(lz->data));

    for (i = 0; i < WINSIZE; i++) {
	lz->hashnext[i] = -1;
	lz->matchnext[i] = 0;
    }

    for (i = 0; i < HASHMAX; i++) {
	lz->hashhead[i] = -1;
    }

    for (i = 0; i < NMATCHES; i++) {
	lz->matchhead[i] = -1;
	lz->matchlen[i] = 0;
	lz->matchdist[i] = 0;
    }

    for (i = 0; i < HASHCHARS * (HASHCHARS+1); i++) {
	lz->literals[i] = 0;
    }

    for (i = 0; i < HASHCHARS * HASHCHARS; i++) {
	lz->matchlater[i] = 0;
    }

    lz->nextindex = HASHCHARS;
    lz->winpos = 0;
    lz->k = 0;
    lz->nvalid = 0;
    lz->rewound = 0;

	for (i = 0; i < MAXLEN; i++)
		lz->result[i] = 0;

	lz->resultlen = 0;
	return lz;
}

void lz77_free(LZ77 *lz)
{
    free(lz);
}

static void lz77_hashsearch(LZ77 *lz, int hash, unsigned char *currchars,
			    int index)
{
    int pos, nextpos, matchlimit;
    int posval, nextposval;

    //assert(lz->matchhead[index] < 0);
    //assert(lz->matchdist[index] == 0);
    //assert(lz->matchlen[index] == 0);

    matchlimit = (lz->nvalid < MAXMATCHDIST ? lz->nvalid : MAXMATCHDIST);
    matchlimit = (matchlimit + lz->winpos) % WINSIZE;

    pos = lz->hashhead[hash];
	//printf(" pos=%d ", pos);
	posval = (matchlimit + WINSIZE - pos) % WINSIZE;
    while (pos != -1) 
	{
		
		int bdist;
		int i;

		bdist = (pos + WINSIZE - lz->winpos) % WINSIZE;
		// if(hash == 617)
		// printf(" bdist=%d, matchnext=%d", bdist, lz->matchnext[bdist]);
		if (bdist > 0 && bdist <= MAXMATCHDIST) //&& !lz->matchnext[bdist]) 
		{
			//printf(" in search ");
			for (i = 0; i < HASHCHARS; i++)
				{
					// printf("%c", lz->data[pos + i]);
					// printf(" ");
					// printf("%c", currchars[i]);
					// printf(" ");
					if (lz->data[pos + i] != currchars[i])
						break;
				}
			if (i == HASHCHARS) 
			{
				//lz->matchnext[bdist] = lz->matchhead[index];
				lz->matchhead[index] = bdist;
				//printf("%d", lz->matchhead[index]);
				// if(hash == 617)
				// 	printf(" matchhead=%d", lz->matchhead[index]);
				if (!lz->matchlen[index]) 
				{
					lz->matchlen[index] = HASHCHARS;
					lz->matchdist[index] = bdist;
				}
	    	}
		}

	/*
	 * Step along the hash chain to try the next candidate
	 * location. If we've gone past matchlimit, stop.
	 * 
	 * Special case: we could also link to ourself here. This
	 * occurs when this window position is repeating a previous
	 * hash and nothing else has had the same hash in between.
	 */
	//printf(" matchhead0 = %d ", lz->matchhead[0]);
	//printf(" matchhead1 = %d ", lz->matchhead[1]);
		nextpos = lz->hashnext[pos];
		nextposval = (matchlimit + WINSIZE - nextpos) % WINSIZE;
		if (nextposval >= posval)
		{
			break;
		} else {
		pos = nextpos;
		posval = nextposval;
		}
    }
}

int lz77_lazysearch(LZ77 *lz, int index1, int index2)
{
	int posi1, posi2, matchdist1, matchdist2, bestmatchdist;
	int chaincount = 0;
	// matchdist1 = lz->matchhead[index1];
	// matchdist2 = lz->matchhead[index2];
	// //int a = lz->hashhead[index2];
	// //printf("dist1 = %d, dist2 = %d, a = %d", matchdist1, matchdist2, a);
	// if(matchdist2 <= 0)
	// 	return 0;
	// /* ????????????????????????????????????match?????????????????????matchnext??????????????? */
	// while(matchdist1 != matchdist2)
	// {
	// 	matchdist1 = lz->matchnext[matchdist1];
	// 	matchdist2 = lz->matchnext[matchdist2];
	// 	chaincount++;
	// 	if((matchdist1 <= 0 || matchdist2 <= 0) || chaincount == MAXCHAIN)
	// 		return 0;	/* ??????????????????????????????function????????????????????? */
	// }
	// return 1;
	posi1 = lz->matchhead[index1];
	posi2 = lz->matchhead[index2];
	//printf(" hash=%d ", lz->matchhead[index2]);
	if(posi2 <= 0)
		return 0;
	matchdist1 = posi1;
	matchdist2 = posi2;

	while(matchdist1 != matchdist2)
	{
		chaincount++;
		posi1 = lz->hashnext[posi1];
		posi2 = lz->hashnext[posi2];
		matchdist1 = posi1 - lz->winpos;
		matchdist2 = posi2 - lz->winpos;
		if(posi1 == -1 || posi2 == -1 || chaincount == MAXCHAIN)
			return 0;
	}
	return 1;
}

void lz77_cleanmatch(LZ77 *lz, int start, int count)
{
	for (start; start <= count; start++)
		{
			lz->matchdist[start] = lz->matchlen[start] = 0;
			// while(lz->matchhead[count] >= 0)
			// {
			// 	int tmp = lz->matchhead[count];
			// 	lz->matchhead[count] = lz->matchnext[tmp];
			// 	lz->matchnext[tmp] = 0;
			// }
			lz->matchhead[start] = -1;
		}
}

void lz77_compress(LZ77 *lz, const void *vdata, int len)
{
	const unsigned char *data = (const unsigned char *)vdata;
	int LAZY_MATCHING_DONE = 0;
	int LONGEST_MATCH = 0;

	int hash, searchindex, i, newchardist;
	unsigned char currchars[HASHCHARS];
	while(len > 0)
	{
		len--;
		int hash;
		lz->winpos = (lz->winpos + WINSIZE-1) % WINSIZE;
		lz->data[lz->winpos] = *data++;

		lz->k++;
		if (lz->nvalid < WINSIZE)
	    	lz->nvalid++;
		
		if (lz->k < HASHCHARS)
	    	continue;
		
		
	    for (i = 0; i < HASHCHARS; i++)
			currchars[i] = lz->data[lz->winpos + i];
		
		hash = lz77_hash(currchars);
		
		// if(hash == 617)
		// 	printf("got");
		// if(searchindex == 1)
		// 	printf(" hash1=%d ", hash);
		// if(searchindex == 1)
		// 	printf(" index1=%d ", lz->hashhead[hash]);
		//printf(" %d ", lz->k);
		if (lz->k >= HASHCHARS) 
		{
			searchindex = lz->k - HASHCHARS;
			// if(hash == 617)
			// {
			// 	printf(" %d ", searchindex);
			// 	printf(" winpos = %d", lz->winpos);
			// }
			lz77_hashsearch(lz, hash, currchars, searchindex);
		}
		
		

		/*
		 * save the literal at this position
		 */
		if (lz->k >= HASHCHARS) //&& lz->k <= HASHCHARS + MAXLAZY
		{
			lz->literals[lz->k - HASHCHARS] = lz->data[lz->winpos + HASHCHARS - 1];
		}
		
		
		/* ????????????????????????3??????????????????????????? */
		if (lz->k == HASHCHARS && lz->matchhead[0] < 0) 
		{
			lz->literal(lz, lz->ctx, currchars[HASHCHARS-1]);
			lz->k--;
		}
		
		
		

		/* lazy matching??????????????????????????????????????????????????????????????????longest match */
		while(LAZY_MATCHING_DONE == 1 && LONGEST_MATCH == 1)
		{
			newchardist = lz->matchhead[searchindex];
			/* ????????????match?????????????????????????????????????????????0???1???2 */
			for (lz->matchstart = 0; lz->matchstart <= 2; lz->matchstart++)
			{
				if(lz->matchhead[lz->matchstart] == -1)
					continue;
				else
					//printf("%d", lz->matchstart);
					break;
			}
			
			if(lz->k == MAXMATCHLEN)
			{
				lz->match(lz, lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
				lz->k -= lz->matchlen[lz->matchstart];
				LONGEST_MATCH = 0;
				LAZY_MATCHING_DONE = 0;
				lz77_cleanmatch(lz, 0, searchindex);
				break;
			}
			/* ?????????????????????????????????????????????????????????match */
			if(newchardist == -1)
			{
				//printf("aa");
				lz->match(lz, lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
				lz->k -= lz->matchlen[lz->matchstart];
				LONGEST_MATCH = 0;
				LAZY_MATCHING_DONE = 0;
				lz77_cleanmatch(lz, 0, searchindex);
				break;
			}
			else if (newchardist >= 0)
			{
				/* ?????????????????????????????????????????????????????????????????????????????? */	

				//printf("here?");
				if(newchardist != lz->matchdist[lz->matchstart])
				{
					//printf("x");
					lz->match(lz, lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
					lz->k -= lz->matchlen[lz->matchstart];
					LONGEST_MATCH = 0;
					LAZY_MATCHING_DONE = 0;
					lz77_cleanmatch(lz, 0, searchindex);
					break;
				}
				//printf("rr");
				/* ????????????????????????match???dist???len???????????????????????? */					lz->matchdist[lz->matchstart]++;
				lz->matchlen[lz->matchstart]++;
				lz->matchdist[lz->matchstart]++;
				break;
			}
				// /* ??????????????????????????????match */
				// else
				// {
				// 	lz->match(lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
				// 	lz->k -= lz->matchlen[lz->matchstart];
				// 	LONGEST_MATCH = 0;
				// 	LAZY_MATCHING_DONE = 0;
				// 	lz77_cleanmatch(lz, 0, searchindex);
				// 	break;
				// }
		}
	
		
		/* lazy matching */
		while(lz->k > HASHCHARS && searchindex >= 2 && LAZY_MATCHING_DONE == 0 && LONGEST_MATCH == 0)
		{
			/*  
			 * ???????????????index???1???????????????match
			 * ???????????????????????????????????????????????????match
			 */
			if(lz77_lazysearch(lz, 0, 1) == 0)
			{
				//printf(" 1N ");
				if(lz77_lazysearch(lz, 1, 2) == 0)
				{
					//printf("NN");
					//printf("%d", lz->matchdist[0]);
					lz->match(lz, lz->ctx, lz->matchdist[0], lz->matchlen[0]);
					lz->k -= lz->matchlen[0];
					//lz77_cleanmatch(lz, 0, 2);
					break;
				}
				else if(lz77_lazysearch(lz, 1, 2) == 1) 
				{
					//printf("NY");
					for (int i = 0; i < 2; i++)
						lz->literal(lz, lz->ctx, lz->literals[i]);
					lz->k -= i;
					LAZY_MATCHING_DONE = 1;
					LONGEST_MATCH = 1;
					//lz77_cleanmatch(lz, 0, 1);
					lz->matchhead[0] = lz->matchdist[0] = lz->matchhead[2];
					lz->matchlen[0] = HASHCHARS;
					//lz77_cleanmatch(lz, 2, 2);
					//lz77_cleanmatch(lz, 0, 2);
					break;
				}		
			}
			/*  
			 * ?????????????????????????????????????????????match
			 */
			else if(lz77_lazysearch(lz, 0, 1) == 1)
			{
				//printf(" 1Y ");
				lz->matchlen[0]++;
				if(lz77_lazysearch(lz, 1, 2) == 1)
				{
					//printf("YY ");
					lz->matchlen[0]++;
					LAZY_MATCHING_DONE = 1;
					LONGEST_MATCH = 1;
					break;/* ????????????match?????????????????????????????? */
				} 
				else if(lz77_lazysearch(lz, 1, 2) == 0)
				{
					//printf("YN ");
					/* ????????????match??????????????????match??????????????? */
					if(lz77_lazysearch(lz, 2, 3) == 0)
					{
						//printf("YYN");
						/*?????????????????????????????????????????????match */
						lz->match(lz, lz->ctx, lz->matchdist[0], lz->matchlen[0]);
						lz->k -= lz->matchlen[0];
						lz77_cleanmatch(lz, 0, 3);
					}
					else if(lz77_lazysearch(lz, 2, 3) == 1)
					{
						//printf("YNY");
						lz->matchlen[1] = ++lz->matchlen[0];
						lz->literal(lz, lz->ctx, lz->literals[0]);
						lz->k--;
						LAZY_MATCHING_DONE = 1;
						LONGEST_MATCH = 1;
						lz->matchhead[0] = lz->matchhead[1];
						lz->matchlen[0] = lz->matchlen[1];
						lz->matchdist[0] = lz->matchdist[1];
						lz77_cleanmatch(lz, 1, 3);
						break;
					}
				}				
			}
		}
		/* ???????????????????????????hash??? */
		lz->hashnext[lz->winpos] = lz->hashhead[hash];
		lz->hashhead[hash] = lz->winpos;
		// if(hash == 617)
		// 	printf(" position = %d ", lz->hashhead[617]);
		//printf(" %d ", searchindex);
		//printf(" %d", lz->hashhead[searchindex]);
	}

	/* ???len????????????0?????????????????????????????? */
	if(LAZY_MATCHING_DONE == 1 && LONGEST_MATCH == 1)
		lz->match(lz, lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
	if(LAZY_MATCHING_DONE == 1 && LONGEST_MATCH == 0)
	{
		for (int i = 0; i < lz->k; i++)
			lz->literal(lz, lz->ctx, currchars[i]);
	}

	if(LAZY_MATCHING_DONE == 0 && lz->hashhead[hash] != 0)
	{
		switch(lz->k)
		{
			case 3:
				lz->match(lz, lz->ctx, lz->matchdist[0], lz->matchlen[0]);
				break;
			case 4:
				lz->match(lz, lz->ctx, lz->matchdist[0], lz->matchlen[0]);
				lz->literal(lz, lz->ctx, currchars[0]);
				break;
			case 5:
				lz->match(lz, lz->ctx, lz->matchdist[0], lz->matchlen[0]);
				for (int i = 0; i < lz->k; i++)
					lz->literal(lz, lz->ctx, currchars[i]);
				break;
			/*
			 *????????????????????????????????????match??????????????????
			 */
			default:
				break;
		}
	}
	if(LAZY_MATCHING_DONE == 0 && LONGEST_MATCH == 0)
	{
		lz->k--;
		for (lz->k; lz->k >= 0 ; lz->k--)
			lz->literal(lz, lz->ctx, lz->data[lz->winpos + lz->k]);
	}
}

const char *const tests[] = 
{
    /*
     * Basics: an incompressible string to make sure we don't
     * compress it by mistake.
     */
    "AAABAACAADAAEAAFAAGAAHAAIAAJAAKAALAAMAANAAOAAPAAQAARAASAATAAUAAVAAWAAXAA",

    /*
     * Simple repeated string. Repeating the string three times
     * rather than two also checks that we prefer to match against
     * more recent data when there's no length difference.
     */
    "ZAabcBBABCABabcDABEABFabcABGABHA",
    "ZAabcdBBABCABabcdDABEABFabcdABGABHA",
    "ZAabcdeBBABCABabcdeDABEABFabcdeABGABHA",

    /*
     * Self-overlapping match.
     */
    "RABSABTAspoonspoonspoonspoonspoonspoonspoonspoonBUABVAB",

    /*
     * Lazy matching, one step on. `flapping' should be rendered as
     * a literal `f' plus a match against `lapping', rather than as
     * a match against `flap' followed by one against `ping'.
     */
    "ACCACDACflapEACFACGlappingACHACIAflappingCJACK",

    /*
     * Lazy matching, two steps on. (Transmitting `fl' as literals
     * is still superior to transmitting two matches.) Then
     * gradually reduce the length of the later match until it's no
     * longer profitable to use it.
     */
    "ACCACDACflapEACFACGapplianceACHACIAflapplianceCJACK",
    "ACCACDACflapEACFACGappliancACHACIAflapplianceCJACK",
    "ACCACDACflapEACFACGapplianACHACIAflapplianceCJACK",
    "ACCACDACflapEACFACGappliaACHACIAflapplianceCJACK",
    "ACCACDACflapEACFACGappliACHACIAflapplianceCJACK",
    "ACCACDACflapEACFACGapplACHACIAflapplianceCJACK",

    /*
     * Non-lazy matching, three steps on. (Transmitting the `fla'
     * as literals is _not_ superior to transmitting it as a match;
     * and in fact it's best to transmit the longest initial match
     * we can - `flap' - and then cover `athetic' with the next
     * match.)
     */
    "ACCACDACflapEACFACGpatheticACHACIAflapatheticCJACK",

    /*
     * Test that various kinds of match correctly find an
     * immediately following match in all circumstances.
     */
    "WAabcdeCXfghijACYACabcdefghijZAD",
    "WAabcdeCXbcdefACYACfghijZADabcdefghijBADCA",
    "WAabcdeCXbcdefgACYACfghijZADabcdefghijBADCA",
    "0WAabcdeCXbcdefACcdefghYACfghijklmZADabcdefghijklmBADCA",
    "2WAabcdeCXbcdefACcdefghiYACfghijklmZADabcdefghijklmBADCA",
    "1WAabcdeCXbcdefgACcdefghYACfghijklmZADabcdefghijklmBADCA",
    "0WAabcdefCXbcdefACcdefgYACfghijklmZADabcdefghijklmBADCA",
    "0WAabcdefCXbcdefgACcdefgYACfghijklmZADabcdefghijklmBADCA",
    "0WAabcdefCXbcdefACcdefghYACfghijklmZADabcdefghijklmBADCA",
    "1WAabcdeCXbcdefgACcdefghYACfghijklmZADabcdefghijklmBADCA",
    "0WAabcdefCXbcdefgACcdefghYACfghijklmZADabcdefghijklmBADCA",
    "WAabcdeCXbcdefgACcdefghiYACfghijZADabcdefghijBADCA",

    /*
     * Lazy matching: nasty cases in which it can be marginally
     * better _not_ to lazily match. In some of these cases,
     * choosing the superficially longer lazy match eliminates an
     * opportunity to render the entire final lower-case section
     * using one more match and at least three fewer literals.
     */
    "0WAabcdeCXbcdefghijklACYACfghijklmnoZADabcdefghijklmnoBADCA",
    "[01]WAabcdeCXbcdefghijklACYACghijklmnoZADabcdefghijklmnoBADCA",
    "0WAabcdeCXbcdefghijklACYACfghijklmnZADabcdefghijklmnBADCA",
    "1WAabcdeCXbcdefghijklACYACghijklmnZADabcdefghijklmnBADCA",
    "1WAabcdeCXbcdefghijklACYACfghijklmZADabcdefghijklmBADCA",
    "1WAabcdeCXbcdefghijklACYACghijklmZADabcdefghijklmBADCA",
    "1WAabcdCXbcdefACcdefghijkYACghijklmnZADabcdefghijklmnBADCA",
    "1WAabcdCXbcdefACcdefghijkYACfghijklmnZADabcdefghijklmnBADCA",
    "0WAabcdCXbcdefACcdefghijkYACefghijklmnZADabcdefghijklmnBADCA",
    "1WAabcdCXbcdefACcdefghijkYACghijklmZADabcdefghijklmBADCA",
    "[01]WAabcdCXbcdefACcdefghijkYACfghijklmZADabcdefghijklmBADCA",
    "0WAabcdCXbcdefACcdefghijkYACefghijklmZADabcdefghijklmBADCA",
    "2WAabcdCXbcdefACcdefghijkYACghijklmZADabcdefghijklBADCA",
    "2WAabcdCXbcdefACcdefghijkYACfghijklmZADabcdefghijklBADCA",
    "0WAabcdCXbcdefACcdefghijkYACefghijklmZADabcdefghijklBADCA",

    /*
     * Regression tests against specific things I've seen go wrong
     * in the past. All I really ask of these cases is that they
     * don't fail assertions; optimal compression is not critical.
     */
    "AabcBcdefgCdefDefghiEhijklFabcdefghijklG",
    "AabcBcdeCefgDfghijkEFabcdefghijklG",
    "AabcBbcdefgCcdefghiDhijklEjklmnopFabcdefghijklmnopqrstG",
    "AabcBbcdefghCcdefghijklmnopqrstuvDijklmnopqrstuvwxyzEdefghijklmnoF"
	"abcdefghijklmnopqrstuvwxyzG",
    "AabcdefBbcdeCcdefghijklmDfghijklmnoEFGHIJabcdefghijklmnoK",
    "AabcdBbcdCcdefDefgEabcdefgF",
    "AabcdeBbcdCcdefgDefgEabcdefghiF",

    /*
     * Fun final test.
     */
    "Pease porridge hot, pease porridge cold, pease porridge"
	" in the pot, nine days old.",
};

struct testctx
{
    const char *data;
    int len, ptr;
};

void match(LZ77 *lz, void *vctx, int distance, int len)
{
	struct testctx *ctx = (struct testctx *)vctx;
	
	assert(distance > 0);
    //assert(distance <= ctx->ptr);
    assert(len >= HASHCHARS);
    //assert(len <= ctx->len - ctx->ptr);
    assert(len <= MAXMATCHLEN);
    //assert(!memcmp(ctx->data + ctx->ptr, ctx->data + ctx->ptr - distance, len));

    //printf("<%d,%d>", distance, len);
    //fflush(stdout);
	int lend, lenl;
	char d[32768];
	char l[258];
	sprintf(d, "%d", distance);
	sprintf(l, "%d", len);
	lend = strlen(d);
	lenl = strlen(l);
	//printf("lend = %d", lend);
	//printf("len = %d", lenl);
	char dist[lend];

	char lenn[lenl];
	//strcpy(dist, d);
	//strcpy(lenn, l);
	int i;
	for (i = 0; i < lend; i++)
	{
		//printf("=%c", dist[i]);
		lz->result[lz->resultlen] = d[i];
		lz->resultlen++;
	}
	//lz->resultlen++;
	for (i = 0; i < lenl; i++)
	{
		lz->result[lz->resultlen] = l[i];
		lz->resultlen++;
	}


    ctx->ptr += len;
}

void literal(LZ77 *lz, void *vctx, unsigned char c)
{
    struct testctx *ctx = (struct testctx *)vctx;
	//assert(ctx->ptr < ctx->len);
    //assert(c == (unsigned char)(ctx->data[ctx->ptr]));
	lz->result[lz->resultlen] = c;
	lz->resultlen++;
	//fputc(c, stdout);
    //fflush(stdout);

    ctx->ptr++;
}

void dotest(PMEMobjpool *pop, const void *data, int len, int step)
{
    struct testctx t;
    LZ77 *lz;
    int j;
	int i;
	int slen, resultsize;
	//char string[MAXLEN + 1];
	//char *string2 = '123';
	t.data = data;
    t.len = len;
    t.ptr = 0;
	int size;
	lz = lz77_new(literal, match, &t);
    for (j = 0; j < t.len; j += step)
	lz77_compress(lz, t.data + j, (t.len - j < step ? t.len - j : step));
	//lz77_flush(lz);
	//for (int i = 0; i <= lz->resultlen; i++)
	//	printf("%c", lz->result[i]);
	resultsize = sizeof(lz->result) / sizeof(lz->result[0]);
	//memcpy(string, lz->result, MAXLEN);
	lz->result[MAXLEN + 1] = '\0';
	//printf("%s\n", lz->result);
	printf("\nstringsize = %d\n", lz->resultlen);
	printf("resultsize = %d\n", resultsize);
	slen = strlen(lz->result);
	printf("string length = %d", slen);
	TOID(struct my_root) root = POBJ_ROOT(pop, struct my_root);
	
	/* persist write */
	TX_BEGIN(pop)
	{
		TX_MEMCPY(D_RW(root)->r, lz->result, strlen(lz->result));
	} TX_END
	//if(OID_IS_NULL(root))
	//{
	//	printf("cannot access root object");
	//	return 1;
	//}
	//printf("persist!");
	
	/* read data in optane */
	//printf("%s", D_RO(root)->r);

	pmemobj_close(pop);
	lz77_free(lz);

    //assert(t.len == t.ptr);
    printf("\n");
}


int main(int argc, char **argv)
{
    clock_t begin = clock();
	int i, len, truncate = 0;
	int step;
    char *filename = NULL;
	LZ77 *lz;
    step = 48000;		       /* big step by default */

    //while (--argc) {
	//char *p = *++argv;
	//if (!strcmp(p, "-t")) {
	//    truncate = 1;
	//} else if (p[0] == '-' && p[1] == 'b') {
	//    step = atoi(p+2);	       /* -bN sets block size to N */
	//} else if (p[0] != '-') {
	//    filename = p;
	//}
    //}
	filename = argv[2];
	
	/* PMEM pointer */
	PMEMobjpool *pop = pmemobj_create(argv[1], POBJ_LAYOUT_NAME(rstore), PMEMOBJ_MIN_POOL, 0666);
	if(pop == NULL)
	{
		perror("pmemobj_create");
		return 1;
	}
   

	if (filename)
	{
	
	char *data = NULL;
	int datalen = 0, datasize = 0;
	int c;
	FILE *fp = fopen(filename, "rb");

	//char buf[] = "abc";

	while ( (c = fgetc(fp)) != EOF){ 
	    if (datalen >= datasize) {
		datasize = (datalen * 3 / 2) + 512;
		data = realloc(data, datasize);
	    }
	    data[datalen++] = c;
	}
	
	fclose(fp);
	dotest(pop, data, datalen, step);
	printf("\nsize of raw data = %d\n", datasize);
	
	//TOID(struct my_root) root = POBJ_ROOT(pop, struct my_root);
	//TX_BEGIN(pop)
	//{
	//	TX_MEMCPY(D_RW(root)->r, buf, strlen(buf));
	//} TX_END
	

	//printf("%s", D_RO(root)->r);

	//pmemobj_close(pop);


    } else {
	for(i = 0; i < lenof(tests); i++) 
	{
	    for (len = (truncate ? 0 : strlen(tests[i]));
		 len <= strlen(tests[i]); len++) 
		{
		dotest(pop, tests[i], len, step);
		lz77_free(lz);
		}
	}
    }
	clock_t end = clock();
	double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("total time : %f\n", time_spent);
	return 0;
}
