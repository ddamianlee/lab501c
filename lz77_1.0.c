#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "misc.h"
#include "lz77.h"


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

struct LZ77 
{
    /*
     * Administrative data passed in from lz77_new().
     */
    void *ctx;
    void (*literal)(void *ctx, unsigned char c);
    void (*match)(void *ctx, int distance, int len);

    
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

    assert(lz->matchhead[index] < 0);
    assert(lz->matchdist[index] == 0);
    assert(lz->matchlen[index] == 0);

    matchlimit = (lz->nvalid < MAXMATCHDIST ? lz->nvalid : MAXMATCHDIST);
    matchlimit = (matchlimit + lz->winpos) % WINSIZE;

    pos = lz->hashhead[hash];
    posval = (matchlimit + WINSIZE - pos) % WINSIZE;
    while (pos != -1) 
	{
		int bdist;
		int i;

		bdist = (pos + WINSIZE - lz->winpos) % WINSIZE;
		if (bdist > 0 && bdist <= MAXMATCHDIST && !lz->matchnext[bdist]) 
		{
			for (i = 0; i < HASHCHARS; i++)
			if (lz->data[pos + i] != currchars[i])
				break;
			if (i == HASHCHARS) 
			{
				
				lz->matchnext[bdist] = lz->matchhead[index];
				lz->matchhead[index] = bdist;
				
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
	nextpos = lz->hashnext[pos];
	nextposval = (matchlimit + WINSIZE - nextpos) % WINSIZE;
	if (nextposval >= posval) {
	    break;
	} else {
	    pos = nextpos;
	    posval = nextposval;
	}
    }
}

int lz77_lazysearch(LZ77 *lz, int index1, int index2)
{
	int matchdist1, matchdist2, bestmatchdist;
	int chaincount = 0;
	matchdist1 = lz->matchhead[index1];
	matchdist2 = lz->matchhead[index2];
	if(matchdist1 <= 0 || matchdist2 <= 0)
		return 0;
	/* 如果新來的不符合最近一個match的後一個，沿著matchnext一直找下去 */
	while(lz->matchhead[index1] != lz->matchhead[index2])
	{
		matchdist1 = lz->matchnext[matchdist1];
		matchdist2 = lz->matchnext[matchdist2];
		chaincount++;
		if((matchdist1 <= 0 || matchdist2 <= 0) || chaincount == MAXCHAIN)
		{
			/* 假如都沒有找到，跳出function再進下一個字元 */
			//bestmatchdist = lz->matchdist[index1];
			return 0;
		}	
	}
	lz->matchlen[index1]++;
	bestmatchdist = lz->matchdist[matchdist1];
	lz->matchdist[index1] = bestmatchdist;
	return 1;
}

void lz77_compress(LZ77 *lz, const void *vdata, int len)
{
	const unsigned char *data = (const unsigned char *)vdata;
	int LAZY_MATCHING_DONE = 0;
	int LONGEST_MATCH = 0;

	int hash, searchindex, i, checkmatch;
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
		
		if (lz->nvalid < HASHCHARS)
	    	continue;
		
		{
	    
	    for (i = 0; i < HASHCHARS; i++)
		{
			currchars[i] = lz->data[lz->winpos + i];
	    	hash = lz77_hash(currchars);
		}
		}
		

		if (lz->k >= HASHCHARS) 
		{
			searchindex = lz->k - HASHCHARS;
			lz77_hashsearch(lz, hash, currchars, searchindex);
		}
		
		/*
		 * save the literal at this position
		 */
		if (lz->k >= HASHCHARS && lz->k <= HASHCHARS + MAXLAZY)
		{
			lz->literals[lz->k - HASHCHARS] = lz->data[lz->winpos + HASHCHARS - 1];
		}

		/* 假如hash search找到match，再輸入一個字元進行lazy match 
		while(lz->hashhead[hash] != 0 && lz->k <= MAXMATCHLEN && lazy <= MAXLAZY)
		{
			lz->datalen--;
			lz->winpos = (lz->winpos + WINSIZE-1) % WINSIZE;
			lz->data[lz->winpos] = *data++
			lz->k++
			if (lz->nvalid < WINSIZE)
				lz->nvalid++;	
			if (lz->nvalid < HASHCHARS)
				continue;

			{
			for (int j = 0; j < HASHCHARS; j++)
			{
				currchars[j] = lz->data[lz->winpos + j];
				hash = lz77_hash(currchars);
			}
			}
			if(lz77_lazysearch(lz, hash, currchars) == 1)
			{
				lazy++;
			}

		}*/
			
		/* 如果是第一次出現3個一組，輸出第一個 */
		if (lz->k == HASHCHARS && lz->matchhead[0] < 0) 
		{
			lz->literal(lz->ctx, currchars[HASHCHARS-1]);
			lz->k--;
		}


		/* lazy matching結束後，只要遇到不匹配的字元就直接輸出前面的longest match */
		while(LAZY_MATCHING_DONE == 1 && LONGEST_MATCH == 1)
		{
			checkmatch = lz->matchhead[searchindex];
			/* 尋找這個match的初始位置在哪裡，因為有可能是0或1或2 */
			for (lz->matchstart = 0; lz->matchstart <= 2; lz->matchstart++)
			{
				if(lz->matchhead[lz->matchstart] == -1)
					continue;
				else
					break;
			}
			
			if(lz->k == MAXMATCHLEN)
			{
				lz->match(lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
				lz->k -= lz->matchlen[lz->matchstart];
				LONGEST_MATCH = 0;
				LAZY_MATCHING_DONE = 0;
				break;
			}
			/* 如果接下來的字元沒有出現過，輸出之前的match */
			if (lz->hashhead[hash] == -1)
			{
				lz->match(lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
				lz->k -= lz->matchlen[lz->matchstart];
				LONGEST_MATCH = 0;
				LAZY_MATCHING_DONE = 0;
				break;
			}
			else
			{
				/* 如果新字元有出現過，去找是不是出現在最長匹配的下一個 */	
				while (checkmatch != 0)
				{
					if (checkmatch != lz->matchdist[searchindex - 1])
						checkmatch = lz->matchnext[checkmatch];
					else
					{
						/* 假如又符合的話把match的dist跟len都加一，跳出迴圈 */	
						lz->matchdist[lz->matchstart]++;
						lz->matchlen[lz->matchstart]++;
						break;
					}
				}
				/* 假如不是，輸出之前的match */
				if(checkmatch == 0)
				{
					lz->match(lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
					lz->k -= lz->matchlen[lz->matchstart];
					LONGEST_MATCH = 0;
					LAZY_MATCHING_DONE = 0;
				}
			}
		}
		
		/* lazy matching */
		while(lz->k > HASHCHARS && searchindex >= 2 && LAZY_MATCHING_DONE == 0 && LONGEST_MATCH == 0)
		{
			/* 新一個字元index為1，看是否有match 
			 * 如果沒有的話判斷下一組有沒有更好的match
			 */
			if(lz77_lazysearch(lz, 0, 1) == 0)
			{
				if(lz77_lazysearch(lz, 1, 2) == 0)
				{
					lz->match(lz->ctx, lz->matchdist[0], lz->matchlen[0]);
					lz->k -= lz->matchlen[0];
					lz->matchhead[0] = -1;
				}
				else if(lz77_lazysearch(lz, 1, 2) == 1) 
				{
					for (int i = 0; i < 2; i++)
					{
						lz->literal(lz->ctx, lz->literals[i]);
						lz->matchhead[i] = -1;
					}
					lz->k - i;
					LAZY_MATCHING_DONE = 1;
					LONGEST_MATCH = 1;
					break;
				}		
			}
			/* 如果有的話再看下一個是不是也是match */
			else if(lz77_lazysearch(lz, 0, 1) == 1)
			{
				if(lz77_lazysearch(lz, 1, 2) == 1)
				{
					LAZY_MATCHING_DONE = 1;
					LONGEST_MATCH = 1;
					break;/* 如果又是match的話再繼續補字元進來 */
				} 
				else if(lz77_lazysearch(lz, 1, 2) == 0)
				{
					/* 如果不是match的話看後面的match有沒有更長 */
					if(lz77_lazysearch(lz, 2, 3) == 0)
					{
						/*並沒有比較長所以輸出前面找到的match */
						lz->match(lz->ctx, lz->matchdist[1], lz->matchlen[1]);
						lz->k -= lz->matchlen[1];
						lz->matchhead[0] = -1;
						lz->matchhead[1] = -1;
					}
					else if(lz77_lazysearch(lz, 2, 3) == 1)
					{
						lz->literal(lz->ctx, lz->literals[0]);
						lz->k--;
						lz->matchhead[0] = -1;
						LAZY_MATCHING_DONE = 1;
						LONGEST_MATCH = 1;
						break;
					}
				}				
			}
		}
		lz->hashnext[lz->winpos] = lz->hashhead[hash];
		lz->hashhead[hash] = lz->winpos;
	}

	/* 當len已經等於0的時候還有東西沒輸出 */
	if(LAZY_MATCHING_DONE == 1 && LONGEST_MATCH == 1)
		lz->match(lz->ctx, lz->matchdist[lz->matchstart], lz->matchlen[lz->matchstart]);
	
	if(LAZY_MATCHING_DONE == 1 && LONGEST_MATCH == 0)
	{
		for (int i = 0; i < lz->k; i++)
			lz->literal(lz->ctx, currchars[i]);
	}

	if(LAZY_MATCHING_DONE == 0 && lz->hashhead[hash] != 0)
	{
		switch(lz->k)
		{
			case 3:
				lz->match(lz->ctx, lz->matchdist[0], lz->matchlen[0]);
				break;
			case 4:
				lz->match(lz->ctx, lz->matchdist[0], lz->matchlen[0]);
				lz->literal(lz->ctx, currchars[0]);
				break;
			case 5:
				lz->match(lz->ctx, lz->matchdist[0], lz->matchlen[0]);
				for (int i = 0; i < lz->k; i++)
					lz->literal(lz->ctx, currchars[i]);
				break;
			/*
			 *應該還能再做比較看哪一個match比較輸出哪個
			 */
			default:
				break;
		}
	}
	if(LAZY_MATCHING_DONE == 0 && LONGEST_MATCH == 0)
	{
		for (int i = 0; i < lz->k; i++)
			lz->literal(lz->ctx, currchars[i]);
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

void match(void *vctx, int distance, int len)
{
    struct testctx *ctx = (struct testctx *)vctx;

    assert(distance > 0);
    assert(distance <= ctx->ptr);
    assert(len >= HASHCHARS);
    assert(len <= ctx->len - ctx->ptr);
    assert(len <= MAXMATCHLEN);
    assert(!memcmp(ctx->data + ctx->ptr, ctx->data + ctx->ptr - distance,
		   len));

    printf("<%d,%d>", distance, len);
    fflush(stdout);

    ctx->ptr += len;
}

void literal(void *vctx, unsigned char c)
{
    struct testctx *ctx = (struct testctx *)vctx;

    assert(ctx->ptr < ctx->len);
    assert(c == (unsigned char)(ctx->data[ctx->ptr]));

    fputc(c, stdout);
    fflush(stdout);

    ctx->ptr++;
}

void dotest(const void *data, int len, int step)
{
    struct testctx t;
    LZ77 *lz;
    int j;

    t.data = data;
    t.len = len;
    t.ptr = 0;
    lz = lz77_new(literal, match, &t);
    for (j = 0; j < t.len; j += step)
	lz77_compress(lz, t.data + j, (t.len - j < step ? t.len - j : step));
	//lz77_flush(lz);
    lz77_free(lz);
    assert(t.len == t.ptr);
    printf("\n");
}


int main(int argc, char **argv)
{
    int i, len, truncate = 0;
	int step;
    char *filename = NULL;

    step = 48000;		       /* big step by default */

    while (--argc) {
	char *p = *++argv;
	if (!strcmp(p, "-t")) {
	    truncate = 1;
	} else if (p[0] == '-' && p[1] == 'b') {
	    step = atoi(p+2);	       /* -bN sets block size to N */
	} else if (p[0] != '-') {
	    filename = p;
	}
    }

    if (filename) {
	char *data = NULL;
	int datalen = 0, datasize = 0;
	int c;
	FILE *fp = fopen(filename, "rb");

	while ( (c = fgetc(fp)) != EOF) {
	    if (datalen >= datasize) {
		datasize = (datalen * 3 / 2) + 512;
		data = realloc(data, datasize);
	    }
	    data[datalen++] = c;
	}

	fclose(fp);
	dotest(data, datalen, step);
	
    } else {
	for (i = 0; i < lenof(tests); i++) 
	{
	    for (len = (truncate ? 0 : strlen(tests[i]));
		 len <= strlen(tests[i]); len++) 
		{
		dotest(tests[i], len, step);
	    }
	}
    }

    return 0;
}