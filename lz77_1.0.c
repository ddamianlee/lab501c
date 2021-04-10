if (lz->k > HASHCHARS) 
{
	    int i;
	    for (i = 0; i < lz->nextindex; i++) 
        {
		    int outpos = -1, inpos = lz->matchhead[i], nextinpos;
		    int bestdist = 0; 

		    if (i == thissearchindex)
		        continue;	       /* this is the one we've just started */

		    while (inpos >= 0) 
            {
		        nextinpos = lz->matchnext[inpos];

		    /*
		     * See if this match is still going.
		     */
		        if (lz->matchlen[i] < MAXMATCHLEN && lz->data[(lz->winpos + inpos) % WINSIZE] == lz->data[lz->winpos]) 
                {
			/*
			 * It is; put it back on the winnowed list.
			 */
			        if (outpos < 0)
			            lz->matchhead[i] = inpos;
			        else
			            lz->matchnext[outpos] = inpos;
			        outpos = inpos;
			/*
			 * Because we built up the match list in
			 * reverse order compared to the hash
			 * chain, we must prefer _later_ entries in
			 * the match list in order to prefer
			 * matching against the most recent data.
			 */
			        bestdist = inpos;
		        } 
                else 
                {
			/*
			 * It isn't; mark it as unused in the
			 * matchnext array.
			 */
			        lz->matchnext[inpos] = 0;
		        }

		        inpos = nextinpos;
		    }   

		/*
		 * Terminate the new list.
		 */
		if (outpos < 0)
		    lz->matchhead[i] = -1;
		else
		    lz->matchnext[outpos] = -1;

		/*
		 * And update the distance/length tracker for this
		 * match.
		 */
		if (bestdist) 
        {
		    lz->matchdist[i] = bestdist;
		    lz->matchlen[i]++;
		}
	    }
}
#define MAXMATCHDIST 32768	       /* maximum backward distance */
#define MAXMATCHLEN 258		       /* maximum length of a match */
#define HASHMAX 2039		       /* one more than max hash value */
#define HASHCHARS 3		       /* how many chars make a hash */
#define MAXLAZY 3		       /* limit of lazy matching */
#define MAXREWIND (HASHCHARS * 2)
#define WINSIZE (MAXMATCHDIST + HASHCHARS + MAXREWIND)
#define NMATCHES (MAXLAZY + MAXLAZY * MAXLAZY)
#define TOO_FAR 4096

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
};

static void lz77_hashsearch(LZ77 *lz, int hash, unsigned char *currchars,
			    int index)
{
    int pos, nextpos, matchlimit;
    int posval, nextposval;

    assert(lz->matchhead[] < 0);
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
		if (bdist > 0 && bdist <= MAXMATCHDIST) 
		{
			for (i = 0; i < HASHCHARS; i++)
			if (lz->data[pos + i] != currchars[i])
				break;
			if (i == HASHCHARS) 
			{
			
				lz->matchnext[lz->winpos] = pos;
				//lz->matchhead[] = bdist;
				
				if (!lz->matchlen[lz->winpos]) 
				{
					lz->matchlen[lz->winpos] = HASHCHARS;
					lz->matchdist[lz->winpos] = bdist;
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

int lz77_lazysearch(LZ77 *lz, int hash, unsigned char *currchars)
{
	int pos, nextmatchpos, bdist;
	unsigned char currchars[HASHCHARS];
	
	
	pos = lz->hashhead[hash];
	nextmatchpos = lz->hashnext[lz->winpos + 1];
	/* 判斷是否新的這個字元+符合match的後兩個曾經出現過 */
	if(pos != -1)
	{
		/* 尋找下一組字串的match */
		while(nextmatchpos != -1)
		{
			if(lz->data[nextmatchpos - 1] == lz->data[lz->hashhead[hash]])
			{
				lz->matchdist[lz->winpos + 1] = nextmatchpos - lz->hashhead[head];
				lz->matchlen[lz->winpos + 1]++;
				break; /* 如果符合最近一個匹配的話匹配長度+1然後跳出迴圈 */
			}
			else if(lz->data[lz->winpos] != lz->data[nextmatchpos] - 1)
				nextmatchpos = lz->hashnext[nextmatchpos]; /* 如果也不是下一個hash位置的匹配的話繼續尋找hashnext的下一個位置 */
			else
				nextmatchpos = -1;
		}
		//return;
		
		bdist = (pos + WINSIZE - lz->winpos) % WINSIZE;

		lz->matchnext[lz->winpos] = pos;
		if (!lz->matchlen[lz->winpos]) 
		{
			lz->matchlen[lz->winpos] = HASHCHARS;
			lz->matchdist[lz->winpos] = bdist;
		}
		return 1;
	}

	/* if next character isn't a match , output match */ 
	if(lz->matchdist[lz->winpos + 1] < TOO_FAR)
		lz->match(lz->ctx, lz->matchdist[lz->winpos + 1], lz->matchlen[lz->winpos + 1]);
}

void lz77_compress(LZ77 *lz, const void *vdata, int len)
{
	const unsigned char *data = (const unsigned char *)vdata;
	lz->datalen = len;
	while(lz->datalen > 0)
	{
		lz->datalen--;
		unsigned char currchars[HASHCHARS];
		int hash, lazy;

		lz->winpos = (lz->winpos + WINSIZE-1) % WINSIZE;
		lz->data[lz->winpos] = *data++


		lz->k++
		if (lz->nvalid < WINSIZE)
	    	lz->nvalid++;
		
		if (lz->nvalid < HASHCHARS)
	    	continue;

		{
	    int i;
	    for (i = 0; i < HASHCHARS; i++)
		{
			currchars[i] = lz->data[lz->winpos + i];
	    	hash = lz77_hash(currchars);
		}
		}

		if (lz->k >= HASHCHARS && lz->k < HASHCHARS+MAXLAZY) 
		{
	    	
		}
		if (lz->k >= HASHCHARS && lz->k < HASHCHARS+MAXLAZY) 
		{
	   		lz77_hashsearch(lz, hash, currchars);
		}
		/*
		 * save the literal at this position
		 */
		if (lz->k >= HASHCHARS && lz->k < HASHCHARS + MAXLAZY - 1)
		{
			lz->literals[lz->k - HASHCHARS] = lz->data[lz->winpos + HASHCHARS - 1];
		}

		/* 假如hash search找到match，再輸入一個字元進行lazy match */
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

		}
			

		

		if (lz->k == HASHCHARS && lz->matchnext[lz->winpos] < 0) 
		{
			lz->literal(lz->ctx, currchars[HASHCHARS-1]);
			lz->k--;
		}

		lz->hashnext[lz->winpos] = lz->hashhead[hash];
		lz->hashhead[hash] = lz->winpos;
		}
}