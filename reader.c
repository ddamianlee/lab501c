// SPDX-License-Identifier: BSD-3-Clause
/* Copyright 2015-2017, Intel Corporation */

/*
 * reader.c -- example from introduction part 3
 */

#include <stdio.h>
#include <string.h>
#include <libpmemobj.h>
#include <time.h>
#include "layout.h"

int
main(int argc, char *argv[])
{
	time_t begin = clock();
	
	if (argc != 2) {
		printf("usage: %s file-name\n", argv[0]);
		return 1;
	}

	PMEMobjpool *pop = pmemobj_open(argv[1],
					POBJ_LAYOUT_NAME(rstore));
	if (pop == NULL) {
		perror("pmemobj_open");
		return 1;
	}

	TOID(struct my_root) root = POBJ_ROOT(pop, struct my_root);
	printf("%s\n", D_RO(root)->r);

	pmemobj_close(pop);
	
	time_t end = clock();
	double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("total time : %f\n", time_spent);

	return 0;
}
