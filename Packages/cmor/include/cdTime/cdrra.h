/* -*-Mode: C;-*-
 * Module:      cdunif right-ragged array definitions
 *
 * Copyright:	1994, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Bob Drach, Lawrence Livermore National Laboratory
 *              drach@llnl.gov
 *
 * Version:     $Id$
 *
 * Revision History:
 *
 * $Log: cdrra.h,v $
 * Revision 1.1  1994/11/18  23:42:26  drach
 * Initial version
 *
 *
 */


				/* ddi right ragged array declarations */
#ifndef _DDI_RRA
#define _DDI_RRA

#include <stddef.h>

typedef struct {
	long rank;	/* number of dimensions */
	long *arraySize; /* vector of lengths of dimensions of base array */
	long *dimensionSize; /* vector of lengths of index vectors */
	long **indices;		/* index vectors; indices[k] is vector of length dimensionSize[k] */
} DDI_RRA;

				/* Set the ith element of dimension id of a to value  */
#define SETRRA(a,id,i,value) a->indices[id][i]=value

extern DDI_RRA *createRRA(long rank, const long arraySize[], const long dimensionSize[]);
extern void destroyRRA(DDI_RRA *);
extern int sortRRA(DDI_RRA *, DDI_RRA *, const long []);
extern int printRRA(DDI_RRA *);
#endif
