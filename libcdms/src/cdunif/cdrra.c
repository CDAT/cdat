/* -*-Mode: C;-*-
 * Module:      
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
 * $Log: cdrra.c,v $
 * Revision 1.3  1995/07/31  17:39:46  drach
 * - Replaced sort2 with CuSort
 *
 * Revision 1.2  1994/12/20  01:18:43  drach
 * - Prefixed cdrra functions with 'cu'
 *
 * Revision 1.1  1994/11/18  23:39:53  drach
 * Initial version
 *
 *
 */


				/* cdunif right ragged array functions */
#include <stdio.h>
#include <stdlib.h>
#include "cdunif.h"

				/* Allocate space for a right-ragged array, with 
				   rank dimensions, and the ith dimension having 
				   size dimensionSize[i]. 
				   
				   Returns a pointer to the RRA, or NULL if an error 
				   occurred .
				   */
CuRRA *cucreateRRA(long rank, const long arraySize[], const long dimensionSize[]){
	CuRRA *p;
	int i;

	for(i=0;i<rank;i++){
		if(dimensionSize[i] < 0){
			fprintf(stderr,"Dimension %d has invalid size %d.\n",i,dimensionSize[i]);
			return (CuRRA*)0;
		}
	}

	if(!(p = (CuRRA*) malloc(sizeof(CuRRA)))){
		fprintf(stderr,"Cannot allocate memory for right ragged array.\n");
		return (CuRRA*)0;
	}
	p->rank = rank;
	if(!(p->arraySize = (long *) malloc(rank*sizeof(long)))){
		fprintf(stderr,"Cannot allocate memory for right ragged array.\n");
		return (CuRRA*)0;
	}
	if(!(p->dimensionSize = (long *) malloc(rank*sizeof(long)))){
		fprintf(stderr,"Cannot allocate memory for right ragged array.\n");
		return (CuRRA*)0;
	}
	if(!(p->indices = (long **) malloc(rank*sizeof(long *)))){
		fprintf(stderr,"Cannot allocate memory for right ragged array.\n");
		return (CuRRA*)0;
	}
	for(i=0;i<rank;i++){
		p->arraySize[i] = arraySize[i];
		p->dimensionSize[i] = dimensionSize[i];
		if(dimensionSize[i]>0)
			if(!(p->indices[i] = (long *) malloc(dimensionSize[i] * sizeof(long)))){
				fprintf(stderr,"Cannot allocate memory for right ragged array.\n");
				return (CuRRA*)0;
			}
	}
	return p;
}

				/* Destroy the right-ragged array a. */
void cudestroyRRA(CuRRA *a){
	int i;

	for(i=0;i<a->rank;i++)
		if(a->dimensionSize[i]>0)
			free(a->indices[i]);
	free(a->indices);
	free(a->dimensionSize);
	free(a->arraySize);
	free(a);
}

				/* Sort the index vectors of a, making the corresponding
				   rearrangement to the elements of b, where dimension i
				   of b corresponds to dimension transpose[i] of a, i=0..rank(a)-1.
				   Returns 0 on success, -1 on failure.
				   */
int cusortRRA(CuRRA *a, CuRRA *b, const long transpose[]){
	extern void CuSort(long ix[], long iy[], long n, int kflag);
	int i;

	if(a->rank != b->rank){
		fprintf(stderr,"Rank of a (%d) != rank of b (%d).\n",a->rank,b->rank);
		return -1;
	}
					     /* Sort a->indices increasing, b->indices wrt a */
	for(i=0;i<a->rank;i++)
		CuSort(a->indices[transpose[i]], b->indices[i], b->dimensionSize[i], 2);

	return 0;
}

void cuprintRRA(CuRRA *a){
	int i,j;

	for(i=0;i<a->rank;i++){
		printf("[%3d] <",a->arraySize[i]);
		for(j=0;j<a->dimensionSize[i];j++)
			printf("%3d ",a->indices[i][j]);
		printf(">\n");
	}
	printf("--------\n");
	return;
}

