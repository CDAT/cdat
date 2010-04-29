/* -*-Mode: C;-*-
 * Module:      cdunif - CDMS extended user-level hyperslab I/O functions
 *
 * Copyright:	1996, Regents of the University of California
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
 * $Log: cdunifslab.c,v $
 * Revision 1.2  1997/12/03  22:22:12  drach
 * - In cdunifdrs.c, dimensions which are reversed or subsetted wrt a
 *   coordinate dimension are now treated as local.
 * - Added cdDimGetDouble to cdmsslab.c
 * - Fixed wraparound, reading wraparound dimensions in cdmsslab.c
 *
 * Revision 1.1  1997/11/10  19:22:41  drach
 * - Added cuvargets to cdunif, cdSlabRead to cdms
 *
 *
 */

#include "cdunifint.h"

int cuvargets(int fileid, int varid, const long order[], const long start[], const long count[], const long stride[], CuType usertype, void *values){

	CuFile* file;			     /* Input file */
	CuRRA* fileRRA;			     /* requested indices, right-ragged array, file order */
	CuType dtype;		             /* cdunif file datatype */
	int dimids[CU_MAX_VAR_DIMS];	     /* variable cdunif dimension IDs */
	int ndims;			     /* number of dimensions */
	int i,j,k, ctr;			     /* i = user array index, j = file array index */
	int kcycle;			     /* number of cycles */
	long dlen;			     /* dimension length */
	long filedcount[CU_MAX_VAR_DIMS];    /* retrieval counts, in file order */
	long filedlen[CU_MAX_VAR_DIMS];	     /* dimension lengths, in file order */
	long filedstart[CU_MAX_VAR_DIMS];    /* retrieval start indices, in file order */
	long filedstride[CU_MAX_VAR_DIMS];   /* dimension strides, in file order */
	long transpose[CU_MAX_VAR_DIMS];     /* transpose vector in C majority */
	char varname[CU_MAX_NAME+1];	     /* variable name */

					     /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;

					     /* Get the number of dimensions, dimension IDs, and file datatype */
	if(cuvarinq(fileid, varid, varname, &dtype, &ndims, dimids, 0)==-1){
		return -1;
	}
	if(usertype==0)
		usertype = dtype;

					     /* Get the file dimensions lengths */
	for(j=0; j<ndims; j++){
		if(cudiminq(fileid, dimids[j], 0, 0, 0, 0, 0, &filedlen[j])==-1)
			return -1;
	}

					     /* Set the transpose vector. Default order is [0, 1, ..., ndims-1] */
					     /* Set the file counts, as transposed user counts */
	for(i=0; i<ndims; i++){
		transpose[i] = (order ? order[i] : i);
		filedcount[transpose[i]] = count[i];
		filedstart[transpose[i]] = start[i];
		filedstride[transpose[i]] = (stride ? stride[i] : 1);
	}

					     /* Set the file RRA. If indices exceed the dimension length,
						they wrap modulo the dimension length. */
	fileRRA = cucreateRRA(ndims, filedlen, filedcount);
	for(j=0; j<ndims; j++){
		for(k=filedstart[j], ctr=0; ctr<filedcount[j]; k+=filedstride[j], ctr++){
			kcycle = (k>=0 ? (k/filedlen[j]) : ((k+1)/filedlen[j])-1);
			CU_SETRRA(fileRRA, j, ctr, (k-kcycle*filedlen[j]));
		}
	}

					     /* Read the data */
	if(cureadarray(fileid, varid, fileRRA, 0, transpose, usertype, values)==-1){
		CuError(CU_DRIVER, "Error reading data, file %s, variable %s", file->controlpath, varname);
		return -1;
	}

					     /* Clean up */
	cudestroyRRA(fileRRA);

	return CU_SUCCESS;
}
