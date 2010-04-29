/* -*-Mode: C;-*-
 * Module:      CDMS API slab functions
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
 * $Log: cdmsslab.c,v $
 * Revision 1.3  1997/12/03  22:22:06  drach
 * - In cdunifdrs.c, dimensions which are reversed or subsetted wrt a
 *   coordinate dimension are now treated as local.
 * - Added cdDimGetDouble to cdmsslab.c
 * - Fixed wraparound, reading wraparound dimensions in cdmsslab.c
 *
 * Revision 1.2  1997/11/24  17:28:25  drach
 * - Added QL package to cdunif
 * - Added NdimIntersect function to CDMS
 *
 * Revision 1.1  1997/11/10  19:22:34  drach
 * - Added cuvargets to cdunif, cdSlabRead to cdms
 *
 *
 */

#include <stdlib.h>
#include "cdms.h"
#include "cddrsint.h"
#include "cdunifint.h"

					     /* Map from CDMS to cdunif file id and vice versa */
#define cd2cuFileid(id) ((int)id)
#define cd2cuVarid(id) ((int)id)
#define cu2cdFileid(id) ((long)id)
#define cu2cdVarid(id) ((long)id)

long cdDimGetDouble(long dsetid, long dimid, long start, long count, long stride, double modulus, double *values){
	CuType dtype;
	char *cp;
	double *dp, *ddp;
	float *fp;
	int *ip;
	int cufileid, cudimid;
	int i,k,newk;
	int direc;
	long *lp;
	long dlen, dlenbytes;
	long kcycle, kbase;
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
	long double *ldp;
#endif
	short *sp;
	void *dim;

	cufileid = cd2cuFileid(dsetid);
	cudimid = cd2cuVarid(dimid);
	

					     /* Retrieve the dimension type and length */
	if(cudiminq(cufileid,cudimid,0,0,&dtype,0,0,&dlen)==-1)
		return 0;

					     /* Get memory for the dimension */
	dlenbytes = dlen*cutypelen(dtype);
	if((dim = malloc(dlenbytes)) == (void*)0){
		cdError("Cannot allocate %d bytes of memory for dsetid = %d, dimid = %d",dlenbytes,dsetid,dimid);
		return 0;
	}
					     /* Get the dimension */
	if(cudimget(cufileid,cudimid,dim)==-1)
		return 0;

	switch(dtype){
	  case cdDouble:
		ddp = (double *)dim;
		direc = (*ddp<=*(ddp+dlen-1) ? 1 : -1);
		for(i=0, k=start, dp=values; i<count; i++, k+=stride){
			kcycle = (k>=0 ? k/dlen : ((k+1)/dlen)-1);
			kbase = k - kcycle*dlen;
			*dp++ = (double)ddp[kbase] + kcycle*direc*modulus;
		}				
		break;
	  case cdFloat:
		fp = (float *)dim;
		direc = (*fp<=*(fp+dlen-1) ? 1 : -1);
		for(i=0, k=start, dp=values; i<count; i++, k+=stride){
			kcycle = (k>=0 ? k/dlen : ((k+1)/dlen)-1);
			kbase = k - kcycle*dlen;
			*dp++ = (double)fp[kbase] + kcycle*direc*modulus;
		}				
		break;
	  case cdInt:
		ip = (int *)dim;
		direc = (*ip<=*(ip+dlen-1) ? 1 : -1);
		for(i=0, k=start, dp=values; i<count; i++, k+=stride){
			kcycle = (k>=0 ? k/dlen : ((k+1)/dlen)-1);
			kbase = k - kcycle*dlen;
			*dp++ = (double)ip[kbase] + kcycle*direc*modulus;
		}				
		break;
	  case cdLong:
		lp = (long*)dim;
		direc = (*lp<=*(lp+dlen-1) ? 1 : -1);
		for(i=0, k=start, dp=values; i<count; i++, k+=stride){
			kcycle = (k>=0 ? k/dlen : ((k+1)/dlen)-1);
			kbase = k - kcycle*dlen;
			*dp++ = (double)lp[kbase] + kcycle*direc*modulus;
		}				
		break;
	  case cdShort:
		sp = (short*)dim;
		direc = (*sp<=*(sp+dlen-1) ? 1 : -1);
		for(i=0, k=start, dp=values; i<count; i++, k+=stride){
			kcycle = (k>=0 ? k/dlen : ((k+1)/dlen)-1);
			kbase = k - kcycle*dlen;
			*dp++ = (double)sp[kbase] + kcycle*direc*modulus;
		}				
		break;
	  case cdChar:
	  case cdByte:
		cp = (char*)dim;
		direc = (*cp<=*(cp+dlen-1) ? 1 : -1);
		for(i=0, k=start, dp=values; i<count; i++, k+=stride){
			kcycle = (k>=0 ? k/dlen : ((k+1)/dlen)-1);
			kbase = k - kcycle*dlen;
			*dp++ = (double)cp[kbase] + kcycle*direc*modulus;
		}				
		break;
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
	  case cdLongDouble:
		ldp = (long double*)dim;
		direc = (*ldp<=*(ldp+dlen-1) ? 1 : -1);
		for(i=0, k=start, dp=values; i<count; i++, k+=stride){
			kcycle = (k>=0 ? k/dlen : ((k+1)/dlen)-1);
			kbase = k - kcycle*dlen;
			*dp++ = (double)ldp[kbase] + kcycle*direc*modulus;
		}				
		break;
#endif
	  default:
		cdError("Invalid dimension datatype %d",dtype);
		return 0;
	}

	free(dim);

	return 1;
}

long
cdNdimIntersect(long dsetid, long varid, const long order[], const double first[], const double last[], const double modulus[], cdIntersectPolicy policy, long start[], long count[], long stride[]){
	CuFile* file;			     /* Input file */
	CuType dtype;		             /* cdunif file datatype */
	int cufileid, cuvid;		     /* cdunif ids */
	int ndims;			     /* number of dimensions */
	int i,j;			     /* i = user array index, j = file array index */
	int dimids[CU_MAX_VAR_DIMS];	     /* variable cdunif dimension IDs, in file order */
	long transpose[CU_MAX_VAR_DIMS];     /* transpose vector */
	double *dvalues;		     /* Dimension values returned from cw_dimmap */
	double xdf, xdl;		     /* Actual first and last values of dimension */
	double cycle[CU_MAX_VAR_DIMS];	     /* Modulus vector, in user order */
	char dimname[CU_MAX_NAME+1];	     /* Dimension name */
	char varname[CU_MAX_NAME+1];	     /* Variable name */
	long idf, idl;			     /* First and last dimension indices */

					     /* Map from CDMS to cdunif ids */
	cufileid = cd2cuFileid(dsetid);
	cuvid = cd2cuVarid(varid);

					     /* Lookup the file */
	if((file=CuLookupFile(cufileid)) == (CuFile*)0)
		return 0;
					     /* Get the number of dimensions, dimension IDs, and file datatype */
	if(cuvarinq(cufileid, cuvid, varname, &dtype, &ndims, dimids, 0)==-1){
		return 0;
	}
					     /* Set the transpose vector. Default order is [0, 1, ..., ndims-1] */
	for(i=0; i<ndims; i++){
		transpose[i] = (order ? order[i] : i);
		cycle[i] = (modulus ? modulus[i] : 0.0);
	}
					     /* Map dimension ranges to indices. Free memory allocated in cw_dimget */
	for(i=0; i<ndims; i++){
		dvalues = (double *)0;	     /* Must be initialized for cw_dimmap !!! */
		if(cw_dimmap(cufileid, dimids[transpose[i]], first[i], last[i], (CwRoundPolicy)policy, 1.0e-5, cycle[i]>0.0, cycle[i], &dvalues, &idf, &idl, &xdf, &xdl)){
			if(cudiminq(cufileid, dimids[transpose[i]], dimname, 0, 0, 0, 0, 0)==-1)
				return 0;
			cdError("Error mapping range [%lf,%lf] onto dimension %s, variable %s, file %s", first[i], last[i], dimname, varname, file->controlpath);
			return 0;
		}

		free((void *)dvalues);

		start[i] = idf;
		if(idl >= idf){
			count[i] = idl-idf+1;
			stride[i] = 1;
		}
		else{
			count[i] = idf-idl+1;
			stride[i] = -1;
		}
	}

	return 1;
}

long
cdSlabRead(long dsetid, long varid, const long order[], const double first[], const double last[], const double modulus[], cdIntersectPolicy policy, cdType usertype, void *values){
	CuFile* file;			     /* Input file */
	int cufileid, cuvid;		     /* cdunif ids */
	int ndims;			     /* number of dimensions */
	int i;  			     /* i = user array index */
	long transpose[CU_MAX_VAR_DIMS];     /* transpose vector */
	long start[CU_MAX_VAR_DIMS];         /* Start indices in user order */
	long count[CU_MAX_VAR_DIMS];	     /* Counts, in user order */
	long stride[CU_MAX_VAR_DIMS];	     /* Strides, in user order */
	char varname[CU_MAX_NAME+1];	     /* Variable name */

					     /* Map from CDMS to cdunif ids */
	cufileid = cd2cuFileid(dsetid);
	cuvid = cd2cuVarid(varid);

					     /* Lookup the file */
	if((file=CuLookupFile(cufileid)) == (CuFile*)0)
		return 0;
					     /* Get the number of dimensions, dimension IDs, and file datatype */
	if(cuvarinq(cufileid, cuvid, varname, 0, &ndims, 0, 0)==-1){
		return 0;
	}
					     /* Set the transpose vector. Default order is [0, 1, ..., ndims-1] */
	for(i=0; i<ndims; i++){
		transpose[i] = (order ? order[i] : i);
	}
					     /* Map dimension ranges to indices. Free memory allocated in cw_dimget */
	if(cdNdimIntersect(dsetid, varid, order, first, last, modulus, policy, start, count, stride)==0)
		return 0;

					     /* Read the data */
	if(cuvargets(cufileid, cuvid, transpose, start, count, stride, (CuType)usertype, values)==-1){
		cdError("Error reading data for file %s, variable %s", varname, file->controlpath);
		return 0;
	}

	return 1;
}
