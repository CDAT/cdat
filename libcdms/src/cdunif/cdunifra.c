/* -*-Mode: C;-*-
 * Module:      cureadarray - cdunif generalized array read
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
 * $Log: cdunifra.c,v $
 * Revision 1.1.1.1  1997/12/09 18:57:40  drach
 * Copied from cirrus
 *
 * Revision 1.9  1995/10/16  18:54:32  drach
 * - Add CuInt datatype: add appropriate casting routines
 *
 * Revision 1.8  1995/09/15  21:08:53  drach
 * - Changed uninitialized reference to elemSize.
 *
 * Revision 1.7  1995/07/12  22:04:28  drach
 * - Removed long double type for SGI version
 *
 * Revision 1.6  1995/06/26  17:46:10  drach
 * Fixed casting bugs
 *
 * Revision 1.5  1995/03/15  02:40:22  drach
 * Solaris port
 *
 * Revision 1.4  1995/03/09  00:29:32  drach
 * Added casting, user-specified indices.
 *
 * Revision 1.3  1995/01/18  02:52:36  drach
 * - Improved comments
 *
 * Revision 1.2  1994/12/20  01:19:03  drach
 * - Prefixed cdrra functions with 'cu'
 *
 * Revision 1.1  1994/11/18  23:35:51  drach
 * Initial version
 *
 * Revision 1.1  1994/11/17  19:58:43  drach
 * Initial CVS version
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cdunifint.h"

#define CU_IS_FLOAT(t) (t==CuFloat || t==CuDouble || t==CuLongDouble)
#define CU_IS_INT(t) (t==CuByte || t==CuChar || t==CuShort || t==CuInt || t==CuLong)

/*

Name:
----

  int cureadarray(int fileid, int varid, CuRRA *vIndices, CuRRA* uIndices, const long transpose[],
                  CuType usertype, void *userArray);

Synopsis:
--------

  Read variable varid from file fileid, indices specified by vIndices, into
userArray. Dimensions may be transposed; dimension i of userArray
corresponds to dimension transpose[i] of var (0-origin dimensions and
dimension indices). Cast results to type usertype. Function returns -1 on error,
0 on success.

Arguments:
---------

fileid      : cdunif fileid of file containing the data, as returned from cuopenread.

varid       : cdunif variable id of variable to read.

vIndices    : pointer to CuRRA structure (defined in cdunif.h) specifying elements of var to read:

	      vIndices->rank = rank of var (number of dimensions)
	      vIndices->arraySize[i] = length of ith dimension of v, as it
	          exists in the file, elements of last dimension vary most rapidly.
	      vIndices->dimensionSize[i] = length of index vector vIndices->indices[i]
	      vIndices->indices[i] = vector of 0-origin indices pointing to elements
	          of the ith dimension of var to be read. Indices may be in any
	          order, and may be duplicated, e.g., for wraparound.

	      The elements of var to be read are identified by the
	      Cartesian product of the vIndices->indices index vectors (see
	      example below)

	      routines cucreateRRA and cudestroyRRA may be used to create and
	      free a CuRRA structure.

uIndices    : pointer to CuRRA structure specifying elements of userArray
	      to set.

	      uIndices->rank = rank of u, must equal vIndices->rank;
	      uIndices->arraySize[i] = length of ith dimension of userArray;
	      uIndices->dimensionSize[i] = length of index vector uIndices->indices[i],
			must equal vIndices->dimensionSize[transpose[i]];
	      uIndices->indices[i] = vector of 0-origin indices pointing to elements of
			the ith dimension of userArray to be set. Indices may be in any order.

	      If null (0), the structure is set such that contiguous elements of userArray
	      are set, specifically:
	      (1) uIndices->rank = vIndices->rank;
	      (2) uIndices->arraySize[i] = vIndices->arraySize[transpose[i]];
	      (3) uIndices->dimensionSize[i] = vIndices->dimensionSize[transpose[i]], and
	      (4) uIndices->indices[i] = <0,1,...,dimensionSize[i]-1>;
	      
	      	   
transpose   : transposition vector of length rank. The ith dimension of
	      userArray is assumed to correspond to dimension transpose[i]
	      of v, i=0 .. rank-1, and is assumed to have length vIndices->dimensionSize[i].
	      If transpose is the identity (<0 1 2 ... rank-1>) then no
	      dimension transposition occurs.

	      IMPORTANT NOTE: C majority is assumed here - first dimension
	      varies LEAST rapidly. transpose should be set such that
	      fileDimension = transpose[userDimension], where user dimension 0
	      varies least rapidly. Calling routines that observe the Fortran
	      majority convention - first dimension varies MOST rapidly - should
	      set transpose to reflect this fact.

usertype    : desired datatype of result

userArray   : destination array

Author: Bob Drach
------

Date: 10/6/93
----

Notes:
-----

(1) If no transposition or reordering is needed, data is read directly from
the file into the userArray, with no intermediate buffering.

(2) A copy buffer of length CU_BUFSIZE bytes is allocated, if necessary.
In general, the larger the copyBuffer, the fewer calls to cuvarget
to actually read the data. The routine translates requests for arbitrary
elements into one or more reads of hyperslabs in file ordering. If the
buffer is not large enough to hold all the data, the first dimension
(varying least rapidly) is broken into intervals small enough that the data
can fit into the buffer, and cuvarget is called iteratively. If this is
not possible, then the routine recurses, looping over the next dimension in
a similar fashion until the slabs are small enough. In the extreme, the
buffer can be as little as elemSize bytes, causing as many reads as
there are elements of var to be read.

Data is read in file order, to minimize disk thrashing. When reordering is
required, data is first read into copyBuffer, then copied to userArray, in
the physical ordering of userArray, again to minimize thrashing. Copying is
not recursive, for efficiency.

Example:
-------

vIndices->rank = 3
vIndices->arraySize = <2 3 5>
vIndices->dimensionSize = <1 4 2>
vIndices->indices[0] = <1>
vIndices->indices[1] = <0 1 2 0>
vIndices->indices[2] = <3 1>
transpose = <2 0 1>

Then userArray is assumed to have dimensionality 2 x 1 x 4, and the
following values are read:

v(1,0,3) -> user(0,0,0)
v(1,0,1) -> user(1,0,0)
v(1,1,3) -> user(0,0,1)
v(1,1,1) -> user(1,0,1)
v(1,2,3) -> user(0,0,2)
v(1,2,1) -> user(1,0,2)
v(1,0,3) -> user(0,0,3)
v(1,0,1) -> user(1,0,3)

Note that dimension 1 of var has duplicate indices. Also note that the actual
order in which the data is read is determined by the physical ordering of v.

*/

#define CU_NO_CASTING 0
#define CU_CASTING_UP 1
#define CU_CASTING_DOWN 2

static CuType fromType, toType;		     /* from, to datatypes */
static int fromLen, toLen;		     /* byte lengths of from, to datatypes */
static int mustCast, castInBuffer;	     /* mustCast = indicates nature of casting from file to
					      *   file to user datatypes:
					      *   = CU_NO_CASTING if no casting need be done,
					      *   = CU_CASTING_UP if casting to a higher datatype,
					      *   = CU_CASTING_DOWN if casting to a lower datatype;
					      *
					      * castInBuffer = true iff casting must be done in the copy buffer,
					      *              = false iff casting can be done in the user array
					      */ 

int cureadarray(int fileid, int varid, CuRRA *vIndices, CuRRA *uIndices, const long transpose[], CuType usertype, void *userArray){
	register int i,j;
	long rank;
	int mustCopy;
	CuType dtype;
	static void* copyBuffer = (void*)0;
	void *userOffsetArray;
	long nelems;
	long userOffset;
	long imap[CU_MAX_VAR_DIMS];
	long start[CU_MAX_VAR_DIMS];
	long count[CU_MAX_VAR_DIMS];
	long ud[CU_MAX_VAR_DIMS];
	int dns;			     /* First non-singleton dimension, 0-origin */
	int localUserIndices;		     /* True iff user indices created internally */


	                        /* Check that casting is valid */

	if(cuvarinq(fileid, varid, 0, &dtype, 0, 0, 0)==-1)
		return -1;

	fromType = dtype;
	toType = usertype;
	fromLen = cutypelen(dtype);
	toLen = cutypelen(toType);

	if((CU_IS_INT(dtype) && CU_IS_FLOAT(usertype)) ||
	   (CU_IS_FLOAT(dtype) && CU_IS_INT(usertype))){
		CuError(CU_ENOCAST,"Cannot cast from an integer to floating point datatype, or vice versa");
		return -1;
	}

					     /* Determine casting direction, if any */
	if(fromLen == toLen)
		mustCast = CU_NO_CASTING;
	else if(fromType < toType)
		mustCast = CU_CASTING_UP;
	else
		mustCast = CU_CASTING_DOWN;

				/* If var index vectors are all increasing intervals, and no transposition,
				   then read data directly from file into user array, as no reordering
				   needs to be done. */

	rank= vIndices->rank;
	if(rank<0 || rank>CU_MAX_VAR_DIMS){
		CuError(CU_EINTERN,"Rank = %d, is out of valid range 1 .. %d",rank,CU_MAX_VAR_DIMS);
		return -1;
	}

					     /* Handle rank 0 (scalar) */
	if(rank==0){
		if(cuvarget(fileid,varid,start,count,userArray)==-1)
			return -1;

		if(mustCast){
			if(cuCast(fromType, toType, 1, userArray)==-1){
				CuError(CU_ENOCAST,"Error casting from type %d to type %d",fromType,toType);
				return -1;
			}
		}

		return CU_SUCCESS;
	}

					     /* Casting down must be done in copy buffer, since not */
					     /* enough space in user array */
	
	mustCopy = castInBuffer = (mustCast == CU_CASTING_DOWN);
				  
				/* Create indices for user array if a null RRA was passed.
				   Since userArray is (by default) filled contiguously, the index vectors are
				   just 0-origin intervals (0,1,2,...,dimensionSize[i]) */

	if(uIndices == (CuRRA*)0){
		localUserIndices = 1;
		for(i=0; i<rank; i++)
			ud[i]=vIndices->dimensionSize[transpose[i]];
		
		uIndices= cucreateRRA(rank,ud,ud);
		for(i=0; i<rank; i++)
			for(j=0; j<uIndices->dimensionSize[i]; j++)
				uIndices->indices[i][j]= j;
	}
	else{
		localUserIndices = 0;
		for(i=0; i<rank; i++){
			if(uIndices->dimensionSize[i] != vIndices->dimensionSize[transpose[i]]){
				CuError(CU_EINTERN,"Dimension %d size of destination array = %d, does not match file dimension size %d",
					i,uIndices->dimensionSize[i],vIndices->dimensionSize[transpose[i]]);
				return -1;
			}
		}
	}
					     /* Find the first non-singleton dimension (or the last if none) */
	dns=0;
	while(dns<rank){
		if((uIndices->dimensionSize[dns] != 1) ||
		   dns == (rank-1))
			break;
		dns++;
	}
					     /* Initialize the imap for offset calc */
	imap[rank-1] = 1;
	for(i=rank-2; i>=0; i--)
		imap[i] = imap[i+1] * uIndices->arraySize[i+1];

					     /* Check that each dimension past the slowest varying nonsingleton */
					     /* dimension (dimension dns) */
					     /* is the full range - i.e., we are reading into */
					     /* contiguous memory in the user array; if not, */
					     /* must copy via the buffer. */
	if(!mustCopy){
		for(i=dns+1; i<rank; i++){
			if(uIndices->dimensionSize[i] != uIndices->arraySize[i]){
				mustCopy = castInBuffer = 1;
				break;
			}
		}
	}

	if(!mustCopy){
		for(i=dns+1; i<rank; i++){
			for(j=0; j<uIndices->dimensionSize[i]; j++){
				if(uIndices->indices[i][j] != j){
					mustCopy = castInBuffer = 1;
					break;
				}
			}
			if(mustCopy)
				break;
		}
	}
					     /* Check that the slowest varying nonsingleton */
					     /* dimension (dimension dns) is a range. */
					     /* If not, must copy via the buffer */
	if(!mustCopy){
		for(j=1; j<uIndices->dimensionSize[dns]; j++){
			if(uIndices->indices[dns][j] != uIndices->indices[dns][j-1] + 1){
				mustCopy = castInBuffer = 1;
				break;
			}
		}
	}
	
	
					     /* Must copy if any dimension transposition */
	if(!mustCopy)
		for(i=0; i<rank; i++)
			if(transpose[i] != i){
				mustCopy= 1;
				break;
			}
	
					     /* Must copy if source is not a hyperslab */
	if(!mustCopy)
		for(i=0; i<rank; i++){
			for(j=1; j<vIndices->dimensionSize[i]; j++){
				if(vIndices->indices[i][j] != (vIndices->indices[i][j-1] + 1)){
					mustCopy=1;
					break;
				}
				if(mustCopy)
					break;
			}
		}
					     /* The above checks ensure that: 
					      * If (!mustCopy or !castInBuffer (i.e., cast in user array) )) then 
					      * the user destination array is a contiguous chunk of memory, and
					      * the first index of the slowest varying dimension (0) is the
					      * minimum of the range in that dimension =>
					      * Number of elements to cast (if necessary) in the user array
					      * is the product of the user dimension sizes, and
					      * the byte offset in the user array may be calculated as
					      * offset = sum(indices[i][0] * imap[i]), i=1..rank-1, since ranges are increasing.
					      * Note that nelems and userOffset are used only if at least one
					      * of these conditions holds.
					      * */

	nelems = 1;
	userOffset = 0;
	for(i=0; i<rank; i++){
		nelems *= uIndices->dimensionSize[i];
		userOffset += imap[i]*(uIndices->indices[i][0]);
	}
	userOffset *= toLen;

					     /* If no copying required, read directly into user array */
	if(!mustCopy){
		
		for(i=0; i<rank; i++){
			start[i]= vIndices->indices[i][0];
			count[i]= vIndices->dimensionSize[i];
		}

					     /* Read directly into the offset user array */
		userOffsetArray = (void*)((char*)userArray + userOffset);

		if(cuvarget(fileid,varid,start,count,userOffsetArray)==-1)
			return -1;

		if(mustCast){
			if(cuCast(fromType, toType, nelems, userOffsetArray)==-1){
				CuError(CU_ENOCAST,"Error casting from type %d to type %d",fromType,toType);
				return -1;
			}
		}

		if(localUserIndices)
			cudestroyRRA(uIndices);

		return CU_SUCCESS;
	}
				/* Sort var indices, and user indices wrt var */

	if(cusortRRA(vIndices,uIndices,transpose))
		return -1;

					     /* Create the buffer */
	if(copyBuffer == (void*)0)
		if((copyBuffer = malloc(CU_BUFSIZE))==(void*)0){
			CuError(CU_SERROR,"Cannot allocate buffer of size %d bytes",CU_BUFSIZE);
			return -1;
		}

				/* Read var into userArray */

	if(cureadarray1(fileid,varid,vIndices,transpose,userArray,uIndices,copyBuffer, CU_BUFSIZE, 0)==-1)
		return -1;

					     /* Cast data in user array if necessary */
	if(mustCast && !castInBuffer){
		userOffsetArray = (void*)((char*)userArray + userOffset);
		if(cuCast(fromType, toType, nelems, userOffsetArray)==-1){
			CuError(CU_ENOCAST,"Error casting from type %d to type %d",fromType,toType);
			return -1;
		}
	}


	if(localUserIndices)
		cudestroyRRA(uIndices);

	return CU_SUCCESS;
}

int cureadarray1(int fileid, int varid, CuRRA *vIndices, const long transpose[], void *userArray, CuRRA *uIndices, void *copyBuffer, long lenBuffer, long iter){

	register int i,j,k0,k1;
	long rank, prod, *ud, *bd, *vd;
	long *start, *count;
	long foundk;
	CuRRA *ur, *b, *vr;
	size_t elemSize;
	long nelems;

				/* Check that iteration dimension is valid */
	rank= vIndices->rank;
	if(iter>=rank){
		CuError(CU_EINVAL,"Buffer size = %d bytes, must be at least %d bytes!", lenBuffer, fromLen);
		return -1;
	}

				/* Loop over iteration dimension */
	k1=k0=0;
	prod = ((castInBuffer && (mustCast==CU_CASTING_UP)) ? toLen : fromLen);
	for(i=iter+1; i<rank; i++)
		prod *= (vIndices->indices[i][vIndices->dimensionSize[i]-1] - vIndices->indices[i][0] + 1);
	
	while(k0<vIndices->dimensionSize[iter]){
		
				/* k1 = largest i, k0 <= i <= dimensionSize[iter] such that
				   the buffer is big enough to hold the array generated by
				   vIndices restricted to the interval [k0,k1] in dimension iter,
				   after any necessary casting of data */
		
		for(foundk=0, k1=vIndices->dimensionSize[iter]-1; k1>=k0; k1--)
			if((prod * (vIndices->indices[iter][k1] - vIndices->indices[iter][k0] + 1)) <= lenBuffer){
				foundk=1;
				break;
			}

				/* If such a k1 exists, read from var into the buffer */

		if(foundk){
			
			if(!(ud = (long *) malloc(rank * sizeof(long))) ||
			   !(bd = (long *) malloc(rank * sizeof(long))) ||
			   !(start = (long *) malloc(rank * sizeof(long))) ||
			   !(count = (long *) malloc(rank * sizeof(long)))
			   ){
				CuError(CU_SERROR,"Cannot allocate memory in cureadarray1.");
				return -1;
			}
			nelems = 1;
			for(i=0; i<rank; i++){
				if(i==iter){
					start[i]= vIndices->indices[i][k0];
					count[i]= vIndices->indices[i][k1] - vIndices->indices[i][k0]+1;
				}
				else{
					start[i]= vIndices->indices[i][0];
					count[i]= vIndices->indices[i][vIndices->dimensionSize[i]-1]
						- vIndices->indices[i][0] + 1;
				}
				nelems *= count[i];
			}

			if(cuvarget(fileid, varid, start, count, copyBuffer)==-1)
				return -1;

					     /* Cast data in buffer if necessary */
			if(mustCast && castInBuffer){
				if(cuCast(fromType,toType,nelems,copyBuffer)==-1){
					CuError(CU_ENOCAST,"Error casting from type %d to type %d",fromType,toType);
					return -1;
				}
			}
			
				/* ur is identical to uIndices, except that the iter
				   dimension is restricted to elements k0 through k1. */

			for(i=0; i<rank; i++)
				if(transpose[i]==iter)
					ud[i]= k1-k0+1;
				else
					ud[i]= uIndices->dimensionSize[i];
					
			ur= cucreateRRA(rank,uIndices->arraySize,ud);
			for(i=0; i<rank; i++)
				for(j=0; j<ud[i]; j++)
					if(transpose[i]==iter)
						ur->indices[i][j]= uIndices->indices[i][j+k0];
					else
						ur->indices[i][j]= uIndices->indices[i][j];

				/* b is the indices of var just read via cuvarget */
			for(i=0; i<rank; i++)
				if(i==iter)
					bd[i]= k1-k0+1;
				else
					bd[i] = vIndices->dimensionSize[i];

			b= cucreateRRA(rank, count, bd);
			for(i=0; i<rank; i++)
				for(j=0; j<b->dimensionSize[i]; j++)
					if(i==iter)
						b->indices[i][j]= vIndices->indices[i][j+k0] - vIndices->indices[i][k0];
					else
						b->indices[i][j]= vIndices->indices[i][j] - vIndices->indices[i][0];
			

				/* Now copy from the buffer into userArray */

			elemSize = ((mustCast && castInBuffer) ? toLen : fromLen);
			if(cuCopyArray((char *)copyBuffer,b,(char *)userArray,ur,transpose,elemSize)==-1)
				return -1;

			cudestroyRRA(ur);
			cudestroyRRA(b);
			free(ud);
			free(bd);
			free(start);
			free(count);
		}
			

				/* No k1 exists, so iterate over the next dimension,
				   letting k1 be the largest i, k0 <= i <= dimensionSize[iter],
				   such that index(iter,k0) = index(iter,k1), hence only
				   one value will be read for the iter dimension, as the index
				   vectors of var are sorted. */
		else {
			for(k1=k0; k1<vIndices->dimensionSize[iter]; k1++)
				if(vIndices->indices[iter][k1] != vIndices->indices[iter][k0])
					break;
			k1--;

				/* ur, vr are identical to uIndices, vIndices respectively,
				   except that the iter dimension
				   is restricted to elements k0 through k1. */
			
			if(!(ud = (long *) malloc(rank * sizeof(long))) ||
			   !(vd = (long *) malloc(rank * sizeof(long)))
			   ){
				CuError(CU_SERROR,"Cannot allocate memory in cureadarray1.");
				return -1;
			}

			for(i=0; i<rank; i++)
				if(transpose[i]==iter)
					ud[i]= k1-k0+1;
				else
					ud[i]= uIndices->dimensionSize[i];

			for(i=0; i<rank; i++)
				if(i==iter)
					vd[i]= k1-k0+1;
				else
					vd[i]= vIndices->dimensionSize[i];

			ur= cucreateRRA(rank,uIndices->arraySize,ud);
			vr= cucreateRRA(rank,vIndices->arraySize,vd);

			for(i=0; i<rank; i++)
				for(j=0; j<ur->dimensionSize[i]; j++)
					if(transpose[i]==iter)
						ur->indices[i][j]= uIndices->indices[i][j+k0];
					else
						ur->indices[i][j]= uIndices->indices[i][j];

			for(i=0; i<rank; i++)
				for(j=0; j<vr->dimensionSize[i]; j++)
					if(i==iter)
						vr->indices[i][j]= vIndices->indices[i][j+k0];
					else
						vr->indices[i][j]= vIndices->indices[i][j];

				/* Loop over the next dimension recursively */

			if(cureadarray1(fileid,varid,vr,transpose,userArray,ur,copyBuffer,lenBuffer,iter+1)==-1)
				return -1;
			
			cudestroyRRA(ur);
			cudestroyRRA(vr);
			free(ud);
			free(vd);
		}
				/* Increment loop counters and continue */
		
	k0= k1+1;
	}

	return CU_SUCCESS;
}

				/* Copy elements of array A specified by aIndices to
				 to elements of B specified by bIndices. Dimension k
				 of B corresponds to dimension transpose[k] of a.
				 Elements are copied in the physical ordering of B.
				 C-style array ordering is assumed, that is, elements
				 of the last dimension vary most rapidly. Each element
				 is elemSize bytes long. */
	
int cuCopyArray(char *a, CuRRA *aIndices, char *b, CuRRA *bIndices, const long transpose[], size_t elemSize){

	register int i, j, k;
	register long aoffset, boffset;
	long *qa, *qb, *adt, *aat, *bdt, *bat, *sortt, *odom, *ttemp1, *ttemp2;
	long *qat, *qbt;
	long rank, d0, nvals, r;
	CuRRA *at, *bt, *adiff, *bdiff;

	rank = aIndices->rank;
	if(rank != bIndices->rank){
		fprintf(stderr,"A rank = %d, B rank = %d, do not agree in cuCopyArray.",rank,bIndices->rank);
		return -1;
	}
	for(i=0;i<rank;i++){
		if(aIndices->dimensionSize[transpose[i]] != bIndices->dimensionSize[i]){
			CuError(CU_EINVAL,"A vector %d has length %d, != B vector %d, length %d.",transpose[i],aIndices->dimensionSize[transpose[i]],i,bIndices->dimensionSize[i]);
			return -1;
		}
	}

				/* Allocate temp arrays */
	if(!(qa = (long *) malloc(rank * sizeof(long))) ||
	   !(qb = (long *) malloc(rank * sizeof(long))) ||
	   !(qat= (long *) malloc(rank * sizeof(long))) ||	/* qa transpose */
	   !(adt= (long *) malloc(rank * sizeof(long))) ||	/* a dimensionSize transpose */
	   !(aat= (long *) malloc(rank * sizeof(long))) ||    /* a arraySize transpose */
	   !(qbt= (long *) malloc(rank * sizeof(long))) ||
	   !(bdt= (long *) malloc(rank * sizeof(long))) ||
	   !(bat= (long *) malloc(rank * sizeof(long))) ||
	   !(sortt = (long *) malloc(rank * sizeof(long))) ||
	   !(odom = (long *) malloc(rank * sizeof(long))) ||
	   !(ttemp1 = (long *) malloc(rank * sizeof(long))) ||
	   !(ttemp2 = (long *) malloc(rank * sizeof(long)))
	   ){
		CuError(CU_SERROR,"Cannot allocate memory in cuCopyArray.");
		return -1;
	}

				/* q(k,A) = product of s(j,A), j=k+1 .. rank(A)-1, where s(j,A) is
				   the length of the jth dimension of A */
	qa[rank-1]=qb[rank-1]=1;
	for(i=rank-2; i>=0; i--){
		qa[i]=qa[i+1] * aIndices->arraySize[i+1];
		qb[i]=qb[i+1] * bIndices->arraySize[i+1];
	}
	     
				/* Transpose B dimensions to correspond to A array,
				 putting singleton dimensions first. d0 = first non-singleton
				 dimension. In the process, copy aIndices and bIndices to at and
				 bt, respectively. */

	for(i=0, d0=0; i<rank; i++){
		if(bIndices->dimensionSize[i]==1){
			ttemp1[d0]=transpose[i];
			ttemp2[d0]=i;
			d0++;
		}
	}
	for(i=0,j=0,k=d0;i<rank;i++){
		if(j<d0 && i==ttemp2[j]){
			j++;
		}
		else{
			ttemp1[k]=transpose[i];
			ttemp2[k]=i;
			k++;
		}
	}
				
	for(i=0; i<rank; i++){
		qat[i]=qa[ttemp1[i]];
		adt[i]=aIndices->dimensionSize[ttemp1[i]];
		aat[i]=aIndices->arraySize[ttemp1[i]];
		qbt[i]=qb[ttemp2[i]];
		bdt[i]=bIndices->dimensionSize[ttemp2[i]];
		bat[i]=bIndices->arraySize[ttemp2[i]];
		sortt[i]=i;
	}
	at = cucreateRRA(rank,aat,adt);
	bt = cucreateRRA(rank,bat,bdt);
	for(i=0;i<rank;i++)
		for(j=0;j<at->dimensionSize[i];j++){
			at->indices[i][j] = aIndices->indices[ttemp1[i]][j];
			bt->indices[i][j] = bIndices->indices[ttemp2[i]][j];
		}
		       
				/* Sort A indices, B indices wrt A */
	if(cusortRRA(bt,at,sortt)<0)
		return -1;

				/* Create base, difference values for calculating offsets */
	adiff = cucreateRRA(rank,at->dimensionSize,at->dimensionSize);
	bdiff = cucreateRRA(rank,bt->dimensionSize,bt->dimensionSize);
	aoffset = boffset = 0;
	nvals = 1;
	for(i=0;i<rank;i++){
		odom[i]=0;
		nvals *= at->dimensionSize[i];
		aoffset += qat[i]*at->indices[i][0];
		boffset += qbt[i]*bt->indices[i][0];
		adiff->indices[i][adiff->dimensionSize[i]-1] = qat[i]*(at->indices[i][0] - at->indices[i][at->dimensionSize[i]-1]);
		bdiff->indices[i][bdiff->dimensionSize[i]-1] = qbt[i]*(bt->indices[i][0] - bt->indices[i][bt->dimensionSize[i]-1]);
		for(j=0;j<adiff->dimensionSize[i]-1;j++){
			adiff->indices[i][j]=qat[i]*(at->indices[i][j+1] - at->indices[i][j]);
			bdiff->indices[i][j]=qbt[i]*(bt->indices[i][j+1] - bt->indices[i][j]);
		}
	}

				/* Generate successive index vectors in order of A, and copy. */
	r=rank-1;
	for(i=0;i<nvals;i++){
				/* The actual copy */

		memcpy(b+boffset*elemSize,a+aoffset*elemSize,elemSize);

		k=r;
		aoffset += adiff->indices[k][odom[k]];
		boffset += bdiff->indices[k][odom[k]];
		odom[k] = (odom[k]+1) % adiff->dimensionSize[k];
		while(k>d0 && odom[k]==0){
			k--;
			aoffset += adiff->indices[k][odom[k]];
			boffset += bdiff->indices[k][odom[k]];
			odom[k] = (odom[k]+1) % adiff->dimensionSize[k];
		}
	}

				/* Free malloced structures */
	cudestroyRRA(at);
	cudestroyRRA(bt);
	cudestroyRRA(adiff);
	cudestroyRRA(bdiff);
	free(qa);
	free(qb);
	free(qat);
	free(adt);
	free(aat);
	free(qbt);
	free(bdt);
	free(bat);
	free(sortt);
	free(odom);
	free(ttemp1);
	free(ttemp2);

	return CU_SUCCESS;
}

					     /* Cast from fromType, to toType, number of elements nelems */
					     /* of array, in place */

int cuCast(CuType fromType, CuType toType, long nelems, void *array){
	int i;
	
	if(CU_IS_FLOAT(fromType) && CU_IS_FLOAT(toType)){
		if(cutypelen(fromType)==cutypelen(toType))
			return CU_SUCCESS;
		if(fromType < toType){
			for(i=fromType; i<toType; i++){
				switch(i){
				  case CuFloat:
					cuFloat2Double(nelems,array);
					break;
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
				  case CuDouble:
					cuDouble2LongDouble(nelems,array);
					break;
#endif
				  default:
					CuError(CU_ENOCAST,"Invalid file datatype %d",fromType);
					return -1;
				}
			}
		}
		else {
			for(i=fromType; i>toType; i--){
				switch(i){
				  case CuDouble:
					cuDouble2Float(nelems,array);
					break;
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
				  case CuLongDouble:
					cuLongDouble2Double(nelems, array);
					break;
#endif
				  default:
					CuError(CU_ENOCAST,"Invalid file datatype %d",fromType);
					return -1;
				}
			}
		}
	}
	else if(CU_IS_INT(fromType) && CU_IS_INT(toType)){
		if(cutypelen(fromType)==cutypelen(toType))
			return CU_SUCCESS;
		if(fromType < toType){
			for(i=fromType; i<toType; i++){
				switch(i){
				  case CuByte:
					break;
				  case CuChar:
					cuChar2Short(nelems,array);
					break;
				  case CuShort:
					cuShort2Int(nelems,array);
					break;
				  case CuInt:
					cuInt2Long(nelems,array);
					break;
				  default:
					CuError(CU_ENOCAST,"Invalid file datatype %d",fromType);
					return -1;
				}
			}
		}
		else {
			for(i=fromType; i>toType; i--){
				switch(i){
				  case CuChar:
					break;
				  case CuShort:
					cuShort2Char(nelems,array);
					break;
				  case CuInt:
					cuInt2Short(nelems,array);
					break;
				  case CuLong:
					cuLong2Int(nelems, array);
					break;
				  default:
					CuError(CU_ENOCAST,"Invalid file datatype %d",fromType);
					return -1;
				}
			}
		}
	}
	else {
		CuError(CU_ENOCAST,"Cannot cast from type %d to type %d",fromType,toType);
		return -1;
	}
	return CU_SUCCESS;
}

void cuFloat2Double(long nelems, void *buf){
	float *from;
	double *to;
	long i;

	from = (float*)buf + nelems - 1;
	to = (double*)buf + nelems - 1;

	for(i=0; i<nelems; i++)
		*to-- = *from--;

	return;
}
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
void cuDouble2LongDouble(long nelems, void *buf){
	double *from;
	long double *to;
	long double temp;
	long i;

	from = (double*)buf + nelems - 1;
	to = (long double*)buf + nelems - 1;

	for(i=0; i<nelems; i++){
		temp = (long double)*from--;
		*to-- = temp;
	}

	return;
}
#endif
void cuChar2Short(long nelems, void *buf){
	char *from;
	short *to;
	long i;

	from = (char*)buf + nelems - 1;
	to = (short*)buf + nelems - 1;

	for(i=0; i<nelems; i++)
		*to-- = *from--;

	return;
}
void cuShort2Int(long nelems, void *buf){
	short *from;
	int *to;
	long i;

	from = (short*)buf + nelems - 1;
	to = (int*)buf + nelems - 1;

	for(i=0; i<nelems; i++)
		*to-- = *from--;

	return;
}
void cuInt2Long(long nelems, void *buf){
#if defined(__alpha) || defined(__ia64) || defined(__x86_64__)
	int *from;
	long *to;
	long i;

	from = (int*)buf + nelems - 1;
	to = (long*)buf + nelems - 1;

	for(i=0; i<nelems; i++)
		*to-- = *from--;
#endif
	return;
}
void cuDouble2Float(long nelems, void *buf){
	double *from;
	float *to;
	long i;

	from = (double*)buf;
	to = (float*)buf;

	for(i=0; i<nelems; i++)
		*to++ = *from++;

	return;
}
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
void cuLongDouble2Double(long nelems, void *buf){
	long double *from;
	double *to;
	long i;

	from = (long double*)buf;
	to = (double*)buf;

	for(i=0; i<nelems; i++)
		*to++ = *from++;

	return;
}
#endif
void cuShort2Char(long nelems, void *buf){
	short *from;
	char *to;
	long i;

	from = (short*)buf;
	to = (char*)buf;

	for(i=0; i<nelems; i++)
		*to++ = *from++;

	return;
}
void cuInt2Short(long nelems, void *buf){
	int *from;
	short *to;
	long i;

	from = (int*)buf;
	to = (short*)buf;

	for(i=0; i<nelems; i++)
		*to++ = *from++;

	return;
}
void cuLong2Int(long nelems, void *buf){
#if defined(__alpha) || defined(__ia64) || defined(__x86_64__)
	long *from;
	int *to;
	long i;

	from = (long*)buf;
	to = (int*)buf;

	for(i=0; i<nelems; i++)
		*to++ = *from++;
#endif
	return;
}
