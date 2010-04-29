/* -*-Mode: C;-*-
 * Module:      cddrs - DRS wrapper routines
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
 * $Log: cddrs.c,v $
 * Revision 1.21  1997/01/28  15:08:32  drach
 * Added functions getcdimD and getedimD. These functions pass back to
 * the user double precision values.
 *
 * Revision 1.20  1996/10/31  23:52:41  drach
 * - Cleaned up error returns
 *
 * Revision 1.19  1996/09/09  18:17:31  drach
 * - (cddrs.c) alias 'comments' and 'source', 'title' and 'long_name'
 * - (cddrswrap.c) include math.h
 *
 * Revision 1.18  1995/10/16  18:55:22  drach
 * - Added CuInt type, DEC Alpha version
 *
 * Revision 1.17  1995/07/12  22:07:16  drach
 * - Remove long double type for SGI version
 * - Add cw_get_fileid function to get cdunif file ID
 *
 * Revision 1.16  1995/06/26  17:58:37  drach
 * - Blank dunits, dname in getcdim, in case netCDF doesn't return
 *   the null terminator.
 *
 * Revision 1.15  1995/06/26  17:52:06  drach
 * - Set dimension names, units to blank before setting, in case netCDF
 *   doesn't return the null terminator
 *
 * Revision 1.14  1995/06/09  22:39:42  drach
 * - Add option for strings to be up to CW_MAX_NAME chars in length
 * - Add option for alternate dimension types to be returned
 * - Allow non-open files to be closed without error
 *
 * Revision 1.13  1995/03/30  00:20:02  drach
 * Allow 99 as a valid lu
 *
 * Revision 1.12  1995/03/15  02:39:45  drach
 * Solaris port
 *
 * Revision 1.11  1995/03/09  00:34:01  drach
 * Updated for version of cureadarray which has user-specified indices, casting
 * Various bug fixes.
 *
 * Revision 1.10  1995/01/30  18:08:56  drach
 * - Minor revision
 *
 * Revision 1.9  1995/01/30  17:58:56  drach
 * - Corrected for Fortran I/O:
 *   Set transpose properly for Fortran majority when not all dimensions
 *     are set by user
 *   Treat "" as string null, for cfortran.h compatibility
 * - Null variable name matches anything, to allow 'old-style' searching.
 * Revision 1.8  1995/01/21  00:50:31  drach
 * - Added getelemd, getrge2, cw_unif_to_enumtype, compatibility wrappers for
 *   write functions
 * - Numerous bug fixes and error message improvements
 *
 * Revision 1.7  1995/01/18  02:51:02  drach
 * - Added cw_getslab, cw_majority, cw_seterr
 * - Added recognition of majority in cw_getdat, cw_getslab, cw_inqdict
 *
 * Revision 1.6  1995/01/13  01:00:20  drach
 * - Added getcdim, getedim, getnd, cw_lookup_cycle
 * - Pulled cw_dimget out of cw_dimmap, into a separate routine
 *
 * Revision 1.5  1994/12/20  01:17:46  drach
 * - Added generalized read for transposition and reversal
 *
 * Revision 1.4  1994/12/17  00:36:32  drach
 * - Save path for error reporting
 * - Handle null dimension limits in setdim, setvdim
 *
 * Revision 1.3  1994/12/16  00:44:18  drach
 * - Added automatic file extensions to cw_aslun
 * - Added cw_getdat, cw_varid, cw_string_attget, cw_drs_to_unif_datatype,
 *   cw_add_extension, and cw_strtrim
 *
 * Revision 1.2  1994/12/14  02:30:35  drach
 * - Added cw_aslun, cw_cllun, cw_cluvdb, cw_setdim, cw_setname, cw_setvdim,
 *   cw_error, cw_lookup, cw_lookup_with_policy, and cw_dimmap
 *
 * Revision 1.1  1994/11/23  22:59:10  drach
 * Initial version.
 *
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <math.h>
#include "drscdf.h"
#include "cdunifint.h"
#include "cddrsint.h"

					     /* Portability options */
static CwExtensionOption cwDimensionOption = CW_STANDARD; /* CW_EXTENDED => recognizes dimension type IDRS_IMPLICIT_VECTOR, */
					     /*   meaning a vector dimension with no corresponding variable*/
static cwSourceLen = IDRS_SOURCELEN;	     /* Source string length */
static cwNameLen = IDRS_NAMELEN;	     /* Name string length */
static cwTitleLen = IDRS_TITLELEN;	     /* Title string length */
static cwUnitsLen = IDRS_UNITSLEN;	     /* Units string length */
static cwDateLen = IDRS_DATELEN;	     /* Date string length */
static cwTimeLen = IDRS_TIMELEN;	     /* Time string length */
static cwTypeLen = IDRS_TYPELEN;	     /* Datatype string length */

static CwMajority cwMajority = CW_FORTRAN_MAJORITY; /* Default majority */
static int fileMap[CU_MAX_LU+1];		     /* fileMap[lu] = fileid */
static char* filePath[CU_MAX_LU+1];	     /* filePath[lu] = file pathname */
static int currentVar[CU_MAX_LU+1];	     /* varid of most recently read or inquired var */

					     /* vdb, vdims, and transpose */
static CwVar vdb;			     /* Variable descriptor block */
static CwDim vdims[CU_MAX_VAR_DIMS];	     /* Dimensions for VDB */
static long transpose[CU_MAX_VAR_DIMS];      /* transposition vector for most recent I/O */
static int libInit = 0;		             /* 1 iff cw_aslun has been called */

int cw_aslun(int lud,char* dicfil,int lu,char* datfil,int istat){
	char controlpath[CU_MAX_PATH],datapath[CU_MAX_PATH];
	int fileid;
	int i;

					     /* Initialize lu-to-fileid map, if necessary */
	if(!libInit){
		for(i=0;i<=CU_MAX_LU;i++){
			fileMap[i]=-1;
			filePath[i]=(char*)0;
			currentVar[i] = -1;
		}
		libInit=1;
	}

					     /* Check argument validity */
	if(lud<0 || lud>CU_MAX_LU || lu<0 || lu>CU_MAX_LU){
		cw_error("Opening DRS file %s",dicfil);
		return IDRS_BADLU;
	}
	
	if(istat == IDRS_CREATE){
		cw_error("Warning: Cannot create file %s, interface is read-only.",dicfil);
		return IDRS_BADSTATUS;
	}
					     /* Add file extensions if necessary */
	cw_add_extension(dicfil,".dic",controlpath);
	cw_add_extension(datfil,".dat",datapath);
	
					     /* Set the logical units */
	if(cusetlu(lud,lu) != CU_SUCCESS)
		return cw_geterr();

					     /* Open */
	if((fileid=cuopenread(controlpath,datapath)) == -1)
		return cw_geterr();

					     /* Cache lu and control pathname */
	fileMap[lud] = fileid;
	if(!filePath[lud])
		if((filePath[lud]=malloc(CU_MAX_PATH))==(char*)0){
			cw_error("Error allocating memory");
			return IDRS_NOMEMORY;
		}
	strncpy(filePath[lud],controlpath,CU_MAX_PATH);
	currentVar[lu] = -1;
		
	cw_cluvdb();

	return IDRS_SUCCESS;
}
int cw_cllun(int lu){
	if(lu<0 || lu>CU_MAX_LU){
		cw_error("Closing lu %d",lu);
		return IDRS_BADLU;
	}

					     /* OK to close a non-opened file */
	if(libInit && fileMap[lu]!=-1 && cuclose(fileMap[lu]) != CU_SUCCESS)
		return cw_geterr();

	fileMap[lu] = -1;
	currentVar[lu] = -1;

	return IDRS_SUCCESS;
}
int cw_cluvdb(void){

	static int valsInit = 0;	     /* 1 iff dimension pointers have been initialized */
	int i;

	if(valsInit == 0){
		for(i=0; i<CU_MAX_VAR_DIMS; i++){
			vdims[i].values = (double*)0;
		}
		valsInit = 1;
	}

	vdb.filelu = -1;
	strcpy(vdb.source,CW_STRING_NULL);
	strcpy(vdb.name,CW_STRING_NULL);
	strcpy(vdb.title,CW_STRING_NULL);
	strcpy(vdb.units,CW_STRING_NULL);
	strcpy(vdb.date,CW_STRING_NULL);
	strcpy(vdb.time,CW_STRING_NULL);
	strcpy(vdb.type,CW_STRING_NULL);
	vdb.ndims = 0;

	for(i=0; i<CU_MAX_VAR_DIMS; i++){
		strcpy(vdims[i].source,CW_STRING_NULL);
		strcpy(vdims[i].name,CW_STRING_NULL);
		strcpy(vdims[i].title,CW_STRING_NULL);
		strcpy(vdims[i].units,CW_STRING_NULL);
		vdims[i].type = 0;
		vdims[i].dfreq = CW_FLOAT_NULL;
		vdims[i].dlreq = CW_FLOAT_NULL;
		vdims[i].dfactual = CW_FLOAT_NULL;
		vdims[i].dlactual = CW_FLOAT_NULL;
		vdims[i].len = 0;
		vdims[i].isset = 0;
		if(vdims[i].values != (double*)0){
			free(vdims[i].values);
			vdims[i].values = (double*)0;
		}
		vdims[i].isCycle = 0;	     /* No dimension cycle by default */
		vdims[i].cycle = 0.0;
		transpose[i] = -1;	     /* getdat, getslab, and inqdict will set*/
	}
		
	return IDRS_SUCCESS;
}
int cw_drstest(int ierr){
	return ((ierr != IDRS_SUCCESS) && (ierr<1000));
}
int cw_getdat(int lu,void* a,int isize){

	CuRRA* fileRRA;			     /* requested indices, right-ragged array */
	CuType dtype, usertype;		     /* cdunif file and user datatype */
	char* filename;			     /* Controlfile pathname */
	double xdf, xdl;		     /* actual first and last values to be read */
	int countsArePositive;		     /* true iff no user dimension reversals specified */
	int dimid;			     /* cdunif dimension ID */
	int dimids[CU_MAX_VAR_DIMS];	     /* variable cdunif dimension IDs */
	int filedir[CU_MAX_VAR_DIMS];	     /* +1/-1 for increasing/decreasing dimension */
	int fileid, varid;		     /* cdunif file and variable IDs */
	int found;			     /* true iff user dimension name matched */
	int i,j,j2;			     /* i = user array index, j = file array index */
	int nbytesRequested;		     /* number of bytes of data to be read */
	int ndims;			     /* number of dimensions */
	int transposeIsIdentity;	     /* true iff the canonical transpose is the identity */
	long canonTranspose[CU_MAX_VAR_DIMS]; /* transpose in C majority */
	long count[CU_MAX_VAR_DIMS];	     /* dimension counts - number of values to be retrieved */
	long filedcount[CU_MAX_VAR_DIMS];    /* absolute dimension counts, in file order */
	long filedlen[CU_MAX_VAR_DIMS];	     /* dimension lengths, in file order */
	long filedstart[CU_MAX_VAR_DIMS];    /* dimension start indices, in file order */
	long idf, idl;			     /* first and last index of dimension range */
	long k, ktr;			     /* counters */
	long start[CU_MAX_VAR_DIMS];	     /* start indices, in user order */
	long transtemp[CU_MAX_VAR_DIMS];     /* scratch vector, in user order */

					     /* Check args */
	if(lu<0 || lu> CU_MAX_LU){
		cw_error("Bad logical unit %d",lu);
		return IDRS_BADLU;
	}

	fileid = fileMap[lu];
	if(fileid==-1){
		cw_error("File not open, logical unit %d",lu);
		return IDRS_BADLU;
	}
	filename = filePath[lu];

					     /* Lookup the variable id */
	if((varid = cw_varid(fileid,vdb.source,vdb.name,vdb.title,vdb.units))==-1){
		cw_error("Variable not found: %s, file %s",vdb.name,filename);
		return IDRS_VDBNOTFOUND;
	}
	
	vdb.filelu = lu;		     /* At this point, we know which file will be accessed */
	currentVar[lu] = varid;		     /* and which variable */
	
					     /* Get the variable datatype, number of dimensions, dimension IDs */
	if(cuvarinq(fileid, varid, 0, &dtype, &ndims, dimids, 0)==-1){
		cw_error("Error inquiring variable %s, file %s",vdb.name,filename);
		return IDRS_VDBNOTFOUND;
	}
					     /* Check that setdim or setvdim was not called with n>ndims */
	for(i=ndims;i<CU_MAX_VAR_DIMS;i++){
		if(vdims[i].isset){
			cw_error("Invalid dimension %d was set for variable %s, file %s",i+1,vdb.name,filename);
			return IDRS_BADDIM;
		}
	}
					     /* Convert requested datatype from 'X*n' form
					      * to a CuType; null-string defaults to file type*/
	if(!strcmp(vdb.type,CW_STRING_NULL))
		usertype = dtype;
	else if((usertype=cw_drs_to_unif_datatype(vdb.type))==-1){
		cw_error("Invalid datatype: %s, variable %s, file %s",vdb.type,vdb.name,filename);
		return IDRS_BADTYPE;
	}
	
					     /* Initialize the transposition vector */
	for(i=0;i<ndims;i++){
		transtemp[i] = i;
		transpose[i] = -1;
	}
	
					     /* Create the transposition vector for user-defined dimensions. */
	for(i=0;i<ndims;i++){
		if(!vdims[i].isset)
			continue;
					     /* Lookup ith (user) dimension by name; */
					     /* If a null name was specified, set dimid to the i-th */
					     /* file dimension, respective of majority*/
		if(!strcmp(vdims[i].name,CW_STRING_NULL)){
			dimid = dimids[cwMajority==CW_C_MAJORITY ? i : ndims-i-1];
		}
		else if((dimid=cudimid(fileid,varid,vdims[i].name))==-1){
			cw_error("Looking up dimension: %s of variable %s, file %s", vdims[i].name, vdb.name, filename);
			return -1;
		}
					     /* Find the ith (user) dimension in the (file) dimension IDs */
		for(j=0, found=0; j<ndims; j++){
			if(dimid==dimids[j]){
				transpose[i]=j;
				transtemp[j]=-1; /* This file dimension has been mapped */
				found=1;
				break;
			}
		}
		if(!found){
					     /* This should never occur unless there's an internal error. */
			cw_error("Looking up dimension: %s of variable %s, file %s", vdims[i].name, vdb.name, filename);
			return IDRS_BADDIMNAME;
		}
	}
					     /* Create transposition vector for non-user-specified dimensions.
					      * i points to user array, j to file ordering */
	for(j2=0,i=0; j2<ndims; j2++){
		j = (cwMajority==CW_C_MAJORITY ? j2 : (ndims - j2 - 1));
		                             /* move j to the next file dimension not mapped */
		if(transtemp[j]==-1)
			continue;
					     /* move i to the next user dimension not mapped */
		while((i<ndims) && (transpose[i] != -1))
			i++;                 
		transpose[i]=j;
	}

					     /* Map dimensions to start and count arrays */
	countsArePositive = 1;
	nbytesRequested = cutypelen(usertype);
	for(i=0; i<ndims; i++){
		if(!vdims[i].isset){	     /* Non-user-specified dimensions - just get the length */
			if(cudiminq(fileid, dimids[transpose[i]], 0, 0, 0, 0, 0, count+i)==-1){
					     /* This should never happen unless an internal error occurs. */
				cw_error("Internal error looking up dimension %d of var %s, lu %d",i,vdb.name,lu);
				return IDRS_BADDIM;
			}
			start[i]=0;
			vdims[i].len = count[i];
		}
		else{			     /* User-specified dimensions */
			if(cw_dimmap(fileid,dimids[transpose[i]], vdims[i].dfreq, vdims[i].dlreq,
				     CW_RANGE, 1.0e-5, vdims[i].isCycle, vdims[i].cycle,
				     &vdims[i].values, start+i, &idl, &vdims[i].dfactual,
				     &vdims[i].dlactual) == -1){
				cw_error("Error mapping range [%lf,%lf] onto dimension %s, variable %s, file %s",
					 vdims[i].dfreq,vdims[i].dlreq, vdims[i].name, vdb.name, filename);
				return IDRS_BADDIM;
			}
			vdims[i].len = count[i] = ((idl >= start[i]) ? (idl - start[i] + 1) : (idl - start[i] -1));
			nbytesRequested *= abs(count[i]);
			countsArePositive &= (count[i]>0);
		}
	}

					     /* Set canonical transpose: C majority for cdunif
					      * Note: really should be 'j' (file) indexing here. */
	transposeIsIdentity = 1;
	for(i=0; i<ndims; i++){
		canonTranspose[i] = transpose[(cwMajority==CW_C_MAJORITY) ? i : (ndims-i-1)];
		transposeIsIdentity &= (canonTranspose[i]==i);
	}
	
					     /* Setting isize = 0 same as inquiry in that:
					      * 1) currentVar[lu] and vdb.filelu have been set, and
					      * 2) the transposition vector has been set
					      * */
	if(isize == CW_INT_NULL)
		return IDRS_SUCCESS;
					     /* Check that number of bytes matches requested size */
	if(nbytesRequested > isize){
		cw_error("Number of bytes specified: %d, does not match number to be read: %d, variable %s, file %s",
			 isize,nbytesRequested,vdb.name,filename);
		return IDRS_BADLEN;
	}
					     /* If canonical transpose is the identity vector,
					      * AND counts are positive (no reversals),
					      * AND no casting is needed,
					      * use cuvarget to read data */
	if(transposeIsIdentity && countsArePositive && (usertype==dtype)){
					     /* cdunif uses file ordering */
		for(i=0; i<ndims; i++){
			filedcount[transpose[i]] = count[i];
			filedstart[transpose[i]] = start[i];
		}
		if(cuvarget(fileid,varid,filedstart,filedcount,a)==-1){
			cw_error("Error reading data for variable %s, file %s",vdb.name,filename);
			return IDRS_CANNOTREADDATA;
		}
	}
	else{				     /* Call generalized array reader */
		for(i=0; i<ndims; i++){
			if(cudiminq(fileid, dimids[transpose[i]], 0, 0, 0, 0, 0, &(filedlen[transpose[i]]))==-1){
					     /* This should never happen unless an internal error occurs. */
				cw_error("Internal error looking up dimension %d of var %s, lu %d",i,vdb.name,lu);
				return IDRS_BADDIM;
			}
			filedcount[transpose[i]] = abs(count[i]);
			filedstart[transpose[i]] = start[i];
			filedir[transpose[i]] = (count[i]>0 ? 1 : -1);
		}
					     /* Create right-ragged-array struct for requested indices
					      * Note that dimensions are in FILE order.
					      */
		fileRRA = cucreateRRA( ndims, filedlen, filedcount);

					     /* Specify the indices of elements to be read */
		for(j=0; j<ndims; j++){
			for(k=filedstart[j], ktr=0; ktr<filedcount[j]; ktr++, k+=filedir[j]){
				CU_SETRRA(fileRRA, j, ktr, k);
			}
		}
		if(cureadarray(fileid, varid, fileRRA, 0, canonTranspose, usertype, a)==-1){
			cw_error("Error reading data for variable %s, file %s",vdb.name,filename);
			return IDRS_CANNOTREADDATA;
		}
		cudestroyRRA(fileRRA);
	}

	return IDRS_SUCCESS;
}
int cw_getcdim(int n,char* dsrc,char* dna,char* dti,char* dun,int* dtype,int reqlen,float* var,int* retlen){

	CuDimType dimtype;		     /* Global or local */
	char varname[CU_MAX_NAME];	     /* Name of current variable */
	double cycle;			     /* Dimension period */
	double xdf, xdl;		     /* Actual range limits */
	double *tab;			     /* Base dimension */
	int dimid, dimvarid;		     /* Dimension ID, Dimension variable ID (if any) */
	int fileid;			     /* ID of current file */
	int i;
	int incr;			     /* +1 if dimension is increasing, -1 else */
	int incrindex;			     /* +1 if indices are increasing, -1 else */
	int nd;				     /* Number of dimensions */
	int varid;			     /* ID of current variable */
	int dimids[CU_MAX_VAR_DIMS];	     /* Dimension IDs, in file order */
	long dlen;			     /* Base dimension length */
	long icycle;			     /* Number of cycles from base dimension range */
	long idf, idl;			     /* Virtual range indices */
	long vlen;			     /* Virtual dimension length */
	char dname[CU_MAX_NAME];	     /* Dimension name */
	char dunits[CU_MAX_NAME];	     /* Dimension units */
	char format[CU_MAX_NAME];	     /* Data file format */

					     /* Check for valid current fileid */
	if(vdb.filelu < 0 || vdb.filelu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}

					     /* Get the fileid, varid */
	fileid = fileMap[vdb.filelu];
	if((varid=currentVar[vdb.filelu]) < 0){
		cw_error("No variable current for file %s",filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Get the dimension ID */
	if(cuvarinq(fileid, varid, varname, 0, &nd, dimids, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}
	if((n-1)<0 || (n-1)>nd){
		cw_error("Dimension number = %d, is out of range for file %s, variable %s (max of %d)",
			 n, filePath[vdb.filelu], varname, nd);
		return IDRS_BADDIM;
	}
	dimid = dimids[transpose[n-1]];

					     /* Inquire the dimension */
	for(i=0;i<CU_MAX_NAME;i++)
		dname[i]=dunits[i]='\0';
	if(cudiminq(fileid, dimid, dname, dunits, 0, &dimtype, 0, &dlen)==-1){
		cw_error("Error inquiring dimension %d of file %s, variable %s",n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* If the dimension name matches that of the variable,
					      * and the variable has only one dimension, then
					      * this is a dimension variable.
					      */
	if((!strcmp(dname,varname)) && (nd==1)){
		if(dna) strcpy(dna,"internal");
		if(dun) strcpy(dun,"internal");
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
		if(dtype) *dtype = (cwDimensionOption == CW_STANDARD ? IDRS_EQUALLY_SPACED : CW_LOCAL);
		if(reqlen == 0){
			if(retlen) *retlen = 0;
		}
		else if(reqlen >= dlen){
			for(i=1; i<=dlen; i++)
				*var++ = (float)i;
			if(retlen) *retlen = dlen;
		}
		else{
			cw_error("Requested dimension length = %d, should be at least %d, file %s, variable %s",
				 reqlen, dlen, filePath[vdb.filelu], varname);
			return IDRS_BADLEN;
		}
		return IDRS_SUCCESS;
	}
	
	if(dna) strncpy(dna,dname,cwNameLen);
	if(dun) strncpy(dun,dunits,cwUnitsLen);

					     /* Get the dimension varid, if any, or -1 if none */
	dimvarid = cw_dimension_varid(fileid, dimid, dname);
	
					     /* Set the dimension type */
	if(dtype){
					     /* If the dimension is shared, explicit ... */
		if(dimtype == CuGlobalDim && (dimvarid != -1)){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_SHARED);
		}
					     /* If the dimension is shared, implicit ... */
		else if(dimtype == CuGlobalDim){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_IMPLICIT_SHARED);
		}
					     /* If the dimension is local, report it as equally-spaced */
					     /* only if the file is in DRS format, as the other formats */
					     /* can have non-variable dimensions which require a vector */
					     /* representation (e.g., GrADS) */
		else {
			if(cwDimensionOption == CW_STANDARD){
				cw_string_attget(fileid,CU_GLOBAL,"format",format);
				*dtype = (!strcmp(format,"DRS") ? IDRS_EQUALLY_SPACED : IDRS_UNEQUALLY_SPACED);
			}
			else{
				*dtype = CW_LOCAL;
			}
		}
	}
		

					     /* If the dimension has a varid, get source and title, if any */
	if(dimvarid != -1){
		if(dsrc) cw_string_attget(fileid, dimvarid, "source", dsrc);
		if(dti) cw_string_attget(fileid, dimvarid, "title", dti);
	}
	else{
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
	}

					     /* Get the dimension limits */
	if(cw_dimmap(fileid, dimid, vdims[n-1].dfreq, vdims[n-1].dlreq, CW_RANGE, 1.0e-5, vdims[n-1].isCycle,
		     vdims[n-1].cycle, &vdims[n-1].values, &idf, &idl, &xdf, &xdl)==-1){
		cw_error("Error inquiring dimension range for dimension %d of file %s, variable %s",
			 n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* Copy the dimension to the user array */
	vlen = (idf < idl ? (idl-idf+1) : (idf-idl+1));

	if(reqlen == 0 || (var==(float*)0)){
		if(retlen) *retlen = 0;
	}
	else if (reqlen < vlen){
		if(retlen) *retlen = 0;
		cw_error("Requested dimension length = %d, should be at least %d, file %s, variable %s",
			 reqlen, vlen, filePath[vdb.filelu], varname);
		return IDRS_BADLEN;
	}
	else {
		tab = vdims[n-1].values;
		cycle = (vdims[n-1].isCycle ? vdims[n-1].cycle : 0.0);
		incrindex = (idf <= idl ? 1 : -1);
		incr = (tab[0] < tab[dlen-1] ? 1 : -1);
		for(i=idf; incrindex*i <= incrindex*idl; i+=incrindex){
			icycle = (i>=0 ? (i/dlen) : ((i+1)/dlen)-1);
			*var++ = (float)(tab[i-icycle*dlen] + incr*icycle*cycle);
		}
		if(retlen) *retlen = vlen;
	}
	   
	return IDRS_SUCCESS;
}
int cw_getcdimD(int n,char* dsrc,char* dna,char* dti,char* dun,int* dtype,int reqlen,double* var,int* retlen){

	CuDimType dimtype;		     /* Global or local */
	char varname[CU_MAX_NAME];	     /* Name of current variable */
	double cycle;			     /* Dimension period */
	double xdf, xdl;		     /* Actual range limits */
	double *tab;			     /* Base dimension */
	int dimid, dimvarid;		     /* Dimension ID, Dimension variable ID (if any) */
	int fileid;			     /* ID of current file */
	int i;
	int incr;			     /* +1 if dimension is increasing, -1 else */
	int incrindex;			     /* +1 if indices are increasing, -1 else */
	int nd;				     /* Number of dimensions */
	int varid;			     /* ID of current variable */
	int dimids[CU_MAX_VAR_DIMS];	     /* Dimension IDs, in file order */
	long dlen;			     /* Base dimension length */
	long icycle;			     /* Number of cycles from base dimension range */
	long idf, idl;			     /* Virtual range indices */
	long vlen;			     /* Virtual dimension length */
	char dname[CU_MAX_NAME];	     /* Dimension name */
	char dunits[CU_MAX_NAME];	     /* Dimension units */
	char format[CU_MAX_NAME];	     /* Data file format */

					     /* Check for valid current fileid */
	if(vdb.filelu < 0 || vdb.filelu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}

					     /* Get the fileid, varid */
	fileid = fileMap[vdb.filelu];
	if((varid=currentVar[vdb.filelu]) < 0){
		cw_error("No variable current for file %s",filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Get the dimension ID */
	if(cuvarinq(fileid, varid, varname, 0, &nd, dimids, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}
	if((n-1)<0 || (n-1)>nd){
		cw_error("Dimension number = %d, is out of range for file %s, variable %s (max of %d)",
			 n, filePath[vdb.filelu], varname, nd);
		return IDRS_BADDIM;
	}
	dimid = dimids[transpose[n-1]];

					     /* Inquire the dimension */
	for(i=0;i<CU_MAX_NAME;i++)
		dname[i]=dunits[i]='\0';
	if(cudiminq(fileid, dimid, dname, dunits, 0, &dimtype, 0, &dlen)==-1){
		cw_error("Error inquiring dimension %d of file %s, variable %s",n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* If the dimension name matches that of the variable,
					      * and the variable has only one dimension, then
					      * this is a dimension variable.
					      */
	if((!strcmp(dname,varname)) && (nd==1)){
		if(dna) strcpy(dna,"internal");
		if(dun) strcpy(dun,"internal");
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
		if(dtype) *dtype = (cwDimensionOption == CW_STANDARD ? IDRS_EQUALLY_SPACED : CW_LOCAL);
		if(reqlen == 0){
			if(retlen) *retlen = 0;
		}
		else if(reqlen >= dlen){
			for(i=1; i<=dlen; i++)
				*var++ = (double)i;
			if(retlen) *retlen = dlen;
		}
		else{
			cw_error("Requested dimension length = %d, should be at least %d, file %s, variable %s",
				 reqlen, dlen, filePath[vdb.filelu], varname);
			return IDRS_BADLEN;
		}
		return IDRS_SUCCESS;
	}
	
	if(dna) strncpy(dna,dname,cwNameLen);
	if(dun) strncpy(dun,dunits,cwUnitsLen);

					     /* Get the dimension varid, if any, or -1 if none */
	dimvarid = cw_dimension_varid(fileid, dimid, dname);
	
					     /* Set the dimension type */
	if(dtype){
					     /* If the dimension is shared, explicit ... */
		if(dimtype == CuGlobalDim && (dimvarid != -1)){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_SHARED);
		}
					     /* If the dimension is shared, implicit ... */
		else if(dimtype == CuGlobalDim){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_IMPLICIT_SHARED);
		}
					     /* If the dimension is local, report it as equally-spaced */
					     /* only if the file is in DRS format, as the other formats */
					     /* can have non-variable dimensions which require a vector */
					     /* representation (e.g., GrADS) */
		else {
			if(cwDimensionOption == CW_STANDARD){
				cw_string_attget(fileid,CU_GLOBAL,"format",format);
				*dtype = (!strcmp(format,"DRS") ? IDRS_EQUALLY_SPACED : IDRS_UNEQUALLY_SPACED);
			}
			else{
				*dtype = CW_LOCAL;
			}
		}
	}
		

					     /* If the dimension has a varid, get source and title, if any */
	if(dimvarid != -1){
		if(dsrc) cw_string_attget(fileid, dimvarid, "source", dsrc);
		if(dti) cw_string_attget(fileid, dimvarid, "title", dti);
	}
	else{
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
	}

					     /* Get the dimension limits */
	if(cw_dimmap(fileid, dimid, vdims[n-1].dfreq, vdims[n-1].dlreq, CW_RANGE, 1.0e-5, vdims[n-1].isCycle,
		     vdims[n-1].cycle, &vdims[n-1].values, &idf, &idl, &xdf, &xdl)==-1){
		cw_error("Error inquiring dimension range for dimension %d of file %s, variable %s",
			 n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* Copy the dimension to the user array */
	vlen = (idf < idl ? (idl-idf+1) : (idf-idl+1));

	if(reqlen == 0 || (var==(double*)0)){
		if(retlen) *retlen = 0;
	}
	else if (reqlen < vlen){
		if(retlen) *retlen = 0;
		cw_error("Requested dimension length = %d, should be at least %d, file %s, variable %s",
			 reqlen, vlen, filePath[vdb.filelu], varname);
		return IDRS_BADLEN;
	}
	else {
		tab = vdims[n-1].values;
		cycle = (vdims[n-1].isCycle ? vdims[n-1].cycle : 0.0);
		incrindex = (idf <= idl ? 1 : -1);
		incr = (tab[0] < tab[dlen-1] ? 1 : -1);
		for(i=idf; incrindex*i <= incrindex*idl; i+=incrindex){
			icycle = (i>=0 ? (i/dlen) : ((i+1)/dlen)-1);
			*var++ = (double)(tab[i-icycle*dlen] + incr*icycle*cycle);
		}
		if(retlen) *retlen = vlen;
	}
	   
	return IDRS_SUCCESS;
}
int cw_getedim(int n,char* dsrc,char* dna,char* dti,char* dun,int* dtype,int* idim,float* df,float* dl){

	CuDimType dimtype;		     /* Global or local */
	char varname[CU_MAX_NAME];	     /* Name of current variable */
	double xdf, xdl;		     /* Actual range limits */
	int dimid, dimvarid;		     /* Dimension ID, Dimension variable ID (if any) */
	int fileid;			     /* ID of current file */
	int nd;				     /* Number of dimensions */
	int varid;			     /* ID of current variable */
	int dimids[CU_MAX_VAR_DIMS];	     /* Dimension IDs, in file order */
	long dlen;			     /* Base dimension length */
	long idf, idl;			     /* Virtual range indices */
	char dname[CU_MAX_NAME];	     /* Dimension name */
	char dunits[CU_MAX_NAME];	     /* Dimension units */
	char format[CU_MAX_NAME];	     /* Data file format */
	int i;				     /* Loop index */

					     /* Check for valid current fileid */
	if(vdb.filelu < 0 || vdb.filelu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}

					     /* Get the fileid, varid */
	fileid = fileMap[vdb.filelu];
	if((varid=currentVar[vdb.filelu]) < 0){
		cw_error("No variable current for file %s",filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Get the dimension ID */
	if(cuvarinq(fileid, varid, varname, 0, &nd, dimids, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}
	if((n-1)<0 || (n-1)>nd){
		cw_error("Dimension number = %d, is out of range for file %s, variable %s (max of %d)",
			 n, filePath[vdb.filelu], varname, nd);
		return IDRS_BADDIM;
	}
	dimid = dimids[transpose[n-1]];

					     /* Inquire the dimension */
	for(i=0;i<CU_MAX_NAME;i++)
		dname[i]=dunits[i]='\0';
	if(cudiminq(fileid, dimid, dname, dunits, 0, &dimtype, 0, &dlen)==-1){
		cw_error("Error inquiring dimension %d of file %s, variable %s",n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* If the dimension name matches that of the variable,
					      * and the variable has only one dimension, then
					      * this is a dimension variable.
					      */
	if((!strcmp(dname,varname)) && (nd==1)){
		if(dna) strcpy(dna,"internal");
		if(dun) strcpy(dun,"internal");
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
		if(dtype) *dtype = (cwDimensionOption == CW_STANDARD ? IDRS_EQUALLY_SPACED : CW_LOCAL);
		if(idim) *idim = dlen;
		if(df) *df = 1.0;
		if(dl) *dl = (float)dlen;
		return IDRS_SUCCESS;
	}
	
	if(dna) strncpy(dna,dname,cwNameLen);
	if(dun) strncpy(dun,dunits,cwUnitsLen);

					     /* Get the dimension varid, if any, or -1 if none */
	dimvarid = cw_dimension_varid(fileid, dimid, dname);
	
					     /* Set the dimension type */
	if(dtype){
					     /* If the dimension is shared, explicit ... */
		if(dimtype == CuGlobalDim && (dimvarid != -1)){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_SHARED);
		}
					     /* If the dimension is shared, implicit ... */
		else if(dimtype == CuGlobalDim){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_IMPLICIT_SHARED);
		}
					     /* If the dimension is local, report it as equally-spaced */
					     /* only if the file is in DRS format, as the other formats */
					     /* can have non-variable dimensions which require a vector */
					     /* representation (e.g., GrADS) */
		else {
			if(cwDimensionOption == CW_STANDARD){
				cw_string_attget(fileid,CU_GLOBAL,"format",format);
				*dtype = (!strcmp(format,"DRS") ? IDRS_EQUALLY_SPACED : IDRS_UNEQUALLY_SPACED);
			}
			else{
				*dtype = CW_LOCAL;
			}
		}
	}
		

					     /* If the dimension has a varid, get source and title, if any */
	if(dimvarid != -1){
		if(dsrc) cw_string_attget(fileid, dimvarid, "source", dsrc);
		if(dti) cw_string_attget(fileid, dimvarid, "title", dti);
	}
	else{
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
	}

					     /* Get the dimension limits */
	if(cw_dimmap(fileid, dimid, vdims[n-1].dfreq, vdims[n-1].dlreq, CW_RANGE, 1.0e-5, vdims[n-1].isCycle,
		     vdims[n-1].cycle, &vdims[n-1].values, &idf, &idl, &xdf, &xdl)==-1){
		cw_error("Error inquiring dimension range for dimension %d of file %s, variable %s",
			 n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

	if(idim) *idim = (idf < idl ? (idl-idf+1) : (idf-idl+1));
	if(df) *df = xdf;
	if(dl) *dl = xdl;
	   

	return IDRS_SUCCESS;
}
int cw_getedimD(int n,char* dsrc,char* dna,char* dti,char* dun,int* dtype,int* idim,double* df,double* dl){

	CuDimType dimtype;		     /* Global or local */
	char varname[CU_MAX_NAME];	     /* Name of current variable */
	double xdf, xdl;		     /* Actual range limits */
	int dimid, dimvarid;		     /* Dimension ID, Dimension variable ID (if any) */
	int fileid;			     /* ID of current file */
	int nd;				     /* Number of dimensions */
	int varid;			     /* ID of current variable */
	int dimids[CU_MAX_VAR_DIMS];	     /* Dimension IDs, in file order */
	long dlen;			     /* Base dimension length */
	long idf, idl;			     /* Virtual range indices */
	char dname[CU_MAX_NAME];	     /* Dimension name */
	char dunits[CU_MAX_NAME];	     /* Dimension units */
	char format[CU_MAX_NAME];	     /* Data file format */
	int i;				     /* Loop index */

					     /* Check for valid current fileid */
	if(vdb.filelu < 0 || vdb.filelu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}

					     /* Get the fileid, varid */
	fileid = fileMap[vdb.filelu];
	if((varid=currentVar[vdb.filelu]) < 0){
		cw_error("No variable current for file %s",filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Get the dimension ID */
	if(cuvarinq(fileid, varid, varname, 0, &nd, dimids, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}
	if((n-1)<0 || (n-1)>nd){
		cw_error("Dimension number = %d, is out of range for file %s, variable %s (max of %d)",
			 n, filePath[vdb.filelu], varname, nd);
		return IDRS_BADDIM;
	}
	dimid = dimids[transpose[n-1]];

					     /* Inquire the dimension */
	for(i=0;i<CU_MAX_NAME;i++)
		dname[i]=dunits[i]='\0';
	if(cudiminq(fileid, dimid, dname, dunits, 0, &dimtype, 0, &dlen)==-1){
		cw_error("Error inquiring dimension %d of file %s, variable %s",n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* If the dimension name matches that of the variable,
					      * and the variable has only one dimension, then
					      * this is a dimension variable.
					      */
	if((!strcmp(dname,varname)) && (nd==1)){
		if(dna) strcpy(dna,"internal");
		if(dun) strcpy(dun,"internal");
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
		if(dtype) *dtype = (cwDimensionOption == CW_STANDARD ? IDRS_EQUALLY_SPACED : CW_LOCAL);
		if(idim) *idim = dlen;
		if(df) *df = 1.0;
		if(dl) *dl = (double)dlen;
		return IDRS_SUCCESS;
	}
	
	if(dna) strncpy(dna,dname,cwNameLen);
	if(dun) strncpy(dun,dunits,cwUnitsLen);

					     /* Get the dimension varid, if any, or -1 if none */
	dimvarid = cw_dimension_varid(fileid, dimid, dname);
	
					     /* Set the dimension type */
	if(dtype){
					     /* If the dimension is shared, explicit ... */
		if(dimtype == CuGlobalDim && (dimvarid != -1)){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_SHARED);
		}
					     /* If the dimension is shared, implicit ... */
		else if(dimtype == CuGlobalDim){
			*dtype = (cwDimensionOption == CW_STANDARD ? IDRS_UNEQUALLY_SPACED : CW_IMPLICIT_SHARED);
		}
					     /* If the dimension is local, report it as equally-spaced */
					     /* only if the file is in DRS format, as the other formats */
					     /* can have non-variable dimensions which require a vector */
					     /* representation (e.g., GrADS) */
		else {
			if(cwDimensionOption == CW_STANDARD){
				cw_string_attget(fileid,CU_GLOBAL,"format",format);
				*dtype = (!strcmp(format,"DRS") ? IDRS_EQUALLY_SPACED : IDRS_UNEQUALLY_SPACED);
			}
			else{
				*dtype = CW_LOCAL;
			}
		}
	}
		

					     /* If the dimension has a varid, get source and title, if any */
	if(dimvarid != -1){
		if(dsrc) cw_string_attget(fileid, dimvarid, "source", dsrc);
		if(dti) cw_string_attget(fileid, dimvarid, "title", dti);
	}
	else{
		if(dsrc) strcpy(dsrc,CW_STRING_NULL);
		if(dti) strcpy(dti,CW_STRING_NULL);
	}

					     /* Get the dimension limits */
	if(cw_dimmap(fileid, dimid, vdims[n-1].dfreq, vdims[n-1].dlreq, CW_RANGE, 1.0e-5, vdims[n-1].isCycle,
		     vdims[n-1].cycle, &vdims[n-1].values, &idf, &idl, &xdf, &xdl)==-1){
		cw_error("Error inquiring dimension range for dimension %d of file %s, variable %s",
			 n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

	if(idim) *idim = (idf < idl ? (idl-idf+1) : (idf-idl+1));
	if(df) *df = xdf;
	if(dl) *dl = xdl;
	   

	return IDRS_SUCCESS;
}
int cw_getelemd(int* drstype,int* bpe){

        int drtype;                          /* DRS type */
	int fileid;			     /* ID of current file */
	char varname[CU_MAX_NAME];	     /* Name of current variable */
	int varid;			     /* ID of current variable */
	CuType dtype;			     /* cdunif datatype */

					     /* Check for valid current fileid */
	if(vdb.filelu < 0 || vdb.filelu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}

					     /* Get the fileid, varid */
	fileid = fileMap[vdb.filelu];
	if((varid=currentVar[vdb.filelu]) < 0){
		cw_error("No variable current for file %s",filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Get the dimension ID */
	if(cuvarinq(fileid, varid, varname, &dtype, 0, 0, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}
					     /* Map to a DRS enumerated type */
	if(cw_unif_to_drs_enumtype(dtype,&drtype)==-1)
		return IDRS_BADTYPE;

        if(drstype) *drstype = drtype;
	if(bpe) *bpe = cutypelen(dtype);
	
	return IDRS_SUCCESS;
}
int cw_getname(char* source,char* name,char* title,char* units,char* date,char* time,char* typed,int* nd){

	int fileid;
	int varid;
	CuType dtype;
	char vname[CU_MAX_NAME];

					     /* Check for valid current fileid, variable */
	if(vdb.filelu < 0 || vdb.filelu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}
	fileid = fileMap[vdb.filelu];
	if((varid=currentVar[vdb.filelu]) < 0){
		cw_error("No variable current for file %s",filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Name, type, number of dimensions */
	if(cuvarinq(fileid, varid, vname, &dtype, nd, 0, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}
	strncpy(name,vname,cwNameLen);
	name[cwNameLen-1]='\0';
	if(cw_unif_to_drs_datatype(dtype,typed)==-1){
		cw_error("Error mapping cdunif datatype: %d to DRS type",dtype);
		return IDRS_BADTYPE;
	}
					     /* Source, title, units */
	if(source) cw_string_attget(fileid, varid, "source", source);
	if(title) cw_string_attget(fileid, varid, "title", title);
	if(units) cw_string_attget(fileid, varid, "units", units);

					     /* Date, time */
	if(date) cw_string_attget(fileid, varid, "date", date);
	if(time) cw_string_attget(fileid, varid, "time", time);

	return IDRS_SUCCESS;
}
int cw_getnd(int* nd){
	int fileid;
	int varid;
	
					     /* Check for valid current fileid, variable */
	if(vdb.filelu < 0 || vdb.filelu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}
	fileid = fileMap[vdb.filelu];
	if((varid=currentVar[vdb.filelu]) < 0){
		cw_error("No variable current for file %s",filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}

	if(cuvarinq(fileid, varid, 0, 0, nd, 0, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[vdb.filelu]);
		return IDRS_VDBNOTFOUND;
	}
	return IDRS_SUCCESS;
}
int cw_getrge2(int lu,int n,double elem1,double elem2,int* ind1,int* ind2,float* dlow,float* dhigh){

	int varid;			     /* ID of current variable */
	int fileid;			     /* ID of current file */
	char varname[CU_MAX_NAME];	     /* Name of current variable */
	char dname[CU_MAX_NAME];	     /* Dimension name */
	int nd;				     /* Number of dimensions */
	int dimids[CU_MAX_VAR_DIMS];	     /* Dimension IDs, in file order */
        int dimid;                           /* Dimension ID */
        double xdf, xdl;                     /* Actual range to be read */
        long idf, idl;                       /* Dimension first, last index */
	long dlen;			     /* Base dimension length */
	double xelem1, xelem2;

					     /* Check for valid current fileid */
	lu = vdb.filelu;		     /* Note: Input lu is ignored! */
	if(lu < 0 || lu > CU_MAX_LU){
		cw_error("Invalid logical unit: %d",lu);
		return IDRS_BADLU;
	}

					     /* Get the fileid, varid */
	fileid = fileMap[lu];
	if((varid=currentVar[lu]) < 0){
		cw_error("No variable current for file %s",filePath[lu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Get the dimension ID */
	if(cuvarinq(fileid, varid, varname, 0, &nd, dimids, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[lu]);
		return IDRS_VDBNOTFOUND;
	}
	if((n-1)<0 || (n-1)>nd){
		cw_error("Dimension number = %d, is out of range for file %s, variable %s",
			 n, filePath[lu], varname);
		return IDRS_BADDIM;
	}
	dimid = dimids[transpose[n-1]];

					     /* Inquire the dimension */
	if(cudiminq(fileid, dimid, dname, 0, 0, 0, 0, &dlen)==-1){
		cw_error("Error inquiring dimension %d of file %s, variable %s",n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* If the dimension name matches that of the variable,
					      * and the variable has only one dimension, then
					      * this is a dimension variable.
					      */
	if((!strcmp(dname,varname)) && (nd==1)){
		xelem1 = elem1 - 1.0e-5;
		xelem2 = elem2 + 1.0e-5;
		if(xelem1<1.0){
			if(ind1) *ind1=1;
			if(dlow) *dlow=1.0;
		}
		else if(xelem1>(double)dlen){
			if(ind1) *ind1 = dlen;
			if(dlow) *dlow = (float)dlen;
		}
		else{
			if(dlow) *dlow = (float)ceil(xelem1);
			if(ind1) *ind1 = (int)ceil(xelem1);
		}
		if(xelem2<1.0){
			if(ind2) *ind2=1;
			if(dhigh) *dhigh=1.0;
		}
		else if(xelem2>(double)dlen){
			if(ind2) *ind2 = dlen;
			if(dhigh) *dhigh = (float)dlen;
		}
		else{
			if(dhigh) *dhigh = (float)floor(xelem2);
			if(ind2) *ind2 = (int)floor(xelem2);
		}
		return IDRS_SUCCESS;
	}

					     /* Get the dimension limits */
	if(cw_dimmap(fileid, dimid, elem1, elem2, CW_RANGE, 1.0e-5, 0,
		     0.0, &vdims[n-1].values, &idf, &idl, &xdf, &xdl)==-1){
		cw_error("Error inquiring dimension range for dimension %d of file %s, variable %s",
			 n, filePath[lu], varname);
		return IDRS_BADDIM;
	}
					     /* NB: 1-origin indices */
        if(ind1) *ind1 = idf+1;
        if(ind2) *ind2 = idl+1;
        if(dlow) *dlow = xdf;
        if(dhigh) *dhigh = xdl;
	   
	return IDRS_SUCCESS;
}
int cw_getrge2D(int lu,int n,double elem1,double elem2,int* ind1,int* ind2,double* dlow,double* dhigh){

	int varid;			     /* ID of current variable */
	int fileid;			     /* ID of current file */
	char varname[CU_MAX_NAME];	     /* Name of current variable */
	char dname[CU_MAX_NAME];	     /* Dimension name */
	int nd;				     /* Number of dimensions */
	int dimids[CU_MAX_VAR_DIMS];	     /* Dimension IDs, in file order */
        int dimid;                           /* Dimension ID */
        double xdf, xdl;                     /* Actual range to be read */
        long idf, idl;                       /* Dimension first, last index */
	long dlen;			     /* Base dimension length */
	double xelem1, xelem2;

					     /* Check for valid current fileid */
	lu = vdb.filelu;		     /* Note: Input lu is ignored! */
	if(lu < 0 || lu > CU_MAX_LU){
		cw_error("Invalid logical unit: %d",lu);
		return IDRS_BADLU;
	}

					     /* Get the fileid, varid */
	fileid = fileMap[lu];
	if((varid=currentVar[lu]) < 0){
		cw_error("No variable current for file %s",filePath[lu]);
		return IDRS_VDBNOTFOUND;
	}

					     /* Get the dimension ID */
	if(cuvarinq(fileid, varid, varname, 0, &nd, dimids, 0)==-1){
		cw_error("Error inquiring variable %d for file %s", varid, filePath[lu]);
		return IDRS_VDBNOTFOUND;
	}
	if((n-1)<0 || (n-1)>nd){
		cw_error("Dimension number = %d, is out of range for file %s, variable %s",
			 n, filePath[lu], varname);
		return IDRS_BADDIM;
	}
	dimid = dimids[transpose[n-1]];

					     /* Inquire the dimension */
	if(cudiminq(fileid, dimid, dname, 0, 0, 0, 0, &dlen)==-1){
		cw_error("Error inquiring dimension %d of file %s, variable %s",n, filePath[vdb.filelu], varname);
		return IDRS_BADDIM;
	}

					     /* If the dimension name matches that of the variable,
					      * and the variable has only one dimension, then
					      * this is a dimension variable.
					      */
	if((!strcmp(dname,varname)) && (nd==1)){
		xelem1 = elem1 - 1.0e-5;
		xelem2 = elem2 + 1.0e-5;
		if(xelem1<1.0){
			if(ind1) *ind1=1;
			if(dlow) *dlow=1.0;
		}
		else if(xelem1>(double)dlen){
			if(ind1) *ind1 = dlen;
			if(dlow) *dlow = (double)dlen;
		}
		else{
			if(dlow) *dlow = (double)ceil(xelem1);
			if(ind1) *ind1 = (int)ceil(xelem1);
		}
		if(xelem2<1.0){
			if(ind2) *ind2=1;
			if(dhigh) *dhigh=1.0;
		}
		else if(xelem2>(double)dlen){
			if(ind2) *ind2 = dlen;
			if(dhigh) *dhigh = (double)dlen;
		}
		else{
			if(dhigh) *dhigh = (double)floor(xelem2);
			if(ind2) *ind2 = (int)floor(xelem2);
		}
		return IDRS_SUCCESS;
	}

					     /* Get the dimension limits */
	if(cw_dimmap(fileid, dimid, elem1, elem2, CW_RANGE, 1.0e-5, 0,
		     0.0, &vdims[n-1].values, &idf, &idl, &xdf, &xdl)==-1){
		cw_error("Error inquiring dimension range for dimension %d of file %s, variable %s",
			 n, filePath[lu], varname);
		return IDRS_BADDIM;
	}
					     /* NB: 1-origin indices */
        if(ind1) *ind1 = idf+1;
        if(ind2) *ind2 = idl+1;
        if(dlow) *dlow = xdf;
        if(dhigh) *dhigh = xdl;
	   
	return IDRS_SUCCESS;
}
int cw_getslab(int lu,int rank,int* order,float* fe,float* le,float* cycle,void* data,int* datadim){

	CuRRA* fileRRA;			     /* requested indices, right-ragged array */
	CuType dtype, usertype;		     /* cdunif file and user datatype */
	char* filename;			     /* Controlfile pathname */
	int dimids[CU_MAX_VAR_DIMS];	     /* variable cdunif dimension IDs */
	int fileid, varid;		     /* cdunif file and variable IDs */
	int i,j;			     /* i = user array index, j = file array index */
	int incr;			     /* +1 if dimension indices are increasing, -1 else */
	int k;				     /* Virtual dimension index */
	int m;				     /* counter */
	int ndims;			     /* number of dimensions */
	long canonTranspose[CU_MAX_VAR_DIMS]; /* transpose in C majority */
	long dlen, dcount;		     /* dimension length, count */
	long filedcount[CU_MAX_VAR_DIMS];    /* absolute dimension counts, in file order */
	long filedlen[CU_MAX_VAR_DIMS];	     /* dimension lengths, in file order */
	long icycle;			     /* Number of cycles virtual index is away from base dimension */
	long idf[CU_MAX_VAR_DIMS];	     /* first indices of dimension range, file order */
	long idl[CU_MAX_VAR_DIMS];	     /* last indices of dimension range, file order */
	long idfx, idlx;		     /* first, last index of dimension range */

					     /* Check arguments */
	if(lu<0 || lu> CU_MAX_LU){
		cw_error("Bad logical unit %d",lu);
		return IDRS_BADLU;
	}

	fileid = fileMap[lu];
	if(fileid==-1){
		cw_error("File not open, logical unit %d",lu);
		return IDRS_BADLU;
	}
	filename = filePath[lu];

					     /* Lookup the variable id */
	if((varid = cw_varid(fileid,vdb.source,vdb.name,vdb.title,vdb.units))==-1){
		cw_error("Variable not found: %s, file %s",vdb.name,filename);
		return IDRS_VDBNOTFOUND;
	}
	
	vdb.filelu = lu;		     /* At this point, we know which file will be accessed */
	currentVar[lu] = varid;		     /* and which variable */
	
					     /* Get the variable datatype, number of dimensions, dimension IDs */
	if(cuvarinq(fileid, varid, 0, &dtype, &ndims, dimids, 0)==-1){
		cw_error("Error inquiring variable %s, file %s",vdb.name,filename);
		return IDRS_VDBNOTFOUND;
	}
					     /* Check that setdim or setvdim was not called with n>ndims */
	for(i=ndims;i<CU_MAX_VAR_DIMS;i++){
		if(vdims[i].isset){
			cw_error("Invalid dimension %d was set for variable %s, file %s",i+1,vdb.name,filename);
			return IDRS_BADDIM;
		}
	}

	if(rank != ndims){
		cw_error("Invalid rank: %d, for variable %s, file %s, file rank is %d",
			 rank,vdb.name,filename,ndims);
		return IDRS_BADLEN;
	}
					     /* Convert requested datatype from 'X*n' form
					      * to a CuType; null-string defaults to file type*/
	if(!strcmp(vdb.type,CW_STRING_NULL))
		usertype = dtype;
	else if((usertype=cw_drs_to_unif_datatype(vdb.type))==-1){
		cw_error("Invalid datatype: %s, variable %s, file %s",vdb.type,vdb.name,filename);
		return IDRS_BADTYPE;
	}

					     /* Set transpose:
					      * For Fortran majority, transpose has Fortran
					      * majority user dimension indices, but C
					      * majority file dimension indices, so adjust.
					      * Also note that 'order' is 1-origin.
					      */
	for(i=0; i<ndims; i++)
		transpose[i] = (cwMajority == CW_C_MAJORITY) ? (order[i]-1) : (ndims-order[i]);

					     /* Canonical transpose always has C majority
					      * user dimension indices.
					      */
	for(i=0; i<ndims; i++)
		canonTranspose[i] = transpose[(cwMajority==CW_C_MAJORITY) ? i : (ndims-i-1)];

					     /* Map ranges to indices*/
	for(i=0; i<ndims; i++){
		
		vdims[i].dfreq = fe[i];
		vdims[i].dlreq = le[i];
		vdims[i].isset = 1;
		vdims[i].isCycle = (cycle[i]>0.0);
		vdims[i].cycle = vdims[i].isCycle ? cycle[i] : 0.0;
		if(cw_dimmap(fileid,dimids[transpose[i]], vdims[i].dfreq, vdims[i].dlreq,
			     CW_RANGE, 1.0e-5, vdims[i].isCycle, vdims[i].cycle,
			     &vdims[i].values, &idfx, &idlx, &vdims[i].dfactual,
			     &vdims[i].dlactual) == -1){
			cw_error("Error mapping range [%lf,%lf] onto dimension %s, variable %s, file %s",
				 vdims[i].dfreq,vdims[i].dlreq, vdims[i].name, vdb.name, filename);
			return IDRS_BADDIM;
		}
		idf[transpose[i]] = idfx;
		idl[transpose[i]] = idlx;
		vdims[i].len = ((idlx >= idfx) ? (idlx - idfx + 1) : (idfx - idlx +1));
	}

					     /* Create right-ragged array struct for generalized read */
	for(i=0; i<ndims; i++){
		if(cudiminq(fileid, dimids[transpose[i]], 0, 0, 0, 0, 0, &(filedlen[transpose[i]]))==-1){
			/* This should never happen unless an internal error occurs. */
			cw_error("Internal error looking up dimension %d of var %s, lu %d",i,vdb.name,lu);
			return IDRS_BADDIM;
		}
		filedcount[transpose[i]] = vdims[i].len;
	}
	fileRRA = cucreateRRA( ndims, filedlen, filedcount);

					     /* Fill the right-ragged array.
					      * Note that dimensions are in canonical FILE order.
					      * Adjust indices to fall in the range 0..dlen-1
					      */
	for(j=0; j<ndims; j++){
		dlen = filedlen[j];
		dcount = filedcount[j];
		incr = (idf[j]<idl[j] ? 1 : -1);
		for(m=0, k=idf[j]; m<dcount; m++, k+=incr){
			icycle = (k>=0 ? (k/dlen) : ((k+1)/dlen)-1);
			CU_SETRRA(fileRRA,j,m,(k-icycle*dlen));
		}
	}

					     /* Read the data */
	if(cureadarray(fileid, varid, fileRRA, 0, canonTranspose, usertype, data)==-1){
		cw_error("Error reading data for variable %s, file %s",vdb.name,filename);
		return IDRS_CANNOTREADDATA;
	}
	cudestroyRRA(fileRRA);
	
	return IDRS_SUCCESS;
}
int cw_inqdict(int lu,int oper){

	char fsource[CU_MAX_NAME], ftitle[CU_MAX_NAME], funits[CU_MAX_NAME], fname[CU_MAX_NAME];
	int fileid;
	int i;
	int ndims;
	int nvar;
	int found;
	int varid;
	CwVar savevdb;

					     /* Check validity of lu */

	if(lu<0 || lu>CU_MAX_LU || fileMap[lu]==-1){
		cw_error("Bad logical unit: %d",lu);
		return IDRS_BADLU;
	}
	fileid = fileMap[lu];
	if(fileid==-1){
		cw_error("File not open, logical unit %d",lu);
		return IDRS_BADLU;
	}

					     /* Get number of variables */
	if(cuinquire(fileid,0,&nvar,0,0)==-1){
		cw_error("Cannot get number of variables for file %s",filePath[lu]);
		return IDRS_CANNOTREADHEADER;
	}
	if(nvar <= 0){
		cw_error("Non-positive number of variables: %d",nvar);
		return IDRS_NOMOREVARS;
	}
					     /* Reset context for first variable */
	if(oper == IDRS_GETFIRSTVAR)
		currentVar[lu] = -1;
	else if(oper != IDRS_GETNEXTVAR){
		cw_error("Invalid inquire operator: %d",oper);
		return IDRS_NOMOREVARS;
	}

					     /* If current variable is non-null and
					      * this is the first lookup, use lookup proc */
	found = 0;
	if(oper == IDRS_GETFIRSTVAR && strcmp(vdb.name,CW_STRING_NULL)){
		if((varid = cw_varid(fileid,vdb.source,vdb.name,vdb.title,vdb.units))==-1){
			cw_error("Variable not found: %s, file %s",vdb.name,filePath[lu]);
			return IDRS_VDBNOTFOUND;
		}
		found = 1;
	}
					     /* Else check successive varids for a match */
	else{
		for(varid = currentVar[lu]+1; varid<nvar; varid++){
			cuvarinq(fileid, varid, fname, 0, 0, 0, 0);
			cw_string_attget(fileid, varid, "source", fsource);
			cw_string_attget(fileid, varid, "title", ftitle);
			cw_string_attget(fileid, varid, "units", funits);
			
			if(CW_STRING_MATCH(vdb.name,fname) &&
			   CW_STRING_MATCH(vdb.source,fsource) &&
			   CW_STRING_MATCH(vdb.title,ftitle) &&
			   CW_STRING_MATCH(vdb.units,funits)){
				found = 1;
				break;
			}
		}
		if(!found)
			return IDRS_NOMOREVARS;
	}

					     /* Set context for successful lookup: */
					     /* Save the naming info */
	strncpy(savevdb.source,vdb.source,cwSourceLen);
	strncpy(savevdb.name,vdb.name,cwNameLen);
	strncpy(savevdb.title,vdb.title,cwTitleLen);
	strncpy(savevdb.units, vdb.units,cwUnitsLen);
	cw_cluvdb();			     /* Reset dimension cache */
					     /* Restore the naming info */
	strncpy(vdb.source,savevdb.source,cwSourceLen);
	strncpy(vdb.name,savevdb.name,cwNameLen);
	strncpy(vdb.title,savevdb.title,cwTitleLen);
	strncpy(vdb.units, savevdb.units,cwUnitsLen);

					     /* Set transpose vector */
	cuvarinq(fileid, varid, 0, 0, &ndims, 0, 0);
	for(i=0; i<ndims; i++){
		transpose[i] = (cwMajority == CW_C_MAJORITY) ? i : (ndims-i-1);
	}
	currentVar[lu] = varid;
	vdb.filelu = lu;
	
	return IDRS_SUCCESS;
}
int cw_inqlun(int lu,char* datafile,int* nvar,float* version){

	int fileid;

	if(lu<0 || lu>CU_MAX_LU || fileMap[lu]==-1){
		cw_error("Bad logical unit: %d",lu);
		return IDRS_BADLU;
	}
	fileid = fileMap[lu];
	if(fileid==-1){
		cw_error("File not open, logical unit %d",lu);
		return IDRS_BADLU;
	}
					     /* Get number of variables */
	if(nvar && cuinquire(fileid,0,nvar,0,0)==-1){
		cw_error("Cannot get number of variables for file %s",filePath[lu]);
		return IDRS_CANNOTREADHEADER;
	}

					     /* Get datafile */
	if(datafile){
		if(cuattget(fileid,CU_GLOBAL,"datafile",datafile)==-1){
			strcpy(datafile,CW_STRING_NULL);
		}
	}
					     /* Get version */
	if(version){
		if(cuattget(fileid,CU_GLOBAL,"version",version)==-1){
			*version = CW_FLOAT_NULL;
		}
	}
	return IDRS_SUCCESS;
}
int cw_setdim(int n,char* dna,char* dun,int idim,double df,double dl){

	if(n<1 || n>CU_MAX_VAR_DIMS){
		cw_error("Invalid dimension number %d, current variable = %s",n,vdb.name);
		return IDRS_BADDIM;
	}

	VDB_STRING_SET(vdims[n-1].name,dna,cwNameLen);
	VDB_STRING_SET(vdims[n-1].units,dun,cwUnitsLen);
	vdims[n-1].reqlen = idim;
	vdims[n-1].dfreq = df;
	vdims[n-1].dlreq = dl;
	vdims[n-1].isset = 1;

	return IDRS_SUCCESS;
}
int cw_seterr(int ierrlun,int reportlevel){

	if(reportlevel==IDRS_NOREPORT)
		cuseterropts(0);
	else
		cuseterropts(CU_VERBOSE);

	return IDRS_SUCCESS;
}
int cw_setname(char* source,char* name,char* title,char* units,char* typed){

	VDB_STRING_SET(vdb.source,source,cwSourceLen);
	VDB_STRING_SET(vdb.name,name,cwNameLen);
	VDB_STRING_SET(vdb.title,title,cwTitleLen);
	VDB_STRING_SET(vdb.units,units,cwUnitsLen);
	VDB_STRING_SET(vdb.type,typed,cwTypeLen);

	return IDRS_SUCCESS;
}
int cw_setvdim(int n,char* dso,char* dna,char* dti,char* dun,double df,double dl){

	if(cw_setdim(n,dna,dun,CW_INT_NULL,df,dl) != IDRS_SUCCESS)
		return IDRS_BADDIM;
	VDB_STRING_SET(vdims[n-1].source,dso,cwSourceLen);
	VDB_STRING_SET(vdims[n-1].title,dti,cwTitleLen);

	return IDRS_SUCCESS;
}
					     /* Get most recent cdunif error and map to DRS error */
int cw_geterr(void){
	int lastError, drsError;

	lastError = cugeterr();

					     /* Some errors have no counterpart, just return -1 */
	switch(lastError){
	case CU_SUCCESS:
		drsError = IDRS_SUCCESS;
		break;
	case CU_EBADID:
		drsError = IDRS_BADLU;
		break;
	case CU_OPENFILES:
		drsError = IDRS_TOOMANYFILES;
		break;
	case CU_EINVALCOORDS:
		drsError = IDRS_NORANGE;
		break;
	case CU_EBADTYPE:
		drsError = IDRS_BADTYPE;
		break;
	case CU_EBADDIM:
		drsError = IDRS_BADDIM;
		break;
	case CU_ENOTVAR:
		drsError = IDRS_VDBNOTFOUND;
		break;
	case CU_EINVLU:
		drsError = IDRS_BADLU;
		break;
	case CU_EOPEN:
		drsError = IDRS_CANNOTOPENDICT;
		break;
	case CU_ENOCAST:
		drsError = IDRS_CANNOTCONVERT;
		break;
	default:
		drsError = -1;
		break;
	}
	return drsError;
}
void cw_error(char *fmt, ...){
	va_list args;
	extern int cuErrOpts;

	if(cuErrOpts & CU_VERBOSE){
		va_start(args,fmt);
		fprintf(stderr, "CDMS DRS error: ");
		vfprintf(stderr, fmt, args);
		fprintf(stderr, "\n");
		va_end(args);
		fprintf(stderr,"Current user variable name = %s\n",vdb.name);
	}
	if(cuErrOpts & CU_FATAL)
		exit(1);
	
	return;
}
					     /* Binary lookup */
					     /* Lookup x, in strictly monotonic vector tab (increasing or decreasing) */
					     /* of length n, return index k. */
					     /* k is interpreted as follows: */
					     /* Case: tab is increasing: */
					     /*   k == -1 iff x <= tab[0] */
					     /*   k == (n-1) iff x > tab[n-1] */
					     /*   else tab[k] < x <= tab[k+1] */
					     /* Case: tab is decreasing: */
					     /*   k == -1 iff x > tab[0] */
					     /*   k == (n-1) iff x <= tab[n-1] */
					     /*   else tab[k] >= x > tab[k+1]*/

void cw_lookup(double tab[], long n, double x, long *k)
{
	long kupper,kmid,klower;
	int incr;

	klower=-1;
	kupper=n;
	incr=(tab[n-1] > tab[0]);
	while (kupper-klower > 1) {
		kmid=(kupper+klower) >> 1;
		if (x > tab[kmid] == incr)
			klower=kmid;
		else
			kupper=kmid;
	}
	*k=klower;
}
					     /* Cyclical lookup of index K in vector TAB, of length N, which is strictly
					      * increasing or decreasing. The lookup is in a virtual vector vec, defined
					      * such that for integer j in the range 0..N-1, vec[j]==TAB[j], and
					      * for j outside the range 0..N-1, TAB[j+N] == TAB[j]+incr*CYCLE, where
					      * incr = +1 for TAB increasing, or -1 for TAB decreasing.
					      * POLICY and DELTA are used as in
					      * cw_lookup_with_policy.
					      *
					      * K is the index returned, such that 0<=K<N. ICYCLE is defined such that
					      * the 'virtual' index v in vec may be calculated as v = K + N*ICYCLE.
					      * Similarly, the 'virtual' value vec[v] = TAB[K]+incr*ICYCLE*CYCLE.
					      */
int cw_lookup_cycle(double tab[], long n, double x, double cycle, CwRoundPolicy policy, double delta, long *k, long *icycle){
	double xt;			     /* x mapped to range [tab[0],tab[0]+incr*cycle) */
	double xrange, tabn, xindex;
	long it;			     /* Number of cycles of n indices between x and xt */
	long kt;			     /* Index of xt in tab */
	long incr;

					     /* Check that 0 < |tab(0)-tab(n-1)| < cycle */
	xrange = tab[n-1] - tab[0];
	if(xrange==0.0){
		cw_error("Non-monotonic dimension vector, vec[0] = vec[%d] = %lf",n-1,tab[0]);
		return -1;
	}
	if(fabs(xrange) >= cycle){
		cw_error("Cycle = %lf, must be larger that dimension range %lf",cycle,fabs(xrange));
		return -1;
	}

					     /* Map x into xt, it, where
					      * (1) xt is in the range [tab(0),tab(0)+incr*cycle);
					      * (2) x = xt + incr*it*cycle
					      * where incr = +1 if tab is increasing, -1 if decreasing
					      */
	incr = (xrange>0) ? 1 : -1;
	it = (long)floor((double)incr * (x-tab[0])/cycle);
	xt = x - incr*it*cycle;

					     /* Lookup xt with given policy */
	if(cw_lookup_with_policy(tab, n, xt, policy, delta, &kt) != IDRS_SUCCESS)
		return -1;

					     /* Handle case where xt is in the range (tab(n-1),tab(0)+incr*cycle) */
	if((kt==(n-1)) && (((incr==1)&&(xt>tab[n-1])) || ((incr==-1) && (xt<tab[n-1])))){
		tabn = tab[0]+incr*cycle;
		xindex = (double)kt + (xt-tab[n-1])/(tabn-tab[n-1]);
		switch(policy){
		  case CW_ROUND_NEAREST:
			kt = (long)(xindex+0.5);
			break;
		  case CW_ROUND_UP:
			kt = (long)ceil(xindex - delta);
			break;
		  case CW_ROUND_DOWN:
			kt = (long)floor(xindex + delta);
			break;
		  default:
			cw_error("Invalid lookup policy: %d", policy);
			return -1;
		}
		if(kt == n){
			kt=0;
			it++;
		}
	}
	
	*k = kt;
	*icycle = it;
	return IDRS_SUCCESS;
}
					     /* Lookup index for x in double vector tab, strictly increasing or decreasing,
					      * of length n.
					      * x is first mapped into a real (xindex) in index space. If
					      * policy == CW_ROUND_UP, the xindex is rounded up to the next integer,
					      * unless it is within delta of a (smaller) integer.
					      * If policy == CW_ROUND_DOWN, xindex is rounded down, unless
					      * it is within delta of a (larger) integer. The function returns
					      * 0 on success, -1 on failure.
					      */
int cw_lookup_with_policy(double tab[], long n, double x, CwRoundPolicy policy, double delta, long *k){

	double xindex;

	cw_lookup(tab, n, x, k);

	if(*k == -1){
		*k = 0;
		return IDRS_SUCCESS;
	}
	else if(*k == (n-1)){
		return IDRS_SUCCESS;
	}
	else if((tab[n-1] > tab[0]) && (x == tab[*k+1])){
		(*k)++;
		return IDRS_SUCCESS;
	}
	else if((tab[n-1] < tab[0]) && (x == tab[*k])){
		return IDRS_SUCCESS;
	}
	if(tab[*k] == tab[*k+1]){
		cw_error("Non-monotonic dimension vector, vec[%d] = %lf", *k, tab[*k]);
		return -1;
	}
	xindex = (double)*k + (x - tab[*k])/(tab[*k+1] - tab[*k]);
	switch(policy){
	  case CW_ROUND_NEAREST:
		*k = (long)(xindex+0.5);
		break;
	  case CW_ROUND_UP:
		*k = (long)ceil(xindex - delta);
		break;
	  case CW_ROUND_DOWN:
		*k = (long)floor(xindex + delta);
		break;
	  default:
		cw_error("Invalid lookup policy: %d", policy);
		return -1;
	}
	return IDRS_SUCCESS;
}
					     /* Get the values of the dimension identified by
					      * FILEID, DIMID. Values are returned as a double
					      * vector pointed to by *VALUES, with length DIMLEN.
					      */
int cw_dimget(int fileid, int dimid, double** values, long *dimlen){

	CuType dtype;
	char *cp;
	double *dp, *tab;
	float *fp;
	int i;
	long *lp;
	long dlen, dlenbytes;
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
	long double *ldp;
#endif
	int *ip;
	short *sp;
	void *dim;

					     /* Retrieve the dimension type and length */
	if(cudiminq(fileid,dimid,0,0,&dtype,0,0,&dlen)==-1)
		return -1;

					     /* If dimension is already cached, no-op */
	if(*values != (double*)0){
		*dimlen = dlen;
		return IDRS_SUCCESS;
	}
	
					     /* Get memory for the dimension */
	dlenbytes = dlen*cutypelen(dtype);
	if((dim = malloc(dlenbytes)) == (void*)0){
		cw_error("Cannot allocate %d bytes of memory for fileid = %d, dimid = %d",dlenbytes,fileid,dimid);
		return -1;
	}
					     /* Get the dimension */
	if(cudimget(fileid,dimid,dim)==-1)
		return -1;
	
					     /* malloc a double array (tab) for casting, if necessary */
	if(dtype == CuDouble)
		tab = (double*)dim;	     /* Dimension is already double, no need to cast */
	else{
		dlenbytes = dlen*sizeof(double);
					     /* Note: corresponding free() in cw_cluvdb */
		if((tab = (double*)malloc(dlenbytes))==(double*)0){
			cw_error("Cannot allocate %d bytes for fileid = %d, dimid = %d",dlenbytes,fileid,dimid);
			return -1;
		}
	}
					     /* Cast the dimension values to doubles */
	switch(dtype){
	  case CuDouble:
		break;
	  case CuFloat:
		fp = (float*)dim;
		for(i=0, dp=tab;i<dlen;i++)
			*dp++ = (double)(*fp++);
		break;
	  case CuInt:
		ip = (int*)dim;
		for(i=0, dp=tab;i<dlen;i++)
			*dp++ = (double)(*ip++);
		break;
	  case CuLong:
		lp = (long*)dim;
		for(i=0, dp=tab;i<dlen;i++)
			*dp++ = (double)(*lp++);
		break;
	  case CuShort:
		sp = (short*)dim;
		for(i=0, dp=tab;i<dlen;i++)
			*dp++ = (double)(*sp++);
		break;
	  case CuChar:
	  case CuByte:
		cp = (char*)dim;
		for(i=0, dp=tab;i<dlen;i++)
			*dp++ = (double)(*cp++);
		break;
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
	  case CuLongDouble:
		ldp = (long double*)dim;
		for(i=0, dp=tab;i<dlen;i++)
			*dp++ = (double)(*ldp++);
		break;
#endif
	  default:
		cw_error("Invalid dimension datatype %d",dtype);
		return -1;
	}

	if(dtype != CuDouble){
		free(dim);
	}

	*values = tab;
	*dimlen = dlen;

	return IDRS_SUCCESS;
}
					     /* Map the range [DF,DL] to indices [IDF,IDL] of the dimension.
					      * XDF and XDL are returned as the dimension values corresponding
					      * to IDF and IDL, respectively. POLICY may be CW_ROUND_NEAREST,
					      * in which case the endpoints are mapped to the nearest dimension
					      * values, or CW_RANGE, in which case the range is mapped to
					      * the largest dimension range strictly within [DF,DL].
					      *
					      * DELTA is used as in cw_lookup_with_policy.
					      *
					      * If DF equals DL, rounding occurs, regardless of the rounding policy.
					      * If either DF or DL is outside the dimension boundaries, it is mapped
					      * to the nearest dimension endpoint.
					      *
					      * If ISCYCLE == 1, the dimension is treated as cyclical, with
					      * period CYCLE. If ISCYCLE == 0, CYCLE is ignored.
					      *
					      * The actual dimension values are returned in DP. If *DP is non-null,
					      * the function is a no-op.
					      *
					      * The function returns 0 on success,
					      * -1 on failure.
					      */

int cw_dimmap(int fileid, int dimid, double df, double dl, CwRoundPolicy policy, double delta, int isCycle, double cycle, double** dp, long *idf, long *idl, double *xdf, double *xdl){

	CwRoundPolicy fpolicy, lpolicy;
	double *tab;
	long dlen;
	long idfb, idlb;
	long ifcycle, ilcycle;
	int incr;

	if(cw_dimget(fileid, dimid, dp, &dlen) != IDRS_SUCCESS)
		return -1;

	tab = *dp;

					     /* Handle null values. The policy is:
					      * if both df and dl are null, return the
					      * entire range in file order. If only one
					      * is null, set it to the other. */
	if(CW_IS_FLOAT_NULL(df) && CW_IS_FLOAT_NULL(dl)){
		*idf = 0;
		*idl = dlen-1;
		*xdf = tab[0];
		*xdl = tab[dlen-1];
		return IDRS_SUCCESS;
	}
	else if(CW_IS_FLOAT_NULL(df))
		df = dl;
	else if(CW_IS_FLOAT_NULL(dl))
		dl = df;
					     /* Lookup based on policy */
	switch(policy){
	  case CW_RANGE:
		if(df==dl)
			fpolicy = lpolicy = CW_ROUND_NEAREST;
		else if((tab[0]<tab[dlen-1]) == (df<dl)){
			fpolicy = CW_ROUND_UP;
			lpolicy = CW_ROUND_DOWN;
		}
		else{
			fpolicy = CW_ROUND_DOWN;
			lpolicy = CW_ROUND_UP;
		}
		break;
	  case CW_ROUND_NEAREST:
		fpolicy = lpolicy = CW_ROUND_NEAREST;
		break;
	}

	if(isCycle == 0){
		if(cw_lookup_with_policy(tab, dlen, df, fpolicy, delta, idf)==-1)
			return -1;
		if(cw_lookup_with_policy(tab, dlen, dl, lpolicy, delta, idl)==-1)
			return -1;
		*xdf = tab[*idf];
		*xdl = tab[*idl];
	}
	else{
		if(cw_lookup_cycle(tab, dlen, df, cycle, fpolicy, delta, &idfb, &ifcycle) != IDRS_SUCCESS)
			return -1;
		if(cw_lookup_cycle(tab, dlen, dl, cycle, lpolicy, delta, &idlb, &ilcycle) != IDRS_SUCCESS)
			return -1;

		*idf = idfb + dlen*ifcycle;
		*idl = idlb + dlen*ilcycle;

		incr = (tab[0]<tab[dlen-1] ? 1 : -1);
		*xdf = tab[idfb] + incr*ifcycle*cycle;
		*xdl = tab[idlb] + incr*ilcycle*cycle;
	}

	return IDRS_SUCCESS;
}
					     /* Lookup ID of var with given naming strings.
					      * String nulls match anything, including the name field.
					      * Return -1 on error.
					      */
int cw_varid(int fileid, const char* source, const char* name, const char* title, const char* units){

	char fsource[CU_MAX_NAME], ftitle[CU_MAX_NAME], funits[CU_MAX_NAME], fname[CU_MAX_NAME];
	int varid, nvars, i;

					     /* Lookup variable name. Null string locates variable 0 */
	if(!strcmp(name,CW_STRING_NULL)){
		varid=0;
	}
	else{
		if((varid = cuvarid(fileid, name)) == -1)
			return -1;
	}

					     /* Return varid if other strings match */

	if(cw_string_attget(fileid, varid, "source", fsource) == CuInvalidType) return -1;
	if(cw_string_attget(fileid, varid, "title", ftitle) == CuInvalidType) return -1;
	if(cw_string_attget(fileid, varid, "units", funits) == CuInvalidType) return -1;

	if(CW_STRING_MATCH(source,fsource) &&
	   CW_STRING_MATCH(title,ftitle) &&
	   CW_STRING_MATCH(units,funits))
		return varid;

					     /* First variable found was not a match
					      * in all fields - look at the rest.
					      *
					      * Get the number of variables.
					      */
	if(cuinquire(fileid, 0, &nvars, 0, 0) == -1)
		return -1;
					     /* NB!!! This assumes that cuvarid checks vars in ID order!!! */
	for(i=varid+1; i<nvars; i++){
					     /* Get the next variable name */
		if(cuvarinq(fileid, i, fname, 0, 0, 0, 0) == -1)
			return -1;
		                             /* If variable name does not match, keep looking;
					      * Blank name matches anything.
					      */
		if(!CW_STRING_MATCH(name, fname))
			continue;
					     /* Variable name matches, return i if other strings match */
		if(cw_string_attget(fileid, i, "source", fsource) == CuInvalidType) return -1;
		if(cw_string_attget(fileid, i, "title", ftitle) == CuInvalidType) return -1;
		if(cw_string_attget(fileid, i, "units", funits) == CuInvalidType) return -1;
		
		if(CW_STRING_MATCH(source,fsource) &&
		   CW_STRING_MATCH(title,ftitle) &&
		   CW_STRING_MATCH(units,funits))
			return i;
	}
	return -1;
}
					     /* Return the attname if found and a string,
					      * CW_STRING_NULL if not found or not a string,
					      * function returns -1 on error, 0 on success.
					      */
int cw_string_attget(int fileid, int varid, const char* attname, char* value){
	
	CuType dtype;
	int len, lenout;
	char *cbuf;

					     /* Look for long_name if title not present */
	if(!strcmp(attname,"title") && cuattinq(fileid, varid, attname, 0, 0)==-1)
		attname="long_name";

					     /* Look for comments if source not present */
	if(!strcmp(attname,"source") && cuattinq(fileid, varid, attname, 0, 0)==-1)
		attname="comments";

					     /* If the attribute doesn't exist or
					      * isn't a character string, return a null */
	if((cuattinq(fileid, varid, attname, &dtype, &len)==-1) ||
	   (dtype != CuChar)){
		strcpy(value,CW_STRING_NULL);
		return IDRS_SUCCESS;
	}

					     /* Add one to the length, as there is no */
					     /* guarantee (especially with netCDF) */
					     /* that the returned attribute value will */
					     /* be null-terminated. */
	len++;

					     /* Check 'hardwired' DRS fields first */
	if(!strcmp(attname,"units"))
		lenout = cwUnitsLen;
	else if(!strcmp(attname,"source")||!strcmp(attname,"comments"))
		lenout = cwSourceLen;
	else if(!strcmp(attname,"title")||!strcmp(attname,"long_name"))
		lenout = cwTitleLen;
	else if(!strcmp(attname,"date"))
		lenout = cwDateLen;
	else if(!strcmp(attname,"time"))
		lenout = cwTimeLen;
	else
		lenout = len;

					     /* If not enough memory in value, read into a buffer and */
					     /* copy the correct length */
	if(lenout < len){
		if((cbuf=malloc(len))==(char*)0){
			cw_error("Error allocating memory of length %d for attribute %s",len,attname);
			return -1;
		}
		if(cuattget(fileid,varid,attname,(void*)cbuf) == -1)
			return -1;
		strncpy(value,cbuf,lenout);
		value[lenout-1]='\0';
		free(cbuf);
	}
	else{

		if(cuattget(fileid,varid,attname,(void*)value) == -1)
			return -1;
		value[len-1]='\0';
	}
	cw_strtrim(value);
	return IDRS_SUCCESS;
}
					     /* Map a DRS type to a cdunif type.
					      * Return -1 on error, cdunif type on success.
					      * Cannot have blanks at front !!*/
CuType cw_drs_to_unif_datatype(const char* drstype){

        char typeChar;
        int len, nconv;
	
	if((nconv = sscanf(drstype,"%c*%d",&typeChar,&len)) != 2)
		return -1;
        switch(typeChar){
	  case 'r':
          case 'R':
		switch(len){
		  case 4:
			return CuFloat;
		  case 8:
			return CuDouble;
		  case 16:
			return CuLongDouble;
		  default:
			return -1;
		}
	  case 'i':
          case 'I':
		switch(len){
		  case 1:
			return CuByte;
		  case 2:
			return CuShort;
		  case 4:
#if defined(__alpha) || defined(__ia64) || defined(__x86_64__)
			return CuInt;
#else
			return CuLong;
#endif
		  case 8:
			return CuLong;
		  default:
			return -1;
                }
	  case 'c':
          case 'C':
		return CuChar;
	  default:
		cw_error("Invalid datatype: %s",drstype);
		return -1;
        }
					     /* Unreachable */
}
					     /* Map a cdunif datatype to DRS string representation */
int cw_unif_to_drs_datatype(CuType dtype, char* drstype){
	switch(dtype){
	  case CuByte:
		strcpy(drstype,"I*1");
		break;
	  case CuChar:
		strcpy(drstype,"C*1");
		break;
	  case CuShort:
#ifdef cray
		strcpy(drstype,"I*8");
#else		
		strcpy(drstype,"I*2");
#endif
		break;
	  case CuInt:
#ifdef cray
		strcpy(drstype,"I*8");
#else
		strcpy(drstype,"I*4");
#endif		
		break;
	  case CuLong:
#if defined(cray) || defined(__alpha) || defined(__ia64) || defined(__x86_64__)
		strcpy(drstype,"I*8");
#else
		strcpy(drstype,"I*4");
#endif		
		break;
	  case CuFloat:
#ifdef cray
		strcpy(drstype,"R*8");
#else
		strcpy(drstype,"R*4");
#endif
		break;
	  case CuDouble:
		strcpy(drstype,"R*8");
		break;
	  case CuLongDouble:
		strcpy(drstype,"R*16");
		break;
	  default:
		cw_error("Invalid cdunif datatype: %d",dtype);
		return -1;
	}
	return IDRS_SUCCESS;
}
					     /* Map a cdunif datatype to DRS enumerated datatype */
int cw_unif_to_drs_enumtype(CuType dtype, int* enumtype){
	switch(dtype){
	  case CuByte:
		*enumtype = IDRS_I1;
		break;
	  case CuChar:
		*enumtype = IDRS_ASCII;
		break;
	  case CuShort:
#ifdef cray
		*enumtype = IDRS_I8;
#else		
		*enumtype = IDRS_I2;
#endif
		break;
	  case CuInt:
#ifdef cray
		*enumtype = IDRS_I8;
#else
		*enumtype = IDRS_I4;
#endif		
	  case CuLong:
#if defined(cray) || defined(__alpha) || defined(__ia64) || defined(__x86_64__)
		*enumtype = IDRS_I8;
#else
		*enumtype = IDRS_I4;
#endif		
		break;
	  case CuFloat:
#ifdef cray
		*enumtype = IDRS_CRAY_R8;
#else
		*enumtype = IDRS_IEEE_R4;
#endif
		break;
	  case CuDouble:
#ifdef cray
		*enumtype = IDRS_CRAY_R8;
#else
		*enumtype = IDRS_IEEE_R8;
#endif
		break;
	  case CuLongDouble:
#ifdef cray
		*enumtype = IDRS_CRAY_R16;
#else
		*enumtype = IDRS_IEEE_R16;
#endif
		break;
	  default:
		cw_error("Invalid cdunif datatype: %d",dtype);
		return -1;
	}
	return IDRS_SUCCESS;
}

					     /* Add extension to filename if none exists.
					      * Return result in result string */
void cw_add_extension(const char* filename, const char* extension, char* result){

	char *c;
	int hasExtension;

	if(!filename || !strcmp(filename,CW_STRING_NULL) || !strcmp(filename,"")){
		strcpy(result,CW_STRING_NULL);
		return;
	}
	strcpy(result,filename);

	hasExtension = 0;
	for(c=result; *c; c++){
		if(*c=='/')
			hasExtension = 0;
		else if(*c == '.')
			hasExtension = 1;
	}
	if(!hasExtension){
		strcat(result,extension);
	}
	return;
}
					     /* Trim leading and trailing blanks from s, in place, */
					     /* IF NOT A DRS NULL STRING; */
					     /* s must already be null-terminated */
					     /* Characters are shifted left so that first */
					     /* nonblank character of s is the first character; */
					     /* Null is set after last nonblank character. */
char *cw_strtrim(char* s)
{
	if(strcmp(s,CW_STRING_NULL))
		custrtrim(s);
	return s;
}

int cw_majority(CwMajority majority){
	switch(majority){
	  case CW_C_MAJORITY:
		cwMajority = CW_C_MAJORITY;
		break;
	  case CW_FORTRAN_MAJORITY:
		cwMajority = CW_FORTRAN_MAJORITY;
		break;
	  default:
		cw_error("Invalid majority flag: %d",majority);
		return -1;
	}
	return IDRS_SUCCESS;
}
					     /* Return the id of a variable with: */
					     /* - the same name as dname, */
					     /* - a single dimension, and */
					     /* - a (single) dimension id which equals dimid */
					     /* */
					     /* or -1 if error or none. */
int cw_dimension_varid(int fileid, int dimid, char* dname){
	int dimvarid;
	int ndims;
	int dimids[CU_MAX_VAR_DIMS];
	int found;

					     /* If the variable is found */
	if((dimvarid = cuvarid(fileid, dname)) != -1){
					     /* Get the number of dimensions and dimension IDs */
		if(cuvarinq(fileid, dimvarid, 0, 0, &ndims, dimids, 0)==-1){
			return -1;
		}
					     /* Dimension variable <==> one dimension with same dimension ID */
		found = (ndims == 1 && dimids[0]==dimid);
	}
	else
					     /* No such variable */
		found = 0;
	if(!found) dimvarid = -1;
	return dimvarid;
}

int cw_set_dimension_option(CwExtensionOption option){
	switch(option){
	  case CW_STANDARD:
		cwDimensionOption = CW_STANDARD;
		break;
	  case CW_EXTENDED:
		cwDimensionOption = CW_EXTENDED;
		break;
	  default:
		cw_error("Invalid dimension option: %d",option);
		return -1;
	}
	return IDRS_SUCCESS;
}

int cw_set_string_option(CwExtensionOption option){
	switch(option){
	  case CW_STANDARD:
		cwSourceLen = IDRS_SOURCELEN;	     /* Source string length */
		cwNameLen = IDRS_NAMELEN;	     /* Name string length */
		cwTitleLen = IDRS_TITLELEN;	     /* Title string length */
		cwUnitsLen = IDRS_UNITSLEN;	     /* Units string length */
		cwDateLen = IDRS_DATELEN;	     /* Date string length */
		cwTimeLen = IDRS_TIMELEN;	     /* Time string length */
		cwTypeLen = IDRS_TYPELEN;	     /* Datatype string length */
		break;
	  case CW_EXTENDED:
		cwSourceLen = CU_MAX_NAME;	     /* Source string length */
		cwNameLen = CU_MAX_NAME;	     /* Name string length */
		cwTitleLen = CU_MAX_NAME;	     /* Title string length */
		cwUnitsLen = CU_MAX_NAME;	     /* Units string length */
		cwDateLen = CU_MAX_NAME;	     /* Date string length */
		cwTimeLen = CU_MAX_NAME;	     /* Time string length */
		cwTypeLen = CU_MAX_NAME;	     /* Datatype string length */
		break;
	  default:
		cw_error("Invalid string option: %d",option);
		return -1;
	}
	return IDRS_SUCCESS;
}
					     /* Get the cdunif fileid */
int cw_get_fileid(int lu){
					     /* Check for valid current fileid */
	if(lu < 0 || lu > CU_MAX_LU){
		cw_error("No current file");
		return IDRS_BADLU;
	}
	return fileMap[lu];
}


					     /* Compatibility functions */
int cw_putdat(int lu,void* a){
	cw_error("Error calling putdat on file opened with read-only interface");
	return -1;
}	
int cw_putdic(int lu, int iopt){
	cw_error("Error calling putdic on file opened with read-only interface");
	return -1;
}	
int cw_putvdim(int lu,int len,float* dimvar,int* i1,int* i2){
	cw_error("Error calling putvdim on file opened with read-only interface");
	return -1;
}	
int cw_setdate(char* date,char* time){
	cw_error("Error calling setdate on file opened with read-only interface");
	return -1;
}	
int cw_setrep(int irep){
	cw_error("Error calling setrep on file opened with read-only interface");
	return -1;
}	
