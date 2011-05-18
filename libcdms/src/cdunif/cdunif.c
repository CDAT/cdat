/* -*-Mode: C;-*-
 * Module:      cdunif - CDMS uniform I/O user-level functions
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
 * $Log: cdunif.c,v $
 * Revision 1.1.1.1  1997/12/09 18:57:39  drach
 * Copied from cirrus
 *
 * Revision 1.16  1997/11/24  17:28:30  drach
 * - Added QL package to cdunif
 * - Added NdimIntersect function to CDMS
 *
 * Revision 1.15  1997/01/06  17:47:37  drach
 * - Added HDF to cdunif
 *
 * Revision 1.14  1996/10/31  23:53:32  drach
 * - Cleaned up error returns
 *
 * Revision 1.13  1995/10/16  18:51:58  drach
 * - Added CuInt datatype to cutypelen, for DEC Alpha version
 *
 * Revision 1.12  1995/07/12  22:02:17  drach
 * Removed long double type for SGI version
 *
 * Revision 1.11  1995/03/30  00:20:39  drach
 * Allow 99 as a valid lu
 *
 * Revision 1.10  1995/03/24  21:44:43  fiorino
 * GrADS routines
 *
 * Revision 1.9  1995/03/09  00:30:35  drach
 * Added netCDF (no mods to cdunif.c in this version)
 *
 * Revision 1.8  1995/03/09  00:28:59  drach
 * (No modification)
 *
 * Revision 1.7  1995/01/18  02:51:40  drach
 * - Made seterropts a dispatch function
 *
 * Revision 1.6  1994/12/20  23:06:02  drach
 * - Added grads template
 *
 * Revision 1.4  1994/12/17  00:37:22  drach
 * - Allocate logical unit numbers
 * - Added cugetlu, cufreelu
 *
 * Revision 1.3  1994/11/23  23:02:37  drach
 * - Changed hyperlong to long double
 * - cutypelen returns sizeof(type) rather than hard-wired values.
 *
 * Revision 1.2  1994/11/18  00:13:30  drach
 * Added error processing routines and externs.
 *
 * Revision 1.1  1994/11/17  19:58:39  drach
 * Initial CVS version
 *
 *
 */

#include <cdunifint.h>

/*
 * =================================================================
 *			Function dispatch structures
 * =================================================================
 */

typedef struct cu_dispatch_s CuDispatch;

struct cu_dispatch_s {
	CuFileType filetype;
	int (*cuopenread_dispatch)(const char* controlpath, const char* datapath);
	int (*cuclose_dispatch)(CuFile* file);
	int (*cuinquire_dispatch)(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim);
	int (*cudimid_dispatch)(CuFile* file, int varid, const char* name);
	int (*cudiminq_dispatch)(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length);
	int (*cudimget_dispatch)(CuFile* file, int dimid, void* values);
	int (*cuvarid_dispatch)(CuFile* file, const char* name);
	int (*cuvarinq_dispatch)(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts);
	int (*cuvarget_dispatch)(CuFile* file, int varid, const long start[], const long count[], void* value);
	int (*cuattinq_dispatch)(CuFile* file, int varid, const char* name, CuType* datatype, int* len);
	int (*cuattget_dispatch)(CuFile* file, int varid, const char* name, void* value);
	int (*cuattname_dispatch)(CuFile* file, int varid, int attnum, char* name);
	void (*cuseterropts_dispatch)(int erropts);
};


/* Function dispatch struct */
/* NB! The formats must appear in the same order */
/* as they are defined in the CuFileType enum !!!*/
static CuDispatch cu_dispatch[] = {
{
	CuDrs,
#ifdef drs
	cuopenread_drs,
	cuclose_drs,
	cuinquire_gen,
	cudimid_gen,
	cudiminq_gen,
	cudimget_drs,
	cuvarid_gen,
	cuvarinq_gen,
	cuvarget_drs,
	cuattinq_gen,
	cuattget_gen,
	cuattname_gen,
	cuseterropts_drs,
#else
	cuopenread_stub_drs,
	cuclose_stub,
	cuinquire_stub,
	cudimid_stub,
	cudiminq_stub,
	cudimget_stub,
	cuvarid_stub,
	cuvarinq_stub,
	cuvarget_stub,
	cuattinq_stub,
	cuattget_stub,
	cuattname_stub,
	cuseterropts_gen,
#endif
},
{
	CuGrads,
#ifdef grads
	cuopenread_grads,
	cuclose_grads,
	cuinquire_gen,
	cudimid_gen,
	cudiminq_gen,
	cudimget_grads,
	cuvarid_gen,
	cuvarinq_gen,
	cuvarget_grads,
	cuattinq_gen,
	cuattget_gen,
	cuattname_gen,
	cuseterropts_gen,
#else
	cuopenread_stub_grads,
	cuclose_stub,
	cuinquire_stub,
	cudimid_stub,
	cudiminq_stub,
	cudimget_stub,
	cuvarid_stub,
	cuvarinq_stub,
	cuvarget_stub,
	cuattinq_stub,
	cuattget_stub,
	cuattname_stub,
	cuseterropts_gen,
#endif
},
{
	CuNetcdf,
#ifdef netcdf
	cuopenread_nc,
	cuclose_nc,
	cuinquire_nc,
	cudimid_nc,
	cudiminq_nc,
	cudimget_nc,
	cuvarid_nc,
	cuvarinq_nc,
	cuvarget_nc,
	cuattinq_nc,
	cuattget_nc,
	cuattname_nc,
	cuseterropts_nc,
#else
	cuopenread_stub_nc,
	cuclose_stub,
	cuinquire_stub,
	cudimid_stub,
	cudiminq_stub,
	cudimget_stub,
	cuvarid_stub,
	cuvarinq_stub,
	cuvarget_stub,
	cuattinq_stub,
	cuattget_stub,
	cuattname_stub,
	cuseterropts_gen,
#endif
},
{
        CuHdf,
#ifdef hdf
        cuopenread_hdf,
        cuclose_hdf,
        cuinquire_hdf,
        cudimid_hdf,
        cudiminq_hdf,
        cudimget_hdf,
        cuvarid_hdf,
        cuvarinq_hdf,
        cuvarget_hdf,
        cuattinq_hdf,
        cuattget_hdf,
        cuattname_hdf,
        cuseterropts_hdf,
#else
        cuopenread_stub_hdf,
        cuclose_stub,
        cuinquire_stub,
        cudimid_stub,
        cudiminq_stub,
        cudimget_stub,
        cuvarid_stub,
        cuvarinq_stub,
        cuvarget_stub,
        cuattinq_stub,
        cuattget_stub,
        cuattname_stub,
        cuseterropts_gen,
#endif
},
{
        CuQL,
#ifdef HAVE_QL
        cuopenread_ql,
        cuclose_ql,
        cuinquire_ql,
        cudimid_ql,
        cudiminq_ql,
        cudimget_ql,
        cuvarid_ql,
        cuvarinq_ql,
        cuvarget_ql,
        cuattinq_ql,
        cuattget_ql,
        cuattname_ql,
        cuseterropts_ql,
#else
        cuopenread_stub_ql,
        cuclose_stub,
        cuinquire_stub,
        cudimid_stub,
        cudiminq_stub,
        cudimget_stub,
        cuvarid_stub,
        cuvarinq_stub,
        cuvarget_stub,
        cuattinq_stub,
        cuattget_stub,
        cuattname_stub,
        cuseterropts_gen,
#endif
},
{
	CuPop,
#ifdef HAVE_POP
	cuopenread_pop,
	cuclose_pop,
	cuinquire_pop,
	cudimid_pop,
	cudiminq_pop,
	cudimget_pop,
	cuvarid_pop,
	cuvarinq_pop,
	cuvarget_pop,
	cuattinq_pop,
	cuattget_pop,
	cuattname_pop,
	cuseterropts_pop,
#else
	cuopenread_stub_pop,
	cuclose_stub,
	cuinquire_stub,
	cudimid_stub,
	cudiminq_stub,
	cudimget_stub,
	cuvarid_stub,
	cuvarinq_stub,
	cuvarget_stub,
	cuattinq_stub,
	cuattget_stub,
	cuattname_stub,
	cuseterropts_gen,
#endif
},
{
	CuPP,
#ifdef HAVE_PP
	cuopenread_pp,
	cuclose_pp,
	cuinquire_gen,
	cudimid_gen,
	cudiminq_gen,
	cudimget_pp,
	cuvarid_gen,
	cuvarinq_gen,
	cuvarget_pp,
	cuattinq_gen,
	cuattget_gen,
	cuattname_gen,
	cuseterropts_gen,
#else
	cuopenread_stub_pp,
	cuclose_stub,
	cuinquire_stub,
	cudimid_stub,
	cudiminq_stub,
	cudimget_stub,
	cuvarid_stub,
	cuvarinq_stub,
	cuvarget_stub,
	cuattinq_stub,
	cuattget_stub,
	cuattname_stub,
	cuseterropts_gen,
#endif
}
};

int cuErrOpts = CU_VERBOSE;		   /* Default error options */

static int cuNextLu1;	        	 /* Default next Fortran logical unit for DRS files */
static int cuNextLu2;
static int cuUseLuMap = 1;	     /* True iff should lookup next lu */
static int cuLuMap[CU_MAX_LU+1]; /* cuLuMap[lu]==1 iff lu is in use */
static int cuLuMapInit = 0;		   /* True iff CuLuMap has been initialized */
static int cuLuAvailable;		     /* Number of logical units currently available */


int cuopenread(const char* controlpath, const char* datapath){
	CuFileType filetype;
  /* Determine the file format */
	if((filetype=CuGetFileType(controlpath))==CuUnknown){
		return -1;
  }

  /* Dispatch the open function */
	return (*(cu_dispatch[filetype].cuopenread_dispatch))(controlpath, datapath);
}

int cuclose(int fileid){
	CuFile *file;
	int err;
  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0){
		return -1;
  }

  /* Dispatch the close function */
	if((err=(*(cu_dispatch[file->filetype].cuclose_dispatch))(file)) != CU_SUCCESS)
		return err;

  /* Delete the file structure */
	return CuDeleteFile(fileid);
}


int cuinquire(int fileid, int* ngdims, int* nvars, int* natts, int* recdim){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the file inquiry function */
	return (*(cu_dispatch[file->filetype].cuinquire_dispatch))(file,ngdims,nvars,natts,recdim);
}


int cudimid(int fileid, int varid, const char* name){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the dimension ID function */
	return (*(cu_dispatch[file->filetype].cudimid_dispatch))(file,varid,name);
}


int cudiminq(int fileid, int dimid, char* dimname, char* dimunits, CuType* dataType,
             CuDimType* dimtype, int* varid, long* length){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the dimension inquiry function */
	return (*(cu_dispatch[file->filetype].cudiminq_dispatch))(file, dimid, dimname, dimunits, dataType, dimtype, varid, length);
}


int cudimget(int fileid, int dimid, void* values){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;

  /* Dispatch the dimension read function */
	return (*(cu_dispatch[file->filetype].cudimget_dispatch))(file, dimid, values);
}


int cuvarid(int fileid, const char* name){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the variable ID function */
	return (*(cu_dispatch[file->filetype].cuvarid_dispatch))(file, name);
}


int cuvarinq(int fileid, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the variable inquiry function */
	return (*(cu_dispatch[file->filetype].cuvarinq_dispatch))(file, varid, name, datatype, ndims, dimids, natts);
}


int cuvarget(int fileid, int varid, const long start[], const long count[], void* value){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the variable read function */
	return (*(cu_dispatch[file->filetype].cuvarget_dispatch))(file, varid, start, count, value);
}


int cuattinq(int fileid, int varid, const char* name, CuType* datatype, int* len){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the attribute inquiry function */
	return (*(cu_dispatch[file->filetype].cuattinq_dispatch))(file, varid, name, datatype, len);
}


int cuattget(int fileid, int varid, const char* name, void* value){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the attribute read function */
	return (*(cu_dispatch[file->filetype].cuattget_dispatch))(file, varid, name, value);
}


int cuattname(int fileid, int varid, int attnum, char* name){
	CuFile* file;

  /* Lookup the file */
	if((file=CuLookupFile(fileid)) == (CuFile*)0)
		return -1;
  /* Dispatch the attribute lookup function */
	return (*(cu_dispatch[file->filetype].cuattname_dispatch))(file, varid, attnum, name);
}


int cutypelen(CuType datatype){
	switch(datatype){
	  case CuByte:
	  case CuChar:
		return sizeof(char);
	  case CuShort:
		return sizeof(short);
	  case CuInt:
		return sizeof(int);
	  case CuLong:
		return sizeof(long);
	  case CuFloat:
		return sizeof(float);
	  case CuDouble:
		return sizeof(double);
#if !defined(sgi) && !defined(__alpha) && !defined(__ia64) && !defined(__x86_64__)
	  case CuLongDouble:
		return sizeof(long double);
#endif
	  default:
		CuError(CU_EBADTYPE,"Type = %d",datatype);
		return -1;
	}
}


/* All the logical unit stuff is localized here. */
/* If cusetlu is called, cugetlu returns the lus */
/* specified, otherwise cugetlu looks up two */
/* free lus in cuLuMap. lus must be freed for reuse.*/
int cusetlu(int lu1, int lu2){

	int i;

	if(lu1<0 || lu1>CU_MAX_LU || lu2<0 || lu2>CU_MAX_LU || lu1==lu2){
		CuError(CU_EINVLU,"Logical units = %d, %d, should be unequal, in the range 0..%d",lu1,lu2,CU_MAX_LU);
		return -1;
	}
	if(!cuLuMapInit){
		for(i=0;i<=CU_MAX_LU;i++)
			cuLuMap[i]=0;
		cuLuMapInit = 1;
		cuLuAvailable = 48;	     /* Use 51..98 for lookup*/
	}
	cuNextLu1 = lu1;
	if(lu1>50) cuLuAvailable--;
	cuNextLu2 = lu2;
	if(lu2>50) cuLuAvailable--;
	cuLuMap[lu1] = cuLuMap[lu2] = 1;
	cuUseLuMap = 0;			     /* Use user-specified lus next call to cugetlu*/
	return CU_SUCCESS;
}


int cugetlu(int* lu1, int* lu2){
	int i;

	if(!cuLuMapInit){
		for(i=0;i<=CU_MAX_LU;i++)
			cuLuMap[i]=0;
		cuLuMapInit = 1;
		cuLuAvailable = 48;	     /* Use 51..98 for lookup*/
	}

  /* Use user-specified lus? */
	if(!cuUseLuMap){
		*lu1 = cuNextLu1;
		*lu2 = cuNextLu2;
		cuUseLuMap = 1;		     /* Use lookup unless cusetlu called */
		return CU_SUCCESS;
	}
	else if(cuLuAvailable<2){
		CuError(CU_EINVLU,"No more logical units available");
		return -1;
	}
  /* Lookup two free lus */
	for(i=51; i<CU_MAX_LU && cuLuMap[i]; i++);
	*lu1 = i;
	cuLuMap[i] = 1;

	for(++i; i<CU_MAX_LU && cuLuMap[i]; i++);
	*lu2 = i;
	cuLuMap[i] = 1;

	cuLuAvailable -= 2;
	return CU_SUCCESS;
}
int cufreelu(int lu){
	if(lu<0 || lu>CU_MAX_LU){
		return -1;
	}
	cuLuMap[lu]=0;
	if(lu>50) cuLuAvailable++;
	return CU_SUCCESS;
}
void cuseterropts(int erropts){
	CuFileType filetype;

	cuErrOpts = erropts;

  /* Dispatch function for all formats.
  * Stub functions do not cause an error,
  * are no-ops.
  */
	for(filetype=0;filetype<CuNumberOfFormats;filetype++){
		(*(cu_dispatch[filetype].cuseterropts_dispatch))(erropts);
	}
	return;
}
int cugeterr(void){
	extern int cuLastError;

	return cuLastError;
}
