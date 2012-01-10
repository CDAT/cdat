/* -*-Mode: C;-*-
 * Module:      cdunifint.h - cdunif internal include file
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
 * $Log: cdunifint.h,v $
 * Revision 1.2  1998/07/02 23:40:47  drach
 * - Added support for absolute time, via routines cdAbs2Comp, cdComp2Abs, cdDecodeRelativeTime, and cdDecodeAbsoluteTime
 * - Added support for the LANL POP ocean data format
 *
 * Revision 1.1.1.1  1997/12/09 18:57:39  drach
 * Copied from cirrus
 *
 * Revision 1.15  1997/12/03  22:21:53  drach
 * - In cdunifdrs.c, dimensions which are reversed or subsetted wrt a
 *   coordinate dimension are now treated as local.
 * - Added cdDimGetDouble to cdmsslab.c
 * - Fixed wraparound, reading wraparound dimensions in cdmsslab.c
 *
 * Revision 1.14  1997/11/24  17:28:06  drach
 * - Added QL package to cdunif
 * - Added NdimIntersect function to CDMS
 *
 * Revision 1.13  1997/10/24  18:23:36  drach
 * - Cache netCDF unlimited dimensions
 * - Consistent with GrADS src170
 *
 * Revision 1.12  1997/09/26  21:45:27  drach
 * - Added HDF
 * - Repaired fouled up cddrs includes
 *
 * Revision 1.11  1997/01/06  17:47:14  drach
 * - Added HDF to cdunif
 *
 * Revision 1.10  1995/10/16  18:57:20  drach
 * - Added CuInt datatype, DEC Alpha version
 * - Modified prototypes for casting routines
 *
 * Revision 1.9  1995/09/15  21:18:52  drach
 * - Modified for Cray
 *
 * Revision 1.8  1995/03/09  00:35:19  drach
 * Added netCDF, upgraded cureadarray with casting, user-specified indices
 *
 * Revision 1.7  1995/01/18  02:53:59  drach
 * - Made cuseterropts a dispatch function
 *
 * Revision 1.6  1994/12/20  23:12:46  drach
 * - Added GrADS function declarations
 *
 * Revision 1.5  1994/12/20  01:20:07  drach
 * - Added copybuffer size define
 *
 * Revision 1.4  1994/12/17  00:43:26  drach
 * - remove cuNextLu extern def - localized in cdunif.c now
 *
 * Revision 1.3  1994/12/14  02:33:40  drach
 * - Renamed _CDUNIFIO_H to _CDUNIFINT_H
 *
 * Revision 1.2  1994/11/18  00:12:47  drach
 * Added error processing routines and externs.
 *
 * Revision 1.1  1994/11/17  21:39:13  drach
 * Initial version
 *
 *
 */
#ifndef _CDUNIFINT_H
#define _CDUNIFINT_H

#include <stdlib.h>
#include "cdunif.h"

/*
 * =================================================================
 *			Magic cookies
 * =================================================================
 */
#if defined(cray)
#define DRS_MAGIC_COOKIE 0x4452532044494354
#define NETCDF_MAGIC_COOKIE 0x4344460100000000
#define NETCDF4_MAGIC_COOKIE 0x894844460000000
#define NETCDF4_64BIT_MAGIC_COOKIE 0x460243440000000
#define NETCDF4_64BIT_MAGIC_COOKIE2 0x444689480000000
#define HDF_MAGIC_COOKIE 0x0e03130100000000
#elif defined(__alpha)
#define DRS_MAGIC_COOKIE 0x5443494420535244
#define NETCDF_MAGIC_COOKIE 0x0000000001464443
#define NETCDF4_MAGIC_COOKIE 0x000000046444889
#define NETCDF4_64BIT_MAGIC_COOKIE 0x000000002464443
#define NETCDF4_64BIT_MAGIC_COOKIE2 0x0000000046444889
#define HDF_MAGIC_COOKIE 0x000000000113030e
#elif defined(__ia64)
#define DRS_MAGIC_COOKIE 0x5443494420535244
#define NETCDF_MAGIC_COOKIE 0x0000000001464443
#define NETCDF4_MAGIC_COOKIE 0x000000046444889
#define NETCDF4_64BIT_MAGIC_COOKIE 0x000000002464443
#define NETCDF4_64BIT_MAGIC_COOKIE2 0x000000046444889
#define HDF_MAGIC_COOKIE 0x000000000113030e
#elif defined(__x86_64__)
#define DRS_MAGIC_COOKIE 0x5443494420535244
#define NETCDF_MAGIC_COOKIE 0x0000000001464443
#define NETCDF4_MAGIC_COOKIE 0x000000002464443
#define NETCDF4_64BIT_MAGIC_COOKIE 0x000000002464443
#define NETCDF4_64BIT_MAGIC_COOKIE2 0x000000046444889
#define HDF_MAGIC_COOKIE 0x000000000113030e
#elif defined(BYTESWAP)
#define DRS_MAGIC_COOKIE 0x20535244
#define NETCDF_MAGIC_COOKIE 0x01464443
#define HDF_MAGIC_COOKIE 0x0113030e
#define NETCDF4_MAGIC_COOKIE 1178880137 
#define NETCDF4_64BIT_MAGIC_COOKIE 0x02464443
#define NETCDF4_64BIT_MAGIC_COOKIE2 0x46444889
#else
#define DRS_MAGIC_COOKIE 0x44525320
#define NETCDF_MAGIC_COOKIE 0x43444601
#define HDF_MAGIC_COOKIE 0x0e031301
#define NETCDF4_MAGIC_COOKIE  -1991752634
#define NETCDF4_64BIT_MAGIC_COOKIE 0x46024344
#define NETCDF4_64BIT_MAGIC_COOKIE2 0x44468948
#endif

#define CU_BUFSIZE 204800		     /* Size of cureadarray copy buffer, in bytes */
/*
 * =================================================================
 *			Structures
 * =================================================================
 */

typedef struct cu_file CuFile;
typedef struct cu_var CuVar;
typedef struct cu_dim CuDim;
typedef struct cu_att CuAtt;

struct cu_att {  			     /* Attribute */
	char name[CU_MAX_NAME+1];	     /* Attribute name */
	CuType datatype;		     /* Datatype of the attribute */
	long len;			     /* Number of elements (NOT bytes) */
	void *val;			     /* Pointer to internal structure for GrADS */
};

struct cu_dim {	        		     /* Dimension */
	char name[CU_MAX_NAME+1];	     /* Dimension name */
	char units[CU_MAX_NAME+1];	     /* Dimension units */
	CuVar* var;			     /* Variable for local dimensions; (CuVar*)0 for global */
	CuVar* coord;			     /* Associated coordinate variable if global, or (CuVar*)0 if local */
	int internid;			     /* Internal integer ID (0-origin dimension number for DRS) */
	int spacing;			     /* For DRS, IDRS_EQUALLY_SPACED or IDRS_UNEQUALLY_SPACED */
	double first;			     /* First dimension value, for DRS eq-spaced dimensions, or first coordinate value for DRS uneq-spaced dimensions */
	double interval;		     /* Dimension interval for DRS eq-spaced dimensions */
	void *internp;			     /* Pointer to internal structure for GrADS */
	long len;			     /* Number of elements (NOT bytes) */
	CuType datatype;		     /* Datatype of related coordinate dimension, or CuDouble if none */
	CuDimType dimtype;		     /* Type of dimensions (CuGlobalDim or CuLocalDim) */
};

struct cu_var { 			     /* Variable */
	int id;				     /* Cdunif ID */
	char name[CU_MAX_NAME+1];	     /* Variable name */
	CuFile *file;			     /* File containing this variable */
	int internalid;			     /* Internal ID (e.g., for netCDF) */
	void *internp;			     /* Pointer to internal structure for GrADS */
	CuType datatype;		     /* Datatype of variable */
	int ndims;			     /* Number of dimensions */
	int dims[CU_MAX_VAR_DIMS];	     /* Pointer to array of dimension IDs */
	int natts;			     /* Number of attributes for this variable */
	CuAtt *atts;			     /* Pointer to array of attributes */
};

struct cu_file { 			     /* File */
	char controlpath[CU_MAX_PATH+1];     /* Pathname of control file */
	char datapath[CU_MAX_PATH+1];	     /* Pathname of datafile (e.g., for DRS) */
	int id;				     /* Cdunif ID */
	int internid1;			     /* First internal ID (e.g. dictionary LU for DRS); netCDF file ID */
	int internid2;			     /* Second internal ID (e.g., datafile LU for DRS) */
	void *internp;			     /* Pointer to internal structure for GrADS */
	CuFileType filetype;		     /* File format */
	int ndims;			     /* Total number of global AND local dimensions */
	CuDim* dims;			     /* Pointer to array of global and local dimensions */
	int recdim;			     /* Record dimension, or -1 if not applicable */
	void *recdimcache;		     /* Record dimension cached values, or 0 if no values */
	int nvars;			     /* Number of variables */
	CuVar* vars;			     /* Pointer to array of variables */
	int ngatts;			     /* Number of global attributes */
	CuAtt* atts;			     /* Pointer to array of global attributes */
	CuFile* next;			     /* Next file in list */
};

/*
 * =================================================================
 *			Function prototypes
 * =================================================================
 */
					     /* cdunif internal functions */
extern CuFile* CuCreateFile(CuFileType filetype);
extern CuFile* CuLookupFile(int fileid);
extern int CuDeleteFile(int fileid);
extern CuVar* CuCreateVars(CuFile* file, int nvars);
extern CuVar* CuLookupVar(CuFile* file, int varid);
extern int CuDeleteVars(CuFile* file);
extern CuDim* CuCreateDims(CuFile* file, int ndims);
extern CuDim* CuLookupDim(CuFile* file, int dimid);
extern CuAtt* CuCreateAtts(CuFile* file, CuVar* var, int natts);
extern int CuSetAtt(CuFile* file, CuVar* var, int attnum, const char *name, CuType datatype, long len, void *values);
extern CuAtt* CuLookupAtt(CuFile* file, int varid, const char *name);
extern int CuDeleteAtts(CuFile* file, CuVar *var);
extern CuFileType CuGetFileType(const char *controlpath);
extern void CuError(int ierr, char *fmt, ...);

					     /* cureadarray internal functions */
extern int cuCopyArray(char *a, CuRRA *aIndices, char *b, CuRRA *bIndices, const long transpose[], size_t elemSize);
extern int cureadarray1(int fileid, int varid, CuRRA *vIndices, const long transpose[], void *userArray,
			CuRRA *uIndices, void *copyBuffer, long lenBuffer, long iter);
extern int cuCast(CuType fromType, CuType toType, long nelems, void *array);
extern void cuFloat2Double(long nelems, void *buf);
extern void cuDouble2LongDouble(long nelems, void *buf);
extern void cuChar2Short(long nelems, void *buf);
extern void cuShort2Int(long nelems, void *buf);
extern void cuInt2Long(long nelems, void *buf);
extern void cuDouble2Float(long nelems, void *buf);
extern void cuLongDouble2Double(long nelems, void *buf);
extern void cuShort2Char(long nelems, void *buf);
extern void cuInt2Short(long nelems, void *buf);
extern void cuLong2Int(long nelems, void *buf);

					     /* Stub functions */
extern int cuclose_stub(CuFile* file);
extern int cuinquire_stub(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim);
extern int cudimid_stub(CuFile* file, int varid, const char* name);
extern int cudiminq_stub(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length);
extern int cudimget_stub(CuFile* file, int dimid, void* values);
extern int cuvarid_stub(CuFile* file, const char* name);
extern int cuvarinq_stub(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts);
extern int cuvarget_stub(CuFile* file, int varid, const long start[], const long count[], void* value);
extern int cuattinq_stub(CuFile* file, int varid, const char* name, CuType* datatype, int* len);
extern int cuattget_stub(CuFile* file, int varid, const char* name, void* value);
extern int cuattname_stub(CuFile* file, int varid, int attnum, char* name);

					     /* Generic functions use cdunifint structures built at */
					     /* file open, can be used for any format which builds */
					     /* the proper structures */
extern int cuinquire_gen(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim);
extern int cudimid_gen(CuFile* file, int varid, const char* name);
extern int cudiminq_gen(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length);
extern int cuvarid_gen(CuFile* file, const char* name);
extern int cuvarinq_gen(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts);
extern int cuattinq_gen(CuFile* file, int varid, const char* name, CuType* datatype, int* len);
extern int cuattget_gen(CuFile* file, int varid, const char* name, void* value);
extern int cuattname_gen(CuFile* file, int varid, int attnum, char* name);
extern void cuseterropts_gen(int erropts);

					     /* DRS functions */
#ifdef drs
extern int cuopenread_drs(const char* controlpath, const char* datapath);
extern int cuclose_drs(CuFile* file);
extern int cudimget_drs(CuFile* file, int dimid, void* values);
extern int cuvarget_drs(CuFile* file, int varid, const long start[], const long count[], void* value);
extern void cuseterropts_drs(int erropts);
extern char* custrtrim(char* s);
#else
extern int cuopenread_stub_drs(const char* controlpath, const char* datapath);
#endif

					     /* GrADS functions */
#ifdef grads
extern int cuopenread_grads(const char* controlpath, const char* datapath);
extern int cuclose_grads(CuFile* file);
extern int cudimget_grads(CuFile* file, int dimid, void* value);
extern int cuvarget_grads(CuFile* file, int varid, const long start[], const long count[], void* value);
#else
extern int cuopenread_stub_grads(const char* controlpath, const char* datapath);
#endif

					     /* netCDF functions */
#ifdef netcdf
#include "netcdf.h"
extern int cuopenread_nc(const char* controlpath, const char* datapath);
extern int cuclose_nc(CuFile* file);
extern int cuinquire_nc(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim);
extern int cudimid_nc(CuFile* file, int varid, const char* name);
extern int cudiminq_nc(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length);
extern int cudimget_nc(CuFile* file, int dimid, void* values);
extern int cuvarid_nc(CuFile* file, const char* name);
extern int cuvarinq_nc(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts);
extern int cuvarget_nc(CuFile* file, int varid, const long start[], const long count[], void* value);
extern int cuattinq_nc(CuFile* file, int varid, const char* name, CuType* datatype, int* len);
extern int cuattget_nc(CuFile* file, int varid, const char* name, void* value);
extern int cuattname_nc(CuFile* file, int varid, int attnum, char* name);
extern void cuseterropts_nc(int erropts);
extern void cumapdatatype_nc(nc_type nctype, CuType* cutype);
extern int cugetattany_nc(CuFile* file, int varid, const char *name, CuType xtype, void *data);
#else
extern int cuopenread_stub_nc(const char* controlpath, const char* datapath);
#endif

					     /* HDF functions */
#ifdef hdf
#include "hdfi.h"
typedef int32 hdf_type;
extern int cuopenread_hdf(const char* controlpath, const char* datapath);
extern int cuclose_hdf(CuFile* file);
extern int cuinquire_hdf(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim);
extern int cudimid_hdf(CuFile* file, int varid, const char* name);
extern int cudiminq_hdf(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length);
extern int cudimget_hdf(CuFile* file, int dimid, void* values);
extern int cuvarid_hdf(CuFile* file, const char* name);
extern int cuvarinq_hdf(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts);
extern int cuvarget_hdf(CuFile* file, int varid, const long start[], const long count[], void* value);
extern int cuattinq_hdf(CuFile* file, int varid, const char* name, CuType* datatype, int* len);
extern int cuattget_hdf(CuFile* file, int varid, const char* name, void* value);
extern int cuattname_hdf(CuFile* file, int varid, int attnum, char* name);
extern void cuseterropts_hdf(int erropts);
extern void cuerrorreport_hdf(/* NO ARGS */);
extern void cumapdatatype_hdf(hdf_type nctype, CuType* cutype);
#else
extern int cuopenread_stub_hdf(const char* controlpath, const char* datapath);
#endif

					     /* QL functions */
#ifdef HAVE_QL
extern int cuopenread_ql(const char* controlpath, const char* datapath);
extern int cuclose_ql(CuFile* file);
extern int cuinquire_ql(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim);
extern int cudimid_ql(CuFile* file, int varid, const char* name);
extern int cudiminq_ql(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length);
extern int cudimget_ql(CuFile* file, int dimid, void* values);
extern int cuvarid_ql(CuFile* file, const char* name);
extern int cuvarinq_ql(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts);
extern int cuvarget_ql(CuFile* file, int varid, const long start[], const long count[], void* value);
extern int cuattinq_ql(CuFile* file, int varid, const char* name, CuType* datatype, int* len);
extern int cuattget_ql(CuFile* file, int varid, const char* name, void* value);
extern int cuattname_ql(CuFile* file, int varid, int attnum, char* name);
extern void cuseterropts_ql(int erropts);
#else
extern int cuopenread_stub_ql(const char* controlpath, const char* datapath);
#endif

					     /* POP functions */
#ifdef HAVE_POP
#include "apic.h"
extern int cuopenread_pop(const char* controlpath, const char* datapath);
extern int cuclose_pop(CuFile* file);
extern int cuinquire_pop(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim);
extern int cudimid_pop(CuFile* file, int varid, const char* name);
extern int cudiminq_pop(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length);
extern int cudimget_pop(CuFile* file, int dimid, void* values);
extern int cuvarid_pop(CuFile* file, const char* name);
extern int cuvarinq_pop(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts);
extern int cuvarget_pop(CuFile* file, int varid, const long start[], const long count[], void* value);
extern int cuattinq_pop(CuFile* file, int varid, const char* name, CuType* datatype, int* len);
extern int cuattget_pop(CuFile* file, int varid, const char* name, void* value);
extern int cuattname_pop(CuFile* file, int varid, int attnum, char* name);
extern void cuseterropts_pop(int erropts);
extern void cumapdatatype_pop(Type nctype, CuType* cutype);
#else
extern int cuopenread_stub_pop(const char* controlpath, const char* datapath);
#endif

					     /* PP functions */
#ifdef HAVE_PP
extern int cuopenread_pp(const char* controlpath, const char* datapath);
extern int cuclose_pp(CuFile* file);
extern int cudimget_pp(CuFile* file, int dimid, void* values);
extern int cuvarget_pp(CuFile* file, int varid, const long start[], const long count[], void* value);
#else
extern int cuopenread_stub_pp(const char* controlpath, const char* datapath);
#endif

					     /* Globals */

extern int cuLastDrsErr;		     /* Most recent DRS error number */
extern int cuLastError;			     /* Most recent cdunif error */

#endif
