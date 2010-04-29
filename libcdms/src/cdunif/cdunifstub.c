/* -*-Mode: C;-*-
 * Module:      cdunif stub functions
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
 * $Log: cdunifstub.c,v $
 * Revision 1.1.1.1  1997/12/09 18:57:40  drach
 * Copied from cirrus
 *
 * Revision 1.4  1997/11/24  17:28:35  drach
 * - Added QL package to cdunif
 * - Added NdimIntersect function to CDMS
 *
 * Revision 1.3  1997/01/06  17:47:40  drach
 * - Added HDF to cdunif
 *
 * Revision 1.2  1994/11/18  00:13:35  drach
 * Added error processing routines and externs.
 *
 * Revision 1.1  1994/11/17  19:58:45  drach
 * Initial CVS version
 *
 *
 */

#include <cdunifint.h>
int cuopenread_stub_drs(const char* controlpath, const char* datapath){
	CuError(CU_EBADFORM,"DRS I/O routines not available, file %s.",controlpath);
	return -1;
}
int cuopenread_stub_grads(const char* controlpath, const char* datapath){
	CuError(CU_EBADFORM,"GrADS I/O routines not available, file %s.",controlpath);
	return -1;
}
int cuopenread_stub_nc(const char* controlpath, const char* datapath){
	CuError(CU_EBADFORM,"netCDF I/O routines not available, file %s.",controlpath);
	return -1;
}
int cuopenread_stub_hdf(const char* controlpath, const char* datapath){
	CuError(CU_EBADFORM,"HDF I/O routines not available, file %s.",controlpath);
	return -1;
}
int cuopenread_stub_ql(const char* controlpath, const char* datapath){
	CuError(CU_EBADFORM,"QL I/O routines not available, file %s.",controlpath);
	return -1;
}
int cuopenread_stub_pop(const char* controlpath, const char* datapath){
	CuError(CU_EBADFORM,"POP I/O routines not available, file %s.",controlpath);
	return -1;
}
int cuopenread_stub_pp(const char* controlpath, const char* datapath){
	CuError(CU_EBADFORM,"PP I/O routines not available, file %s.",controlpath);
	return -1;
}
int cuclose_stub(CuFile* file){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cuinquire_stub(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cudimid_stub(CuFile* file, int varid, const char* name){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cudiminq_stub(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cudimget_stub(CuFile* file, int dimid, void* values){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cuvarid_stub(CuFile* file, const char* name){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cuvarinq_stub(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cuvarget_stub(CuFile* file, int varid, const long start[], const long count[], void* value){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cuattinq_stub(CuFile* file, int varid, const char* name, CuType* datatype, int* len){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cuattget_stub(CuFile* file, int varid, const char* name, void* value){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
int cuattname_stub(CuFile* file, int varid, int attnum, char* name){
	CuError(CU_EBADFORM,"Stub called, file %s",file->controlpath);
	return -1;
}
