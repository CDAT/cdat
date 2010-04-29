/* -*-Mode: C;-*-
 * Module:      cdunif netCDF driver functions
 *
 * Copyright:	1994, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Bob Drach, Lawrence Livermore National Laboratory
 *              drach@llnl.gov
 *
 * Version:     cdunifnc.c,v 1.3 1995/06/09 22:35:55 drach Exp
 *
 * Revision History:
 *
 * cdunifnc.c,v
 * Revision 1.3  1995/06/09  22:35:55  drach
 * - Made grads error string length consistent
 * - Allow null return parameters for netCDF files
 *
 * Revision 1.2  1995/03/30  00:20:41  drach
 * Allow 99 as a valid lu
 *
 * Revision 1.1  1995/03/09  00:30:36  drach
 * Added netCDF (no mods to cdunif.c in this version)
 *
 *
 */

#ifdef netcdf
#include <stdlib.h>
#include "netcdf.h"
#include "cdunifint.h"

int cuopenread_nc(const char* controlpath, const char* datapath){
	CuFile* file;
	int cdfid;
	int t_ngdims, t_nvars, t_natts, t_recdim;

	if((cdfid=ncopen(controlpath,NC_NOWRITE))==-1){
		CuError(CU_EOPEN,"Opening netCDF file %s",controlpath);
		return -1;
	}

	if((file = CuCreateFile(CuNetcdf))==(CuFile*)0){
		return -1;
	}
	strncpy(file->controlpath,controlpath,CU_MAX_PATH);
	file->internid1 = cdfid;

					     /* Inquire the record dimension */
	if(ncinquire(cdfid, &t_ngdims, &t_nvars, &t_natts, &file->recdim)==-1)
		return -1;

	return file->id;
}
int cuclose_nc(CuFile* file){

	ncclose(file->internid1);

					     /* Free record dimension cache if necessary */
	if(file->recdim != -1 && file->recdimcache != (void *)0)
		free(file->recdimcache);

	return CU_SUCCESS;
}
int cuinquire_nc(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim){
	int t_ngdims, t_nvars, t_natts, t_recdim;
	return (ncinquire(file->internid1,
			  (ngdims ? ngdims : &t_ngdims),
			  (nvars ? nvars : &t_nvars),
			  (natts ? natts : &t_natts),
			  (recdim ? recdim : &t_recdim)) == -1 ? -1 : CU_SUCCESS);
}
int cudimid_nc(CuFile* file, int varid, const char* name){
	return ncdimid(file->internid1,name);
}
int cudiminq_nc(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length){
	char dname[MAX_NC_NAME+1];
	int cdfid;
	int dimvarid;			     /* netCDF ID of variable associated with this dimension (if any) */
	int found;			     /* True iff a dimension variable was found. */
	int ndims;
	int dimids[MAX_VAR_DIMS];
	int saveopts;
	long len;
	nc_type nctype, ncunitstype;
	int natts;
	char varname[MAX_NC_NAME+1];
	int attlen;

	cdfid = file->internid1;
	if(ncdiminq(cdfid, dimid, dname, &len)==-1){
		return -1;
	}
	if(dimname) strncpy(dimname,dname,CU_MAX_NAME);
	if(length) *length = len;

					     /* netCDF dimensions are always global */
	if(varid) *varid = CU_GLOBAL;
	if(dimtype) *dimtype = CuGlobalDim;

					     /* Inquire a variable with */
					     /* - the same name as dimname, */
					     /* - a single dimension, and */
					     /* - a dimension name which equals the variable name. */
	saveopts = ncopts;
	ncopts = 0;
	if((dimvarid = ncvarid(cdfid, dname)) != -1){
		ncopts = saveopts;
		if(ncvarinq(cdfid, dimvarid, varname, &nctype, &ndims, dimids, &natts)==-1){
			return -1;
		}
		found = (ndims == 1 && dimids[0]==dimid);
	}
	else
		found = 0;
	ncopts = saveopts;

					     /* If dimension variable was found, */
					     /* inquire the units attribute (if any) */
	if(found){
		saveopts = ncopts;
		ncopts = 0;
		if(ncattinq(cdfid, dimvarid, "units", &ncunitstype, &attlen) != -1 &&
		   ncunitstype == NC_CHAR){
			ncopts = saveopts;
			if(dimunits && cugetattany_nc(file, dimvarid, "units", CuChar, dimunits)==-1)
				return -1;
		}
					     /* Dimension variable was found, but no character units string */
		else{
			if(dimunits) strcpy(dimunits,"");
		}
		ncopts = saveopts;
		if(dataType) cumapdatatype_nc(nctype, dataType);
	}
	else{
					     /* The dimension variable was not found: */
					     /* return default units and datatype */
		if(dimunits) strcpy(dimunits,"");
		if(dataType) *dataType = CuFloat;
	}

	return CU_SUCCESS;
}
int cudimget_nc(CuFile* file, int dimid, void* values){
	char dimname[MAX_NC_NAME+1];
	float *fp;
	int cdfid;
	int dimids[MAX_VAR_DIMS];
	int dimvarid;
	long dlenbytes;
	int found;
	int ndims;
	int saveopts;
	long i;
	long length;
	long start;
	char varname[MAX_NC_NAME+1];
	nc_type nctype;
	int natts;

	cdfid = file->internid1;
	if(ncdiminq(cdfid, dimid, dimname, &length)==-1){
		return -1;
	}

					     /* Inquire a variable with */
					     /* - the same name as dimname, */
					     /* - a single dimension, and */
					     /* - a (single) dimension id which equals dimid */
	saveopts = ncopts;
	ncopts = 0;
	if((dimvarid = ncvarid(cdfid, dimname)) != -1){
		ncopts = saveopts;
		if(ncvarinq(cdfid, dimvarid, varname, &nctype, &ndims, dimids, &natts)==-1){
			return -1;
		}
		found = (ndims == 1 && dimids[0]==dimid);
	}
	else
		found = 0;
	ncopts = saveopts;

					     /* If the dimension variable was found, read it */
	if(found){
		dlenbytes = length * nctypelen(nctype);
		                             /* Read non-record dimensions directly */
		if(dimid != file->recdim){
			start = 0;
			if(values && ncvarget(cdfid, dimvarid, &start, &length, values)==-1)
				return -1;
		}
		                             /* Copy the cached record dimension */
		else if(file->recdimcache != (void *)0){
			memcpy(values, file->recdimcache, dlenbytes);
		}
		                             /* Read and save the record dimension */
		else{
			start = 0;
			if(values && ncvarget(cdfid, dimvarid, &start, &length, values)==-1)
				return -1;
			if((file->recdimcache = malloc(dlenbytes))==(void *)0){	/* Freed in cuclose_nc */
				CuError(CU_SERROR,"Allocating %d bytes for record dimension cache, file %s, dimension %s", dlenbytes, file->controlpath, dimname);
				return -1;
			}
			memcpy(file->recdimcache, values, dlenbytes);
		}
	}
	else{
					     /* Otherwise assign the default dimension */
		if(values){
			for(i=0, fp=(float*)values; i<length; i++){
				*fp++ = (float)i;
			}
		}
	}
	return CU_SUCCESS;
}
int cuvarid_nc(CuFile* file, const char* name){
	int saveopts;
	int varid;
	
	saveopts = ncopts;
	ncopts = 0;
	varid = ncvarid(file->internid1,name);
	ncopts = saveopts;
	return varid;
}
int cuvarinq_nc(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts){
	int err;
	nc_type dtype;
	char t_name[MAX_NC_NAME+1];
	int t_ndims, t_natts;
	int t_dimids[MAX_VAR_DIMS];

	if((err=ncvarinq(file->internid1,varid,(name ? name : t_name),&dtype,
			 (ndims ? ndims : &t_ndims),
			 (dimids ? dimids : t_dimids),
			 (natts ? natts : &t_natts))) != -1)
		if(datatype) cumapdatatype_nc(dtype,datatype);
	return (err==-1 ? -1 : CU_SUCCESS);
}
int cuvarget_nc(CuFile* file, int varid, const long start[], const long count[], void* values){
	return (ncvarget(file->internid1, varid, start, count, values) == -1 ? -1 : CU_SUCCESS);
}
int cuattinq_nc(CuFile* file, int varid, const char* name, CuType* datatype, int* len){
	int err, saveopts;
	nc_type dtype;
	int t_len;

	saveopts = ncopts;
	ncopts = 0;
	if((err = ncattinq(file->internid1, varid, name, &dtype,
			   (len ? len : &t_len))) != -1)
		if(datatype) cumapdatatype_nc(dtype, datatype);
	ncopts = saveopts;
	return (err == -1 ? -1 : CU_SUCCESS);
}
int cuattget_nc(CuFile* file, int varid, const char* name, void* value){
    CuType dtype;
    int t_len, err;
    if (err=cuattinq_nc(file, varid, name, &dtype, &t_len)!=CU_SUCCESS)
	return err;
    if (err=cugetattany_nc(file, varid, name, dtype, value)!=CU_SUCCESS)
	return err;
    else
	return CU_SUCCESS;
}
int cuattname_nc(CuFile* file, int varid, int attnum, char* name){
	return (ncattname(file->internid1,varid,attnum,name) == -1 ? -1 : CU_SUCCESS);
}
void cuseterropts_nc(int erropts){
	ncopts = ((erropts & CU_VERBOSE) ? NC_VERBOSE : 0) |
		 ((erropts & CU_FATAL) ? NC_FATAL : 0);
	return;
}
void cumapdatatype_nc(nc_type nctype, CuType* cutype){
	if(cutype==(CuType*)0)
		return;
	switch (nctype){
	  case NC_BYTE:
		*cutype = CuByte;
		break;
	  case NC_CHAR:
		*cutype = CuChar;
		break;
	  case NC_SHORT:
		*cutype = CuShort;
		break;
	  case NC_INT:
	        *cutype = CuInt;
		break;
	  case NC_FLOAT:
		*cutype = CuFloat;
		break;
	  case NC_DOUBLE:
		*cutype = CuDouble;
		break;
	  default:
		CuError(CU_DRIVER,"Unrecognized netCDF type %d",(int)nctype);
		break;
	}
	return;
}

int cugetattany_nc(CuFile* file, int varid, const char *name, CuType xtype, void *data)
{
    int ncid;
    int result;

    ncid = file->internid1;
    switch (xtype) {
    case CuByte:
	result = nc_get_att_uchar(ncid, varid, name, (unsigned char *)data);
	break;
    case CuChar:
	result = nc_get_att_text(ncid, varid, name, (char *)data);
	break;
    case CuShort:
	result = nc_get_att_short(ncid, varid, name, (short *)data);
	break;
    case CuInt:
	result = nc_get_att_int(ncid, varid, name, (int *)data);
	break;
    case CuLong:
	result = nc_get_att_long(ncid, varid, name, (long *)data);
	break;
    case CuFloat:
	result = nc_get_att_float(ncid, varid, name, (float *)data);
	break;
    case CuDouble:
	result = nc_get_att_double(ncid, varid, name, (double *)data);
	break;
    default:
	result =  NC_EBADTYPE;
    }
    if (result==NC_NOERR)
	return CU_SUCCESS;
    else {
	CuError(CU_SERROR, "Netcdf error for attribute %s: %s", name, nc_strerror(result));
	return -1;
    }
}
#endif
