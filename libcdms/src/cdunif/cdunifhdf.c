/* -*-Mode: C;-*-
 * Module:      cdunif HDF driver functions
 *
 * Copyright:	1994, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Dean N. Williams, Lawrence Livermore National Laboratory
 *              williams@pcmdi.llnl.gov
 *
 * Version:     cdunifnc.c,v 1.4 1996/06/12 22:35:55 williams Exp
 *
 * Revision History:
 *
 * cdunifnc.c,v
 * Revision 1.4  1996/06/19  22:35:55  williams
 * - Added HDF 
 *
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

/*
  The mapping of index model from cdunif to HDF is a follows:

  - cdunif varid corresponds to HDF sds_index, an integer (0..nvars-1)

  - cdunif dimid corresponds to HDF dimid, but HDF dimids are not numbered
    0..ndims-1, as are cdunif dimids. The mapping is done by cudimid2hdf.
    The assumption is that HDF dimids are consecutive integers in the
    range minid..maxid.

    min HDF dimension ID is stored in file->internid2.
    number of HDF dimensions is stored in file->ndims.
    All HDF dimensions are treated as global, that is, they can be
    shared by variables.

    The SD interface ID returned by SDstart is stored in file->internid1.

*/

#ifdef hdf
#include "cdunifint.h"
#include "hdf.h"			     /* Should follow cdunifint.h to */
					     /* get the HDF defs for MAX_NC_XXX */

					     /* Map cdunif dimid (0 .. file->ndims-1) to HDF dimid */
int32 cudimid2hdf(CuFile* file, int dimid){

	if(dimid<0 || dimid>=file->ndims){
		CuError(CU_EBADDIM,"File %s, dimension ID %d",file->controlpath, dimid);
		return -1;
	}

	return (int32)(file->internid2 + dimid);
}


/* Open and read the HDF file. Return the file ID if successful, or
 * report error and send back failure status (-1).
 */
int cuopenread_hdf(const char* controlpath, const char* datapath){
	CuFile* file;
	int32 cdfid;
	int nvars=0, natts=0;
	int varid, ndims, dimidx, maxdim, mindim;
	int32 dimid, sds_id;
	CuDim *dim;

        /* Open the HDF file. DFACC_RDONLY is defined in hdf.h. */
        if ((cdfid=SDstart(controlpath, DFACC_RDONLY))==-1) {
		CuError(CU_EOPEN,"Opening HDF file %s",controlpath);
                cuerrorreport_hdf();
		return -1;
	}
					     /* Make sure there is a scientific dataset */
	SDfileinfo(cdfid, &nvars, &natts);
	if (nvars==0 && natts==0){
		CuError(CU_EOPEN,"HDF file %s does not contain any scientific datasets",controlpath);
                cuerrorreport_hdf();
		return -1;
	}

	if((file = CuCreateFile(CuHdf))==(CuFile*)0){
		return -1;
	}

	/* Set the file path and file ID */
	strncpy(file->controlpath,controlpath,CU_MAX_PATH);
	file->internid1 = cdfid;

					     /* Set up a mapping of cdunif dimid to HDF dimid: */
					     /* Go through all the dimensions, find min and max HDF_dimid. */
					     /* ASSUME THAT DIMENSION IDS ARE CONSECUTIVE NONNEGATIVE NUMBERS. */
					     /* Store base dimid in file->internid2. */
					     /* The mapping is 0 --> mindim, 1 --> mindim+1, etc. */

	mindim = maxdim = -1;
	for (varid=0; varid<nvars; varid++){
		if (cuvarinq_hdf(file, varid, NULL, NULL, &ndims, NULL, NULL))
			return -1;
		if ((sds_id=SDselect(file->internid1, varid)) == -1) {
			cuerrorreport_hdf();
			return -1;
		}
	        for (dimidx=0; dimidx<ndims; dimidx++){
			if((dimid=SDgetdimid(sds_id, dimidx))==-1){
				cuerrorreport_hdf();
				return -1;
			}
			if (maxdim==-1){
				mindim = maxdim = dimid;
			}
			else{
				mindim = MIN(mindim, dimid);
				maxdim = MAX(maxdim, dimid);
			}
		}
	}

	file->internid2 = mindim;
	file->ndims = maxdim-mindim+1;

	/* Return file ID */
	return file->id;
}

/* Close the file. Return success (0) if the file was close sucessfully,
 * or report error and send back failure status (-1).
 */
int cuclose_hdf(CuFile* file){

	/* Dispose of the file identifier to close the file. */
        if (SDend(file->internid1) == -1 ) {
                CuError(CU_EOPEN,"Closing HDF file %s",file->controlpath);
                cuerrorreport_hdf();
                return -1;
        }

	/* Return success ( 0 ). */
	return CU_SUCCESS;
}

/* Obtain information about the specific HDF file. 
 *
 */
int cuinquire_hdf(CuFile* file, int* ngdims, int* nvars, int* natts, int* recdim){
	int t_ngdims, t_nvars, t_natts, t_recdim;
	int dimidx;
	int32 dimid, datatype, ndattrs;
	long len;

 	/* Determine the contents of the file. */
        if (SDfileinfo(file->internid1, 
		   (nvars ? nvars : &t_nvars), /* # of data sets in file */
                   (natts ? natts : &t_natts)) /* # of global attr. in file */
                    == -1 ) {
                CuError(CU_DRIVER,"Determining the contents of the HDF file %s", file->controlpath);
                cuerrorreport_hdf();
                return -1;
        }

	if (ngdims) *ngdims = file->ndims;
	
					     /* Look for an unlimited dimension */
	if (recdim) {
		*recdim = -1;
		for (dimidx=0; dimidx<file->ndims; dimidx++){
			if((dimid=cudimid2hdf(file, dimidx))==-1)
				return -1;
			if(SDdiminfo(dimid, NULL, &len, &datatype, &ndattrs)==-1){
				cuerrorreport_hdf();
				return -1;
			}
			if (len==0)
				*recdim = dimidx;
		}
	}

	/* Return success ( 0 ). */
	return CU_SUCCESS;
}

/* Given the file ID, variable ID, and name of the dimension, return
 * the dimension ID.
 */
int cudimid_hdf(CuFile* file, int varid, const char* name){
	int32   sds_idx, sds_id;

	/* Search for the index of the named array data set. */
	if ((sds_idx=SDnametoindex(file->internid1, name)) == -1) {
	   CuError(CU_DRIVER,"Obtaining dataset in file %s",file->controlpath);
           cuerrorreport_hdf();
           return -1;
        }

	/* Select the data set corresponding to the returned index. */
        if ((sds_id=SDselect(file->internid1, sds_idx)) == -1) {
	   CuError(CU_DRIVER,"Obtaining dataset in file %s",file->controlpath);
           cuerrorreport_hdf();
           return -1;
        }

	/* Return dimension ID or failure ( -1 ). */
        return (SDgetdimid(sds_id, 0) - file->internid2); /* pass back the dimension id */
}

/* Get information about the dimension. Return success (0) if the 
 * dimension information  was obtained sucessfully, otherwise this 
 * function returns the failure status (-1).
 */
int cudiminq_hdf(CuFile* file, int dimidx, char* dimname, char* dimunits, CuType* dataType, CuDimType* dimtype, int* varid, long* length){
	char dname[H4_MAX_NC_NAME+1];
	int cdfid;
	int dimvarid; /* HDF ID of variable associated with this dimension. */
	int found;   /* True iff a dimension variable was found. */
	int ndims;
	int get_dimid;
	int saveopts;
	long len;
	hdf_type hdftype, hdfunitstype;
	int natts;
	char varname[H4_MAX_NC_NAME+1];
	int attlen;

	int32 sds_id, attr_index, datatype, nattrs;
	int32 dim_sizes[H4_MAX_VAR_DIMS];
	char attr_name[H4_MAX_NC_NAME];
	int32 dimid;

	cdfid = file->internid1;
	if((dimid = cudimid2hdf(file, dimidx))==-1)
		return -1;

	/* Get information about the selected dimension. */
	if(SDdiminfo(dimid, dname, &len, &datatype, &nattrs)==-1){
           	cuerrorreport_hdf();
                return -1;
        }

	if(dimname) strncpy(dimname,dname,CU_MAX_NAME);
	if(length) *length = len;

        /* HDF dimensions are always global */
	if(varid) *varid = CU_GLOBAL;
	if(dimtype) *dimtype = CuGlobalDim;

        /* Inquire a variable with */
        /* - the same name as dimname, */
        /* - a single dimension, and */
        /* - a dimension name which equals the variable name. */
	if((dimvarid = SDnametoindex(cdfid, dname)) != -1){
                sds_id = SDselect(cdfid, dimvarid);
                if (SDgetinfo(sds_id, varname, &ndims, dim_sizes,
                                   &hdftype, &natts) == -1){
           	        cuerrorreport_hdf();
			return -1;
		}

                /* pass back the dimension id */
                if ((get_dimid = SDgetdimid(sds_id, 0)) == -1){
           	        cuerrorreport_hdf();
                        return -1;
                }

		found = (ndims == 1 && get_dimid == dimid);
	}
	else
		found = 0;

	/* If dimension variable was found, */
	/* inquire the units attribute (if any) */
	if(found){
                sds_id = SDselect(cdfid, dimvarid);

		/* Set the length of an unlimited dimension. */
		if (len==0 && length) *length = dim_sizes[0];

        	/* Find the data set attribute name index. */
        	attr_index = SDfindattr(sds_id, "units");

        	/* Get information about the data set attribute. */
        	if(SDattrinfo(sds_id, attr_index, attr_name, &hdfunitstype, 
                    &attlen) != -1 && hdfunitstype == DFNT_CHAR) {
			if(dimunits && SDreadattr(sds_id, attr_index, dimunits)==-1)
				return -1;
		}
	    /* Dimension variable was found, but no character units string */
		else{
			if(dimunits) strcpy(dimunits,"");
		}
		if(dataType) {
			cumapdatatype_hdf(hdftype, dataType);
			if (*dataType==CuInvalidType)
				return -1;
		}
	}
	else{
		/* The dimension variable was not found: */
		/* return default units and datatype */
		if(dimunits) strcpy(dimunits,"");
		if(dataType) *dataType = CuFloat;
	}

	/* Return success ( 0 ). */
	return CU_SUCCESS;
}

/* Get dimension coordinates values. Return success (0) if the
 * dimension values were obtained sucessfully, otherwise this
 * function returns the failure status (-1).
 */
int cudimget_hdf(CuFile* file, int dimidx, void* values){
	char dimname[H4_MAX_NC_NAME+1];
	float *fp;
	int cdfid;
	int get_dimid;
	int dimvarid;
	int found;
	int ndims;
	int saveopts;
	long i;
	long length;
	long start;
	char varname[H4_MAX_NC_NAME+1];
	hdf_type hdftype;
	int natts;
	int32 dimid;

        int32 sds_id, datatype, nattrs, attr_index, num_type, count;
        int32 dim_sizes[H4_MAX_VAR_DIMS];
	char attr_name[H4_MAX_NC_NAME];

	cdfid = file->internid1;
	if((dimid = cudimid2hdf(file, dimidx))==-1)
		return -1;

	/* Get information about the selected dimension. */
        if(SDdiminfo(dimid, attr_name, &length, &datatype, &nattrs)==-1){
		return -1;
	}

  	/* Inquire a variable with */
     	/* - the same name as dimname, */
     	/* - a single dimension, and */
     	/* - a (single) dimension id which equals dimid */
        if((dimvarid = SDnametoindex(cdfid, attr_name)) != -1){
                sds_id = SDselect(cdfid, dimvarid);
                if (SDgetinfo(sds_id, varname, &ndims, dim_sizes,
                                   &hdftype, &natts) == -1){
           	        cuerrorreport_hdf();
			return -1;
		}

                /* pass back the dimension id */
                if ((get_dimid = SDgetdimid(sds_id, 0)) == -1){
           	        cuerrorreport_hdf();
                        return -1;
                }

		found = (ndims == 1 && get_dimid == dimid);
	}
	else
		found = 0;

        /* If the dimension variable was found, read it */
	if(found){
		start = 0;
		if(values && SDgetdimscale(dimid, (VOIDP) values)==-1)
			return -1;
	}
	else{
		/* Otherwise assign the default dimension */
		if(values){
			for(i=0, fp=(float*)values; i<length; i++){
				*fp++ = (float)i;
			}
		}
	}

	/* Return success ( 0 ). */
	return CU_SUCCESS;
}

/* Given the file ID and variable name, return the variable id.
 * Return success (0) if the variable ID was obtained sucessfully;
 * otherwise this function returns the failure status (-1).
 */
int cuvarid_hdf(CuFile* file, const char* name){
	int saveopts;
	int varid;
	
	/* The the index of the variable array data set. */
	varid = SDnametoindex(file->internid1,name);

	/* Return variable ID, or failure ( -1 ). */
	return varid;
}

/* Get information about the variable. Return success (0) if the
 * variable information was obtained sucessfully, otherwise this
 * function returns the failure status (-1).
 */
int cuvarinq_hdf(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts){
	int err, i;
	hdf_type dtype;
	char t_name[H4_MAX_NC_NAME+1];
	int t_ndims, t_natts, numdims;
	int t_dimids[H4_MAX_VAR_DIMS];
	int32 sds_id, index;
	int32 dim_sizes[H4_MAX_VAR_DIMS];

	/* Select the data set corresponding to the returned index. */
	sds_id = SDselect(file->internid1, varid);

	/* Get the variable information. */
	if ((err=SDgetinfo(sds_id, 
                           (name ? name : t_name), 
                           (ndims ? ndims : &t_ndims),
                           dim_sizes,
                           &dtype, 
                           (natts ? natts : &t_natts))) != -1) {
		if(datatype) {
			cumapdatatype_hdf(dtype,datatype);
			if (*datatype==CuInvalidType)
				return -1;
		}
 
         	/* Retrieve dimension IDs. */
		if (dimids) {
		   numdims = (ndims ? *ndims : t_ndims);
		   for (i=0; i < numdims; ++i) /* reverse the order */
		       dimids[i] = SDgetdimid (sds_id, i) - file->internid2;
		}
	}

	/* Return variable ID, or failure ( -1 ). */
	return (err==-1 ? -1 : CU_SUCCESS);
}

/* Retrieve the variable values. Return success (0) if the
 * variable values were obtained sucessfully, otherwise this
 * function returns the failure status (-1).
 */
int cuvarget_hdf(CuFile* file, int varid, const long start[], const long count[], void* values){

	int32 sds_id, i, ndims;
	int32 *startvalues, *edges, natts;
	int32 dim_sizes[H4_MAX_VAR_DIMS];
	char name[H4_MAX_NC_NAME+1];
	hdf_type dtype;

 	/* Get the identifier for the data set. */
        sds_id = SDselect(file->internid1, varid);

	/* Get the number of dimensions. */
        if (SDgetinfo(sds_id, name, &ndims, dim_sizes, &dtype, &natts) == -1) {
             cuerrorreport_hdf();
             return (-1);
	}

	/* Define dimension size */
        startvalues = (int32 *)malloc((ndims)*sizeof(int32));
        edges       = (int32 *)malloc((ndims)*sizeof(int32));

	for (i = 0; i < ndims; ++i) {
	   startvalues[i] = start[i];
	   edges[i] = count[i];
	}

	/* Read the data array. */
        if (SDreaddata(sds_id, startvalues, NULL, edges, (VOIDP)values) == -1) {
             cuerrorreport_hdf();
             return (-1);
        }

	free ((char *) startvalues);
	free ((char *) edges);

	/* Return success ( 0 ). */
        return (CU_SUCCESS);
}

/* Given the file ID and variable ID, retrieve information about the
 * specified attribute. Return success (0) if the attribute information 
 * was obtained sucessfully, otherwise this function returns the failure 
 * status (-1).
 */
int cuattinq_hdf(CuFile* file, int varid, const char* name, CuType* datatype, int* len){
	int err, saveopts;
	hdf_type dtype;
	int t_len;

	int32 sds_id, attr_index;
	char attr_name[H4_MAX_NC_NAME];

	/* Get the identifier for the first data set or file. */
	if (varid == CU_GLOBAL)
		sds_id = file->internid1;
	else
		sds_id = SDselect(file->internid1, varid);

        /* Find the data set attribute name index. */
        attr_index = SDfindattr(sds_id, name);

        /* Get information about the data set attribute. */
        if((err = SDattrinfo(sds_id, 
                            attr_index, 
                            attr_name, 
                            &dtype, 
                            (len ? len : &t_len))) != -1)
		if(datatype) {
			cumapdatatype_hdf(dtype, datatype);
			if (*datatype==CuInvalidType)
				return -1;
		}

	/* Return success ( 0 ), or failure ( -1 ). */
	return (err == -1 ? -1 : CU_SUCCESS);
}

/* Given the file ID, variable ID, and attribute name, retrieve attribute 
 * values. Return success (0) if the attribute information was obtained 
 * sucessfully, otherwise this function returns the failure status (-1).
 */
int cuattget_hdf(CuFile* file, int varid, const char* name, void* value){
	int32 sds_id, attr_index, num_type, count;
	int32 status;
	int8 *buffer;
	char attr_name[H4_MAX_NC_NAME];

        /* Get the identifier for the data set or file. */
	if (varid == CU_GLOBAL)
		sds_id = file->internid1;
	else
		sds_id = SDselect(file->internid1, varid);

        /* Find the data set attribute name index. */
        attr_index = SDfindattr(sds_id, name);

        /* Get information about the data set attribute. */
        SDattrinfo(sds_id, attr_index, attr_name, &num_type, &count);

        /* Read the attribute data and return success ( 0 ),
 	 * or failure ( -1 ).
	 */
        return (SDreadattr(sds_id, attr_index, value) == -1 ? -1 : CU_SUCCESS);
}

/* Given the file ID and variable ID, get information about the 
 * data set attribute.
 */
int cuattname_hdf(CuFile* file, int varid, int attnum, char* name){
        int32 sds_id, num_type, count, status;
        char attr_name[H4_MAX_NC_NAME];

        /* Get the identifier for the data set or file. */
	if (varid == CU_GLOBAL)
		sds_id = file->internid1;
	else
		sds_id = SDselect(file->internid1, varid);

        /* Get information about the data set attribute. */
        status = SDattrinfo(sds_id, attnum, name, &num_type, &count);

	/* Return success ( 0 ), or failure ( -1 ). */
	return (status == -1 ? -1 : CU_SUCCESS);

}

/* HDF Error Reporting. At the start of opening the HDF file clear
 * the HDF error stack.
 */
void cuseterropts_hdf(int erropts){
	HEclear(); /* Clear the HDF error stack. */
	return;
}

/* HDF Error Reporting. If there is an HDF error, then print all the
 * errors via CuError and clear the HDF error stack.
 */
void cuerrorreport_hdf() {
	int32 i=0, e;
	const char *estr;

	/* Print all errors stored in the error HDF stack. */
	while ((e = HEvalue(i)) != DFE_NONE) {
		estr = HEstring(e);
		CuError(CU_DRIVER,"HDF reported error(s) - (%s)", estr);
		++i;
	}
	HEclear(); /* Clear the HDF error stack. */
}

/* Given the HDF data type, return the cdunif data type. */
void cumapdatatype_hdf(hdf_type hdftype, CuType* cutype){
	if(cutype==(CuType*)0)
		return;
	*cutype = CuInvalidType;
	switch (hdftype){
	  case DFNT_INT8:
	  case DFNT_UINT8:
		*cutype = CuByte;
		break;
	  case DFNT_CHAR: /* Same as DFNT_CHAR8. */
		*cutype = CuChar;
		break;
	  case DFNT_INT16: /* 16-bit integer type. */
		*cutype = CuShort;
		break;
	  case DFNT_INT32: /* 32-bit integer type. */
		*cutype = CuLong;
		break;
	  case DFNT_FLOAT32: /* 32-bit float type. */
		*cutype = CuFloat;
		break;
	  case DFNT_FLOAT64: /* 64-bit float type. */
		*cutype = CuDouble;
		break;
	  default:
		CuError(CU_DRIVER,"Unrecognized HDF type %d",(int)hdftype);
                cuerrorreport_hdf();
		break;
	}
	return;
}
#endif
