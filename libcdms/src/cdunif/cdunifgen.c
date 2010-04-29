/* -*-Mode: C;-*-
 * Module:      cdunif generic driver functions
 * 
 *              These functions can be used for any format that builds
 *              the proper data structures at file open.
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
 * $Log: cdunifgen.c,v $
 * Revision 1.4  1995/07/31  17:41:20  drach
 * - return correct varid in cudiminq
 *
 * Revision 1.3  1995/01/18  02:52:14  drach
 * - Made seterropts a dispatch function
 *
 * Revision 1.2  1995/01/13  01:01:37  drach
 * - Failure to find a variable name does not generate an error message,
 *   but still generates an error return value.
 *
 * Revision 1.1  1994/11/17  19:58:41  drach
 * Initial CVS version
 *
 *
 */

#include <string.h>
#include <cdunifint.h>

int cuinquire_gen(CuFile* file, int* ndims, int* nvars, int* natts, int* recdim){
	if(ndims) *ndims = file->ndims;
	if(nvars) *nvars = file->nvars;
	if(natts) *natts = file->ngatts;
	if(recdim) *recdim = file->recdim;

	return CU_SUCCESS;
}
int cudimid_gen(CuFile* file, int varid, const char* name){
	CuVar* var;
	CuDim* dim;
	int i;

					     /* Global (shared) dimension lookup */
	if(varid == CU_GLOBAL){
		for(i=0, dim=file->dims; i<file->ndims && dim; i++, dim++){
			if((dim->dimtype == CuGlobalDim) &&
			   (!strncmp(name,dim->name,CU_MAX_NAME)))
				return i;
		}
		return -1;
	}
					     /* Local (variable-specific) lookup */
	else {
		if((var = CuLookupVar(file,varid))==(CuVar*)0)
			return -1;
		for(i=0; i<var->ndims; i++)
			if(!strncmp(name,file->dims[var->dims[i]].name,CU_MAX_NAME))
				return var->dims[i];
		return -1;
	}
}
int cudiminq_gen(CuFile* file, int dimid, char* dimname, char* dimunits, CuType* datatype, CuDimType* dimtype, int* varid, long* length){
	CuDim* dim;

	if((dim=CuLookupDim(file,dimid))==(CuDim*)0)
		return -1;

	if(dimname) strncpy(dimname,dim->name,CU_MAX_NAME);
	if(dimunits) strncpy(dimunits,dim->units,CU_MAX_NAME);
	if(datatype) *datatype=dim->datatype;
	if(dimtype) *dimtype=dim->dimtype;
	if(varid) *varid = (dim->dimtype == CuLocalDim ? dim->var->id : CU_GLOBAL);
	if(length) *length = dim->len;

	return CU_SUCCESS;
}
int cuvarid_gen(CuFile* file, const char* name){
	CuVar* var;
	int i;

	for(i=0, var=file->vars; i<file->nvars && var; i++, var++){
		if(!strncmp(name,var->name,CU_MAX_NAME))
			return i;
	}

/*	CuError(CU_ENOTVAR,"Variable not found: %s, in file %s",name,file->controlpath);
*/
	return -1;
}
int cuvarinq_gen(CuFile* file, int varid, char* name, CuType* datatype, int* ndims, int dimids[], int* natts){
	CuVar* var;
	int i;
	
	if((var = CuLookupVar(file,varid))==(CuVar*)0)
		return -1;

	if(name) strcpy(name,var->name);
	if(datatype) *datatype = var->datatype;
	if(ndims) *ndims = var->ndims;
	if(dimids)
		for(i=0; i<var->ndims; i++)
			dimids[i]=var->dims[i];
	if(natts) *natts = var->natts;

	return CU_SUCCESS;
}
int cuattinq_gen(CuFile* file, int varid, const char* name, CuType* datatype, int* len){
	CuAtt* att;

	if((att=CuLookupAtt(file,varid,name)) == (CuAtt*)0)
		return -1;
	
	if(datatype) *datatype = att->datatype;
	if(len) *len = att->len;

	return CU_SUCCESS;
}
int cuattget_gen(CuFile* file, int varid, const char* name, void* value){
	CuAtt* att;

	if((att=CuLookupAtt(file,varid,name))==(CuAtt*)0)
		return -1;

	memcpy(value,att->val,(att->len)*cutypelen(att->datatype));
	return CU_SUCCESS;
}
int cuattname_gen(CuFile* file, int varid, int attnum, char* name){
	CuVar* var;
	
					     /* Lookup global attribute */
	if(varid==CU_GLOBAL){
		if(attnum<0 || attnum>=file->ngatts){
			CuError(CU_ENOTATT,"Invalid attribute number %d for file %s.",attnum,file->controlpath);
			return -1;
		}
		if(name) strncpy(name,file->atts[attnum].name,CU_MAX_NAME);
	}
					     /* Lookup local attribute */
	else{
		if((var=CuLookupVar(file,varid))==(CuVar*)0)
			return -1;
		if(attnum<0 || attnum>=var->natts){
			CuError(CU_ENOTATT,"Invalid attribute number %d for file %s, var %s.",attnum,file->controlpath,var->name);
			return -1;
		}
		if(name) strncpy(name,var->atts[attnum].name,CU_MAX_NAME);
	}
	return CU_SUCCESS;
}
					     /* Generic seterropts is a no-op */
void cuseterropts_gen(int erropts){
	return;
}
