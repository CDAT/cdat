/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */
/* -*-Mode: C;-*-
 * Module:      cdunif PP driver functions
 *
 * Copyright:	2004, CCLRC
 *
 * Author:      British Atmosphere Data Centre
 *              badc@badc.rl.ac.uk
 *
 * Revision History:
 */

#ifdef HAVE_PP

#include "cdunifpp.h"

int cuopenread_pp(const char* controlpath, const char* datapath){
  CuFile *file;
  PPfile *ppfile;

  pp_errorhandle_init();

  if (pp_check_sizes()!=0) goto err_no_close;
  
  if((   file = pp_create_file(controlpath)   )==NULL) goto err_no_close;
  ppfile = file->internp;  

  /* open file */
  if((   ppfile->fh=fopen(controlpath,"r")   )==NULL) goto err_no_close;

  /* get file type */
  if(   pp_determine_file_type(ppfile,controlpath,1)   !=0) goto err;

  /* read all the PP headers */
  if(   pp_read_all_headers(file)   !=0) goto err;

  /* do the processing to sort out vars and dims */
  if(   pp_process(file)   !=0) goto err;

  return file->id;
  
 err:
  cuclose_pp(file);
 err_no_close:
  return -1;
}

/*---------------------------------------------------------*/

int cuclose_pp(CuFile* file){
  PPfile *ppfile;

  pp_errorhandle_init();

  if (file != NULL){
    ppfile=file->internp;
    if (ppfile != NULL && ppfile->fh != NULL) {
      fclose(ppfile->fh);
    }
    pp_delete_file(file);
  }

  return CU_SUCCESS;
}

/*---------------------------------------------------------*/

int cuvarget_pp(CuFile* file, int varid, const long start[], const long count[], void* values){
  CuVar *var;
  PPvar *ppvar;

  pp_errorhandle_init();

  if (file == NULL || values == NULL) return CU_EINVAL;
  if (file->vars == NULL) return CU_ENOVARS;
  if (varid < 0 || varid >= file->nvars) return CU_ENOTVAR;
  var=&file->vars[varid];
  ppvar=var->internp;
  if (ppvar == NULL) return CU_EINTERN;
  
  if (ppvar->data != NULL) {
    /* coord var */
    return pp_data_copy(file,var,start,count,values);
  }
  else {
    /* field var */
    return pp_data_read(file,var,start,count,values);
  }
}

/*---------------------------------------------------------*/

int cudimget_pp(CuFile* file, int dimid, void* values){
  CuDim *dim;
  CuVar *var;
  int length,i;
  long start[1];
  long count[1];

  pp_errorhandle_init();

  if (file == NULL || values == NULL) return CU_EINVAL;
  if (dimid < 0 || dimid >= file->ndims) return CU_EBADDIM;
  if (file->dims == NULL) return CU_SERROR;
  
  dim=&file->dims[dimid];

  var=dim->coord;
  length=dim->len;

  if (var==NULL) {

    /* copy default dim according to type */
    if (dim->datatype==inttype)
      for (i=0; i<length; i++)
	*((Fint *)values + i) = (Fint) i;

    else if (dim->datatype==realtype)
      for (i=0; i<length; i++)
	*((Freal *)values + i) = (Freal) i;

    else {
      pp_switch_bug("cudimget_pp");
      return CU_SERROR;
    }
  }
  else {
    start[0]=0;    
    count[0]=length;    
    return cuvarget_pp(file, var->id, start, count, values);
  }

  return CU_SUCCESS;
}

#endif
