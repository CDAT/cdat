/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */
#ifdef HAVE_PP
#include "cdunifpp.h"

/* pp_data_copy and pp_data_read are the back end to cuvarget
 * 
 * pp_data_copy is for copying data already stored from memory (coordinate vars)
 * pp_data_read is for reading data from a file (field vars)
 */

/* pp_data_copy handles the general n-dimensional case for dealing with hyperslabs (coordinate
 * variables are not guaranteed to be 1d, because they may have "nv" dimension from cell_methods -- 
 * okay, maybe no more than 2d, but n-dimensional problem is not particularly more difficult)
 *
 * pp_data_read handles general n-dimensional case in a similar way
 *
 */
  

int pp_data_copy(const CuFile *file, const CuVar *var, const long start[], const long count[], void *values) {

  const PPfile *ppfile;
  PPlist *heaplist;
  const PPvar *ppvar;
  const PPdata *data;
  const void *src;
  int ndim,idim;
  int *indices, *size;
  int srcoffset, destoffset;
  int carryout;
  int is_int, is_real;

  ppfile = file->internp;
  ppvar = var->internp;
  heaplist = ppfile->heaplist;
  data = ppvar->data;

  CKP(   src=data->values    );

  ndim = var->ndims;

  if (ndim > 0) {
    CKP(   size=pp_malloc(ndim*sizeof(int),heaplist)   );
    CKP(   indices=pp_malloc(ndim*sizeof(int),heaplist)   );
  } else {
    size = indices = NULL;
    /* won't use these NULL values - only to suppress compiler warnings about uninitialised */
  }

  for (idim=0; idim<ndim; idim++) {
    size[idim] = file->dims[var->dims[idim]].len;
    indices[idim]=0;

    if (start[idim]<0 || count[idim]<0 || start[idim]+count[idim]>size[idim])
      return CU_EINVALCOORDS;    

  }
  carryout=0;

  is_int = (data->type == inttype);
  is_real = (data->type == realtype);
  ERRIF(!is_int && !is_real);

  while (!carryout) {
    /* indices loop from 0 to count-1 in each dimension: keep looping until
     * carry-out from most slowly varying dimension
     */

    /* locate hyperslab element within source and destination 1d arrays */
    srcoffset=0;
    destoffset=0;
    for (idim=0; idim < ndim ; idim++) {
      srcoffset *= size[idim];      
      destoffset *= count[idim];

      srcoffset += indices[idim]+start[idim];
      destoffset += indices[idim];
    }

    /* copy data */
    if (is_int)
      *((Fint *)values + destoffset) = *((Fint *)src + srcoffset);
    else if (is_real)
      *((Freal *)values + destoffset) = *((Freal *)src + srcoffset);    

    /* increment indices */
    for (idim=ndim-1 ; idim>=0; idim--) {
      indices[idim]++;
      if (indices[idim]==count[idim])
	indices[idim]=0; /* carry, so keep looping */
      else
	break; /* no internal carry */
    }
    if (idim<0)
      carryout=1;
  }
  
  if (ndim > 0) {
    CKI(  pp_free(size,heaplist)  );
    CKI(  pp_free(indices,heaplist)  );
  }

  return CU_SUCCESS;    

  ERRBLK("pp_data_copy",CU_SERROR);
}

int pp_data_read(const CuFile *file, const CuVar *var, const long start[], const long count[], void *values)
{
  const PPfile *ppfile;
  PPlist *heaplist;
  const PPvar *ppvar;
  int startrec, endrec, nrec, recno;
  int cx,cy,sx,sy,iy;
  const void *src;
  void *data, *ptr, *dest;
  int nx,ny, ndim;
  int idim;
  int *indices, *size;
  int carryout;
  int destoffset;

  ndim=var->ndims;

  ERRIF(ndim < 2);
    
  ppfile = file->internp;
  ppvar = var->internp;
  heaplist = ppfile->heaplist;

  startrec = ppvar->firstrecno;
  endrec = ppvar->lastrecno;
  nrec = endrec - startrec + 1;

  CKP(   size=pp_malloc(ndim*sizeof(int),heaplist)   );
  /* last two items in indices aren't actually used but define for completeness */
  CKP(   indices=pp_malloc(ndim*sizeof(int),heaplist)   );

  for (idim=0; idim<ndim; idim++) {
    size[idim] = file->dims[var->dims[idim]].len;
    indices[idim]=0;

    if (start[idim]<0 || count[idim]<0 || start[idim]+count[idim]>size[idim])
      return CU_EINVALCOORDS;    

  }

  /* some constants for use later */
  nx=size[ndim-1];
  ny=size[ndim-2];
  cx=count[ndim-1];
  cy=count[ndim-2];
  sx=start[ndim-1];
  sy=start[ndim-2];
  
  /*
   * JAK this needs moving to pp_process
   *  if (nrec != nz * nt) {
   * CuError(CU_EINVALCOORDS,"refusing to read variable which has missing combinations of z,t");
   * return CU_EINVALCOORDS;
   *}
  */ 
  /* we can now assume that the records loop over correct times and levels
   * (loop over time is the more slowly varying dimension because that's
   * how we sorted them)
   */

  carryout=0;
  while (!carryout) {

    recno=0;
    destoffset=0; 

    for (idim=0; idim < ndim-2 ; idim++) { /* treat inner 2 dim as record dims */
      recno *= size[idim];
      destoffset *= count[idim];

      recno += indices[idim]+start[idim];
      destoffset += indices[idim];
      
    }

    recno=startrec+recno;
    ptr=(char*)values+destoffset*wordsize*cx*cy;

    CKP(   data=pp_read_data_record(ppfile->recs[recno],ppfile,heaplist)   );
    
    for (iy=0; iy<cy; iy++) {
      src = (char*) data + ((sy+iy)*nx + sx) * wordsize;
      dest = (char*) ptr + (iy*cx) * wordsize;	
      memcpy(dest,src,cx*wordsize);
    }

    CKI(  pp_free(data,heaplist)  );
    /* increment indices */
    for (idim=ndim-2-1 ; idim>=0; idim--) {
      indices[idim]++;
      if (indices[idim]==count[idim])
	indices[idim]=0; /* carry, so keep looping */
      else
	break; /* no internal carry */
    }
    if (idim<0)
      carryout=1;
  }
  
  return CU_SUCCESS;

  ERRBLK("pp_data_read",CU_SERROR);
}


#endif
