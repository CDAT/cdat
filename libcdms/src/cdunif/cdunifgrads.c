/* -*-Mode: C;-*-
 * Module:
 *
 * Copyright:	1995, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Mike Fiorino
 *
 * Version:     $Id$
 *
 * Revision History:
 *
 * $Log: cdunifgrads.c,v $
 * Revision 1.1.1.1  1997/12/09 18:57:39  drach
 * Copied from cirrus
 *
 * Revision 1.13  1997/10/24  18:23:45  drach
 * - Cache netCDF unlimited dimensions
 * - Consistent with GrADS src170
 *
 * Revision 1.11  1997/01/28  15:09:28  drach
 * Mike corrected errors in the GrADS interface.
 *
 * Revision 1.9  1996/09/09  18:22:09  drach
 * - Integrated with configuration scripts
 * - (CRAY) added ieee2cray conversion
 * - GrADS/GRIB I/O works on all platforms now
 * - Support for platform-independent gribmap
 *
 * Revision 1.8  1996/02/23  01:12:54  drach
 * - Generate time dimension values as relative time ("units since basetime")
 *
 * Revision 1.7  1996/01/18  17:29:18  drach
 * Changed x,y,z,t to longitude, latitude, level, time
 *
 * Revision 1.6  1995/07/12  22:04:04  drach
 * - fclose open data file if necessary
 * - make data file an absolute pathname, if the control file is absolute
 *   and the data file is a relative pathname.
 *
 * Revision 1.5  1995/06/26  17:45:28  drach
 * Changed default GrADS units to "hour"
 *
 * Revision 1.4  1995/06/09  22:35:54  drach
 * - Made grads error string length consistent
 * - Allow null return parameters for netCDF files
 *
 * Revision 1.3  1995/03/30  00:20:40  drach
 * Allow 99 as a valid lu
 *
 * Revision 1.2  1995/03/24  21:44:44  fiorino
 * GrADS routines
 *
 * Revision 1.1  1994/12/20  23:01:50  drach
 * - Template
 *
 *
 */

#include <stdio.h>
#ifndef __APPLE__
#include <malloc.h>
#endif
#include <math.h>
#include <signal.h>
#include <string.h>
#include "cdmsint.h"
#include "cdunifint.h"
#include "grads.h"

FILE *ofile;

static struct gafile *pfi;
static struct gavar *pvr;
static struct gagrid *pgr;
static struct gaindx *pindx;
static int timerr;
static int msgflg=1;

/* GRIB I/O caching.  GRIB data is chached, as well as the bit
   maps, if present.  Sometimes the expanded bit map is cached. */

static char *cache;           /* I/O cache for GRIB */
static char *bcache;          /* Bit map cache */
static int cflag=0;           /* cache flag */
static int bcflag=0;          /* Bit cache flag */
static int bpsav = -999;      /* Bit cache pointer */
static int bssav = -999;      /* Bit cache size */
static int *bpcach;           /* expanded bit map cache */

/* Station data I/O caching.  We will cache fairly small I/O
   requests that fit within the specified size buffer.  If the buffer
   gets overfilled, we just forget the caching.  */

static int scflg = 0;         /* Anything cached? */
static int scuca = 0;         /* Can use cache for this request */
static int scerr = 0;         /* Buffer full? */
static int scok;              /* Ok to fill buffer */
static int scpnt;             /* Current cache offset */
static struct gastn scstn;    /* Previous request */
static char *scbuf=NULL;      /* Cache */

/* Size of cache */

#define SCNUM 50000

int ngdims;

/* global variables */

static int cpos;

extern int grads_varget(CuFile*, int, int*, int*, void*) ;




/*--------------------------- cuopenread for GrADS -----------------*/


int cuopenread_grads(const char* controlpath, const char* datapath) {

  CuFile* file;			     /* CDMS file struct */
  CuVar* var;			     /* CDMS var struct */
  CuDim* dim;			     /* CDMS dimension struct */
  CuAtt* att;			     /* CDMS attribute struct */
  int dim_ctr;			     /* dimension ID counter */

  char rec[256],  *ch, *dn, *pos, *name;

  double *fp;

  long idmo, idmin;
  int i,j,k,len,rc;
  int nv,size,ioff;
  int verb=0;

  float undefValue;


  /*grads --------    allocate memory for the file struct    grads*/

  pfi=getpfi();
  if (pfi==NULL) {
    gaprnt (0,"Memory Allocation Error: On File Open\n");
    return (1);
  }

  /*grads --------    get the .ctl file name    grads*/

  i=0;
  for(i=0;(*(controlpath+i)!=0);i++) rec[i]=*(controlpath+i);
  name=(char *)malloc(i+1);
  strncpy(name,&rec[0],i);
  *(name+i)='\0';

  /*grads --------    open the grads .ctl file     grads*/

  rc = gaddes(name, pfi, 1);
  free(name);
  if(rc != 0) return -1;

  /*cdunif ------  Create the file struct     cdunif*/

  if((file = CuCreateFile(CuGrads))==(CuFile*)0)
    return -1;

  /* Determine the number of variables, dimensions,
   * and GLOBAL attributes in the file
   * Note: the number of dimensions includes
   * GLOBAL dimensions (as defined with XDEF, YDEF, etc.
   * for GrADS), plus LOCAL dimensions (e.g., proper
   * subsets of Z dimensions for GrADS.)
   * 'ngdims' is the number of global dimensions.
   */

  file->nvars = pfi->vnum;
  file->ndims =  4 ;
  file->ngatts =  1 ;
  ngdims =  4 ;

  /* Fill the file struct */

  strncpy(file->controlpath,controlpath,CU_MAX_PATH);
  file->internid1 =  0 ;
  file->internp =  pfi ;

  /* Create the array of variables for this file */

  if((var = CuCreateVars(file,file->nvars))==(CuVar*)0) ;

  /* Create the array of dimensions for this file */

  if((dim = CuCreateDims(file,file->ndims+file->nvars))==(CuDim*)0) ;

  /* For each GLOBAL dimension (XDEF, YDEF, etc. in GrADS) ...
   *
   * For GrADS:
   * T id = 0
   * Z id = 1
   * Y id = 2
   * X id = 3
   */

					     /* Generate the time units: */
					     /* fp[0] = base year */
					     /*   [1] = base month */
					     /*   [2] = base day */
					     /*   [3] = base hour */
					     /*   [4] = base minute */
					     /*   [5] = delta months */
					     /*   [6] = delta minutes */

  fp = pfi->grvals[3];
  idmo = (long)(fp[5] + 1.e-5);
  idmin = (long)(fp[6] + 1.e-5);
  if(idmo && idmin){
	  CuError(CU_DRIVER,"GrADS time dimension has both year/month and day/hour/minute delta\n");
	  return -1;
  }
  if(idmo){
	  if(idmo % 12 == 0){
		  sprintf(dim->units,"years since %.0f-%.0f-%.0f %.0f:%.0f",fp[0],fp[1],fp[2],fp[3],fp[4]);
		  dim->interval = idmo/12;
	  }
	  else{
		  sprintf(dim->units,"months since %.0f-%.0f-%.0f %.0f:%.0f",fp[0],fp[1],fp[2],fp[3],fp[4]);
		  dim->interval = fp[5];
	  }
  }
  else if(idmin){
	  if(idmin % 1440 == 0){
		  sprintf(dim->units,"days since %.0f-%.0f-%.0f %.0f:%.0f",fp[0],fp[1],fp[2],fp[3],fp[4]);
		  dim->interval = idmin/1440;
	  }
	  else if(idmin % 60 == 0){
		  sprintf(dim->units,"hours since %.0f-%.0f-%.0f %.0f:%.0f",fp[0],fp[1],fp[2],fp[3],fp[4]);
		  dim->interval = idmin/60;
	  }
	  else {
		  sprintf(dim->units,"minutes since %.0f-%.0f-%.0f %.0f:%.0f",fp[0],fp[1],fp[2],fp[3],fp[4]);
		  dim->interval = fp[6];
	  }
  }

  strcpy(dim->name, "time");
  dim->datatype =  CuDouble ;
  dim->dimtype = CuGlobalDim;
  dim->coord = (CuVar*)0;		     /* No associated coordinate variable */
  dim->len =  pfi->dnum[3] ;
  dim++;

  strcpy(dim->name, "level");
  strncpy(dim->units, "lev" , CU_MAX_NAME);
  dim->datatype =  CuFloat ;
  dim->dimtype = CuGlobalDim;
  dim->coord = (CuVar*)0;		     /* No associated coordinate variable */
  dim->len = pfi->dnum[2] ;
  dim++;

  strcpy(dim->name, "latitude");
  strncpy(dim->units, "degrees_north" , CU_MAX_NAME);
  dim->datatype =  CuFloat ;
  dim->dimtype = CuGlobalDim;
  dim->coord = (CuVar*)0;		     /* No associated coordinate variable */
  dim->len =  pfi->dnum[1] ;
  dim++;

  strcpy(dim->name, "longitude");
  strncpy(dim->units,  "degrees_east" , CU_MAX_NAME);
  dim->datatype =  CuFloat ;
  dim->dimtype = CuGlobalDim;
  dim->coord = (CuVar*)0;		     /* No associated coordinate variable */
  dim->len = pfi->dnum[0] ;
  dim++;

  dim_ctr = file->ndims-1;


  /* Create the global attribute array */

  if((att = CuCreateAtts(file,(CuVar*)0,file->ngatts))==(CuAtt*)0) ;


  /* Set the global attributes, IDs = 0..file->ngatts-1
   * 0 = format (GRADS, GRIB, DRS, whatever ...)
   *
   * Attributes can have ANY cdunif datatype (not just CuChar)
   */

  if(CuSetAtt(file,(CuVar*)0,0,"format",CuChar,6,"GRADS") != CU_SUCCESS) ;

  /* For each variable ... */

  pvr=pfi->pvar1;

  for(i=0; i<file->nvars; i++){

    strncpy(var->name,  pvr->abbrv , CU_MAX_NAME);
    var->datatype = CuFloat ;

    /* Determine the number of dimensions and attributes of this variable
     * GrADS surface variables have three dimensions, others have four.
     */

    if(pvr->levels == 0) {
      var->ndims = 3 ;
    } else {
      var->ndims = 4 ;
    }

    var->natts = 2 ;

    /* Create the variable attribute array */

    if((att = CuCreateAtts(file,var,var->natts))==(CuAtt*)0) ;

    /* Set the variable attributes; use IDs 0..var->natts-1
     * Attributes can have ANY datatype (not just CuChar). */

    if(CuSetAtt(file,var,0,"title",CuChar, strlen(pvr->varnm)+1,pvr->varnm) != CU_SUCCESS);
    undefValue = (float)pfi->undef;
    CuSetAtt(file,var,1,"missing_value",CuFloat,1,(void *)&undefValue);

    /* Set dimension info for the variable.
     * For GrADS, first two dimensions are always x and y.
     * NOTE! C-majority is canonical dimension order:
     * LAST DIMENSION VARIES MOST RAPIDLY!
     *
     * Note: this assumes that X always varies most rapidly,
     * then Y, Z, T. This may need to be generalized.
     *
     * For GrADS:
     * T id = 0
     * Z id = 1
     * Y id = 2
     * X id = 3
     */

    if(pvr->levels == 0){
      var->ndims = 3;
      var->dims[0] = 0;
      var->dims[1] = 2;
      var->dims[2] = 3;
    } else {
      var->ndims = 4;
      var->dims[0] = 0;
      var->dims[2] = 2;
      var->dims[3] = 3;
      if(pvr->levels == pfi->dnum[2]){
	var->dims[1] = 1;
      }	else {
	strcpy(dim->name,"level");
	strncpy(dim->units, "lev" ,CU_MAX_NAME);
	dim->var = var;
        dim->coord = (CuVar*)0;
	dim->len =  pvr->levels ;
	dim->datatype = CuFloat ;
	dim->dimtype = CuLocalDim;
	var->dims[1] = ++dim_ctr;
	file->ndims=dim_ctr+1;
	dim++;
      }
    }

    var++;
    pvr++;

  }

  /* Successful return */

  return file->id;

}


int cuclose_grads(CuFile* file) {

  struct gafile *pfi;

  pfi=file->internp;
  if(pfi->infile != NULL){
	  fclose(pfi->infile);
	  pfi->infile = NULL;
  }
                                           /* Free a gafile structure, associated storage */
  frepfi(pfi,0);
  return CU_SUCCESS;

}

/*--------------------------- dimget for GrADS -----------------*/

int cudimget_grads(CuFile* file, int dimid, void* value){

  struct gafile *pfi;

  CuDim* dim;

  double* dp;
  double delta, prev;
  float* fp;
  float cyr,cda,cmo,chr,dtmo,dtmn;
  int i;

  pfi=file->internp ;

  if((dim=CuLookupDim(file,dimid))==(CuDim*)0) return -1;

  switch(dimid){
  case 0:

/*----  l -> t (time) map -----*/

    dp=value;
// fp is not actually used     fp=pfi->grvals[3];

    delta = dim->interval;
    *dp++ = prev = 0.0;
    for(i=1;i<dim->len;i++)
	    *dp++ = prev += delta;
    break;

/*----  k -> z (lev) map -----*/

  case 1:
    fp=value;
    if(pfi->linear[2] == 0)  {
      for(i=0;i<dim->len;i++) {
	fp[i]=*(pfi->abvals[2]+i+1);
      }
    } else {
      for(i=0;i<dim->len;i++) {
        fp[i] = ( *pfi->grvals[2] * (float)(i+1) ) + *(pfi->grvals[2]+1) ;
      }
    }
    break;

/*----  j -> y (lat) map -----*/

  case 2:
    fp=value;
    if(pfi->linear[1] == 0)  {
      for(i=0;i<dim->len;i++) {
	fp[i]=*(pfi->abvals[1]+i+1);
      }
    } else {
      for(i=0;i<dim->len;i++) {
        fp[i] = ( *pfi->grvals[1] * (float)(i+1) ) + *(pfi->grvals[1]+1) ;
      }
    }
    break;


/*----  i -> x (lon) map -----*/

  case 3:

    fp=value;
    if(pfi->linear[0] == 0)  {
      for(i=0;i<dim->len;i++) {
	fp[i]=*(pfi->abvals[0]+i+1);
      }
    } else {
      for(i=0;i<dim->len;i++) {
        fp[i] = ( *pfi->grvals[0] * (float)(i+1) ) + *(pfi->grvals[0]+1) ;
      }
    }
    break;

  default:
    /* Lookup dimension struct */
    if((dim=CuLookupDim(file,dimid))==(CuDim*)0) return -1;

    fp=value;
    if(pfi->linear[2] == 0)  {
      for(i=0;i<dim->len;i++) {
	fp[i]=*(pfi->abvals[2]+i+1);
      }
    } else {
      for(i=0;i<dim->len;i++) {
        fp[i] = ( *pfi->grvals[2] * (float)(i+1) ) + *(pfi->grvals[2]+1) ;
      }
    }

  }

  return CU_SUCCESS;
}


int cuvarget_grads(CuFile* file, int varid, const long start[], const long count[], void* value){

  CuVar* var;
  CuDim* dim;

  struct gagrid *pgr;
  struct gafile *pfi;
  struct gavar *pvr;

  int ib[4],ie[4],jb[4],je[4];
  int dimlen[4];
  int i,j,k,ii,siz;
  int rc,len,verb=0;


  cpos = 0;

  /* Lookup the variable */

  if(verb) printf("qqq read for var lookup %d %d %d\n",file->ndims,file->nvars,varid);
  if((var = CuLookupVar(file,varid)) == (CuVar*)0) {
    printf("qqq failure\n");
    return -1;
  }


  if(verb) {
    printf("qqq varget %d %s %d\n",var->id,var->name,var->ndims);
    for(i=0;i<var->ndims;i++) printf("%d %d %d\n",i,start[i],count[i]);
  }

/* set up the dimension environment for GrADS I/0 */

  for(i=0;i<4;i++) { ib[i]=1; ie[i]=1; je[i]=1; jb[i]=1; dimlen[i]=1;}

  for(i=0;i<var->ndims;i++) {
    ii=var->dims[i];
    if(ii>3) ii=1;
    ib[3-ii]=start[i]+1;
    ie[3-ii]=ib[3-ii]+count[i]-1;
    if((dim=CuLookupDim(file,var->dims[i]))==(CuDim*)0) return -1;
    dimlen[3-ii]=dim->len;
  }

/* bounds check */

  for(i=0;i<4;i++) {
    if( (ib[i]<1) || (ie[i]>dimlen[i]) ) {
      CuError(CU_EBADDIM,"BOUND ERROR in cuvarget_grads dim = %d ib= %d ie= %d maxlen= %d\n",i,ib[i],ie[i],dimlen[i] ) ;
      return -1 ;
    }
  }

  if(ib[3]<ie[3]) {
    for(i=ib[3];i<=ie[3];i++) {
      jb[3]=i;
      je[3]=i;
      if (ib[2]<ie[2]) {
	for(j=ib[2];j<=ie[2];j++) {
	  jb[2]=j;
	  je[2]=j;
	  if(verb) printf("********* 4-D, x,y,z,t \n");
	  for(k=0;k<2;k++) { jb[k]=ib[k]; je[k]=ie[k]; }
	  rc=grads_varget(file,varid,jb,je,value);
	}
      } else {
	if(verb) printf("********** 0-D or 3-D, x,y,t \n");
	for(k=0;k<3;k++) { jb[k]=ib[k]; je[k]=ie[k]; }
	rc=grads_varget(file,varid,jb,je,value);
      }
    }
  } else if(ib[2]<ie[2]) {
    if(verb) printf("********** 3-D, x,y,z \n");
    for(j=ib[2];j<=ie[2];j++) {
      for(k=0;k<4;k++) { jb[k]=ib[k]; je[k]=ie[k]; }
      jb[2]=j;
      je[2]=j;
      rc=grads_varget(file,varid,jb,je,value);
    }
  } else {
    if(verb) printf("******** 0-D to 2-D xy \n");
    rc=grads_varget(file,varid,ib,ie,value);
  }

  return CU_SUCCESS;

}

int grads_varget( CuFile* file, int varid, int* ib, int* ie, void* value) {

  struct gagrid *pgr;
  struct gafile *pfi;
  struct gavar *pvr;
  float* fp;
  int i,siz;
  int rc,len,verb=0;
  char tempname[4096];

  extern int cuIsAbsolute(char *path, int len);

/* point to the value array */

  fp=value;

  if(verb) {
    for(i=0;i<5;i++) {
      printf("qqq grads_varge %d %d %d\n",i,*(ib+i),*(ie+i));
    }
  }

  siz = sizeof(struct gagrid);
  pgr = (struct gagrid *)galloc(siz, "varget");
  if (pgr==NULL) {
    gaprnt (0,"Unable to allocate memory for grid structure \n");
    return 0;
  }

  pfi=file->internp;
					     /* Note: template files are opened at */
					     /* a lower level. Only open specific */
					     /* file here. */
  if (pfi->tmplat==0 && pfi->infile==NULL) {
					     /* If the descriptor filename is absolute, and */
					     /* the data file name is not, make the datafile name */
					     /* absolute with the same directory*/
    if(cuIsAbsolute(pfi->dnam,4095) && !cuIsAbsolute(pfi->name,4095)){
					     /* fnmexp removes first character from tempname */
      strncpy(tempname+1,pfi->name,4094);
		  fnmexp(pfi->name,tempname,pfi->dnam);
	  }
    pfi->infile = fopen (pfi->name, "rb");
    if (pfi->infile==NULL) {
      gaprnt (0,"Open Error:  Can't open binary data file\n");
      gaprnt (0,"  File name = ");
      gaprnt (0,pfi->name);
      gaprnt (0,"\n");
      return -1;
    }
  }

  pgr->pfile = pfi;
  pgr->undef = pfi->undef;
  pgr->idim  = 0;
  pgr->jdim  = 1;
  pgr->alocf = 0;
  pgr->toff = 0;

  for(i=0;i<4;i++) {
    pgr->dimmin[i]=ib[i];
    pgr->dimmax[i]=ie[i];
  }
  pgr->dimmin[4]=1;
  pgr->dimmax[4]=1;

  pgr->rmin = 0;
  pgr->rmax = 0;
  pgr->grid = &pgr->rmin;

  pgr->isiz = 0;
  pgr->jsiz = 0;
  pgr->exprsn = NULL;

  pgr->pvar = pfi->pvar1+varid;
  pvr=pgr->pvar;

  rc=gaggrd(pgr);
  len=pgr->isiz*pgr->jsiz;

  if(verb) {
    printf("------gaggrd--------- %d %d %s \n",rc,len,pvr->abbrv);
    for(i=0;i<len;i++) printf("qqq %d %g\n",i,*(pgr->grid+i));
  }

  /* load into output array */

  for(i=0;i<len;i++)
  {
    fp[i+cpos]=(float)(*(pgr->grid+i));
  }

  /* bump cpos */

  cpos=cpos+len;

  gagfre(pgr);

  return -1;

}
					     /* Return true iff path is an absolute file name */
int
cuIsAbsolute(char *path, int len){

	while(*path==' ' && len > 0){
		path++;
		len--;
	}
	while(*path!=' ' && *path!='/' && *path !='\0' && len>0){
		path++;
		len--;
	}
	return (*path=='/');
}
