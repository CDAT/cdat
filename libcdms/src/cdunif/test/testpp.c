/* Test cdunif/PP:
 *
 * Reads the test PP file, and tests certain data values against known contents
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "cdunif.h"
#include <math.h>

extern void usage();

#define UNAME "u"
#define NULAT 72

#define STARTX 10
#define STARTY 20
#define STARTZ 5
#define STARTT 0

#define COUNTX 4
#define COUNTY 5
#define COUNTZ 3
#define COUNTT 1

#define TESTX 2
#define TESTY 3
#define TESTZ 1
#define TESTT 0

#define TESTLAT 10
#define LATVAL 63.75
#define UVAL    -0.0643737
#define TOLERANCE 1e-5

typedef float Real;

main(int argc, char *argv[]){
#ifdef HAVE_PP
  int fileid;
  char *filename="testpp.pp";
  int varid_u,dimid_lat,status;
  
  char varname[CU_MAX_NAME+1],dimname[CU_MAX_NAME+1],dimunits[CU_MAX_NAME+1];
  CuType datatype;
  CuDimType dimtype;
  int varid, ndims, dimids[CU_MAX_VAR_DIMS], natts;
  long length;
  
  Real latitude[NULAT];
  Real u[COUNTT][COUNTZ][COUNTY][COUNTX];

  long start[4]={STARTT,STARTZ,STARTY,STARTX},count[4]={COUNTT,COUNTZ,COUNTY,COUNTX};
  
  if(argc != 1) usage();
  
  /* open */
  fileid = cuopenread(filename,0);
  if (fileid!=0) goto err;

  /* find the U variable */
  varid_u = cuvarid(fileid,UNAME);
  
  /* find its corresponding latitude dimension */
  status = cuvarinq(fileid, varid_u, varname, &datatype, &ndims, dimids, &natts);
  if (status!=0) goto err;

  dimid_lat = dimids[2];
  
  /* get info on the latitude dimension */
  status = cudiminq(fileid, dimid_lat, dimname, dimunits, &datatype, &dimtype, &varid, &length);
  if (status!=0) goto err;
  
  if (length != NULAT) goto err;
  
  /* get values of latitude */
  status = cudimget(fileid, dimid_lat, latitude);
  if (status!=0) goto err;
  
  /* test a latitude value */
  if (fabs(latitude[TESTLAT] - LATVAL) > TOLERANCE) goto err;

  /* get a hyperslab from U */
  status=cuvarget(fileid, varid_u, start, count, u);
  if (status!=0) goto err;

  /* test a U value */
  if (fabs(u[TESTT][TESTZ][TESTY][TESTX] - UVAL) > TOLERANCE) goto err;
  
  /* close */
  status = cuclose(fileid);
  if (status!=0) goto err;
  
  exit(0);
  
 err:
  fprintf(stderr,"*** Error in TestPP verifying file %s\n",filename);
  exit(1);

#else
  fprintf(stderr,"PP library not present\n");
  exit(0);
#endif
}


void
usage(){
  fprintf(stderr,"Usage: testpp\n");
  fprintf(stderr,"   Open the test PP file and check data\n");
  exit(1);
}
