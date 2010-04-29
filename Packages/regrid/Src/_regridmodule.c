 /******************************************************************************************
 *                                                                                         *
 * _regridmodule.c:  a C extension which exports the following functions to Python:        *
 *                    gridattr                                                             *
 *                    pressattr                                                            * 
 *                    maparea                                                              *
 *                    rgdarea                                                              *
 *                    maplength                                                            *
 *                    rgdlength                                                            *
 *                    rgdpressure                                                          *
 *                                                                                         *
\******************************************************************************************/

#include "Python.h"
#include "Numeric/arrayobject.h" 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <string.h>

static PyObject *ErrorObject;            /* locally raised exception */

/*--------------------------- Macros --------------------------------------*/

#define onError(message) { PyErr_SetString(ErrorObject, message); return NULL; }

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

/*-------------------- Function Prototypes --------------------------------*/

/* ----  functions in this file called directly by gridattr ---- */

void uniform_longitude_grid(int nlon, float blon, float elon, double *pts, double *wts, double *bnds);

void uniform_latitude_grid(int nlat, double *pts, double *wts, double *bnds);

void equalarea_grid(int nlat, double *pts, double *wts, double *bnds);

void gaussian_grid(int nlat, double *pts, double *wts, double *bnds);

/* ----  functions in this file called directly by pressattr ---- */

void press_wts_bnds(int nlev, double *pts,  double *wts, double *bnds);

/* ----  functions in this file called by regridding c extensions ---- */

void zero_double(int size, double *data);
void zerodouble(int size, double *data);
void zero_float(int size, float *data);

void zero_int(int size, int *data);

void write_double(int size, char *title, FILE *fp, double *data);

void write_float(int size, char *title, FILE *fp, float *data);

void write_int(int size, char *title, FILE *fp, int *data);


int cd_cint(float value);

void cd_linear_interpolation(int d, double *x, float *y, int dp, double *xp, float *yp, float missingValueIn, char *missingMatch);

/* locate is identical to the one in pcmdimodule.c */
int cd_locate(int d, double *x, double point);

 /*************************************************************************
 **************************************************************************
 *                                                                        *
 *                     EXPORTED MODULE METHOD-FUNCTIONS
 * 
 *                                                                        *
 **************************************************************************
 **************************************************************************/


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   gridattr                                                   *
 *                                                                         *
 *  Purpose:    a c extension module function used to get a grid's         *
 *              attributes( pts, wts and bnds ) for use in python          *
 *                                                                         *
 *  Passed:    nlat      number of latitudes                               *
 *             lat_type  a string such as 'gaussian'                       *
 *                                                                         *
 *             or                                                          *
 *                                                                         *
 *             nlon      number of evenly spaced longitudes                *
 *             blon      first point
 *             elon      end point
 *                                                                         *
 *                                                                         *
 *  Return:    pts       array with grid point values in degrees           *
 *             wts       array of weights as differences in sine of        * 
 *             bnds      array with boundary values                        *
 *                                                                         *
 *  Note:      the input and output are python objects                     *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


static PyObject *rgd_gridattr(PyObject *self, PyObject *args)     /* args: (tuple) */
{
  int j;                          /* loop index */ 

  int size;                       /* size of the grid pts and wts arrays */ 
  int sizep;                      /* size of the grid bnds array */ 

  float blon;                     /* beginning longitude */
  float elon;                     /* ending longitude */

  double *pts;                    /* grid points */
  double *wts;                    /*  weights */
  double *bnds;                    /* grid boundaries */

  PyArrayObject *object_pts;      /* object pointer to 1D array to hold data */
  PyArrayObject *object_wts;      /* object pointer to 1D array to hold data */
  PyArrayObject *object_bnds;     /* object pointer to 1D array to hold data */

  FILE *fp;                       /* file used in ascii write */ 

  char *gridpass;                 /* grid id for file name */
  char gridtype[20];              /* file name construction */
  char filename[20];              /* file name construction */
  char buf[20];                   /* for int to ascii conversion */

  char *title[3] = {              /* titles for print to file */ 
     "points ", "weights ",
     "bounds "
  };

  /* size int and type string are required. blon and elon are for longitude case */

  if (!PyArg_ParseTuple(args, "is|ff", &size, &gridpass, &blon, &elon)) 
  {
     PyErr_SetString(PyExc_TypeError, "Pass to gridattr is wrong.\n");
     return NULL; 
  }

  sizep = size + 1;

  /* only first three characters are required */

  if(!strncmp(gridpass, "gau", 3))
      strcpy(gridtype, "gaussian");
  else if(!strncmp(gridpass, "equ", 3))
      strcpy(gridtype, "equalarea");
  else if(!strncmp(gridpass, "uni", 3))
      strcpy(gridtype, "uniform");
  else if(!strncmp(gridpass, "lon", 3))
      strcpy(gridtype, "longitude");
  else {
      printf("Defined grid types are:\n gaussian\n equalarea\n uniform\n longitude\n\n");
      sprintf(buf, "Unknown grid type: %s\n", gridpass);
      onError(buf);
  }

  /* create the 1D python array objects */

  object_pts = (PyArrayObject *)PyArray_FromDims(1, (int *)&size, PyArray_DOUBLE);
  if(!object_pts) PyErr_NoMemory();

  object_wts = (PyArrayObject *)PyArray_FromDims(1, (int *)&size, PyArray_DOUBLE);
  if(!object_wts) PyErr_NoMemory();

  object_bnds = (PyArrayObject *)PyArray_FromDims(1, (int *)&sizep, PyArray_DOUBLE);
  if(!object_bnds) PyErr_NoMemory();

  /* use pointers to the data attribute in the python array  objects */

  pts = (double *)object_pts->data;
  zero_double(size, pts);

  wts = (double *)object_wts->data;
  zero_double(size, wts);

  bnds = (double *)object_bnds->data;
  zero_double(size + 1, bnds);

  /*  call c functions to calculate the points, weights and bounds  */

  if(!strcmp(gridtype, "gaussian"))
      gaussian_grid(size, pts, wts, bnds);

  else if(!strcmp(gridtype, "equalarea"))
      equalarea_grid(size, pts, wts, bnds);

  else if(!strcmp(gridtype, "uniform"))
      uniform_latitude_grid(size, pts, wts, bnds);

  else if(!strcmp(gridtype, "longitude"))
      uniform_longitude_grid(size, blon, elon, pts, wts, bnds);

  /* ------------------------------------------------------- */
#ifdef DEBUG

  /* write data to a file */
  /*
  sprintf(buf, "%d", size);

  strcpy(filename, gridtype); 
  strcat(filename, "_" );
  strcat(filename, buf);
  strcat(filename, ".asc");

  if((fp = fopen(filename, "w")) == NULL) {
      PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
      return NULL;
  }

  write_double(size, title[0], fp, pts);
  write_double(size, title[1], fp, wts);
  write_double(sizep, title[2], fp, bnds);
  close(fp);
  */
#endif


  return Py_BuildValue(("OOO"), object_pts, object_wts, object_bnds);
}


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   pressattr                                                  *
 *                                                                         *
 *  Purpose:    a c extension module function used to get pressure grid    *
 *              attributes( wts and bnds ) for use in python               *
 *                                                                         *
 *  Passed:    nlev      number of levels                                  *
 *             pts       array with pressue values as doubles              *
 *                                                                         *
 *  Return:    wts       array of weights                                  * 
 *             bnds      array of boundary values                          *
 *                                                                         *
 *  Note:      the input and output are python objects                     *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


static PyObject *rgd_pressattr(PyObject *self, PyObject *args) 
{

  /* fields which are passed from Python in the args tuple */

  int nlev;                       /* size of the grid pts and wts arrays */ 

  double *pts;                    /* pressure grid points */
  PyArrayObject *object_pts;      /* object pointer to 1D array to hold data */

  /* fields which are returned to Python */

  double *wts;                    /* pressure weights */
  double *bnds;                   /* grid boundaries */

  PyArrayObject *object_wts;      /* object pointer to 1D array to hold data */
  PyArrayObject *object_bnds;     /* object pointer to 1D array to hold data */

  int nlevp;                      /* size of the grid bnds array */ 

  FILE *fp;                       /* file used in ascii write */ 

  char *title[3] = {              /* titles for print to file */ 
     "points ", "weights ",
     "bounds "
  };

    /* ----------------------- Start Execution ------------------------------------*/

  if (!PyArg_ParseTuple(args, "iO", &nlev,  &object_pts)) 
  {
     PyErr_SetString(PyExc_TypeError, "Pass to pressattr is wrong.\n");
     return NULL; 
  }

  nlevp = nlev + 1;

  /* create the 1D python array objects */

  object_wts = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlev, PyArray_DOUBLE);
  if(!object_wts) PyErr_NoMemory();

  object_bnds = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlevp, PyArray_DOUBLE);
  if(!object_bnds) PyErr_NoMemory();

  /* use pointers to the data attribute in the python array  objects */

  pts = (double *)object_pts->data;

  wts = (double *)object_wts->data;
  zero_double(nlev, wts);

  bnds = (double *)object_bnds->data;
  zero_double(nlevp, bnds);

  /*  call c function to use pts to calculate wts and bnds  */

  press_wts_bnds(nlev, pts, wts, bnds);

  /* ------------------------------------------------------- */

#ifdef DEBUG
  /* write data to a file */

  /*
  if((fp = fopen("pressout.asc", "w")) == NULL) {
      PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
      return NULL;
  }

  write_double(nlev, title[0], fp, pts);
  write_double(nlev, title[1], fp, wts);
  write_double(nlevp, title[2], fp, bnds);

  close(fp);
  */
#endif

  /* ------------------------------------------------------- */


  return Py_BuildValue(("OO"), object_wts, object_bnds);
}



/*  ************************************************************************* 
*                                                                           *
*                           MAPAREA SYNOPSIS                                *
*                                                                           *
*  Maparea maps the input areas into the output area for each output point. * 
*  Given an output cell centered on the point (io, jo) with the boundaries  *            
*                bwout(io) ----- beout(io)                                  *
*                bnout(jo) ----- bsout(jo)                                  *
*  it finds the set of input points (ii, ji) with the boundaries            *            
*                bwin(ii) ----- bein(ii)                                    *
*                bnin(ji) ----- bsin(ji)                                    *
*                                                                           *
*  which have at least two of these boundary lines crossing inside the      *  
*  the output closed boundary. The code finds the contributions in latitude *      
*  and in longitude separately. The two sets are combined as used in routine*  
*  rgdarea.                                                                 *
*                                                                           *
*  Assembling this set for latitudes is straight forward. It entails:       * 
*                                                                           *
*       1. Find indices ji such that bnin[ji] and/or bsin[ji] fall within   *
*          bnout[jo] ----- bsout[jo].                                       *
*                                                                           *
*       2. Record these indices in latpt[] in order output latitude by      *
*          latitude. The position of the last contributor to each jo is     *
*          recorded in latdx[jo].                                           *
*       3. Find the extend of each input cell (bnorth,bsouth) which lies in *
*          the output cell and store it as the latitude weight in wtlat[]   *
*                                                                           *
*                                                                           *
*                                                                           *
*  The problem of finding the input longitudes points ii contributing to    * 
*  each output longitude io is complicated by the invariance of the         *
*  longitude description under the transformation + - 360 degrees. Before   *
*  following the steps outlined above it is necessry to make new input      *
*  boundary arrays bwinl[] and beinl[] with overlap in order to align the   * 
*  input longitude grid so that the first input cell contributes to the     * 
*  first output longitude. The solution is found using the following steps: *
*                                                                           *
*       1. Find the first contributor which has a western boundary at or    *
*          just westward of the output boundary westout. This index is      *
*          labeled istradle.                                                *
*       2. Make bninl and bsinl starting with istradle adding + - 360       * 
*          degrees as needed to fold the input domain into the output       * 
*          domain.                                                          *
*                                                                           *
*       3. Follow the same steps as in the latitude case recording indices  *
*          in lonpt after correction by adding istradle back in ( the       *
*          use of bwinl and beinl was only an intermediate step allowing    *
*          use of the same algorithm as in the latitudes case)              *
*                                                                           *
****************************************************************************/ 


/*  ************************************************************************* 
*                                                                           *
*                        MAPAREA USAGE                                      *
*                                                                           *
*  This subroutine provides the information needed to interpolate           *
*  from the input grid to the output grid (i.e., it calculates              *
*  the weight of the contribution from each of the input cells              *
*  to each of the output cells.                                             *
*                                                                           *
*  The input and output grid-cell boundaries must be specified by the       *
*  user(bnin, bsin, bein, bwin, bnout, bsout, beout, and bwout), and        *
*  the ordering must be monotonic, but need not be continuous (input or     *
*  output cells may be missing). The units are degrees latitude and         *
*  longitude.                                                               *
*                                                                           *
*   The input and output domains do not need to be identical, and           *
*   the longitude coordinate will be "wrapped around" if necessary.         *
*   The coordinate ordering may differ between input and output grids       *
*   (for example N to S on input, S to N on output.                         *
*                                                                           *
*                                                                           *
*   Input:                                                                  *
*                                                                           *
*    nloni = number of input grid cell longitudes.                          *
*    nlono = number of output grid cells longitudes.                        *
*    nlati = number of input grid cell latitudes.                           *
*    nlato = number of output grid cells latitudes.                         *
*    bnin(nlati) = northern boundary of each grid cell of input field.      *
*    bsin(nlati) = southern boundary of each grid cell of input field.      *
*    bein(nloni) = eastern boundary of each grid cell of input field.       *
*    bwin(nloni) = western boundary of each grid cell of input field.       *
*    bnout(nlato) =northern boundary of each grid cell of output field.     *
*    bsout(nlato) =southern boundary of each grid cell of output field.     *
*    beout(nlono) = eastern boundary of each grid cell of output field.     *
*    bwout(nlono) = western boundary of each grid cell of output field.     *
*                                                                           *
*                                                                           *
*   Output                                                                  *
*                                                                           *
*                                                                           *
*    londx(nlono) = index of the last element in lonpt and wtlon that       *
*                   apply to each of the the output longitudes              *
*    lonpt(nloni+nlono) = array of indices pointing to input grid cells     * 
*                         that contribute to each output grid cell.         *
*    wtlon(nloni+nlono) = array of weights indicating how much each         *
*                         input grid cell contributes to each               *
*                         output grid cell.                                 *
*                                                                           *
*    latdx(nlato) = index of the last element in latpt and wtlat that       *
*                   apply to each of the the output latitudes               *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells     *
*                         that contribute to each output grid cell.         *
*    wtlat(nlati+nlato) = array of weights indicating how much each         *
*                         input grid cell contributes to each               *
*                         output grid cell.                                 *
*                                                                           *
****************************************************************************/ 



static PyObject *rgd_maparea(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int nloni;                              /* number of input longitudes */
    int nlono;                              /* number of output longitudes */
    int nlati;                              /* number of input latitudes */
    int nlato;                              /* number of output latitudes */

    float *bnin;                            /* northern boundary of grid cells of input field */
    float *bnout;                           /* northern boundary of grid cells of output field */
    float *bsin;                            /* southern boundary of grid cells of input field */
    float *bsout;                           /* southern boundary of grid cells of output field */
    float *bein;                            /* eastern boundary of grid cells of input field */
    float *beout;                           /* eastern boundary of grid cells of output field */
    float *bwin;                            /* western boundary of grid cells of input field */
    float *bwout;                           /* western boundary of grid cells of output field */

    PyArrayObject *object_bnin;             /* object pointer northern boundary of grid cells of input field */
    PyArrayObject *object_bnout;            /* object pointer northern boundary of grid cells of output field */
    PyArrayObject *object_bsin;             /* object pointer southern boundary of grid cells of input field */
    PyArrayObject *object_bsout;            /* object pointer southern boundary of grid cells of output field */
    PyArrayObject *object_bein;             /* object pointer eastern boundary of grid cells of input field */
    PyArrayObject *object_beout;            /* object pointer eastern boundary of grid cells of output field */
    PyArrayObject *object_bwin;             /* object pointer western boundary of grid cells of input field */
    PyArrayObject *object_bwout;            /* object pointer western boundary of grid cells of output field */

    /* fields which are returned to Python */

    int *londx;                             /* index of last element in lonpt & wtlon for this cell */
    int *lonpt;                             /* indices of input grid cells for this cell */
    float *wtlon;                           /* weights for input cell's contributions to this cell */

    int *latdx;                             /* index of last element in lonpt & wtlon for this cell */
    int *latpt;                             /* indices of input grid cells for this cell */
    float *wtlat;                           /* weights for input cell's contributions to this cell */

    /* added local variables */

    int j;                                  /* loop index */

    int nlatsize;                           /* sum number of input & output latitudes */
    int nlonsize;                           /* sum number of input & output longitudes */

    /* local variables */

    float pi, bnorth, bsouth;
    float westout, eastout;                 /* output longitude boundaries */
    int istradle;                           /* first contributing input long to first output long */
    int last;                               /* storage index  in latpt,wtlat,lonpt,wtlon */
    int j1, j2, j3;                         /* indices used in finding input contributors*/
    int i;                                  /* loop index */
    int ji, jo;                             /* indices used in input ouput loops */
    int ierr1, ierr2;                       /* malloc check */ 
    int ii, ip;                             /* used to find input cell straddling the out boundary */
    int found;                              /* used as a flag in breaking out fo loops */
    int iflag;                              /* used as a flag */
    float *bwinl, *beinl;
     


    FILE *fp;                               /* file used in ascii write */ 

    char *title[6] = {                      /* titles for print to file */ 
       "londx", "lonpt ", "wtlon",
       "latdx", "latpt ", "wtlat"
    };

    /* python array transform variables */

    PyArrayObject *object_londx;            /* object pointer to 1D array to hold data */
    PyArrayObject *object_lonpt;            /* object pointer to 1D array to hold data */
    PyArrayObject *object_wtlon;            /* object pointer to 1D array to hold data */

    PyArrayObject *object_latdx;            /* object pointer to 1D array to hold data */
    PyArrayObject *object_latpt;            /* object pointer to 1D array to hold data */
    PyArrayObject *object_wtlat;            /* object pointer to 1D array to hold data */

    FILE *fpin;                             /* file used in ascii write */ 

    char *titlesin[8] = {                                            /* titles for print to file */ 
       "bnin ", "bnout ", "bsin ", "bsout ",
       "bein ", "beout ", "bwin ", "bwout "
    };

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iiiiOOOOOOOO", &nloni, &nlono, &nlati, &nlato,  
     &object_bnin, &object_bnout, &object_bsin, &object_bsout, &object_bein, &object_beout, &object_bwin, &object_bwout))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to maparea is wrong.\n");
        return NULL;
    }

    /* assign c pointers to data in the PyArrayOjects passed in */

    bnin  = (float *)object_bnin->data; 
    bnout = (float *)object_bnout->data; 
    bsin  = (float *)object_bsin->data; 
    bsout = (float *)object_bsout->data; 
    bein  = (float *)object_bein->data; 
    beout = (float *)object_beout->data; 
    bwin  = (float *)object_bwin->data; 
    bwout = (float *)object_bwout->data; 

    /* -------- write  input data to a file ----------- */

    /*
#ifdef DEBUG
    if((fpin = fopen("maparea_in.asc", "w")) == NULL) {
        PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
        return NULL;
    }

    fprintf(fpin, "nloni: %d\n", nloni);
    fprintf(fpin, "nlono: %d\n", nlono);

    fprintf(fpin, "nlati: %d\n", nlati);
    fprintf(fpin, "nlato: %d\n", nlato);

    write_float(nlati, titlesin[0], fpin, bnin);
    write_float(nlato, titlesin[1], fpin, bnout);
    write_float(nlati, titlesin[2], fpin, bsin);
    write_float(nlato, titlesin[3], fpin, bsout);

    write_float(nlati, titlesin[4], fpin, bein);
    write_float(nlato, titlesin[5], fpin, beout);
    write_float(nlati, titlesin[6], fpin, bwin);
    write_float(nlato, titlesin[7], fpin, bwout);

    close(fpin);
#endif
    */
    /* ------------------------------------------------ */

    nlonsize = nloni + nlono; 
    nlatsize = nlati + nlato; 

    /* allocate memory for arrays constructed in this routine to pass to python*/

    /* create the 1D python array objects */

    object_londx = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlono, PyArray_INT);
    if(!object_londx) PyErr_NoMemory();

    object_lonpt = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlonsize, PyArray_INT);
    if(!object_lonpt) PyErr_NoMemory();

    object_wtlon = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlonsize, PyArray_FLOAT);
    if(!object_wtlon) PyErr_NoMemory();

    object_latdx = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlato, PyArray_INT);
    if(!object_latdx) PyErr_NoMemory();

    object_latpt = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlatsize, PyArray_INT);
    if(!object_latpt) PyErr_NoMemory();

    object_wtlat = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlatsize, PyArray_FLOAT);
    if(!object_wtlat) PyErr_NoMemory();


    /* assign c pointers to the data attribute in the python array  objects */

    londx = (int *)object_londx->data;
    lonpt = (int *)object_lonpt->data;
    wtlon = (float *)object_wtlon->data;

    latdx = (int *)object_latdx->data;
    latpt = (int *)object_latpt->data;
    wtlat = (float *)object_wtlat->data;

    zero_int(nlono, londx);
    zero_int((nlonsize), lonpt);
    zero_float((nlonsize), wtlon);
    zero_int(nlato, latdx);
    zero_int((nlatsize), latpt);
    zero_float((nlatsize), wtlat);


    pi = 4.*atan(1.0);
      

/*  cycle through output zones */

    if ((bsin[nlati-1] - bsin[0]) * (bsout[nlato-1] - bsout[0]) > 0.) {
	j1 = 0;
	j2 = nlati - 1;
	j3 = 1;
    } 

    else {
	j1 = nlati - 1;
	j2 = 0;
	j3 = -1;
    }

    last = -1;

    for (jo = 0; jo < nlato; ++jo) {
	iflag = 0;

	/* find index of input latitude zones that will contribute. */

	for (ji = j1; j3 < 0 ? ji >= j2 : ji <= j2; ji += j3) {

	    if (bsin[ji] < bnout[jo] && bnin[ji] > bsout[jo]) {
		iflag = 1;
		++last;
		latpt[last] = ji;
		bnorth = min( bnout[jo], bnin[ji] );
		bsouth = max( bsout[jo], bsin[ji] );
		wtlat[last] = sin( bnorth*pi/180. ) - sin( bsouth*pi/180. );
	    }

	     else if (iflag == 1) {
		break;
	    }
	}

	latdx[jo] = last;
	if (last != -1) {
	    j1 = latpt[last];
	}
    }

     
	    
    westout = min( bwout[0], bwout[nlono - 1] );
    eastout = max( beout[0], beout[nlono - 1] );

    /* find input grid cell that straddles the output domain boundary. */

    if (bwin[0] < bwin[nloni - 1]) {
	ii = cd_cint( (westout - bwin[nloni - 1])/360.) + 1;
    }
     else {
	ii = cd_cint( (westout - bwin[nloni - 1])/360.) - 1;
    }

    found = 0;

    for (i = 0; i < nloni; ++i) {
	ip = ii;
	ii = cd_cint( (westout - bwin[i])/360.);
	if (ii != ip) {
	    if (bwin[0] < bwin[nloni - 1]) {
		if (westout == bwin[i]) {
		    istradle = i;
		} else {
		    istradle = i - 1;
		    if (istradle < 0) {
			istradle = nloni - 1;
		    }
		}
	    } else {
		istradle = i;
	    }
	    found = 1;     /* set if ii != ip  implying found the stradle cell */
	    break;
	}
    }

    if (!found) {
	if (bwin[0] < bwin[nloni - 1]) {
	    istradle = nloni - 1;
	}
	else {
	    istradle = 0;
	}
    }



    /* allocate enough space for one more than the number of longitude 
       grid cells and make a copy of longitudes that coincides 
       with output domain. */

    ierr1 = 0;
    ierr2 = 0;

    if( !(bwinl  = (float *)malloc((nloni + 1)*sizeof(float))) )
	ierr1 = 1;
    if( !(beinl  = (float *)malloc((nloni + 1)*sizeof(float))) )
	ierr2 = 1;


    if (ierr1 != 0 || ierr2 != 0) {
	printf("Error in attempting to allocate memory dynamically.\n");
	printf("You may need more memory.\n");
	printf("Try running on cirrus or retrieving smaller portions of the array.\n");
	printf("Error in maparea\n");
	exit(1);
    }


    for (ii = 0; ii < (nloni + 1); ++ii) {

	i = istradle + ii;        /* xform bw(e)in[i] -> bw(e)inl[ii] */
	if (i > (nloni - 1)) {
	    i -= nloni;
	}

	/*  make western bdry of input grid cell fall within output domain. */

	bwinl[ii] = bwin[i] + 360.* cd_cint((westout - bwin[i])/360.);
	beinl[ii] = bein[i] + 360.* cd_cint((westout - bwin[i])/360.);

    }


    /* shift the first point back to place it just westward of westout */
    if (bwin[nloni - 1] > bwin[0]) {
	if (bwinl[0] > westout) {
	    bwinl[0] += -360.;
	    beinl[0] += -360.;
	}
    } 
    else {
	if (bwinl[nloni] > westout) {
	    bwinl[nloni] += -360.;
	    beinl[nloni] += -360.;
	}
    }


    /*    cycle through output longitudes */

    if ((bwinl[nloni] - bwinl[0]) * (bwout[nlono - 1] - bwout[0]) > 0.) {
	j1 = 0;
	j2 = nloni;
	j3 = 1;
    } else {
	j1 = nloni;
	j2 = 0;
	j3 = -1;
    }

    last = -1;

    for (jo = 0; jo < nlono; ++jo) {

	iflag = 0;

	/* find index of input longitude zones that will contribute. */

	for (ji = j1; j3 < 0 ? ji >= j2 : ji <= j2; ji += j3) {
	    if (bwinl[ji] < beout[jo] && beinl[ji] > bwout[jo]) {
		iflag = 1;
		++last;
		lonpt[last] = ji + istradle ;
		if (lonpt[last] > nloni - 1) {
		    lonpt[last] -= nloni;
		}

		wtlon[last] = min( beout[jo], beinl[ji] ) - max( bwout[jo], bwinl[ji] );
	    } 

	    else if (iflag == 1) {
		break;
	    }
	}


	londx[jo] = last;
	if (last != -1) {
	    j1 = lonpt[last] - istradle;
	}
	if (j1 < 0) {
	    j1 += nloni;
	}
    }


    free(bwinl);
    free(beinl);

    /* ------------------------------------------------------- */


#ifdef DEBUG
    /* ------- write results that are passed to python ------- */
    /*
    if((fp = fopen("maparea_out.asc", "w")) == NULL) {
        PyErr_SetString(PyExc_IOError, "Can not open file for maparea checks");
        return NULL;
    }

    write_int(nlono, title[0], fp, londx);
    write_int((nlonsize), title[1], fp, lonpt);
    write_float((nlonsize), title[2], fp, wtlon);

    write_int(nlato, title[3], fp, latdx);
    write_int((nlatsize), title[4], fp, latpt);
    write_float((nlatsize), title[5], fp, wtlat);

    close(fp);
    */
#endif
    /* ------------------------------------------------------- */

    return Py_BuildValue(("OOOOOO"), object_londx, object_lonpt, object_wtlon, object_latdx, object_latpt, object_wtlat);
} 


/*  ********************************************************************* 
*                                                                       *
*                         RGDAREA SYNOPSIS                              *
*                                                                       *
*  Rgdarea makes an area weighted average of all the input cell data    *
*  which contributes to the output data for each particular output cell.*
*                                                                       *
*  For each output cell centered on the point (io, jo), it finds the    *
*  input cells contributing using:                                      *
*                                                                       *
*   jb--je: the begin and end latitudes with index je from the latpt    *
*           array and the corresponding weights from the wtlat array    *
*                                                                       *
*   ib--ie: the begin and end longitudes with index ie from the lonpt   *
*           array and the corresponding weights from the wtlon array    *              
*                                                                       *
*  The weights provide a measure of the overlap of the input cell areas *
*  into the output cell areas. These contributions are the same for all *
*  levels and all times. The structure in level-time is the same for    *
*  the input and output fields. A straight forward area weighted        *
*  calculation produces the regridded data.                             *
*                                                                       *
*  The routine requires a mask, amskin, of 1.0s and 0.0s which is the   *
*  same size as the input data including levels and times. It generates *
*  the corresponding mask for the outdata, amskout. This mask can be    *
*  used to regrid the output data                                       * 
*                                                                       *
*************************************************************************/ 



/*  ********************************************************************* 
*                                                                       *
*                           RGDAREA USAGE                               *
*                                                                       *
*  This subroutine takes an input field (ain) of grid cells and fills   *
*  an output field (aout) of grid cells with area-weighted values,      *
*  thereby achieving a transformation from one grid to another.         *
*                                                                       *
*  The program uses pointers and weights generated outside this         *
*   routine to determine how much each input grid cell contributes      *
*   to each output grid cell                                            *
*                                                                       *
*   Besides the automatic area weighting, an additional user-supplied   *
*   weighting factor (amskin) must be passed to this subroutine, which  *
*   multiplies the area weighting.  If simple area-weighting is needed, *
*   supply a vector of 1.''s in the input.  However, if for example only* 
*   land values should contribute to the new gridded field, then 1.''s  *
*   should be assigned for land points, and 0.''s for ocean points.     *
*                                                                       *
*                                                                       *
*                                                                       *
*   Input:                                                              *
*                                                                       *
*                                                                       *
*    ilong = dimension position for longitude in input and output arrays* 
*          = 0 if first dimension                                       *
*          = 1 if second dimension                                      *
*          = 2 if third dimension                                       *
*          = 3 if fourth dimension                                      *
*    ilat = dimension position for latitude in input and output arrays  *
*    itim1 = dimension position for time or level in input and output   *
*              arrays (if no itim1 dimension, then this parameter is    *
*              ignored)                                                 *
*    itim2 = dimension position for time or level in input and output   *
*              arrays (if no itim2 dimension, then this parameter is    *
*              ignored)                                                 *
*    nloni = number of input grid cell longitudes.                      *
*    nlono = number of output grid cells longitudes.                    *
*    nlati = number of input grid cell latitudes.                       *
*    nlato = number of output grid cells latitudes.                     *
*                                                                       *
*    ntim1 = length of itim1 dimension (if this is a dummy dimension,   *
*              you should set ntim1 to 0)                               *
*    ntim2 = length of itim2 dimension (if this is a dummy dimension,   *
*              you should set ntim2 to 0)                               *
*                                                                       *
*    londx(nlono) = index of the last element in lonpt and wtlon that   *
*                   apply to each of the the output longitudes          *
*    lonpt(nloni+nlono) = array of indices pointing to input grid cells *
*                         that contribute to each output grid cell.     *
*    wtlon(nloni+nlono) = array of weights indicating how much each     *
*                         to input grid cells that contribute to each   *
*                         output grid cell.                             *
*                                                                       *
*    latdx(nlato) = index of the last element in latpt and wtlat that   *
*                   apply to each of the the output latitudes           *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells *
*                         that contribute to each output grid cell.     *
*    wtlat(nlati+nlato) = array of weights indicating how much each     *
*                         to input grid cells that contribute to each   *
*                         output grid cell.                             *
*    omit = value that will be assigned grid points that have either    *
*       been masked out or cannot be computed for lack of input data.   *
*                                                                       *
*                                                                       *
*    Input and Output:                                                  *
*                                                                       *    
*    amskin(*) = input weighting factors (for masking purposes).        *
*    ain(*) = input field                                               *
*                                                                       *
*                                                                       *
*    amskout(*) = output weighting factors                              *
*    aout(*) = output (regridded) field                                 *
*                                                                       *
*                                                                       *
*************************************************************************/ 


static PyObject *rgd_rgdarea(PyObject *self, PyObject *args) 
{


    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int ilong;                              /* dimension position for longitude */
    int ilat;                               /* dimension position for latitude */
    int itim1;                              /* dimension position for time or level*/
    int itim2;                              /* dimension position for time or level*/
    int ntim1;                              /* length of itime1 dimension */
    int ntim2;                              /* length of itime2 dimension */

    int nloni;                              /* number of input longitudes */
    int nlono;                              /* number of output longitudes */
    int nlati;                              /* number of input latitudes */
    int nlato;                              /* number of output latitudes */

    int flag2D;                             /* 1 for 2D, 0 for full size */
    float omit;                             /* missing value */

    int *londx;                             /* index of last element in lonpt & wtlon for this cell */
    int *lonpt;                             /* indices of input grid cells for this cell */
    float *wtlon;                           /* weights for input cell's contributions to this cell */

    int *latdx;                             /* index of last element in lonpt & wtlon for this cell */
    int *latpt;                             /* indices of input grid cells for this cell */
    float *wtlat;                           /* weights for input cell's contributions to this cell */

    float *amskin;                          /* input mask */
    float *ain;                             /* input data */
    float *aout;                            /* output data */

    PyArrayObject *object_londx;            /* object pointer to londx */
    PyArrayObject *object_lonpt;            /* object pointer to lonpt */
    PyArrayObject *object_wtlon;            /* object pointer wtlon */

    PyArrayObject *object_latdx;            /* object pointer to latdx */
    PyArrayObject *object_latpt;            /* object pointer to latpt */
    PyArrayObject *object_wtlat;            /* object pointer wtlat */

    PyArrayObject *object_amskin;           /* object pointer to amskin */
    PyArrayObject *object_ain;              /* object pointer to amskin */
    PyArrayObject *object_aout;             /* object pointer to aout */
     
    /* fields which are returned to Python */

    float *amskout;                         /* mask for the output data */

    /* python array transform variables */

    PyArrayObject *object_amskout;          /* object pointer to array to hold amskout */


    /* added local variables */

    int datasize;                  /* size of output data */
    int number_dim;                /* number of dimensions in the data */
    int dim_array[4] = {0,0,0,0};  /* array with size of the array */
    int nlonsize;                  /* nloni + nlono */
    int nlatsize;                  /* nlati + nlato */
    int n2D;                       /* index into a 2D array */

    FILE *fpin;                    /* for the write of input data */

    char *titlesin[6] = {                                            /* titles for print to file */ 
       "londx ", "lonpt ", "wtlon ",
       "latdx ", "latpt ", "wtlat "
    };


    /* original local variables */

    int id[4];                     /* order of the dimensions */
    int nd[4];                     /* size of the dimensions */

    int found;                     /* used with break out of for loops */

    int imiss1, imiss2;            /* store the mising dimension numbers */   
    int jtim1, jtim2;              /* storing itim1(2) or imiss1(2) */ 
    int mtim1, mtim2;              /* storing mtim1(2) or 1 if missing */
    
    int i, j, k;                   /* loop indices */

    int leni[4], leno[4];          /* vector used in striding through arrays */
    int iai[4];                    /* holds 4 indices for input point */
    int iao[4];                    /* holds 4 indices for output point */  

    int ierr1, ierr2;              /* used in malloc check */           
    
    int ji, ii;                   /* loop indices used for input zones and latitudes */    
    int jo, io;                   /* loop indices used for output zones and latitudes */    
    int jb, je;                   /* loop limits for contributing input zones */
    int ib, ie;                   /* loop limits for contributing input latitudes */ 
    
    int nn;                       /* index into data arrays */  
    int kk;                       /* index into level-times */
    int k1, k2;                   /* loop indices for level-times */    

    double *accum; 
    double *wtmsk;
    double wt;

    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "iiiiiiiiiiifOOOOOOOOO", &ilong, &ilat, &itim1, &itim2, &ntim1,
                                                      &ntim2, &nloni, &nlono, &nlati, &nlato, &flag2D, &omit,
                                                      &object_londx,  &object_lonpt, &object_wtlon, &object_latdx,
                                              &object_latpt, &object_wtlat, &object_amskin, &object_ain, &object_aout))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to rgdarea is wrong.\n");
        return NULL;
    }

    londx  = (int *)object_londx->data; 
    lonpt  = (int *)object_lonpt->data; 
    wtlon  = (float *)object_wtlon->data; 

    latdx  = (int *)object_latdx->data; 
    latpt  = (int *)object_latpt->data; 
    wtlat  = (float *)object_wtlat->data; 

    amskin  = (float *)object_amskin->data; 
    ain  = (float *)object_ain->data; 
    aout  = (float *)object_aout->data; 

    nlonsize = nloni + nlono; 
    nlatsize = nlati + nlato; 

    /* allocate memory for arrays constructed in this routine to pass to python*/

    /* set up the dimension sequence  and the total size */
    number_dim = 2;
    datasize = nlono*nlato;
    dim_array[ilong] = nlono;  
    dim_array[ilat] = nlato;


    if(ntim1 > 0) {
        datasize = datasize* ntim1;
        number_dim++;
        dim_array[itim1] = ntim1;
    }
    if(ntim2 > 0) {
        datasize = datasize* ntim2;
        number_dim++;
        dim_array[itim2] = ntim2;
    }

    object_amskout = (PyArrayObject *)PyArray_FromDims(number_dim, (int *)&dim_array, PyArray_FLOAT);
    if(!object_amskout) PyErr_NoMemory();

    amskout  = (float *)object_amskout->data; 
    zero_float((datasize), amskout);

#ifdef DEBUG
    /* -------- write  input data to a file ----------- */

    /*
    if((fpin = fopen("rgdarea_in.asc", "w")) == NULL) {
        PyErr_SetString(PyExc_IOError, "Can not open file to rgdarea checks");
        return NULL;
    }

    fprintf(fpin, "ilong: %d\n", ilong);
    fprintf(fpin, "ilat: %d\n", ilat);
    fprintf(fpin, "itim1: %d\n", itim1);
    fprintf(fpin, "itim2: %d\n", itim2);
    fprintf(fpin, "ntim1: %d\n", ntim1);
    fprintf(fpin, "ntim2: %d\n", ntim2);

    fprintf(fpin, "nloni: %d\n", nloni);
    fprintf(fpin, "nlono: %d\n", nlono);
    fprintf(fpin, "nlati: %d\n", nlati);
    fprintf(fpin, "nlato: %d\n", nlato);

    fprintf(fpin, "omit: %f\n", omit);

    write_int(nlono, titlesin[0], fpin, londx);
    write_int((nlonsize), titlesin[1], fpin, lonpt);
    write_float((nlonsize), titlesin[2], fpin, wtlon);

    write_int(nlato, titlesin[3], fpin, latdx);
    write_int((nlatsize), titlesin[4], fpin, latpt);
    write_float((nlatsize), titlesin[5], fpin, wtlat);

    close(fpin);
    */
#endif
    /* ------------------------------------------------ */

    /* Store the order (0 thru3) and the corresponding lengths of the dimensions */

    id[0] = ilong;
    id[1] = ilat;
    id[2] = itim1;
    id[3] = itim2;

    nd[0] = nloni;
    nd[1] = nlati;
    nd[2] = ntim1;
    nd[3] = ntim2;

    for (i = 2; i < 4; ++i) {     /* ntim1 and/or ntim2 = 0 if no dimension */
	if (nd[i] == 0) {
	    id[i] = -1;
	}
    }

    imiss1 = 0;
    imiss2 = 0;

    for (k = 0; k < 4; ++k) {

	found = 0;

	for (i = 0; i < 4; ++i) {

	    if (id[i] == k) {
		if (k < 3) {                       /* look for duplicates */

		    for (j = i + 1; j < 4; ++j) {
			if (id[j] == k) {
	  
			    printf("Error in specifying data structure while attempting\n");
			    printf("to regrid (horizontally interpolate) data.\n");
			    printf("You have set:\n");
			    printf("ilong =  %d\n", ilong);
			    printf("ilat =  %d\n", ilat);
			    printf("itim1 = %d\n", itim1);
			    printf("itim2 = %d\n", itim2);
			    printf("One of these should = 0, another = 1, another = 2, and another = 3\n");
			    printf("If itim1 and/or itim2 are dummy dimensions, then\n");
			    printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
			    printf("itim1 and/or itim2 will be ignored.\n");
			    printf("Duplicates error in rgdarea\n"); 
			    exit(1);
			}
		    }
		}
		found = 1;        /* found a id[] = k match for this k */
		break;            /* break out of for over i after a match */
	    }
	}

	if(!found) {
	    if (!imiss1) {
		imiss1 = k;
	    } 
	    else {
		imiss2 = k;
	    }
	}
    }

    /* transfer itim1(2) and ntim1() into jtim1(2) and mtim1(2) to 
       set mtim1(2) = 1 when dimension is missing and jtim1(2) to
       the position of the mising dimensions for check below */

    if (ntim1 > 0) {
	jtim1 = itim1;
	mtim1 = ntim1;
    } 
    else {
	mtim1 = 1;
	jtim1 = imiss1;
    }

    if (ntim2 > 0) {
	jtim2 = itim2;
	mtim2 = ntim2;
    } else if (imiss2 > 0) {
	jtim2 = imiss2;
	mtim2 = 1;
    } else {
	jtim2 = imiss1;
	mtim2 = 1;
    }

    if (ilong + ilat + jtim1 + jtim2 != 6) {

	printf("Error in specifying data structure while attempting\n");
	printf("to regrid (horizontally interpolate) data.  You have set:\n");
        printf("ilong =  %d\n", ilong);
        printf("ilat =  %d\n", ilat);
        printf("itim1 = %d\n", itim1);
        printf("itim2 = %d\n", itim2);
        printf("One of these should = 0, another = 1, another = 2, and another = 3\n");
        printf("If itim1 and/or itim2 are dummy dimensions, then\n");
        printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
	printf("itim1 and/or itim2 will be ignored.\n");
	printf("Data structure error in rgdarea\n"); 
	exit(1);
    }

    /* store the dimension lengths noting that jtim1(2), level-time are the 
       same in the input and output fields */

    leni[ilong] = nloni;
    leni[ilat]  = nlati;
    leni[jtim1] = mtim1;
    leni[jtim2] = mtim2;

    leno[ilong] = nlono;
    leno[ilat]  = nlato;
    leno[jtim1] = mtim1;
    leno[jtim2] = mtim2;


    /* allocate enough space for double precision variables */ 

    ierr1 = 0;
    ierr2 = 0;

    if( !(accum  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr1 = 1;
    if( !(wtmsk  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr2 = 1;
    
    if (ierr1 || ierr2 ) {
	printf("Error in attempting to allocate memory dynamically.\n");
	printf("You may need more memory.\n");
	printf("Try running on cirrus or retrieving smaller portions of the array.\n");
	printf("Error in rgdarea\n");
	exit(1);
    }

    /* branch to special version of the loops for 2D case */

    if (flag2D) {

        /*    cycle through output zones and longitudes */
        je = -1;
        for (jo = 0; jo < nlato; ++jo) {
	    iao[ilat] = jo;
	    jb = je + 1;
	    je = latdx[jo];

	    ie = -1;
	    for (io = 0; io < nlono; ++io) {
	        iao[ilong] = io;
	        ib = ie + 1;
	        ie = londx[io];

	        for (k = 0; k < mtim1*mtim2; ++k) {
		    wtmsk[k] = (double)0.;
		    accum[k] = (double)0.;
	        }

	        if (je >= jb && ie >= ib) {               /* has contributing grid points */
		    wt = (double)0.;

	      
		    for (ji = jb; ji <= je; ++ji) {
		        iai[ilat] = latpt[ji];
		  
		        for (ii = ib; ii <= ie; ++ii) {
			    iai[ilong] = lonpt[ii];
			    wt += wtlat[ji] * wtlon[ii];       /* weights for all level_times */

		            /* cycle through the level-time structure which is the the same
                               for  input and output and for the input zones and longitudes
                               contribution */

			    for (k2 = 0; k2 < mtim2; ++k2) {
			        iai[jtim2] = k2;
			   
			        for (k1 = 0; k1 < mtim1; ++k1) {
				    iai[jtim1] = k1;

				    kk = k2 * mtim1 + k1;

				    nn = iai[0] + leni[0] * (iai[1] + leni[1] * (iai[2] + leni[2] * iai[3] ));
				    n2D = iai[ilong] + leni[ilong] * iai[ilat];

				    wtmsk[kk] += wtlat[ji] * wtlon[ii] * amskin[n2D];

				    accum[kk] += wtlat[ji] * wtlon[ii] * amskin[n2D] * ain[nn];
			        }
			    }
		        }
		    }


		    /* find the weighted output for this single output io, jo point */ 
		
		    for (k2 = 0; k2 < mtim2; ++k2) {
		        iao[jtim2] = k2;
		   
		        for (k1 = 0; k1 < mtim1; ++k1) {
			    iao[jtim1] = k1;

			    kk = k2 * mtim1 + k1;

			    nn = iao[0] + leno[0] * (iao[1]  + leno[1] * (iao[ 2]
                                                                    + leno[2] * iao[3] ));

			    if (wtmsk[kk] > 0.) {
			        aout[nn] = accum[kk] / wtmsk[kk];
			        amskout[nn] = wtmsk[kk] / wt;
			    } 
			    else {
			        aout[nn] = omit;
			        amskout[nn] = 0.;
			    }
		        }
		    }
	        } 

	        else {

	 	    /*  no input grid cells will contribute */

		    for (k2 = 0; k2 < mtim2; ++k2) {
		        iao[jtim2] = k2;
		   
		        for (k1 = 0; k1 < mtim1; ++k1) {
			    iao[jtim1] = k1;
			    nn = iao[0] + leno[0] * (iao[1] + leno[1] * (iao[ 2]
                                                                   + leno[2] * iao[3] ));
			    aout[nn] = omit;
			    amskout[nn] = 0.;
		        }
		    }
	        }
	    }
        }
    }
    else {

        /*    cycle through output zones and longitudes */
        je = -1;
        for (jo = 0; jo < nlato; ++jo) {
	    iao[ilat] = jo;
	    jb = je + 1;
	    je = latdx[jo];

	    ie = -1;
	    for (io = 0; io < nlono; ++io) {
	        iao[ilong] = io;
	        ib = ie + 1;
	        ie = londx[io];

	        for (k = 0; k < mtim1*mtim2; ++k) {
		    wtmsk[k] = (double)0.;
		    accum[k] = (double)0.;
	        }

	        if (je >= jb && ie >= ib) {               /* has contributing grid points */
		    wt = (double)0.;

	      
		    for (ji = jb; ji <= je; ++ji) {
		        iai[ilat] = latpt[ji];
		  
		        for (ii = ib; ii <= ie; ++ii) {
			    iai[ilong] = lonpt[ii];
			    wt += wtlat[ji] * wtlon[ii];       /* weights for all level_times */

		            /* cycle through the level-time structure which is the the same
                               for  input and output and for the input zones and longitudes
                               contribution */

			    for (k2 = 0; k2 < mtim2; ++k2) {
			        iai[jtim2] = k2;
			   
			        for (k1 = 0; k1 < mtim1; ++k1) {
				    iai[jtim1] = k1;

				    kk = k2 * mtim1 + k1;

				    nn = iai[0] + leni[0] * (iai[1] + leni[1] * (iai[2]
                                                                          + leni[2] * iai[3] ));

				    wtmsk[kk] += wtlat[ji] * wtlon[ii] * amskin[nn];

				    accum[kk] += wtlat[ji] * wtlon[ii] * amskin[nn] * ain[nn];
			        }
			    }
		        }
		    }


		    /* find the weighted output for this single output io, jo point */ 
		
		    for (k2 = 0; k2 < mtim2; ++k2) {
		        iao[jtim2] = k2;
		   
		        for (k1 = 0; k1 < mtim1; ++k1) {
			    iao[jtim1] = k1;

			    kk = k2 * mtim1 + k1;

			    nn = iao[0] + leno[0] * (iao[1]  + leno[1] * (iao[ 2]
                                                                    + leno[2] * iao[3] ));

			    if (wtmsk[kk] > 0.) {
			        aout[nn] = accum[kk] / wtmsk[kk];
			        amskout[nn] = wtmsk[kk] / wt;
			    } 
			    else {
			        aout[nn] = omit;
			        amskout[nn] = 0.;
			    }
		        }
		    }
	        } 

	        else {

	 	    /*  no input grid cells will contribute */

		    for (k2 = 0; k2 < mtim2; ++k2) {
		        iao[jtim2] = k2;
		   
		        for (k1 = 0; k1 < mtim1; ++k1) {
			    iao[jtim1] = k1;
			    nn = iao[0] + leno[0] * (iao[1] + leno[1] * (iao[ 2]
                                                                   + leno[2] * iao[3] ));
			    aout[nn] = omit;
			    amskout[nn] = 0.;
		        }
		    }
	        }
	    }
        }
    }

    free(accum);
    free(wtmsk);

    /* ------------------------------------------------------- */

    /* NB! Don't use Py_BuildValue here, since it increments the reference count!
       PyArray_FromDims has already incremented it. */
    return PyArray_Return(object_amskout);
    
}


/*  ************************************************************************* 
*                                                                           *
*                           MAPLENGTH SYNOPSIS                              *
*                                                                           *
*  Maplength maps the input lengths into the output length for each output  *
*  point.                                                                   * 
*  Given an output cell centered on the point jo with the boundaries        *            
*                bnout(jo) ----- bsout(jo)                                  *
*  it finds the set of input points ji with the boundaries ,                *            
*                bnin(ji) ----- bsin(ji)                                    *
*                                                                           *
*  which have boundary lines inside the output boundary. The code finds the *
*  contributions in latitude.                                               *  
*                                                                           *
*  Assembling this set for latitudes is straight forward. It entails:       * 
*                                                                           *
*       1. Find indices ji such that bnin[ji] and/or bsin[ji] fall within   *
*          bnout[jo] ----- bsout[jo].                                       *
*                                                                           *
*       2. Record these indices in latpt[] in order output latitude by      *
*          latitude. The position of the last contributor to each jo is     *
*          recorded in latdx[jo].                                           *
*       3. Find the extend of each input cell (bnorth,bsouth) which lies in *
*          the output cell and store it as the latitude weight in wtlat[]   *
*                                                                           *
****************************************************************************/ 


/*  ************************************************************************* 
*                                                                           *
*                        MAPLENGTH USAGE                                    *
*                                                                           *
*  This subroutine provides the information needed to interpolate           *
*  from the input grid to the output grid (i.e., it calculates              *
*  the weight of the contribution from each of the input cells              *
*  to each of the output cells.                                             *
*                                                                           *
*  The input and output grid-cell boundaries must be specified by the       *
*  user(bnin, bsin, bnout), and the ordering must be monotonic, but need not*
*  be continuous (input or output cells may be missing). The units are      *
*  degrees latitude.                                                        *
*                                                                           *
*   The coordinate ordering may differ between input and output grids       *
*   (for example N to S on input, S to N on output.                         *
*                                                                           *
*                                                                           *
*   Input:                                                                  *
*                                                                           *
*    nlati = number of input grid cell latitudes.                           *
*    nlato = number of output grid cells latitudes.                         *
*    bnin(nlati) = northern boundary of each grid cell of input field.      *
*    bsin(nlati) = southern boundary of each grid cell of input field.      *
*    bnout(nlato) =northern boundary of each grid cell of output field.     *
*    bsout(nlato) =southern boundary of each grid cell of output field.     *
*                                                                           *
*   Output                                                                  *
*                                                                           *
*                                                                           *
*    latdx(nlato) = index of the last element in latpt and wtlat that       *
*                   apply to each of the the output latitudes               *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells     *
*                         that contribute to each output grid cell.         *
*    wtlat(nlati+nlato) = array of weights indicating how much each         *
*                         input grid cell contributes to each               *
*                         output grid cell.                                 *
*                                                                           *
****************************************************************************/ 

static PyObject *rgd_maplength(PyObject *self, PyObject *args) 
{    

    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int nlati;                              /* number of input latitudes */
    int nlato;                              /* number of output latitudes */

    float *bnin;                            /* northern boundary of grid cells of input field */
    float *bnout;                           /* northern boundary of grid cells of output field */
    float *bsin;                            /* southern boundary of grid cells of input field */
    float *bsout;                           /* southern boundary of grid cells of output field */

    PyArrayObject *object_bnin;             /* object pointer northern boundary of grid cells of input field */
    PyArrayObject *object_bnout;            /* object pointer northern boundary of grid cells of output field */
    PyArrayObject *object_bsin;             /* object pointer southern boundary of grid cells of input field */
    PyArrayObject *object_bsout;            /* object pointer southern boundary of grid cells of output field */

    /* fields which are returned to Python */

    int *latdx;                             /* index of last element in lonpt & wtlon for this cell */
    int *latpt;                             /* indices of input grid cells for this cell */
    float *wtlat;                           /* weights for input cell's contributions to this cell */

    /* added local variables */

    int nlatsize;                           /* sum number of input & output latitudes */

    /* local variables */

    float pi, bnorth, bsouth;
    int last;                               /* storage index  in latpt,wtlat,lonpt,wtlon */
    int j1, j2, j3;                         /* indices used in finding input contributors*/
    int i;                                  /* loop index */
    int ji, jo;                             /* indices used in input ouput loops */
    int ierr1, ierr2;                       /* malloc check */ 
    int ii, ip;                             /* used to find input cell straddling the out boundary */
    int found;                              /* used as a flag in breaking out fo loops */
    int iflag;                              /* used as a flag */
    float *bwinl, *beinl;
      
    int size;                       /* used file write */
    FILE *fp;                       /* file used in ascii write */ 
    FILE *fpin;                       /* file used in ascii write */ 

    char *title[3] = {              /* titles for print to file */ 
       "latdx", "latpt ", "wtlat"
    };

    char *titlesin[4] = {                                            /* titles for print to file */ 
       "bnin ", "bnout ", "bsin ", "bsout "
    };

    /* python array transform variables */

    PyArrayObject *object_latdx;      /* object pointer to 1D array to hold data */
    PyArrayObject *object_latpt;      /* object pointer to 1D array to hold data */
    PyArrayObject *object_wtlat;      /* object pointer to 1D array to hold data */


    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "iiOOOO", &nlati, &nlato, &object_bnin, &object_bnout, &object_bsin, &object_bsout ))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to maplength is wrong.\n");
        return NULL;
    }

    /* assign c pointers to data in the PyArrayOjects passed in */

    bnin  = (float *)object_bnin->data; 
    bnout = (float *)object_bnout->data; 
    bsin  = (float *)object_bsin->data; 
    bsout = (float *)object_bsout->data; 

#ifdef DEBUG
    /* -------- write  input data to a file ----------- */

    /*
    if((fpin = fopen("maplength_in.asc", "w")) == NULL) {
        PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
        return NULL;
    }

    fprintf(fpin, "nlati: %d\n", nlati);
    fprintf(fpin, "nlato: %d\n", nlato);

    write_float(nlati, titlesin[0], fpin, bnin);
    write_float(nlato, titlesin[1], fpin, bnout);
    write_float(nlati, titlesin[2], fpin, bsin);
    write_float(nlato, titlesin[3], fpin, bsout);

    close(fpin);
    */
#endif
    /* ------------------------------------------------ */

    nlatsize = nlati + nlato; 

    /* create the 1D python array objects */

    object_latdx = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlato, PyArray_INT);
    if(!object_latdx) PyErr_NoMemory();

    object_latpt = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlatsize, PyArray_INT);
    if(!object_latpt) PyErr_NoMemory();

    object_wtlat = (PyArrayObject *)PyArray_FromDims(1, (int *)&nlatsize, PyArray_FLOAT);
    if(!object_wtlat) PyErr_NoMemory();


    /* assign c pointers to the data attribute in the python array  objects */

    latdx = (int *)object_latdx->data;
    latpt = (int *)object_latpt->data;
    wtlat = (float *)object_wtlat->data;

    zero_int(nlato, latdx);
    zero_int((nlatsize), latpt);
    zero_float((nlatsize), wtlat);


    /* ------------------------------------------------------- */

    pi = 4.*atan(1.0);
      

/*  cycle through output zones */

    if ((bsin[nlati-1] - bsin[0]) * (bsout[nlato-1] - bsout[0]) > 0.) {
	j1 = 0;
	j2 = nlati - 1;
	j3 = 1;
    } 

    else {
	j1 = nlati - 1;
	j2 = 0;
	j3 = -1;
    }

    last = -1;

    for (jo = 0; jo < nlato; ++jo) {
	iflag = 0;

	/* find index of input latitude zones that will contribute. */

	for (ji = j1; j3 < 0 ? ji >= j2 : ji <= j2; ji += j3) {

	    if (bsin[ji] < bnout[jo] && bnin[ji] > bsout[jo]) {
		iflag = 1;
		++last;
		latpt[last] = ji;
		bnorth = min( bnout[jo], bnin[ji] );
		bsouth = max( bsout[jo], bsin[ji] );
		wtlat[last] = sin( bnorth*pi/180. ) - sin( bsouth*pi/180. );
	    }

	     else if (iflag == 1) {
		break;
	    }
	}

	latdx[jo] = last;
	if (last != -1) {
	    j1 = latpt[last];
	}
    }

    /* ------------------------------------------------------- */

    /* ------- write results that are passed to python ------- */

#ifdef DEBUG
    /*
    if((fp = fopen("maplength_out.asc", "w")) == NULL) {
        PyErr_SetString(PyExc_IOError, "Can not open file for maparea checks");
        return NULL;
    }

    write_int(nlato, title[0], fp, latdx);
    write_int((nlatsize), title[1], fp, latpt);
    write_float((nlatsize), title[2], fp, wtlat);

    close(fp);
    */
#endif
    /* ------------------------------------------------------- */


    return Py_BuildValue(("OOO"), object_latdx, object_latpt, object_wtlat);
} 


/*  ********************************************************************* 
*                                                                       *
*                         RGDLENGTH SYNOPSIS                            *
*                                                                       *
*  Rgdlength makes an length weighted average of all the input cell data*
*  which contributes to the output data for each particular output cell.*
*                                                                       *
*  For each output cell centered on the point jo, it finds the input    *
*  cells contributing using:                                            *
*                                                                       *
*   jb--je: the begin and end latitudes with index je from the latpt    *
*           array and the corresponding weights from the wtlat array    *
*                                                                       *
*  The weights provide a measure of the overlap of the input cell       *
*  lengths into the output cell lengths. These contributions are the    *
*  same for all levels and all times. The structure in level-time is the*
*  same for the input and output fields. A straight forward length      *
*  weighted calculation produces the regridded data.                    *
*                                                                       *
*  The routine requires a mask, amskin, of 1.0s and 0.0s which is the   *
*  same size as the input data including levels and times. It generates *
*  the corresponding mask for the outdata, amskout. This mask can be    *
*  used to regrid the output data                                       * 
*                                                                       *
*************************************************************************/ 



/*  ********************************************************************* 
*                                                                       *
*                          RGLENGTH USAGE                               *
*                                                                       *
*   This subroutine takes an input field (ain) of grid cells and fills  *
*   an output field (aout) of grid cells with length-weighted values,   *
*   thereby achieving a 1D transformation from one grid to another.     *
*                                                                       *
*   The program uses pointers and weights generated outside this        *
*   routine to determine how much each input grid cell contributes      *
*   to each output grid cell                                            *
*                                                                       *
*   Besides the automatic area weighting, an additional user-supplied   *
*   weighting factor (amskin) must be passed to this subroutine, which  *
*   multiplies the area weighting.  If simple length-weighting is needed*
*   , supply a vector of 1.''s in the input.  However, if for example   *
*   only* land values should contribute to the new gridded field, then  *
*   1.''s should be assigned for land points, and 0.''s for ocean points*
*                                                                       *
*                                                                       *
*   Input:                                                              *
*                                                                       *
*    ilat = dimension position for latitude in input and output arrays  *
*          = 0 if first dimension                                       *
*          = 1 if second dimension                                      *
*          = 2 if third dimension                                       *
*    itim1 = dimension position for time or level in input and output   *
*              arrays (if no itim1 dimension, then this parameter is    *
*              ignored)                                                 *
*    itim2 = dimension position for time or level in input and output   *
*              arrays (if no itim2 dimension, then this parameter is    *
*              ignored)                                                 *
*    nlati = number of input grid cell latitudes.                       *
*    nlato = number of output grid cells latitudes.                     *
*                                                                       *
*    ntim1 = length of itim1 dimension (if this is a dummy dimension,   *
*              you should set ntim1 to 0)                               *
*    ntim2 = length of itim2 dimension (if this is a dummy dimension,   *
*              you should set ntim2 to 0)                               *
*                                                                       *
*    latdx(nlato) = index of the last element in latpt and wtlat that   *
*                   apply to each of the the output latitudes           *
*    latpt(nlati+nlato) = array of indices pointing to input grid cells *
*                         that contribute to each output grid cell.     *
*    wtlat(nlati+nlato) = array of weights indicating how much each     *
*                         to input grid cells that contribute to each   *
*                         output grid cell.                             *
*    omit = value that will be assigned grid points that have either    *
*       been masked out or cannot be computed for lack of input data.   *
*                                                                       *
*                                                                       *
*    Input and Output:                                                  *
*                                                                       *    
*    amskin(*) = input weighting factors (for masking purposes).        *
*    ain(*) = input field                                               *
*                                                                       *
*                                                                       *
*    amskout(*) = output weighting factors                              *
*    aout(*) = output (regridded) field                                 *
*                                                                       *
*                                                                       *
*************************************************************************/ 

static PyObject *rgd_rgdlength(PyObject *self, PyObject *args) 
{     

    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int ilat;                               /* dimension position for latitude */
    int itim1;                              /* dimension position for time or level*/
    int itim2;                              /* dimension position for time or level*/
    int ntim1;                              /* length of itime1 dimension */
    int ntim2;                              /* length of itime2 dimension */

    int nlati;                              /* number of input latitudes */
    int nlato;                              /* number of output latitudes */

    float omit;                             /* missing value */

    int *latdx;                             /* index of last element in lonpt & wtlon for this cell */
    int *latpt;                             /* indices of input grid cells for this cell */
    float *wtlat;                           /* weights for input cell's contributions to this cell */

    float *amskin;                          /* input mask */
    float *ain;                             /* input data */
    float *aout;                            /* output data */

    PyArrayObject *object_latdx;            /* object pointer to latdx */
    PyArrayObject *object_latpt;            /* object pointer to latpt */
    PyArrayObject *object_wtlat;            /* object pointer wtlat */

    PyArrayObject *object_amskin;           /* object pointer to amskin */
    PyArrayObject *object_ain;              /* object pointer to amskin */
    PyArrayObject *object_aout;             /* object pointer to aout */
     

    /* added local variables */

    int datasize;                           /* size of output data */
    int number_dim;                         /* number of dimensions in the data */
    int dim_array[3] = {0,0,0};             /* array with size of the array */
    int nlatsize;                           /* nlati + nlato  */

    FILE *fpin;                             /* for ascii write of input data */

    char *titlesin[3] = {                   /* titles for print to file */ 
       "latdx ", "latpt ", "wtlat "
    };

    /* original local variables */
     
    int id[3];                     /* order of the dimensions */
    int nd[3];                     /* size of the dimensions */

    int found;                     /* used with break out of for loops */

    int imiss1, imiss2;            /* store the mising dimension numbers */   
    int jtim1, jtim2;              /* storing itim1(2) or imiss1(2) */ 
    int mtim1, mtim2;              /* storing mtim1(2) or 1 if missing */
    
    int i, j, k;                   /* loop indices */

    int leni[3], leno[3];          /* vector used in striding through arrays */
    int iai[3];                    /* holds 4 indices for input point */
    int iao[3];                    /* holds 4 indices for output point */  

    int ierr1, ierr2;              /* used in malloc check */           
    
    int ji;                        /* loop indices used for latitudes */    
    int jo;                        /* loop indices used for latitudes */    
    int jb, je;                   /* loop limits for contributing input latitudes */
    
    int nn;                       /* index into data arrays */  
    int kk;                       /* index into level-times */
    int k1, k2;                   /* loop indices for level-times */    

    double *accum; 
    double *wtmsk;
    double wt;

    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "iiiiiiifOOOOOO", &ilat, &itim1, &itim2, &ntim1, &ntim2, &nlati, &nlato, &omit,
                                                  &object_latdx, &object_latpt, &object_wtlat,
                                                  &object_amskin, &object_ain, &object_aout ))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to rgdlength is wrong.\n");
        return NULL;
    }

    latdx  = (int *)object_latdx->data; 
    latpt  = (int *)object_latpt->data; 
    wtlat  = (float *)object_wtlat->data; 

    amskin  = (float *)object_amskin->data; 
    ain  = (float *)object_ain->data; 
    aout  = (float *)object_aout->data; 

    nlatsize = nlati + nlato; 

    /* allocate memory for arrays constructed in this routine to pass to python*/

    /* find the number of dimensions and the total size */
    number_dim = 1;
    datasize = nlato;
    dim_array[ilat] = nlato;


    if(ntim1 > 0) {
        datasize = datasize* ntim1;
        number_dim++;
        dim_array[itim1] = ntim1;
    }
    if(ntim2 > 0) {
        datasize = datasize* ntim2;
        number_dim++;
        dim_array[itim2] = ntim2;
    }

    /* -------- write  input data to a file ----------- */

#ifdef DEBUG
    /*
    if((fpin = fopen("rgdlength_in.asc", "w")) == NULL) {
        PyErr_SetString(PyExc_IOError, "Can not open file to rgdarea checks");
        return NULL;
    }

    fprintf(fpin, "ilat: %d\n", ilat);
    fprintf(fpin, "itim1: %d\n", itim1);
    fprintf(fpin, "itim2: %d\n", itim2);
    fprintf(fpin, "ntim1: %d\n", ntim1);
    fprintf(fpin, "ntim2: %d\n", ntim2);

    fprintf(fpin, "nlati: %d\n", nlati);
    fprintf(fpin, "nlato: %d\n", nlato);

    fprintf(fpin, "omit: %f\n", omit);

    write_int(nlato, titlesin[0], fpin, latdx);
    write_int((nlatsize), titlesin[1], fpin, latpt);
    write_float((nlatsize), titlesin[2], fpin, wtlat);


    close(fpin);
    */
#endif
    /* ------------------------------------------------ */

    /* Store the order (0 thru3) and the corresponding lengths of the dimensions */

    id[0] = ilat;
    id[1] = itim1;
    id[2] = itim2;

    nd[0] = nlati;
    nd[1] = ntim1;
    nd[2] = ntim2;

    for (i = 1; i < 3; ++i) {     /* ntim1 and/or ntim2 = 0 if no dimension */
	if (nd[i] == 0) {
	    id[i] = -1;
	}
    }

    imiss1 = 0;
    imiss2 = 0;

    for (k = 0; k < 3; ++k) {

	found = 0;

	for (i = 0; i < 3; ++i) {

	    if (id[i] == k) {
		if (k < 2) {                       /* look for duplicates */

		    for (j = i + 1; j < 3; ++j) {
			if (id[j] == k) {
	  
			    printf("Error in specifying data structure while attempting\n");
			    printf("to regrid (horizontally interpolate) data.\n");
			    printf("You have set:\n");
			    printf("ilat =  %d\n", ilat);
			    printf("itim1 = %d\n", itim1);
			    printf("itim2 = %d\n", itim2);
			    printf("One of these should = 0, another = 1, and another = 2\n");
			    printf("If itim1 and/or itim2 are dummy dimensions, then\n");
			    printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
			    printf("itim1 and/or itim2 will be ignored.\n");
			    printf("Duplicates error in rgdlength\n"); 
			    exit(1);
			}
		    }
		}
		found = 1;        /* found a id[] = k match for this k */
		break;            /* break out of for over i after a match */
	    }
	}

	if(!found) {
	    if (!imiss1) {
		imiss1 = k;
	    } 
	    else {
		imiss2 = k;
	    }
	}
    }

    /* transfer itim1(2) and ntim1() into jtim1(2) and mtim1(2) to 
       set mtim1(2) = 1 when dimension is missing and jtim1(2) to
       the position of the mising dimensions for check below */

    if (ntim1 > 0) {
	jtim1 = itim1;
	mtim1 = ntim1;
    } 
    else {
	mtim1 = 1;
	jtim1 = imiss1;
    }

    if (ntim2 > 0) {
	jtim2 = itim2;
	mtim2 = ntim2;
    } else if (imiss2 > 0) {
	jtim2 = imiss2;
	mtim2 = 1;
    } else {
	jtim2 = imiss1;
	mtim2 = 1;
    }

    if (ilat + jtim1 + jtim2 != 3) {

	printf("Error in specifying data structure while attempting\n");
	printf("to regrid (horizontally interpolate) data.  You have set:\n");
        printf("ilat =  %d\n", ilat);
        printf("itim1 = %d\n", itim1);
        printf("itim2 = %d\n", itim2);
        printf("One of these should = 0, another = 1, another = 2, and another = 3\n");
        printf("If itim1 and/or itim2 are dummy dimensions, then\n");
        printf("be sure that ntim1 and/or ntim2 are 0, so that\n");
	printf("itim1 and/or itim2 will be ignored.\n");
	printf("Data structure error in rgdlength\n"); 
	exit(1);
    }

    /* store the dimension lengths noting that jtim1(2), level-time are the 
       same in the input and output fields */

    leni[ilat]  = nlati;
    leni[jtim1] = mtim1;
    leni[jtim2] = mtim2;

    leno[ilat]  = nlato;
    leno[jtim1] = mtim1;
    leno[jtim2] = mtim2;


    /* allocate enough space for double precision variables */ 

    ierr1 = 0;
    ierr2 = 0;

    if( !(accum  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr1 = 1;
    if( !(wtmsk  = (double *)malloc(mtim1*mtim2*sizeof(double))) )
	ierr2 = 1;
    
    if (ierr1 || ierr2 ) {
	printf("Error in attempting to allocate memory dynamically.\n");
	printf("You may need more memory.\n");
	printf("Try retrieving smaller portions of the array.\n");
	printf("Error in rgdlength\n");
	exit(1);
    }

/*    cycle through output zones */

    je = -1;
    for (jo = 0; jo < nlato; ++jo) {
	iao[ilat] = jo;
	jb = je + 1;
	je = latdx[jo];

        for (k = 0; k < mtim1*mtim2; ++k) {
	    wtmsk[k] = (double)0.;
	    accum[k] = (double)0.;
        }

        if (je >= jb ) {               /* has contributing grid points */
            wt = (double)0.;

	      
	   for (ji = jb; ji <= je; ++ji) {
	        iai[ilat] = latpt[ji];
		  
		wt += wtlat[ji];                 /* weights for all level_times */

	        /* cycle through the level-time structure which is the the same
                   for  input and output and for the input zones contribution */

		for (k2 = 0; k2 < mtim2; ++k2) {
		    iai[jtim2] = k2;
			   
		    for (k1 = 0; k1 < mtim1; ++k1) {
			iai[jtim1] = k1;

			kk = k2 * mtim1 + k1;

			nn = iai[0] + leni[0] * (iai[1] + leni[1] * iai[2]);

			wtmsk[kk] += wtlat[ji] * amskin[nn];

			accum[kk] += wtlat[ji] * amskin[nn] * ain[nn];
		    }
		}
           }


		/* find the weighted output for this single output io, jo point */ 
		
		for (k2 = 0; k2 < mtim2; ++k2) {
		    iao[jtim2] = k2;
		   
		    for (k1 = 0; k1 < mtim1; ++k1) {
			iao[jtim1] = k1;

			kk = k2 * mtim1 + k1;

			nn = iao[0] + leno[0] * (iao[1]  + leno[1] * iao[ 2]);

			if (wtmsk[kk] > 0.) 
			    aout[nn] = accum[kk] / wtmsk[kk];
			else 
			    aout[nn] = omit;
		    }
		}
	 } 

	 else {

		/*  no input grid cells will contribute */

	    for (k2 = 0; k2 < mtim2; ++k2) {
	        iao[jtim2] = k2;
		   
	        for (k1 = 0; k1 < mtim1; ++k1) {
	            iao[jtim1] = k1;
		    nn = iao[0] + leno[0] * (iao[1] + leno[1] * iao[ 2]);
		    aout[nn] = omit;
	        }
	    }
         }
    }


    free(accum);
    free(wtmsk);

    Py_INCREF(Py_None);
    return Py_None;
}

/*     ------------------------------------------------------------------
*
*     rgdpressure 
*
*     purpose: 
*           call the linear interpolation routine to regrid pressure 
*           column by column after the latitude has been regridded.
*
*      passed:
*           n = size of original pressure grid
*           np = size of new (primed) pressure grid
*           nlat = size of final latitude grid
*           ntim2 =  number of times
*           missing = missing value
*           x = original pressure grid values
*           xp = new pressure grid values
*           a = original data values
*           ap = regridded data template waiting for the data
*
*-----------------------------------------------------------------------*/

static PyObject *rgd_rgdpressure(PyObject *self, PyObject *args) 
{     

    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int n;                                     /* size of original pressure grid */
    int np;                                    /* size of new (primed) pressure grid */
    int nlat;                                  /* size of final latitude grid */
    int nlon;                                  /* size of longitude grid */
    int ntim2;                                 /* number of times */
    float missing;                             /* missing data value */
    char *missingMatch;                        /* a string greater, equal or less */
    char *logYes;                              /* a string yes for use of log of pressure*/

    double *x;                                 /* original pressure grid values */
    double *xp;                                /* new pressure grid values */

    float *a;                                  /* original data values */
    float *ap;                                 /* regridded data */

    PyArrayObject *object_x;                   /* object pointer to array to hold x */
    PyArrayObject *object_xp;                  /* object pointer to array to hold xp */
    PyArrayObject *object_a;                   /* object pointer to array to hold a */
    PyArrayObject *object_ap;                  /* object pointer to array to hold ap */


    /* added local variables */

    FILE *fpin;                             /* for ascii write of input data */

    char *titlesin[2] = {                   /* titles for print to file */ 
       "source_pressure", "sink_pressure"
    };

    /* original local variables */
     
    int index;                                 /* index into original data */
    int indexp;                                /* index into regridded data */

    double *lnx;                               /* ln of original grid values */
    double *lnxp;                              /* ln of interpolated grid values(primed) */

    float *y;                                  /* original data values */
    float *yp;                                 /* interpolated data values(primed) */

    int t,i,j,k,kp;                            /* for indices */

    char *logCheck = "yes";
    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "iiiiifssOOOO", &n, &np, &nlat, &nlon, &ntim2, &missing, &missingMatch, &logYes,
                                         &object_x, &object_xp, &object_a, &object_ap))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to rgdpressure is wrong.\n");
        return NULL;
    }

    x  = (double *)object_x->data; 
    xp  = (double *)object_xp->data; 
    a  = (float *)object_a->data; 
    ap  = (float *)object_ap->data; 

    /* -------- write  input data to a file ----------- */

    /*
    if((fpin = fopen("rgdpressure_in.asc", "w")) == NULL) {
        PyErr_SetString(PyExc_IOError, "Can not open file to rgdarea checks");
        return NULL;
    }

    fprintf(fpin, "n: %d\n", n);
    fprintf(fpin, "np: %d\n", np);
    fprintf(fpin, "nlat: %d\n", nlat);
    fprintf(fpin, "ntim2: %d\n", ntim2);
    fprintf(fpin, "missing: %f\n", missing);
    fprintf(fpin, "missingMatch: %s\n", missingMatch);

    write_double(n, titlesin[0], fpin, x);
    write_double(np, titlesin[1], fpin, xp);

    close(fpin);
    */
    /* ------------------------------------------------ */
    /* ------------------------------------------------------------------- */

    lnx = (double *)malloc(n*sizeof(double));
    if(!lnx) {
        printf("memory request failed for lnx");
        exit(1);
    }
    zero_double(n, lnx);

    lnxp = (double *)malloc(np*sizeof(double));
    if(!lnxp) {
        printf("memory request failed for lnxp");
        exit(1);
    }
    zero_double(np, lnxp);

    y = (float *)malloc(n*sizeof(float));
    if(!y) {
        printf("memory request failed for y");
        exit(1);
    }
    zero_float(n, y);
  
    yp = (float *)malloc(np*sizeof(float));
    if(!yp) {
        printf("memory request failed for yp");
        exit(1);
    }
    zero_float(np, yp);

   /* ------------------------------------------------------------------- */

    /* make log of pressure levels if requested */

    if(!strcmp(logCheck, logYes)) {
        for(k=0; k<n; k++) 
            lnx[k] = log( x[k] );

        for(kp=0; kp<np; kp++)  
            lnxp[kp] = log( xp[kp] );
    }
    else {
        for(k=0; k<n; k++) 
            lnx[k] = x[k];

        for(kp=0; kp<np; kp++)  
            lnxp[kp] = xp[kp];
    }
 
    /* regrid data */


    /*check if there are no longitudes*/
    if(nlon == 0)
      nlon=1;
    /*check if there are no latitudes*/
    if(nlat == 0)
      nlat=1;
    /*check if there are no times*/
    if(ntim2 == 0)
      ntim2=1;

    for(t=0; t<ntim2; t++) {
        for(j=0; j<nlat; j++) {
            for(i=0; i<nlon; i++) {

                for(k=0; k<n; k++) {               /* select pressure column to regrid */ 
                     index = i + nlon*j + nlon*nlat*k + nlon*nlat*n*t;
                     y[k] = *(a + index); 
                }

                cd_linear_interpolation(n, lnx, y, np, lnxp, yp, missing, missingMatch);

                for(kp=0; kp<np; kp++) {                    /* store regridded pressure column */ 
                    indexp = i + nlon*j + nlon*nlat*kp + nlon*nlat*np*t;
                    *(ap + indexp) = yp[kp];
                }
            }
      
        }
    }

    free(lnx);
    free(lnxp);
    free(y);
    free(yp);

    Py_INCREF(Py_None);
    return Py_None;

}

 /*************************************************************************
 *                                                                        *
 * METHOD REGISTRATION TABLE: NAME-STRING -> FUNCTION-POINTER
 * 
 *                                                                        *
\**************************************************************************/

static struct PyMethodDef rgd_methods[] = {

  { "gridattr",      (PyCFunction)rgd_gridattr,     1 },
  { "pressattr",     (PyCFunction)rgd_pressattr,    1 },
  { "maparea",       (PyCFunction)rgd_maparea,      1 },
  { "rgdarea",       (PyCFunction)rgd_rgdarea,      1 },
  { "maplength",     (PyCFunction)rgd_maplength,    1 },
  { "rgdlength",     (PyCFunction)rgd_rgdlength,    1 },
  { "rgdpressure",   (PyCFunction)rgd_rgdpressure,  1 },
  {  NULL,      NULL }
};


 /*************************************************************************
 *                                                                        *
 * INITIALIZATION FUNCTION        
 * 
 *                                                                        *
\**************************************************************************/

void init_regrid()
{
  PyObject *m, *d;
  
  /* create this module and add the functions */
  m = Py_InitModule("_regrid", rgd_methods);
  import_array();

  /* add symbolic constants to the module */
  d = PyModule_GetDict(m);
  ErrorObject = Py_BuildValue("s", "_regrid.error"); 
  PyDict_SetItemString(d, "error", ErrorObject);  

  /* check for errors */
  if(PyErr_Occurred())
    Py_FatalError("can't initialize module _regrid");
}



 /*************************************************************************
 *                                                                        *
 * C FUNCTIONS USED IN THE C-EXTENSION GAUSSATTR                       
 *                                                                        *
\**************************************************************************/


/********************************************************************************
* Function:   zero_double.c
*
*  Purpose:   Initializes array to 0.0
*
********************************************************************************/
void zero_double(int size, double *data)
{
  int n;         /* loop index */

  for(n=0; n<size; n++)
    *data++ = 0.0;
}

/********************************************************************************
* Function:   zero_int.c
*
*  Purpose:   Initializes array to 0.0
*
********************************************************************************/
void zero_int(int size, int *data)
{
  int n;         /* loop index */
  int *d;        /* pointer to data used in the loop */

  d = data;

  for(n=0; n<size; n++)
    *d++ = 0.0;
}

/********************************************************************************
* Function:   write_double.c
*
*  Procedure: Uses xxxxxf format to write 4 numbers per line
*  Purpose:   write ascii data for one horizontal field or one cross section
*
*  Passed:    data,  pointer to start of data
*             size,  length of data
*             title, label
*             fp,    file pointer
********************************************************************************/

void write_double(int size, char *title, FILE *fp, double *data)
{

  int n;               /* data counter */
  int line;            /* counter to insert a line after each six numbers */
  double *d;           /* pointer to increment through data */

  d = data;

  line = 0;

  fprintf(fp, "\n%s\n", title);

  for(n=0; n<size; n++) {

    fprintf(fp, "%21.15lf", *d);

    d++;
    line++;

    if(line == 4) {          /* add newline after writing 8 numbers */
      fprintf(fp, "\n");
      line = 0;
    }

  }
}

/*
* Function:   write_float.c
*
*  Procedure: Uses 10.3e format to write 6 numbers per line
*  Purpose:   write ascii data for one horizontal field or one cross section
*
*  Passed:    Data, pointer to start of data
*/

void write_float(int size, char *title, FILE *fp, float *data)
{

  int n;               /* data counter */
  int line;            /* counter to insert a line after each six numbers */
  float *d;            /* pointer to increment through data */

  d = data;

  line = 0;

  fprintf(fp, "\n%s\n", title);

  for(n=0; n<size; n++) {

    fprintf(fp, "%10.3e", *d);

    d++;
    line++;

    if(line == 8) {          /* add newline after writing 8 numbers */
      fprintf(fp, "\n");
      line = 0;
    }

  }
}


/*
* Function:   write_int.c
*
*  Procedure: Uses 5d format to write 6 numbers per line
*  Purpose:   write ascii data for one horizontal field or one cross section
*
*  Passed:    Data, pointer to start of data
*/

void write_int(int size, char *title, FILE *fp, int *data)
{

  int n;               /* data counter */
  int line;            /* counter to insert a line after each six numbers */
  int *d;              /* pointer to increment through data */

  d = data;

  line = 0;

  fprintf(fp, "\n%s\n", title);

  for(n=0; n<size; n++) {

    fprintf(fp, "%5d", *d);

    d++;
    line++;

    if(line == 16) {          /* add newline after writing 16 numbers */
      fprintf(fp, "\n");
      line = 0;
    }

  }

}



 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   uniform_latitude_grid.c                                          *
 *                                                                         *
 *  Purpose:    To generate a uniform global latitude grid                 *
 *                                                                         *
 *  Passed:    nlat      number of latitudes                               *
 *                                                                         *
 *  Return:    bnds       array  boundary values                            *
 *             pts       array with grid point values                      *
 *             wts       array with weights                                *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


void uniform_latitude_grid(int nlat, double *pts, double *wts, double *bnds)
{
    int i;                    /* loop index */
    double delta;             /* increment in latitude */   
    double pi;
    double rad;               /* convert to radians */


    pi = 4.0 * atan(1.0);
    rad = pi/((double)180.0);

    delta = ((double)180.0 )/( (double)(nlat- 1));

    /* construct the bounds */

    bnds[0] = 90.0;
    bnds[1] = 90.0 - delta/2.;

    bnds[nlat-1] = - bnds[1];
    bnds[nlat] = -bnds[0];

    for(i = 2; i < nlat - 1 ; i++)
        bnds[i] = bnds[i-1] - delta;    


    /* construct the  points */ 

    pts[0] = 90.0;

    for(i=1; i<nlat; i++)
        pts[i] = pts[i-1] - delta;
    
    
    /* construct the weights */

    for(i = 0; i < nlat ; i++)
        wts[i] = sin(rad * bnds[i]) - sin(rad * bnds[i+1]);
}

 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *
*                                                                         *
*  Function:   uniform_longitude_grid.c                                     *
*                                                                         *
*  Purpose:    To generate a uniform longitude grid                       * 
*                                                                         *
*  Passed:     nlon      number of longitudes                             * 
*              blon      beginning longitude to find E-W direction        *
*              elon      ending longitude to find E-W direction           *
*                                                                         *
*  Return:     pts       array with grid point values                     *
*              wts       array of weights                                 * 
*              bnds      array with boundary values                       *
*                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void uniform_longitude_grid(int nlon, float blon, float elon, double *pts, double *wts, double *bnds)
{
    int i;                                               /* loop index */

    double delta;                                        /* increment in longitude */

    delta = ((elon -blon) / ( (double)(nlon - 1)) );     /* assume west to east */

    pts[0] = blon;
    bnds[0] = blon - delta / 2.0;
    wts[0] = delta;

    for(i = 1; i <= nlon ; i++) {
        bnds[i] = bnds[i-1] + delta;
    }

    for(i = 1; i < nlon ; i++) {
        pts[i] = pts[i-1] + delta;
        wts[i] = delta;
    }
}

 /* Function:   zero_float.c
 *
 *  Purpose:   Initializes array with data[][][] = 0.0
 */
void zero_float(int size, float *data)
{
  int n;         /* loop index */
  float *d;      /* pointer to data used in the loop */

  d = data;

  for(n=0; n<size; n++)
    *d++ = 0.0;
}


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   equalarea_grid.c                                        *
 *                                                                         *
 *  Purpose:    To construct the equal area grid weights and boundaries    *
 *                                                                         *
 *  Passed:     nlat      number of latitudes                              *
 *                                                                         *
 *  Return:     pts       array with grid point values                     *
 *              wts       array of weights                                 * 
 *              bnds      array with boundary values                       *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void equalarea_grid(int nlat, double *pts, double *wts, double *bnds)
{
  double pi;                    /* pi */
  double degrees;               /* convert to degrees */
  int i;                        /* loop index */

  double *sinb;                 /* sin of the boundary values */
  double *sinpts;               /* sin of grid points */

  double pwconst;               /* the constant weight */

  pi = 4.0 * atan(1.0);
  degrees = ((double)180.0)/pi;

  sinb = (double *)malloc((nlat + 1)*sizeof(double));
  if(!sinb) {
      printf("memory request failed for sinb");
      exit(1);
  }
  zero_double(nlat+1, sinb);

  pwconst = 2.0/((double)nlat); 

  sinb[0] = 1.0;
  sinb[nlat] = -1.0;

  sinpts = (double *)malloc(nlat*sizeof(double));
  if(!sinpts) {
      printf("memory request failed for sinpts");
      exit(1);
  }

  sinpts[0] = sinb[0] - (pwconst/2.);

  for(i=1; i<nlat; i++) {
      sinpts[i] = sinpts[i-1] - pwconst;
  } 

  for(i=1; i<nlat; i++) {
      sinb[i] = sinb[i-1] -  pwconst;
  }

  /* convert to angles in degrees */

  for(i=0; i<=nlat; i++) {
      bnds[i] = degrees * asin( sinb[i]);

  } 

  for(i=0; i<nlat; i++) {
      wts[i] = pwconst;

      pts[i] = degrees * asin( sinpts[i]);
  } 
  free(sinb);
  free(sinpts);
}


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   gaussian_grid.c                                            *
 *                                                                         *
 *  Purpose:    To call gauaw to get gaussian grid parameters              *
 *                                                                         *
 *  Passed:    nlat      number of latitudes                               *
 *                                                                         *
 *  Return:     pts       array with grid point values                     *
 *              wts       array of weights                                 * 
 *              bnds      array with boundary values                       *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void gaussian_grid(int nlat, double *pts, double *wts, double *bnds)
{
  double pi;                    /* pi */
  double degrees;               /* convert to degrees */
  int i;                        /* loop index */

  double *sinpts;                   /* sin of the grid points */
  double *sinb;                 /* sin of the boundary values */

  double sign;                  /* sign -- + or - 1.0 */

  pi = (double)4.0 * atan((double)1.0);
  degrees = ((double)180.0)/pi;

  sinpts = (double *)malloc(nlat*sizeof(double));
  if(!sinpts) {
      printf("memory request failed for sinpts");
      exit(1);
  }
  zero_double(nlat, sinpts);

  sinb = (double *)malloc((nlat + 1)*sizeof(double));
  if(!sinb) {
      printf("memory request failed for sinb");
      exit(1);
  }
  zero_double(nlat+1, sinb);

  gauaw(sinpts, wts, nlat); 


  sinb[0] = 1.0;
  sinb[nlat] = -1.0;

  for(i=1; i<=nlat/2; i++) {
      sinb[i] = sinb[i-1] -  wts[i-1];
      sinb[nlat-i] = -sinb[i];
  }

  /* convert to angles in degrees */

  for(i=0; i<=nlat; i++) {
      bnds[i] = degrees * asin(sinb[i]);
  }

  for(i=0; i<nlat; i++) {
      pts[i] =  degrees * asin(sinpts[i]);
  }
  free(sinpts);
  free(sinb);
}


 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * *
 *                                                                         *
 *  Function:   cd_cint.c                                                  *
 *                                                                         *
 *  Purpose:    To generate a the number od 360 degree shifts in maparea   *
 *                                                                         *
 *  Passed:    value     angle/360.                                        *
 *                                                                         *
 *  Return:    offset    number of shifts                                  *
 *                                                                         *
 *  Note:      Replaces fint in fortran code ( does not need the 0.5)      * 
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

int cd_cint(float value)
{
   int offset;
   double small = 0.000001;

   if (value >= 0.0) {
       offset = (int)( ceil((double)value + small) );
   }

   if (value < 0.0) {
       offset = (int)( floor((double)value - small) + 1.0 );
   }
   return offset;
}

 /*     ------------------------------------------------------------------
 *
 *     cd_linear_interpolation 
 *
 *     purpose: 
 *           One dimensional cd_linear_interpolation from y(x) to
 *           yp(xp), where p stands for prime ( or interpolated).
 *           The original data and the interpolated data are 
 *           dimensioned at d and dp respectively.
 *     procedure:
 *           Given a point xp[ip], the bracketing points in the 
 *           original data are x[i] and x[i+1] are found using routine 
 *           cd_locate. Then a linear intepolation formula is used to 
 *           calculate yp[xp[ip].
 *
 *      passed:
 *           d,dp, x, xp, y, missing and missingMatch
 *           where missing is the value for missing data and missingmatch is the choice for
 *           selecting this data. It is greater, equal, less or none.
 *      returned:
 *           yp 
 *
 *     ------------------------------------------------------------------*/
 void cd_linear_interpolation(int d, double *x, float *y, int dp, double *xp, float *yp, float missingValueIn, char *missingMatch)
 {    
   
    int ip;                                                              /* index into new data */
    int i;                                                               /* i index for linear interpolation */
    float N;                                                             /* used in MISSING check */

    int t;                                                               /* t index for checking missingmatch */
    int found = 0;                                                       /* missingMatch error flag */
    char *choices[] = { "greater", "equal", "less", "none", NULL };      /* missingMatch choices */

 /*     ------------------------------------------------------------------ */

    /* make lower case string */

    for(t=0; missingMatch[t]; t++) {
        missingMatch[t] = tolower(missingMatch[t]); 
    }

    /* make lower case string */

    for(t=0; choices[t]; t++) {
        if(!strcmp(choices[t], missingMatch)) {
            found = 1;
            break;
        }
    }

    if(found == 0) {
        printf("Error in value for missingMatch: The choices are %s %s %s %s \n", choices[0], choices[1], choices[2], choices[3]);
        exit(1);
    }


    for (ip = 0; ip < dp; ++ip) {

	    i = cd_locate(d, x, xp[ip]);    /* find index below ip */

	    if( i >= d-1)
		yp[ip] = y[d-1];

	    else if ( i < 0)
		yp[ip] = y[0];

	    else {
                switch(*missingMatch) {

                case 'g':

	            if( missingValueIn > 0.0 )
                        N = 0.99 * missingValueIn;
                    else
                        N = 1.01 * missingValueIn;

                    if( (y[i+1] > N) | (y[i] > N) )
                       yp[ip] = missingValueIn;
                    else   
	               yp[ip] = y[i] +(y[i+1] - y[i])*( (xp[ip] - x[i])/(x[i+1] - x[i]) );

                    break;

                case 'e':

                    N = missingValueIn;

                    if( (y[i+1] == N) | (y[i] == N) )
                       yp[ip] = missingValueIn;
                    else   
	               yp[ip] = y[i] +(y[i+1] - y[i])*( (xp[ip] - x[i])/(x[i+1] - x[i]) );

                    break;

                case 'l':

	            if( missingValueIn < 0.0 )
                        N = 0.99 * missingValueIn;
                    else
                        N = 1.01 * missingValueIn;

                    if( (y[i+1] < N) | (y[i] < N) )
                       yp[ip] = missingValueIn;
                    else   
	               yp[ip] = y[i] +(y[i+1] - y[i])*( (xp[ip] - x[i])/(x[i+1] - x[i]) );

                    break;

                case 'n':

	            yp[ip] = y[i] +(y[i+1] - y[i])*( (xp[ip] - x[i])/(x[i+1] - x[i]) );

                    break;
                }

            }
    } 



}

 /*     ------------------------------------------------------------------
 *
 *     cd_locate 
 *
 *     purpose: 
 *            Given a value for x in the new coordinate system (point)
 *            find out where it fits in the old x coordinates. Return
 *            -1 if it is to the left of the old system or nlev if it 
 *            is to the right. ( values increasing to the right in this 
 *            explantion.
 *
 *      passed:
 *           d = nlev -- the size of the old coordinate system.
 *           x = old coordinate x vector
 *           point = new coordinate x point to fit somewhere
 *      returned:
 *           i = index into old coordinate system. 
 *
 *     ------------------------------------------------------------------*/

int cd_locate(int d, double *x, double point)
{    
   
    int lo;                        /* low part of bracket*/
    int hi;                        /* high part of bracket*/
    int mid;                       /* middle of bracket */
    int ascnd;                     /* 1 if x ascends, 0 otherwise */

 /*     ------------------------------------------------------------------ */

    lo = -1;
    hi = d;
    ascnd = ( x[d-1] > x[0] );       /* direction of the data */

    while( (hi -lo) > 1) {
	mid = (hi + lo)/2;
	if( (point > x[mid]) == ascnd )
	    lo = mid;
	else
	    hi = mid;
	}

	return lo;
}


 /*     ------------------------------------------------------------------
 *
 *     press_wts_bnds 
 *
 *     purpose: 
 *           given the target pressure grid, calculate the weights and bounds
 *      passed:
 *           nlev = size of new (primed) pressure grid
 *           pts = target pressure grid values
 *      returned:
 *           bnds = pressure grid boundaries
 *           wts = pressure grid weights  
 *
 *     ------------------------------------------------------------------*/

void press_wts_bnds(int nlev, double *pts,  double *wts, double *bnds)
{

  int i;                                 /* loop index */


  /* construct the boundaries from points */

  bnds[0] = pts[0] -((pts[1] - pts[0])/2.0);
  bnds[nlev] = pts[nlev - 1] +((pts[nlev - 1] - pts[nlev - 2])/2.0);

  for(i=1; i<nlev; i++) 
      bnds[i] = (pts[i] + pts[i - 1])/2.0;

  /* construct the weights from boundaries */

  if(bnds[nlev] > bnds[0]) {
      for(i=0; i<nlev; i++) 
          wts[i] = bnds[i + 1] - bnds[i];
  }
  else {
      for(i=0; i<nlev; i++) 
          wts[i] = bnds[i] - bnds[i + 1];
  }

}


/* Transferred here from defunct pcmdimodule.c */
 /*     ------------------------------------------------------------------
 *
 *     locate 
 *
 *     purpose: 
 *            Given a value for x in the new coordinate system (point)
 *            find out where it fits in the old x coordinates. Return
 *            -1 if it is to the left of the old system or nlev if it 
 *            is to the right. ( values increasing to the right in this 
 *            explantion.
 *
 *      passed:
 *           d = nlev -- the size of the old coordinate system.
 *           x = old coordinate x vector
 *           point = new coordinate x point to fit somewhere
 *      returned:
 *           i = index into old coordinate system. 
 *
 *     ------------------------------------------------------------------*/

int locate(int d, double *x, double point)
{    
   
    int lo;                        /* low part of bracket*/
    int hi;                        /* high part of bracket*/
    int mid;                       /* middle of bracket */
    int ascnd;                     /* 1 if x ascends, 0 otherwise */

 /*     ------------------------------------------------------------------ */

    lo = -1;
    hi = d;
    ascnd = ( x[d-1] > x[0] );       /* direction of the data */

    while( (hi -lo) > 1) {
	mid = (hi + lo)/2;
	if( (point > x[mid]) == ascnd )
	    lo = mid;
	else
	    hi = mid;
	}

	return lo;
}

/*     ------------------------------------------------------------------
 *
 *     gauaw 
 *
 *     Purpose. 
 *     -------- 
 *
 *     gauaw is called to compute the gaussian abscissas and weights. 
 *
 *     Interface. 
 *     ---------- 
 *
 *          call gauaw(pa,pw,k)
 *
 *               pa     - array, length at least k, to receive abscissas. 
 *
 *               pw     - array, length at least k, to receive weights.
 *
 *     Method. 
 *     ------- 
 *
 *          The zeros of the bessel functions are used as starting 
 *     approximations for the abscissas. Newton iteration is used to 
 *     improve the values to within a tolerance of eps.
 *
 *     External. 
 *     --------- 
 *
 *          bsslzr - routine to obtain zeros of bessel functions. 
 *
 *     Reference. 
 *     ---------- 
 *     ------------------------------------------------------------------*/

 int gauaw(double *pa, double *pw, int k) 
 {    
    double eps = 1e-14;            /* newton iteration tolerance */
    int iter;                      /* iteration counter */
    double avsp;                    /* abs of change to xy per iterayion */
    double c;                       /* constant used in first estimate */
    int l, n;                       /* used in symmetry calculation */
    double pkmrk;
    double fk;                      /* k as a double */
    double fn;                      /* n as a double */
    int kk;                         /* k / 2 */
    double pi;                      /* pi */
    double pk;
    int is;                         /* loop index */
    double sp;                      /* slope of poynomial */
    double xz;                      /* polynomial zero */                     
    double pkm1;
    double pkm2;
    double tp;                    /* temporary storage for expressions */



/*     ------------------------------------------------------------------ */

	 /* 1.     set constants and find zeros of bessel function. 
		   --- --------- --- ---- ----- -- ------ --------- */

    pi = 4.0 * atan(1.0);
    tp = 2.0 / pi;
    c  = (double)0.25 * ( 1.0 -  tp * tp );
    fk = (double)k;

    kk = k / 2;
    bsslzr(pa, kk);

    for (is = 0; is < kk; ++is) {  
    
	tp = fk + (double)0.5;
	xz = cos( pa[is] / sqrt( tp * tp  + c) );

			 /* giving the first approximation for xz. */
	iter = 0;

/*     ------------------------------------------------------------------ 

	   2.     compute abscissas and weights. 
	          ------- --------- --- -------                               */


/*           2.1     set values for next iteration.                       */
L210:
	pkm2 = 1.0;
	pkm1 = xz;
	++iter;
	if (iter > 10) {
/*     ------------------------------------------------------------------ 
	    3.     error processing. 
		   ----- -----------                                      */
            printf("  gauaw failed to converge after 10 iterations.\n");
	    exit(1);

/*     ------------------------------------------------------------------ */ 
	}

/*           2.2     computation of the legendre polynomial.              */

	for (n = 2; n <= k; ++n) {
	    fn = (double) n;
	    pk = ((2.0 * fn - 1.0) * xz * pkm1 - (fn - 1.0)* pkm2) / fn; 
		    
	    pkm2 = pkm1;
	    pkm1 = pk;
	}

	pkm1 = pkm2;
	pkmrk = ( fk * (pkm1 - xz * pk) ) / (1.0 - xz * xz);
	sp = pk / pkmrk;
	xz -= sp;
	avsp = fabs(sp);
	if (avsp > eps) {
	    goto L210;                  /* do another iteration */
	}

/*           2.3     abscissas and weights.                               */

	pa[is] = xz;
	pw[is] = (1.0 - xz * xz) * 2.0 / ( (fk*pkm1) *(fk*pkm1) );

/*           2.4     odd k computation of weight at the equator.          */

	if (k != 2*kk) {
	    pa[kk] = 0.0;  
	    pk = 2.0 / (fk * fk);

	    for (n = 2; n <= k; n += 2) {
		fn = (double) n;
		tp = fn - 1.0;
		pk = ( pk * (fn * fn) ) / ( tp * tp );
	    }

	    pw[kk] = pk; 
	} else {

/*           2.5     use symmetry to obtain remaining values.              */


	    for (n = 0; n < kk; ++n) {
		l = k - 1 - n;
		pa[l] = -pa[n];
		pw[l] = pw[n];
	    }

	}
    }

    return 0;


}

/*     bsslzr  
 *
 *     Purpose. 
 *     -------- 
 *
 *     bsslzr returns knum zeros, or if knum > 50, knum 
 *     approximate zeros of the j0 bessel function
 *
 *     Interface. 
 *     ---------- 
 *
 *            call bsslzr(pbes,knum) 
 *
 *               pbes   - array, dimensioned knum, to receive the values. 
 *               knum   - number of zeros requested. 
 *
 *     Method. 
 *     ------- 
 *
 *          The first 50 values are obtained from a look up table. Any 
 *     additional values requested are interpolated. 
 *
 *     Externals. 
 *     ---------- 
 *
 *          None. 
 *
 *     Reference. 
 *     ---------- 
 *
 *    ------------------------------------------------------------------ */ 



int bsslzr(double *pbes, int knum)
{

    static double zbes[50] = { 2.4048255577,5.5200781103,8.6537279129,
	    11.7915344391,14.9309177086,18.0710639679,21.2116366299,
	    24.3524715308,27.493479132,30.6346064684,33.7758202136,
	    36.9170983537,40.0584257646,43.1997917132,46.3411883717,
	    49.4826098974,52.6240518411,55.765510755,58.9069839261,
	    62.0484691902,65.1899648002,68.3314693299,71.4729816036,
	    74.6145006437,77.7560256304,80.8975558711,84.0390907769,
	    87.1806298436,90.3221726372,93.4637187819,96.605267951,
	    99.7468198587,102.8883742542,106.0299309165,109.1714896498,
	    112.3130502805,115.4546126537,118.5961766309,121.737742088,
	    124.8793089132,128.0208770059,131.1624462752,134.3040166383,
	    137.4455880203,140.5871603528,143.7287335737,146.8703076258,
	    150.011882457,153.1534580192,156.2950342685 };


    int j;                         /* loop index */
    int inum;                      /* the min of knum and 49 */
    double api;                    /* pi */


/*          1.     extract values from look up table. 
		    ------- ------ ---- ---- -- ------                    

		   SET API                                               */

    api = 4.0 * atan( 1.0 );

    inum = min(knum,49);
	

    for (j = 0; j <= inum; ++j) {
	    pbes[j] = zbes[j]; 
		/*printf("  %3.8lf", pbes[j]); */
    }
/*     ------------------------------------------------------------------ 

	   2.     interpolate remaining values. 
		  ----------- --------- -------                          */

    for (j = 50; j < knum; ++j) { 
	    pbes[j] = pbes[j - 1] + api;
    }

/*     ------------------------------------------------------------------ */ 

    return 0;
}
/**************************************************************************
******
* Function:   floattodouble
*
*  Purpose:   changes a float into a double
*
***************************************************************************
*****/
void floattodouble(int size, float *datain, double *dataout)
{
  int n;               /* loop index */

  for(n=0; n<size; n++)
    dataout[n] = (double)datain[n];
}

/**************************************************************************
******
* Function:   doubletofloat
*
*  Purpose:   changes a double into a float
*
***************************************************************************
*****/
void doubletofloat(int size, double *datain, float *dataout)
{
  int n;               /* loop index */

  for(n=0; n<size; n++)
    dataout[n] = (float)datain[n];
}



 /* Function:   zerodouble.c
 *
 *  Purpose:   Initializes array with data_12[i][j][tme] = 0.0
 */
void zerodouble(int size, double *data)
{
  int n;         /* loop index */
  double *d;     /* pointer to data used in the loop */

  d = data;

  for(n=0; n<size; n++)
    *d++ = 0.0;
}

