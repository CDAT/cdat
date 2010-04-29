 /************************************************************************************************
 *                                                                                               *
 * natgridmodule.c:  a C extension which exports the following functions to Python:              *
 *                                                                                               *
 *                   Single precision procedures:                                                *    
 *                                                                                               *
 *                       natgrids     - primary function for gridding.                           *
 *                       seti         - set int parameter values.                                *
 *                       geti         - retrieve values for int parameters.                      *
 *                       setr         - set float parameter values.                              *
 *                       getr         - retrieve values for float parameters                     *
 *                       setc         - set char parameter values.                               * 
 *                       getc         - retrieve values for char parameters.                     *
 *                       getaspects   - get aspect values, if calculated.                        *
 *                       getslopes    - get slope values, if calculated.                         *
 *                       pntinits     - initiate single point mode.                              *
 *                       pnts         - interpolate at a single point.                           *
 *                       pntend       _ terminate single point mode.                             *
 *                                                                                               *
 *                                                                                               *
 *                   Double precision procedures:                                                *    
 *                                                                                               *
 *                       natgridd     - primary function for gridding.                           *
 *                       setrd        - set float parameter values.                              *
 *                       getrd        - retrieve values for float parameters                     *
 *                       getaspectd   - get aspect values, if calculated.                        *
 *                       getsloped    - get slope values, if calculated.                         *
 *                       pntinitd     - initiate single point mode.                              *
 *                       pntd         - interpolate at a single point.                           *
 *                       pntendd      _ terminate single point mode.                             *
 *                                                                                               *
 *   where is getwts and getwtd                                                                  *
 *                                                                                               *
 *************************************************************************************************/

#include "Python.h"
#include "numpy/arrayobject.h" 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define PRINTNATGRIDS  0
#define WRITENATGRIDS  0
#define PRINTPNTINITS  0
#define WRITEPNTINITS  0

static PyObject *ErrorObject;            /* locally raised exception */

/*---------------------------------------- Macros -----------------------------------------------*/

#define onError(message) { PyErr_SetString(ErrorObject, message); return NULL; }

/*--------------------------------- Function Prototypes -----------------------------------------*/

/* ----  external C functions in the library ---- */

extern float *c_natgrids(int npnts, float x[], float y[], float z[], int numxout, int numyout,
                                                                   float xo[], float yo[], int *ier);

extern void c_nnseti(char *pnam, int ival);

extern void c_nngeti(char *pnam, int *ival);

extern void c_nnsetr(char *pnam, float fval);

extern void c_nngetr(char *pnam, float *fval);

extern void c_nnsetc(char *pnam, char *cval);

extern void c_nngetc(char *pnam, char *cval);

extern void c_nngetaspects(int row, int column, float *aspect, int *ier);

extern void c_nngetslopes(int row, int column, float *slope, int *ier);

extern void c_nnpntinits(int row, float x[], float y[], float z[]);

extern void c_nnpnts(float x, float y, float *z);

extern void c_nnpntend();

extern double *c_natgridd(int n, double x[], double y[], double z[], int numxout, int numyout,
                                                                    double xo[], double yo[], int *ier);

extern void c_nnsetrd(char *pnam, double dval);

extern void c_nngetrd(char *pnam, double *dval);

extern void c_nngetaspectd(int row, int column, double *aspect, int *ier);

extern void c_nngetsloped(int row, int column, double *slope, int *ier);

extern void c_nnpntinitd(int row, double x[], double y[], double z[]);

extern void c_nnpntd(double x, double y, double *z);

extern void c_nnpntendd();

/* ----  functions in this file called by c extensions ---- */

void write_int(int size, char *title, FILE *fp, int *data);

void write_float(int size, char *title, FILE *fp, float *data);
void print_float(int size, char *title, float *data);

void write_double(int size, char *title, FILE *fp, double *data);
void print_double(int size, char *title,  double *data);


 /*************************************************************************************************
 **************************************************************************************************
 *                                                                                                *
 *                            EXPORTED MODULE METHOD-FUNCTIONS                                    *
 *                                                                                                *
 *                                                                                                *
 **************************************************************************************************
 **************************************************************************************************/

static char nat_c_natgrids__doc__[] = "                                                    \n\
                                                                                            \n\
                           natgrids - Primary gridding function                                    \n\
                                                                                                   \n\
  natgrids is the C single precision function that does an interpolation from 2D random data       \n\
  to a output grid. natgrids is called after all the desired values for the control parameters     \n\
  have been set using the procedures seti, setr, setc and setd.                                     \n\
                                                                                                   \n\
  natgrids returns a pointer to a linear array of data that is the interpolated grid stored in     \n\
  row-major order. That is, if out is declared as                                                  \n\
            float  out;                                                                           \n\
  and we set:                                                                                      \n\
            out = natgrids(npnts, x, y, z, numxout, numyout, xo, yo, &ier);                        \n\
  then out[i numyout + j] is the interpolated value at coordinate point (xo[i], y[j]) for          \n\
  0 <= i < numxout and 0 <= j < numyout. The space for out is allocated internal to natgrids       \n\
  and is numxout numyout floats in size.                                                           \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
    extern float  c_natgrids(int npnts, float x[], float y[], float z[],                           \n\
                                  int numxout, int numyout, float xo[], float yo[], int  ier);     \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
    out, ier = natgrids(npnts, x, y, z, numxout, numyout, xo, yo)                                  \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
     npnts -- the number of input data points                                                      \n\
                                                                                                   \n\
     x -- array of size npnts containing the x coordinates of the input data points                \n\
                                                                                                   \n\
     y -- array of size npnts containing the y coordinates of the input data points                \n\
                                                                                                   \n\
     z -- array of size npnts containing the functional values of the input data points. That is,  \n\
          z[j] is the value of the input function at coordinate (x[j], y[j]), for 0 <= j < npnts.  \n\
                                                                                                   \n\
     numxout -- the number of x values in the output grid,                                         \n\
                                                                                                   \n\
     numyout -- the number of y values in the output grid.                                         \n\
                                                                                                   \n\
     xo -- array of size numxout containing the x coordinates of the output data grid. The values  \n\
           of xo must be increasing, but need not be equally spaced.                               \n\
                                                                                                   \n\
     yo -- array of size numyout containing the y coordinates of the output data grid. The values  \n\
           of yo must be increasing, but need not be equally spaced.                               \n\
                                                                                                   \n\
                                                                                                  ";
static PyObject *nat_c_natgrids(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* The number of input data points */
    PyArrayObject *object_x;         /* Object pointer containing the x coordinates of the input data points */
    PyArrayObject *object_y;         /* Object pointer containing the y coordinates of the input data points */
    PyArrayObject *object_z;         /* Object pointer containing the functional values of the input data points */

    int numxout;                     /* The number of x values in the output grid */
    int numyout;                     /* The number of y values in the output grid */

    PyArrayObject *object_xo;        /* Object pointer containing the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* Object pointer containing the y coordinates of the output data grid */

    /* fields required by call to c function*/

    float *out;                      /* An array with the interpolated values at the output coordinate points */

    int ier;                         /* An error return value. If *ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;       /* array object to accept the data and return it to Python */
    int dims[2];                     /* used in creating object_out */

    /* declarations for writes to a file */

    FILE *fp;                                                              /* File used in ascii write */ 
    char *title[6] = { "x", "y ", "z", "xo", "yo", "result" };             /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "iOOOiiOO", &npnts, &object_x, &object_y, &object_z,
                                           &numxout, &numyout,  &object_xo, &object_yo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to natgrids is wrong.\n");
        return NULL;
    }

    out = (float *)c_natgrids(npnts, (float *)object_x->data, (float *)object_y->data, (float *)object_z->data,
                                              numxout, numyout, (float *)object_xo->data, (float *)object_yo->data, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */
    dims[0] = numxout;
    dims[1] = numyout; 

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(2, dims, PyArray_FLOAT, (char *)out);

    if (PRINTNATGRIDS == 1) {
        /* -------- print data to the screen ---------- */
        printf("npnts: %d\n", npnts);
        printf("numxout: %d\n", numxout);
        printf("numyout: %d\n", numyout);
        print_float(npnts, title[0], (float *)object_x->data);
        print_float(npnts, title[1], (float *)object_x->data);
        print_float(npnts, title[2], (float *)object_z->data);
        print_float(numxout, title[3], (float *)object_xo->data);
        print_float(numyout, title[4], (float *)object_yo->data);
        print_float(numxout*numyout, title[5], (float *)object_out->data);
    }

    if (WRITENATGRIDS == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("natgrids.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "npnts: %d\n", npnts);
        fprintf(fp, "numxout: %d\n", numxout);
        fprintf(fp, "numyout: %d\n", numyout);
        write_float(npnts, title[0], fp, (float *)object_x->data);
        write_float(npnts, title[1], fp, (float *)object_y->data);
        write_float(npnts, title[2], fp, (float *)object_z->data);
        write_float(numxout, title[3], fp, (float *)object_xo->data);
        write_float(numyout, title[4], fp, (float *)object_yo->data);
        write_float(numxout*numyout, title[5], fp, (float *)object_out->data);

        close(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char nat_c_nnseti__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                               seti - Set int valued parameters                                    \n\
                                                                                                   \n\
   seti is used to set values for any of the control parameters that take int values. The          \n\
   values set by seti remain in effect until changed by subsequent calls to seti.                  \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void nnseti(char  pnam, int ival);                                                     \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
      seti(pnam, ival)                                                                             \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    pnam -- the name of the control parameter whic is assigned an int value.                       \n\
                                                                                                   \n\
    ival -- the value to be assigned to the control parameter whose name is pointed to by pnam.      \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnseti(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter to be assigned an int value */
    int ival;                 /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "si", &pnam, &ival))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnseti is wrong.\n");
        return NULL;
    }

    c_nnseti(pnam, ival);

    Py_INCREF(Py_None);
    return Py_None;
} 

static char nat_c_nngeti__doc__[] =
"                                                                                                    \n\
                                                                                                    \n\
                               geti - Retreive an int valued parameter                               \n\
                                                                                                    \n\
  geti is called to obtain current values for any of the int valued control parameters.             \n\
                                                                                                    \n\
 Prototype:                                                                                          \n\
                                                                                                    \n\
    extern void c_nngeti(char  pnam, int  ival);                                                    \n\
                                                                                                    \n\
 Call from Python:                                                                                  \n\
                                                                                                    \n\
    ival = geti(pnam)                                                                               \n\
                                                                                                    \n\
 where:                                                                                             \n\
                                                                                                    \n\
   pnam -- the name of the control parameter which is assigned an int value.                          \n\
                                                                                                    \n\
   ival -- the value currently assigned to the control parameter whose name is pointed to by pnam.      \n\
                                                                                                    \n\
                                                                                                     "; 
static PyObject *nat_c_nngeti(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter whose value is to be retrieved */

    /* fields required by call to c function*/

    int ival;                 /* the value currently assigned to the control parameter whose name is
                                 pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "s", &pnam))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngeti is wrong.\n");
        return NULL;
    }

    c_nngeti(pnam, &ival);

    return Py_BuildValue("i", ival);
} 

static char nat_c_nnsetr__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                               setr - Set float valued parameters                                  \n\
                                                                                                   \n\
   setr is used to set values for any of the control parameters that take float values. The        \n\
   values set by setr remain in effect until changed by subsequent calls to setr.                  \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nnsetr(char  pnam, float fval);                                                 \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
    setr(pnam, fval)                                                                               \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    pnam -- the name of the control parameter to be  assigned an float value.                      \n\
                                                                                                   \n\
    fval -- the value to be assigned to the control parameter whose name is pointed to by pnam.       \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnsetr(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter to be assigned a float value */
    float fval;               /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "sf", &pnam, &fval))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnsetr is wrong.\n");
        return NULL;
    }

    c_nnsetr(pnam, fval);

    Py_INCREF(Py_None);
    return Py_None;
} 

static char nat_c_nngetr__doc__[] =
"                                                                                                     \n\
                                                                                                     \n\
                               getr - Retreive an float valued parameter                             \n\
                                                                                                     \n\
   getr is called to obtain current values for any of the float valued control parameters.           \n\
                                                                                                     \n\
  Prototype:                                                                                          \n\
                                                                                                     \n\
     extern void c_nngetr(char  pnam, int  fval);                                                    \n\
                                                                                                     \n\
  Call from Python:                                                                                  \n\
                                                                                                     \n\
     fval = getr(pnam)                                                                               \n\
                                                                                                     \n\
  where:                                                                                             \n\
                                                                                                     \n\
    pnam -- the name of the control parameter which is assigned a float value.                       \n\
                                                                                                     \n\
    fval -- the value currently assigned to the control parameter whose name is pointed to by pnam.      \n\
                                                                                                     \n\
                                                                                                    "; 
static PyObject *nat_c_nngetr(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter whose value is to be retrieved */

    /* fields required by call to c function*/

    float fval;               /* the value currently assigned to the control parameter whose name is
                                 pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "s", &pnam))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngetr is wrong.\n");
        return NULL;
    }

    c_nngetr(pnam, &fval);

    return Py_BuildValue("f", fval);
} 

static char nat_c_nnsetc__doc__[] =
"                                                                                                  \n\
                                                                                                   \n\
                               setc - Set char valued parameters                                   \n\
                                                                                                   \n\
   setc is used to set values for any of the control parameters that take string values. The       \n\
   values set by setc remain in effect until changed by subsequent calls to setc.                  \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nnsetc(char  pnam, char  cval);                                                 \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
      setc(pnam, cval)                                                                             \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    pnam -- the name of the control parameter to be assigned a char value.                         \n\
                                                                                                   \n\
    cval -- the value to be assigned to the control parameter whose name is pointed to by pnam.        \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnsetc(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter to be assigned a char value */
    char *cval;               /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "ss", &pnam, &cval))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnsetc is wrong.\n");
        return NULL;
    }

    c_nnsetc(pnam, cval);

    Py_INCREF(Py_None);
    return Py_None;
} 

static char nat_c_nngetc__doc__[] =
"                                                                                                     \n\
                                                                                                     \n\
                               getc - Retreive an char valued parameter                              \n\
                                                                                                     \n\
   getc is called to obtain current values for any of the string valued control parameters.          \n\
                                                                                                     \n\
  Prototype:                                                                                          \n\
                                                                                                     \n\
     extern void c_nngetc(char  pnam, char  cval);                                                   \n\
                                                                                                     \n\
  Call from Python:                                                                                  \n\
                                                                                                     \n\
     cval = getc(pnam)                                                                               \n\
                                                                                                     \n\
  where:                                                                                             \n\
                                                                                                     \n\
    pnam -- the name of the control parameter which is assigned a string value.                      \n\
                                                                                                     \n\
    cval -- the value currently assigned to the control parameter whose name is pointed to by pnam.      \n\
                                                                                                     \n\
                                                                                                     "; 
static PyObject *nat_c_nngetc(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;                /* the name of the control parameter whose value is to be retrieved */

    /* fields required by call to c function*/

    char cval[128];            /* the value currently assigned to the control parameter whose name is
                                  pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "s", &pnam))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngetc is wrong.\n");
        return NULL;
    }

    c_nngetc(pnam, cval);

    return Py_BuildValue("s", cval);
} 

static char nat_c_nngetaspects__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                               getaspects - Retreive aspect values, if calculated                  \n\
                                                                                                   \n\
   getaspects is called to retrieve an aspect, in single-precision, at a specified coordinate      \n\
   value. For further details see the module on computing aspects and slopes.                      \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nngetaspects(int row, int column, float  aspect, int  ier);                     \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
     aspect, ier =  getaspects(row, column)                                                        \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    row -- a subscript indexing the first dimenioned variable in the 2D grid array returned from   \n\
           the most recent call to natgrids.                                                       \n\
                                                                                                   \n\
    column -- a subscript indexing the second dimenioned variable in the 2D grid array returned    \n\
              from the most recent call to natgrids.                                                \n\
                                                                                                   \n\
    aspect -- the aspect at the grid point z[i][j], where z is the output grid in the most recent  \n\
              call to natgrids.                                                                    \n\
                                                                                                   \n\
    ier -- an error return value. If ier is returned as 0, then no errors were detected.  If ier   \n\
           is non-zero, then refer to the list in the error table for details.                      \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nngetaspects(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int row;                         /* A subscript indexing the first dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgrids */
    int column;                      /* A subscript indexing the second dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgrids */

    /* fields required by call to c function*/

    float aspect;                    /* the aspect at the grid point z[i][j], where z is the output grid
                                        in the most recent call to c_natgrids. */

    int ier;                         /* An error return value. If *ier is returned as 0, then no errors were detected.
                                        If *ier is non-zero, then refer to the list in the error table for details. */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "ii", &row, &column))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngetaspects is wrong.\n");
        return NULL;
    }

    c_nngetaspects(row, column, &aspect, &ier);

    return Py_BuildValue("fi", aspect, ier);
} 

static char nat_c_nngetslopes__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                             getslopes - Retreive slope values, if calculated                      \n\
                                                                                                   \n\
   getslopes is called to retrieve a slope, in single-precision, at a specified coordinate         \n\
   value. For further details see the module on computing slopes and slopes.                       \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nngetslopes(int row, int column, float  slope, int  ier);                       \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
     slope, ier =  getslopes(row, column)                                                          \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    row -- a subscript indexing the first dimenioned variable in the 2D grid array returned from   \n\
           the most recent call to natgrids.                                                       \n\
                                                                                                   \n\
    column -- a subscript indexing the second dimenioned variable in the 2D grid array returned    \n\
              from the most recent call to natgrids.                                                \n\
                                                                                                   \n\
    slope -- the slope at the grid point z[i][j], where z is the output grid in the most recent    \n\
              call to natgrids.                                                                    \n\
                                                                                                   \n\
    ier -- an error return value. If ier is returned as 0, then no errors were detected.  If ier   \n\
           is non-zero, then refer to the list in the error table for details.                      \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nngetslopes(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int row;                         /* A subscript indexing the first dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgrids */
    int column;                      /* A subscript indexing the second dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgrids */

    /* fields required by call to c function*/

    float slope;                     /* the slope at the grid point z[i][j], where z is the output grid
                                        in the most recent call to c_natgrids. */

    int ier;                         /* An error return value. If *ier is returned as 0, then no errors were detected.
                                        If *ier is non-zero, then refer to the list in the error table for details. */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "ii", &row, &column))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngetslopes is wrong.\n");
        return NULL;
    }

    c_nngetslopes(row, column, &slope, &ier);

    return Py_BuildValue("fi", slope, ier);
} 

static char nat_c_nnpntinits__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           pntinits - Enter single-point mode                                      \n\
                                                                                                   \n\
   This function calculates all naturnal neighbor relationships in an input data array and sets    \n\
   some internal parameters so that pnts can be called to interpolate at individual points.        \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
    extern void c_nnpntinits(int row, float x[], float y[], float z[]);                            \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
    pntinits(n, x, y, z)                                                                           \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
     n -- the number of input data points                                                          \n\
                                                                                                   \n\
     x -- array of size npnts containing the x coordinates of the input data points                 \n\
                                                                                                   \n\
     y -- array of size npnts containing the y coordinates of the input data points                 \n\
                                                                                                   \n\
     z -- array of size npnts containing the functional values of the input data points. That is,   \n\
          z[j] is the value of the input function at coordinate (x[j], y[j]), for 0 <= j < npnts.   \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnpntinits(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* the number of input data points (npnts > 3 )*/

    PyArrayObject *object_x;         /* object pointer containing the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer containing the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer containing the functional values at the input data points */

    /* declarations for writes to a file */

    FILE *fp;                                      /* File used in ascii write */ 
    char *title[3] = {"x", "y ", "z"};             /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "iOOO", &npnts, &object_x, &object_y, &object_z))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnpntinits is wrong.\n");
        return NULL;
    }

    /* -------- write  input data to a file ----------- */

    c_nnpntinits(npnts, (float *)object_x->data, (float *)object_y->data, (float *)object_z->data);

    if (PRINTPNTINITS == 1) {
        /* -------- print data to the screen ---------- */
        printf("npnts: %d\n", npnts);
        print_float(npnts, title[0], (float *)object_x->data);
        print_float(npnts, title[1], (float *)object_x->data);
        print_float(npnts, title[2], (float *)object_z->data);
    }

    if (WRITEPNTINITS == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("pntinits.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "npnts: %d\n", npnts);
        write_float(npnts, title[0], fp, (float *)object_x->data);
        write_float(npnts, title[1], fp, (float *)object_y->data);
        write_float(npnts, title[2], fp, (float *)object_z->data);

        close(fp);
    }

    Py_INCREF(Py_None);
    return Py_None;
} 

static char nat_c_nnpnts__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           pnts - Interpolate at a single point                                    \n\
                                                                                                   \n\
   This function is called to interpolate at a specified point. Before calling this function,      \n\
   pntinits must be called.                                                                        \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nnpnts(float x, float y, float  z);                                             \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
      z = pnts(x, y)                                                                               \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    x --  x coordinate of the point where interpolation is desired.                                \n\
                                                                                                   \n\
    y --  y coordinate of the point where interpolation is desired.                                \n\
                                                                                                   \n\
    z --  the interpolated functional value at (x,y).                                              \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnpnts(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    float x;                        /* x coordinate of the point where interpolation is desired */
    float y;                        /* y coordinate of the point where interpolation is desired */

    /* fields required by call to c function*/

    float z;                        /* the interpolated functional value at (x,y) */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "ff", &x, &y))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnpnts is wrong.\n");
        return NULL;
    }

    c_nnpnts(x, y, &z); 

    return Py_BuildValue("f", z);
} 

static char nat_c_nnpntend__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           pntend - Exit single-point mode                                         \n\
                                                                                                   \n\
   This function is called to terminate interpolation at single points. It is called after having  \n\
   made previous calls to pntinits and pnts.                                                       \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nnpntend();                                                                     \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
      pntend()                                                                                     \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnpntend(PyObject *self, PyObject *args) 
{
    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, ""))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnpntend is wrong.\n");
        return NULL;
    }

    c_nnpntend(); 

    Py_INCREF(Py_None);
    return Py_None;
} 

static char nat_c_natgridd__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           natgridd - Primary gridding function                                    \n\
                                                                                                   \n\
  natgridd is the C double-precision function that does an interpolation from 2D random data       \n\
  to a output grid. natgridd is called after all the desired values for the control parameters     \n\
  have been set using the procedures seti, setr and setc.                                          \n\
                                                                                                   \n\
  natgridd returns a pointer to a linear array of data that is the interpolated grid stored in     \n\
  row-major order. That is, if out is declared as                                                  \n\
            double  out;                                                                           \n\
  and we set:                                                                                      \n\
            out = natgridd(npnts, x, y, z, numxout, numyout, xo, yo, &ier);                        \n\
  then out[i numyout + j] is the interpolated value at coordinate point (xo[i], y[j]) for          \n\
  0 <= i < numxout and 0 <= j < numyout. The space for out is allocated internal to natgridd       \n\
  and is numxout numyout floats in size.                                                           \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
    extern double  c_natgridd(int n, double x[], double y[], double z[], int numxout,              \n\
                                             int numyout, double xo[], double yo[], int  ier);     \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
    out, ier = natgridd(npnts, x, y, z, numxout, numyout, xi, yi)                                  \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
     npnts -- the number of input data points                                                      \n\
                                                                                                   \n\
     x -- array of size npnts containing the x coordinates of the input data points                \n\
                                                                                                   \n\
     y -- array of size npnts containing the y coordinates of the input data points                \n\
                                                                                                   \n\
     z -- array of size npnts containing the functional values of the input data points. That is,  \n\
          z[j] is the value of the input function at coordinate (x[j], y[j]), for 0 <= j < npnts.   \n\
                                                                                                   \n\
     numxout -- the number of x values in the output grid,                                         \n\
                                                                                                   \n\
     numyout -- the number of y values in the output grid.                                          \n\
                                                                                                   \n\
     xo -- array of size numxout containing the x coordinates of the output data grid. The values  \n\
           of xo must be increasing, but need not be equally spaced.                               \n\
                                                                                                   \n\
     yo -- array of size numyout containing the y coordinates of the output data grid. The values  \n\
           of yo must be increasing, but need not be equally spaced.                               \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_natgridd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* the number of input data points */

    int numxout;                     /* the number of x values in the output grid */
    int numyout;                     /* the number of y values in the output grid */

    PyArrayObject *object_x;         /* object pointer containing the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer containing the y coordinates of the input data points */
    PyArrayObject *object_z;         /* Object pointer containing the functional values of the input data points */
    PyArrayObject *object_xo;        /* object pointer containing the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer containing the y coordinates of the output data grid */

    /* fields required by call to c function*/

    double *out;                     /* An array with the interpolated values at the coordinate points */

    int ier;                         /* An error return value. If *ier is returned as 0, then no errors were detected.
                                        If *ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;       /* array object to accept the data and return it to Python */
    int dims[2];                     /* used in creating object_out */

    /* declarations for writes to a file */

    FILE *fp;                                                        /* File used in ascii write */ 
    char *title[6] = {"x", "y ", "z", "xo", "yo", "result"};         /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "iOOOiiOO", &npnts, &object_x, &object_y, &object_z, 
                                            &numxout, &numyout, &object_xo, &object_yo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to natgridd is wrong.\n");
        return NULL;
    }

    out = (double *)c_natgridd(npnts, (double *)object_x->data, (double *)object_y->data, (double *)object_z->data,
                                            numxout, numyout, (double *)object_xo->data, (double *)object_yo->data, &ier); 

    /* -------- create a NumPy array housing the c language data out ----------- */
    dims[0] = numxout;
    dims[1] = numyout; 

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(2, dims, PyArray_DOUBLE, (char *)out);


    if (PRINTNATGRIDS == 1) {
        /* -------- print data to the screen ---------- */
        printf("npnts: %d\n", npnts);
        printf("numxout: %d\n", numxout);
        printf("numyout: %d\n", numyout);
        print_double(npnts, title[0], (double *)object_x->data);
        print_double(npnts, title[1], (double *)object_x->data);
        print_double(npnts, title[2], (double *)object_z->data);
        print_double(numxout, title[3], (double *)object_xo->data);
        print_double(numyout, title[4], (double *)object_yo->data);
        print_double(numxout*numyout, title[5], (double *)object_out->data);
    }

    if (WRITENATGRIDS == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("natgridd.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "npnts: %d\n", npnts);
        fprintf(fp, "numxout: %d\n", numxout);
        fprintf(fp, "numyout: %d\n", numyout);
        write_double(npnts, title[0], fp, (double *)object_x->data);
        write_double(npnts, title[1], fp, (double *)object_y->data);
        write_double(npnts, title[2], fp, (double *)object_z->data);
        write_double(numxout, title[3], fp, (double *)object_xo->data);
        write_double(numyout, title[4], fp, (double *)object_yo->data);
        write_double(numxout*numyout, title[5], fp, (double *)object_out->data);

        close(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char nat_c_nnsetrd__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           setrd - Set double-precision parameters values                          \n\
                                                                                                   \n\
   setrd is used to set values for any of the control parameters that take double-precision        \n\
   values. The values set by setd remain in effect until changed by subsequent calls to            \n\
   setrd.                                                                                          \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nnsetrd(char  pnam, double dval);                                               \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
    setrd(pnam, dval)                                                                              \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    pnam -- the name of the control parameter to be assigned an double-precision value.            \n\
                                                                                                   \n\
    dval -- the value to be assigned to the control parameter whose name is pointed to by pnam.        \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnsetrd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter to be assigned a double value */
    double dval;               /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "sd", &pnam, &dval))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnsetrd is wrong.\n");
        return NULL;
    }

    c_nnsetrd(pnam, dval);

    Py_INCREF(Py_None);
    return Py_None;
} 

static char nat_c_nngetrd__doc__[] =
"                                                                                                     \n\
                                                                                                     \n\
                           getrd - Retreive an double precision parameter                            \n\
                                                                                                     \n\
   getrd is called to obtain current values for any of the double-precision control parameters.      \n\
                                                                                                     \n\
  Prototype:                                                                                          \n\
                                                                                                     \n\
     extern void c_nngetrd(char  pnam, int  dval);                                                   \n\
                                                                                                     \n\
  Call from Python:                                                                                  \n\
                                                                                                     \n\
     dval = getrd(pnam)                                                                              \n\
                                                                                                     \n\
  where:                                                                                             \n\
                                                                                                     \n\
    pnam -- the name of the control parameter which is assigned a double precision value.            \n\
                                                                                                     \n\
    dval -- the value currently assigned to the control parameter whose name is pointed to by pnam.      \n\
                                                                                                     \n\
                                                                                                     "; 
static PyObject *nat_c_nngetrd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter whose value is to be retrieved */

    /* fields required by call to c function*/

    double dval;              /* the value currently assigned to the control parameter whose name is
                                  pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "s", &pnam))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngetrd is wrong.\n");
        return NULL;
    }

    c_nngetrd(pnam, &dval);

    return Py_BuildValue("d", dval);
} 

static char nat_c_nngetaspectd__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                               getaspectd - Retreive aspect values, if calculated                  \n\
                                                                                                   \n\
   getaspectd is called to retrieve an aspect, in double-precision, at a specified coordinate      \n\
   value. For further details see the module on computing aspects and slopes.                      \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nngetaspectd(int row, int column, double  aspect, int  ier);                    \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
     aspect, ier =  getaspectd(row, column)                                                        \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    row -- a subscript indexing the first dimenioned variable in the 2D grid array returned from   \n\
           the most recent call to natgridd.                                                       \n\
                                                                                                   \n\
    column -- a subscript indexing the second dimenioned variable in the 2D grid array returned    \n\
              from the most recent call to natgridd.                                                \n\
                                                                                                   \n\
    aspect -- the aspect at the grid point z[i][j], where z is the output grid in the most recent  \n\
              call to natgridd.                                                                    \n\
                                                                                                   \n\
    ier -- an error return value. If ier is returned as 0, then no errors were detected.  If ier   \n\
           is non-zero, then refer to the list in the error table for details.                      \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nngetaspectd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int row;                         /* A subscript indexing the first dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgridd */
    int column;                      /* A subscript indexing the second dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgridd */

    /* fields required by call to c function*/

    double aspect;                   /* the aspect at the grid point z[i][j], where z is the output grid
                                        in the most recent call to c_natgridd. */

    int ier;                         /* An error return value. If *ier is returned as 0, then no errors were detected.
                                        If *ier is non-zero, then refer to the list in the error table for details. */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "ii", &row, &column))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngetaspectd is wrong.\n");
        return NULL;
    }

    c_nngetaspectd(row, column, &aspect, &ier);

    return Py_BuildValue("di", aspect, ier);
} 

static char nat_c_nngetsloped__doc__[] =
"                                                                                                 \n\
                                                                                                  \n\
                            getsloped - Retreive slope values, if calculated                      \n\
                                                                                                  \n\
  getsloped is called to retrieve a slope, in single precision, at a specified coordinate         \n\
  value. For further details see the module on computing slopes and slopes.                       \n\
                                                                                                  \n\
 Prototype:                                                                                        \n\
                                                                                                  \n\
    extern void c_nngetsloped(int row, int column, double  slope, int  ier);                      \n\
                                                                                                  \n\
 Call from Python:                                                                                \n\
                                                                                                  \n\
    slope, ier =  getsloped(row, column)                                                          \n\
                                                                                                  \n\
 where:                                                                                           \n\
                                                                                                  \n\
   row -- a subscript indexing the first dimenioned variable in the 2D grid array returned from   \n\
          the most recent call to natgridd.                                                       \n\
                                                                                                  \n\
   column -- a subscript indexing the second dimenioned variable in the 2D grid array returned    \n\
             from the most recent call to natgridd.                                                \n\
                                                                                                  \n\
   slope -- the slope at the grid point z[i][j], where z is the output grid in the most recent    \n\
             call to natgridd.                                                                    \n\
                                                                                                  \n\
   ier -- an error return value. If ier is returned as 0, then no errors were detected.  If ier   \n\
          is non-zero, then refer to the list in the error table for details.                      \n\
                                                                                                  \n\
                                                                                                   "; 
static PyObject *nat_c_nngetsloped(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int row;                         /* A subscript indexing the first dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgridd */
    int column;                      /* A subscript indexing the second dimenioned variable in the 2D grid
                                        array returned from the most recent call to c_natgridd */

    /* fields required by call to c function*/

    double slope;                    /* the slope at the grid point z[i][j], where z is the output grid
                                        in the most recent call to c_natgridd. */

    int ier;                         /* An error return value. If *ier is returned as 0, then no errors were detected.
                                        If *ier is non-zero, then refer to the list in the error table for details. */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "ii", &row, &column))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nngetsloped is wrong.\n");
        return NULL;
    }

    c_nngetsloped(row, column, &slope, &ier);

    return Py_BuildValue("di", slope, ier);
} 

static char nat_c_nnpntinitd__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           pntinitd - Enter single-point mode                                      \n\
                                                                                                   \n\
   This function calculates all naturnal neighbor relationships in an input data array and sets    \n\
   some internal parameters so that pnts can be called to interpolate at individual points.        \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
    extern void c_nnpntinitd(int row, double x[], double y[], double z[]);                         \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
    pntinitd(n, x, y, z)                                                                           \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
     npnts -- the number of input data points                                                      \n\
                                                                                                   \n\
     x -- array of size npnts containing the x coordinates of the input data points                 \n\
                                                                                                   \n\
     y -- array of size npnts containing the y coordinates of the input data points                 \n\
                                                                                                   \n\
     z -- array of size npnts containing the functional values of the input data points. That is,   \n\
          z[j] is the value of the input function at coordinate (x[j], y[j]), for 0 <= j < npnts.   \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnpntinitd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* the number of input data points (npnts > 3)*/

    PyArrayObject *object_x;         /* object pointer containing the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer containing the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer containing the functional values at the input data points */

    /* declarations for writes to a file */

    FILE *fp;                                    /* File used in ascii write */ 
    char *title[3] = { "x", "y ", "z" };         /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "iOOO", &npnts, &object_x, &object_y, &object_z))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnpntinitd is wrong.\n");
        return NULL;
    }

    c_nnpntinitd(npnts, (double *)object_x->data, (double *)object_y->data, (double *)object_z->data);


    if (PRINTPNTINITS == 1) {
        /* -------- print data to the screen ---------- */
        printf("npnts: %d\n", npnts);
        print_double(npnts, title[0], (double *)object_x->data);
        print_double(npnts, title[1], (double *)object_x->data);
        print_double(npnts, title[2], (double *)object_z->data);
    }

    if (WRITEPNTINITS == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("pntinitd.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "npnts: %d\n", npnts);
        write_double(npnts, title[0], fp, (double *)object_x->data);
        write_double(npnts, title[1], fp, (double *)object_y->data);
        write_double(npnts, title[2], fp, (double *)object_z->data);

        close(fp);
    }

    Py_INCREF(Py_None);
    return Py_None;
} 

static char nat_c_nnpntd__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           pntd - Interpolate at a single point                                    \n\
                                                                                                   \n\
   This function is called to interpolate at a specified point. Before calling this function,      \n\
   pntinitd must be called.                                                                        \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nnpnts(double x, double y, double  z);                                          \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
      z = pntd(x, y)                                                                               \n\
                                                                                                   \n\
  where:                                                                                           \n\
                                                                                                   \n\
    x --  x coordinate of the point where interpolation is desired.                                \n\
                                                                                                   \n\
    y --  y coordinate of the point where interpolation is desired.                                \n\
                                                                                                   \n\
    z --  the interpolated functional value at (x,y).                                              \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnpntd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    double x;                        /* x coordinate of the point where interpolation is desired */
    double y;                        /* y coordinate of the point where interpolation is desired */

    /* fields required by call to c function*/

    double z;                       /* the interpolated functional value at (x,y) */

    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "dd", &x, &y))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnpntd is wrong.\n");
        return NULL;
    }

    c_nnpntd(x, y, &z); 

    return Py_BuildValue("d", z);
} 

static char nat_c_nnpntendd__doc__[] =
"                                                                                                   \n\
                                                                                                   \n\
                           pntend - Exit single-point mode                                         \n\
                                                                                                   \n\
   This function is called to terminate interpolation at single points. It is called after having  \n\
   made previous calls to pntinitd and pntd.                                                       \n\
                                                                                                   \n\
  Prototype:                                                                                        \n\
                                                                                                   \n\
     extern void c_nnpntendd();                                                                    \n\
                                                                                                   \n\
  Call from Python:                                                                                \n\
                                                                                                   \n\
      pntendd()                                                                                    \n\
                                                                                                   \n\
                                                                                                   "; 
static PyObject *nat_c_nnpntendd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, ""))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to nnpntendd is wrong.\n");
        return NULL;
    }

    c_nnpntendd(); 

    Py_INCREF(Py_None);
    return Py_None;
} 

 /*************************************************************************
 *                                                                        *
 * METHOD REGISTRATION TABLE: NAME-STRING -> FUNCTION-POINTER
 * 
 *                                                                        *
\**************************************************************************/
static struct PyMethodDef nat_methods[] = {

  { "natgrids",    (PyCFunction)nat_c_natgrids,      METH_VARARGS,   nat_c_natgrids__doc__    },
  { "seti",        (PyCFunction)nat_c_nnseti,        METH_VARARGS,   nat_c_nnseti__doc__      },
  { "geti",        (PyCFunction)nat_c_nngeti,        METH_VARARGS,   nat_c_nngeti__doc__      },
  { "setr",        (PyCFunction)nat_c_nnsetr,        METH_VARARGS,   nat_c_nnsetr__doc__      },
  { "getr",        (PyCFunction)nat_c_nngetr,        METH_VARARGS,   nat_c_nngetr__doc__      },
  { "setc",        (PyCFunction)nat_c_nnsetc,        METH_VARARGS,   nat_c_nnsetc__doc__      },
  { "getc",        (PyCFunction)nat_c_nngetc,        METH_VARARGS,   nat_c_nngetc__doc__      },
  { "getaspects",  (PyCFunction)nat_c_nngetaspects,  METH_VARARGS,   nat_c_nngetaspects__doc__},
  { "getslopes",   (PyCFunction)nat_c_nngetslopes,   METH_VARARGS,   nat_c_nngetslopes__doc__ },
  { "pntinits",    (PyCFunction)nat_c_nnpntinits,    METH_VARARGS,   nat_c_nnpntinits__doc__  },
  { "pnts",        (PyCFunction)nat_c_nnpnts,        METH_VARARGS,   nat_c_nnpnts__doc__      },
  { "pntend",      (PyCFunction)nat_c_nnpntend,      METH_VARARGS,   nat_c_nnpntend__doc__    },

  { "natgridd",    (PyCFunction)nat_c_natgridd,      METH_VARARGS,   nat_c_natgridd__doc__    },
  { "setrd",       (PyCFunction)nat_c_nnsetrd,       METH_VARARGS,   nat_c_nnsetrd__doc__     },
  { "getrd",       (PyCFunction)nat_c_nngetrd,       METH_VARARGS,   nat_c_nngetrd__doc__     },
  { "getaspectd",  (PyCFunction)nat_c_nngetaspectd,  METH_VARARGS,   nat_c_nngetaspectd__doc__},
  { "getsloped",   (PyCFunction)nat_c_nngetsloped,   METH_VARARGS,   nat_c_nngetsloped__doc__ },
  { "pntinitd",    (PyCFunction)nat_c_nnpntinitd,    METH_VARARGS,   nat_c_nnpntinitd__doc__  },
  { "pntd",        (PyCFunction)nat_c_nnpntd,        METH_VARARGS,   nat_c_nnpntd__doc__      },
  { "pntendd",     (PyCFunction)nat_c_nnpntendd,     METH_VARARGS,   nat_c_nnpntendd__doc__   },
  {  NULL,      NULL,     0,     NULL }
};

 /*************************************************************************
 *                                                                        *
 * INITIALIZATION FUNCTION        
 * 
 *                                                                        *
\**************************************************************************/
void initnatgridmodule()
{
  PyObject *m, *d;
  
  /* create this module and add the functions */
  m = Py_InitModule("natgridmodule", nat_methods);
  import_array();

  /* add symbolic constants to the module */
  d = PyModule_GetDict(m);
  ErrorObject = Py_BuildValue("s", "natgridmodule.error"); 
  PyDict_SetItemString(d, "error", ErrorObject);  

  /* check for errors */
  if(PyErr_Occurred())
    Py_FatalError("can't initialize module natgridmodule");
}

 /*************************************************************************
 *                                                                        *
 * C FUNCTIONS USED IN THE C-EXTENSIONS                       
 *                                                                        *
\**************************************************************************/

/********************************************************************************
* Function:   write_int.c
*
*  Procedure: Uses 5d format to write 6 numbers per line
*  Purpose:   write ascii data for one horizontal field or one cross section
*
*  Passed:    Data, pointer to start of data
********************************************************************************/
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

/********************************************************************************
* Function:   write_float.c
*
*  Procedure: Uses 10.3e format to write 6 numbers per line
*  Purpose:   write ascii data for one horizontal field or one cross section
*
*  Passed:    Data, pointer to start of data
********************************************************************************/
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

/********************************************************************************
* Function:  print _float.c
*
*  Procedure: Uses 10.3e format to write 6 numbers per line
*  Purpose:   write ascii data for one horizontal field or one cross section
*
*  Passed:    Data, pointer to start of data
********************************************************************************/
void print_float(int size, char *title, float *data)
{

  int n;               /* data counter */
  int line;            /* counter to insert a line after each six numbers */
  float *d;            /* pointer to increment through data */

  d = data;
  line = 0;

  printf("\n%s\n", title);

  for(n=0; n<size; n++) {
    printf("%10.3e", *d);
    d++;
    line++;

    if(line == 8) {          /* add newline after writing 8 numbers */
      printf("\n");
      line = 0;
    }
  }
}

/********************************************************************************
* Function:   write_double.c
*
*  Procedure: Uses 21.15lf format to write 4 numbers per line
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


/********************************************************************************
* Function:   print_double.c
*
*  Procedure: Uses 21.15lf format to write 4 numbers per line
*  Purpose:   write ascii data for one horizontal field or one cross section
*
*  Passed:    data,  pointer to start of data
*             size,  length of data
*             title, label
*             fp,    file pointer
********************************************************************************/
void print_double(int size, char *title, double *data)
{

  int n;               /* data counter */
  int line;            /* counter to insert a line after each six numbers */
  double *d;           /* pointer to increment through data */

  d = data;
  line = 0;

  printf("\n%s\n", title);

  for(n=0; n<size; n++) {
    printf("%21.15lf", *d);
    d++;
    line++;

    if(line == 4) {          /* add newline after writing 8 numbers */
      printf("\n");
      line = 0;
    }
  }
}

