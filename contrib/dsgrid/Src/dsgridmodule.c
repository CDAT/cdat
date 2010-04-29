 /************************************************************************************************
 *                                                                                               *
 * dsgridmodule.c:  a C extension which exports the following functions to Python:               *
 *                                                                                               *
 *                   Single precision procedures:                                                *    
 *                                                                                               *
 *                       grid2s       - primary function for gridding 2D data.                   *
 *                       grid3s       - primary function for gridding 3D data.                   *
 *                       seti         - set int parameter values.                                *
 *                       geti         - retrieve values for int parameters.                      *
 *                       setr         - set float parameter values.                              *
 *                       getr         - retrieve values for float parameters                     *
 *                       setc         - set char parameter values.                               * 
 *                       getc         - retrieve values for char parameters.                     *
 *                       pnt2s        - interpolate 2D data at specified individual points.      *
 *                       pnt3s        - interpolate 3D data at specified individual points.      *
 *                                                                                               *
 *                                                                                               *
 *                   Double precision procedures:                                                *    
 *                                                                                               *
 *                       grid2d       - primary function for gridding 2D data.                   *
 *                       grid3d       - primary function for gridding 3D data.                   *
 *                       setrd        - set float parameter values.                              *
 *                       getrd        - retrieve values for float parameters                     *
 *                       pnt2d        - interpolate 2D data at specified individual points.      *
 *                       pnt3d        - interpolate 3D data at specified individual points.      *
 *                                                                                               *
\*************************************************************************************************/

#include "Python.h"
#include "numpy/arrayobject.h" 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define PRINTDSGRID  0
#define WRITEDSGRID  0
#define PRINTDSPNT  2
#define WRITEDSPNT  0

static PyObject *ErrorObject;            /* locally raised exception */

/*---------------------------------------- Macros -----         ---------------------------------*/

#define onError(message) { PyErr_SetString(ErrorObject, message); return NULL; }

/*--------------------------------- Function Prototypes -----------------------------------------*/

/* ----  external C functions in the library ---- */

extern float *c_dsgrid2s(int npnts, float x[], float y[], float z[], int numxout, int numyout,
                                                                                    float xo[], float yo[], int *ier);

extern float *c_dsgrid3s(int npnts, float x[], float y[], float z[], float u[], int numxout, int numyout, int numzout,
                                                                        float xo[], float yo[], float zo[], int *ier);

extern void c_dsseti(char *pnam, int ival);

extern void c_dsgeti(char *pnam, int *ival);

extern void c_dssetr(char *pnam, float fval);

extern void c_dsgetr(char *pnam, float *fval);

extern void c_dssetc(char *pnam, char *cval);

extern void c_dsgetc(char *pnam, char *cval);

extern void c_dspnt2s(int n, float x[], float y[], float z[], int m, float xo[], float yo[], float *zo, int *ier);

extern void c_dspnt3s(int n, float x[], float y[], float z[], float u[], int m, float xo[], float yo[], float zo[],
                                                                        float *uo, int *ier);


extern double *c_dsgrid2d(int npnts, double x[], double y[], double z[], int numxout, int numyout, double xo[],
                                                                                             double yo[], int *ier);

extern double *c_dsgrid3d(int npnts, double x[], double y[], double z[], double u[], int numxout, int numyout, int numzout,
                                                                        double xo[], double yo[], double zo[], int *ier);

extern void c_dssetrd(char *pnam, double dval);

extern void c_dsgetrd(char *pnam, double *dval);

extern void c_dspnt2d(int n, double x[], double y[], double z[], int m, double xo[], double yo[], double *zo, int *ier);

extern void c_dspnt3d(int n, double x[], double y[], double z[], double u[], int m, double xo[], double yo[], double zo[],
                                                                        double *uo, int *ier);


/* ----  functions in this file called by c extensions ---- */

void write_int(int size, char *title, FILE *fp, int *data);

void write_float(int size, char *title, FILE *fp, float *data);
void print_float(int size, char *title, float *data);

void write_double(int size, char *title, FILE *fp, double *data);
void print_double(int size, char *title, double *data);


 /*************************************************************************************************
 **************************************************************************************************
 *                                                                                                *
 *                            EXPORTED MODULE METHOD-FUNCTIONS                                    *
 *                                                                                                * 
 *                                                                                                *
 **************************************************************************************************
 **************************************************************************************************/

static char ds_c_dsgrid2s__doc__[] = 
"\n" 
"                          grid2s - Primary gridding function\n"
"\n" 
" grid2s is the C single-precision function that does an interpolation from 2D random data\n"
" to a output grid. grid2s is called after all the desired values for the control parameters\n"
" have been set using the procedures seti, setr and setc.\n"
"\n" 
" grid2s returns a pointer to a linear array of data that is the interpolated grid stored in\n"
" row-major order. That is, if out is declared as\n"
"           float *out;\n" 
" and we set:\n"
"           out = grid2s(npnts, x, y, z, numxout, numyout, xo, yo, &ier);\n"
" then out[i*numyout + j] is the interpolated value at coordinate point (xo[i], y[j]) for\n"
" 0 <= i < numxout and 0 <= j < numyout. The space for out is allocated internal to grid2s\n"
" and is numxout*numyout floats in size. In other words, out is of the form out[i][j] where the\n"
" first dimension indexed with i corresponds to the x direction.\n"
"\n" 
"  ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n" 
" Prototype:\n"
"\n" 
"  extern float *c_dsgrid2s(int npnts, float x[], float y[], float z[], int numxout,\n"
"                                        int numyout, float xo[], float yo[], int *ier);\n"
"\n" 
" Call from Python:\n"
"\n" 
"   out, ier = dsgrid2s(npnts, x, y, z, numxout, numyout, xo, yo)\n"
"\n" 
" where:\n"
"\n" 
"    npnts -- the number of input data points\n"
"\n" 
"    x -- array of size npnts contaning the x coordinates of the input data points\n"
"\n" 
"    y -- array of size npnts contaning the y coordinates of the input data points\n"
"\n" 
"    z -- array of size npnts contaning the functional values of the input data points. That is,\n"
"         z[j] is the value of the input function at coordinate (x[j], y[j]), for 0 <= j < npnts.\n" 
"\n" 
"    numxout -- the number of x values in the output grid,\n"
"\n" 
"    numyout -- the number of y values in the output grid.\n" 
"\n" 
"    xo -- array of size numxout contaning the x coordinates of the output data grid. The values\n"
"          of xo must be increasing, but need not be equally spaced.\n"
"\n" 
"    yo -- array of size numyout contaning the y coordinates of the output data grid. The values\n"
"          of yo must be increasing, but need not be equally spaced.\n"
"\n" 
"\n"; 

static PyObject *ds_c_dsgrid2s(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* the number of input data points */

    int numxout;                     /* the number of x values in the output grid */
    int numyout;                     /* the number of y values in the output grid */

    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the z coordinates of the input data points */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */

    /* fields required by call to c function*/

    float *out;                      /* a pointer to a linear array of the interpolated data */

    int ier;                         /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                        /* array object to accept the data and return it to Python */
    int dims[2];                                      /* used in creating object_out */

    /* declarations for writes to a file */

    FILE *fp;                                                                 /* File used in ascii write */ 
    char *title[6] = { "x", "y ", "z", "xo", "yo", "result"};                 /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

    if(!PyArg_ParseTuple(args, "iOOOiiOO", &npnts, &object_x, &object_y, &object_z, 
                                           &numxout, &numyout,  &object_xo, &object_yo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgrid2s is wrong.\n");
        return NULL;
    }

    out = (float *)c_dsgrid2s(npnts, (float *)object_x->data, (float *)object_y->data, (float *)object_z->data,
                                           numxout, numyout, (float *)object_xo->data, (float *)object_yo->data, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */

    dims[0] = numxout;
    dims[1] = numyout; 

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(2, dims, PyArray_FLOAT, (char *)out);


    if (PRINTDSGRID == 1) {
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

    if (WRITEDSGRID == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dsgrid2s.asc", "w")) == NULL) {
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

        fclose(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char ds_c_dsgrid3s__doc__[] = 
"\n"
"                          grid3s - Primary gridding function\n" 
"\n"
"  grid3s is the C single-precision function that does an interpolation from 3D random data\n"
"  to a output grid. grid3s is called after all the desired values for the control parameters\n"
"  have been set using the procedures seti, setr and setc.\n"
"\n"
" grid3s returns a pointer to a linear array of data that is the interpolated grid stored in\n"
" row-major order. That is, if out is declared as\n" 
"           float *out;\n"
" and we set:\n"
"           out = grid3s(npnts, x, y, z, u, numxout, numyout, numzout, xo, yo, zo, &ier);\n"
" then out[numzout*numyout*i + numzout*j + k] is the interpolated value at coordinate point\n"
" (xo[i], y[j], z[k]) for 0 <= i < numxout, 0 <= j < numyout and 0 <= k < numzout.The space for\n"
"  out is allocated internal to grid3s and is numxout*numyout*numzout floats in size.\n"
"\n"
"  ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n"
" Prototype:\n"
"\n"
" extern float *c_dsgrid3s(int npnts, float x[], float y[], float z[], float u[], int numxout,\n"
"                       int numyout, int numzout, float xo[], float yo[], float zo[], *int *ier);\n"
"\n"
" Call from Python:\n"
"\n"
"   out,  ier = grid3s(npnts, x, y, z, u, numxout, numyout, numzout, xo, yo, zo)\n"
"\n"
" where:\n"
"\n"
"    npnts -- the number of input data points\n"
"\n"
"    x -- array of size npnts contaning the x coordinates of the input data points\n"
"\n"
"    y -- array of size npnts contaning the y coordinates of the input data points\n"
"\n"
"    z -- array of size npnts contaning the z coordinates of the input data points\n"
"\n"
"    u -- array of size npnts contaning the functional values of the input data points. That is,\n"
"         u[n] is the value of the input function at the coordinates (x[n], y[n], z[n]), for\n"
"         0 <= n < npnts.\n"
"\n"
"    numxout -- the number of x values in the output grid.\n"
"\n"
"    numyout -- the number of y values in the output grid.\n"
"\n"
"    numzout -- the number of z values in the output grid.\n"
"\n"
"    xo -- array of size numxout contaning the x coordinates of the output data grid. The values\n"
"          of xo must be increasing, but need not be equally spaced.\n"
"\n"
"    yo -- array of size numyout contaning the y coordinates of the output data grid. The values\n"
"          of yo must be increasing, but need not be equally spaced.\n"
"\n"
"    zo -- array of size numzout contaning the z coordinates of the output data grid. The values\n"
"          of zo must be increasing, but need not be equally spaced.\n" 
"\n" 
"\n"; 

static PyObject *ds_c_dsgrid3s(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* the number of input data points */

    int numxout;                     /* the number of x values in the output grid */
    int numyout;                     /* the number of y values in the output grid */
    int numzout;                     /* the number of z values in the output grid */

    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the z coordinates of the input data points */
    PyArrayObject *object_u;         /* object pointer contaning the functional values of the input data */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */
    PyArrayObject *object_zo;        /* object pointer contaning the z coordinates of the output data grid */

    /* fields required by call to c function*/

    float *out;                       /* a pointer to a linear array of the interpolated data */

    int ier;                          /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                        /* array object to accept the data and return it to Python */
    int dims[3];                                      /* used in creating object_out */


    /* declarations for writes to a file */

    FILE *fp;                                                                             /* File used in ascii write */ 
    char *title[8] = { "x", "y ", "z", "u", "xo", "yo", "zo", "result"};                  /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iOOOOiiiOOO", &npnts, &object_x, &object_y, &object_z, &object_u,
                                 &numxout, &numyout, &numzout, &object_xo, &object_yo, &object_zo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgrid3s is wrong.\n");
        return NULL;
    }

    out = (float *)c_dsgrid3s(npnts, (float *)object_x->data, (float *)object_y->data, (float *)object_z->data,
                                (float *)object_u->data,  numxout, numyout, numzout, (float *)object_xo->data,
                                          (float *)object_yo->data, (float *)object_zo->data, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */

    dims[0] = numxout;
    dims[1] = numyout; 
    dims[2] = numzout; 

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(3, dims, PyArray_FLOAT, (char *)out);


    if (PRINTDSGRID == 1) {
        /* -------- print data to the screen ---------- */
        printf("npnts: %d\n", npnts);
        printf("numxout: %d\n", numxout);
        printf("numyout: %d\n", numyout);
        print_float(npnts, title[0], (float *)object_x->data);
        print_float(npnts, title[1], (float *)object_x->data);
        print_float(npnts, title[2], (float *)object_z->data);
        print_float(npnts, title[3], (float *)object_u->data);
        print_float(numxout, title[4], (float *)object_xo->data);
        print_float(numyout, title[5], (float *)object_yo->data);
        print_float(numyout, title[6], (float *)object_zo->data);
        print_float(numxout*numyout, title[7], (float *)object_out->data);
    }

    if (WRITEDSGRID == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dsgrid3s.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "npnts: %d\n", npnts);
        fprintf(fp, "numxout: %d\n", numxout);
        fprintf(fp, "numyout: %d\n", numyout);
        write_float(npnts, title[0], fp, (float *)object_x->data);
        write_float(npnts, title[1], fp, (float *)object_y->data);
        write_float(npnts, title[2], fp, (float *)object_z->data);
        write_float(npnts, title[3], fp, (float *)object_u->data);
        write_float(numxout, title[4], fp, (float *)object_xo->data);
        write_float(numyout, title[5], fp, (float *)object_yo->data);
        write_float(numyout, title[6], fp, (float *)object_zo->data);
        write_float(numxout*numyout, title[7], fp, (float *)object_out->data);

        fclose(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char ds_c_dsseti__doc__[] =
"\n" 
"                              seti - Set int valued parameters\n"
"\n" 
"  seti is used to set values for any of the control parameters that take int values. The\n"
"  values set by seti remain in effect until changed by subsequent calls to seti.\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void nnseti(char *pnam, int ival);\n"
"\n" 
" Call from Python:\n"
"\n" 
"     seti(pnam, ival)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter to be assigned an int value.\n"
"\n" 
"   ival -- the value to be assigned to the control parameter whose name is pointed to by pnam.\n"    
"\n"; 

static PyObject *ds_c_dsseti(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter to be assigned an int value */
    int ival;                 /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "si", &pnam, &ival))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsseti is wrong.\n");
        return NULL;
    }

    c_dsseti(pnam, ival);

    Py_INCREF(Py_None);
    return Py_None;
} 


static char ds_c_dsgeti__doc__[] =
"\n" 
"                              geti - Retrieve an int valued parameter\n" 
"\n" 
"  geti is called to obtain current values for any of the int valued control parameters.\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void c_nngeti(char *pnam, int *ival);\n"
"\n" 
" Call from Python:\n"
"\n" 
"    ival = geti(pnam)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter which is assigned an int value.\n" 
"\n" 
"   ival -- the value currently assigned to the control parameter whose name is pointed to by pnam.\n"    
"\n" 
"\n"; 

static PyObject *ds_c_dsgeti(PyObject *self, PyObject *args) 
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
        PyErr_SetString(PyExc_TypeError, "Pass to dsgeti is wrong.\n");
        return NULL;
    }

    c_dsgeti(pnam, &ival);

    return Py_BuildValue("i", ival);
} 

static char ds_c_dssetr__doc__[] =
"\n" 
"                              setr - Set float valued parameters\n"
"\n" 
"  setr is used to set values for any of the control parameters that take float values. The\n"
"  values set by setr remain in effect until changed by subsequent calls to setr.\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void c_nnsetr(char *pnam, float fval);\n"
"\n" 
" Call from Python:\n"
"\n" 
"   setr(pnam, fval)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter to be assigned an int value.\n"
"\n" 
"   fval -- the value to be assigned to the control parameter whose name is pointed to by pnam.\n"   
"\n" 
"\n"; 

static PyObject *ds_c_dssetr(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter to be assigned an int value */
    float fval;               /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/
 
    if(!PyArg_ParseTuple(args, "sf", &pnam, &fval))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dssetr is wrong.\n");
        return NULL;
    }

    c_dssetr(pnam, fval);

    Py_INCREF(Py_None);
    return Py_None;
} 

static char ds_c_dsgetr__doc__[] =
"\n" 
"                              getr - Retrieve an float valued parameter\n"
"\n" 
"  getr is called to obtain current values for any of the float valued control parameters.\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void c_nngetr(char *pnam, int *fval);\n"
"\n" 
" Call from Python:\n"
"\n" 
"    fval = getr(pnam)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter which is assigned a float value.\n"
"\n" 
"   fval -- the value currently assigned to the control parameter whose name is pointed to by pnam.\n"    
"\n" 
"\n"; 

static PyObject *ds_c_dsgetr(PyObject *self, PyObject *args) 
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
        PyErr_SetString(PyExc_TypeError, "Pass to dsgetr is wrong.\n");
        return NULL;
    }

    c_dsgetr(pnam, &fval);

    return Py_BuildValue("f", fval);
} 

static char ds_c_dssetc__doc__[] =
"\n" 
"                              setc - Set char valued parameters\n" 
"\n" 
"  setc is used to set values for any of the control parameters that take string values. The\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void c_nnsetc(char *pnam, char *cval);\n"
"\n" 
" Call from Python:\n"
"\n" 
"     setc(pnam, cval)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter to be assigned a char value.\n"
"\n" 
"   cval -- the value to be assigned to the control parameter whose name is pointed to by pnam.\n"    
"\n" 
"\n"; 

static PyObject *ds_c_dssetc(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;                   /* the name of the control parameter to be assigned an char value */
    char *cval;                   /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "ss", &pnam, &cval))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dssetc is wrong.\n");
        return NULL;
    }

    c_dssetc(pnam, cval);

    Py_INCREF(Py_None);
    return Py_None;
} 

static char ds_c_dsgetc__doc__[] =
"\n" 
"                              getc - Retrieve an char valued parameter\n"
"\n" 
"  getc is called to obtain current values for any of the string valued control parameters.\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void c_nngetc(char *pnam, char *cval);\n"
"\n" 
" Call from Python:\n"
"\n" 
"    cval = getc(pnam)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter which is  assigned a string value.\n"
"\n" 
"   cval -- the value currently assigned to the control parameter whose name is pointed to by pnam.\n"    
"\n" 
"\n"; 

static PyObject *ds_c_dsgetc(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter whose value is to be retrieved */

    /* fields required by call to c function*/

    char cval[128];              /* the value currently assigned to the control parameter whose name is
                                 pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "s", &pnam))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgetc is wrong.\n");
        return NULL;
    }

    c_dsgetc(pnam, cval);

    return Py_BuildValue("s", cval);
} 

static char ds_c_dspnt2s__doc__[] = 
"\n" 
"                          pnt2s - Interpolate at specified points\n"
"\n" 
" This function is called to interpolate single precision 2D data at specified points.\n"
"\n" 
" Prototype:\n"
"\n" 
" extern void c_dspnt2s(int n, float x[], float y[], float z[], int m, float xo[], float yo[],\n"
"                                                                           float *zo, int *ier);\n"
"\n" 
" ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n" 
" Call from Python:\n"
"\n" 
"   zo, ier = pnt2s(n, x, y, z, m, xo, yo)\n"
"\n" 
" where:\n"
"\n" 
"    n -- the number of input data points\n"
"\n" 
"    x -- array of size n contaning the x coordinates of the input data points\n"
"\n" 
"    y -- array of size n contaning the y coordinates of the input data points\n"
"\n" 
"    z -- array of size n contaning the functional values of the input data points. That is,\n"
"         z[j] is the value of the input function at coordinate (x[j], y[j]), for j = 0,n-1.\n" 
"\n" 
"    m -- the number of output data points (this may be 1).\n"
"\n" 
"    xo -- array of size n contaning the x coordinates of the output data grid. The values of xo\n"
"          may be in any order.\n"
"\n" 
"    yo -- array of size n contaning the y coordinates of the output data grid. The values of yo\n"
"          may be in any order.\n"
"\n" 
"    zo -- a pointer to a float. Space for m floats must be allocated before calling dspnt2s.\n"
"          zo[i] is the interpolated value at point (xo[i], yo[i]), for i = 0,m-1.\n"
"\n" 
"\n"; 

static PyObject *ds_c_dspnt2s(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int n;                           /* the number of input data points */
    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the functional values of the input data points */

    int m;                           /* the number of output data points (this may be 1) */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */

    /* fields required by call to c function*/

    float *zo;                       /* Pointer to the returned data. Space for m floats is allocated for zo by dspnt2s.
                                        zo[i] is the interpolated value at point (xo[i], yo[i]), for i = 0,m-1. */

    int ier;                         /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                        /* array object to accept the data and return it to Python */
    int dims[1];                                      /* used in creating object_out */


    /* declarations for writes to a file */

    FILE *fp;                                                               /* File used in ascii write */ 
    char *title[6] = { "x", "y ", "z", "xo", "yo", "result"};               /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iOOOiOO", &n, &object_x, &object_y, &object_z, &m, &object_xo, &object_yo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgrid2s is wrong.\n");
        return NULL;
    }

    if( !(zo  = (float *)malloc(m * sizeof(float))) ) {
        printf("memory request failed for zo");
        exit(1);
    }

    c_dspnt2s(n, (float *)object_x->data, (float *)object_y->data, (float *)object_z->data,
               m, (float *)object_xo->data, (float *)object_yo->data, (float *)zo, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */
    dims[0] = m;

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(1, dims, PyArray_FLOAT, (char *)zo);

    if (PRINTDSPNT == 1) {
        /* -------- print data to the screen ---------- */
        printf("n: %d\n", n);
        printf("m: %d\n", m);
        print_float(n, title[0], (float *)object_x->data);
        print_float(n, title[1], (float *)object_x->data);
        print_float(n, title[2], (float *)object_z->data);
        print_float(m, title[3], (float *)object_xo->data);
        print_float(m, title[4], (float *)object_yo->data);
        print_float(m, title[5], (float *)object_out->data);
    }

    if (WRITEDSPNT == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dspnt2s.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "n: %d\n", n);
        fprintf(fp, "m: %d\n", m);
        write_float(n, title[0], fp, (float *)object_x->data);
        write_float(n, title[1], fp, (float *)object_y->data);
        write_float(n, title[2], fp, (float *)object_z->data);
        write_float(m, title[3], fp, (float *)object_xo->data);
        write_float(m, title[4], fp, (float *)object_yo->data);
        write_float(m, title[5], fp, (float *)object_out->data);

        fclose(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char ds_c_dspnt3s__doc__[] = 
"\n" 
"                          pnt3s - Interpolate at specified points\n"
"\n" 
" This function is called to interpolate single precision 3D data at specified points.\n"
"\n" 
" Prototype:\n"
"\n" 
" extern void c_dspnt3s(int n, float x[], float y[], float z[], float u[], int m,\n"
"                                     float xo[], float yo[], float zo[], float *uo, int *ier);\n"
"\n" 
" ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n" 
" Call from Python:\n"
"\n" 
"   uo, ier = dspnt3s(n, x, y, z, u, m, xo, yo, zo)\n"
"\n" 
" where:\n"
"\n" 
"    n -- the number of input data points\n"
"\n" 
"    x -- array  contaning the x coordinates of the input data points\n"
"\n" 
"    y -- array contaning the y coordinates of the input data points\n"
"\n" 
"    z -- array contaning the z coordinates of the input data points\n"
"\n" 
"    u -- array of size n contaning the functional values of the input data points. That is,\n"
"         u[j] is the value of the input function at coordinate (x[j], y[j], z[j]), for j = 0,n-1\n"
"\n" 
"    m -- the number of output data points (this may be 1).\n"
"\n" 
"    xo -- array of dimension m contaning the x coordinates of the output data grid. The values\n"
"          of xo may be in any order.\n"
"\n" 
"    yo -- array of dimension m contaning the y coordinates of the output data grid. The values\n"
"          of yo may be in any order.\n"
"\n" 
"    zo -- array of dimension m contaning the z coordinates of the output data grid. The values\n"
"          of zo may be in any order.\n"
"\n" 
"    uo -- a pointer to a float. Space for m floats must be allocated for uo before calling\n"
"          dspnt3s. uo[i] is the interpolated value at point (xo[i], yo[i], zo[i]), for i = 0,m-1.\n"
"\n" 
"\n"; 

static PyObject *ds_c_dspnt3s(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int n;                           /* the number of input data points */
    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_u;         /* object pointer contaning the functional values of the input data points */

    int m;                           /* the number of output data points (this may be 1) */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */
    PyArrayObject *object_zo;        /* object pointer contaning the y coordinates of the output data grid */

    /* fields required by call to c function*/

    int ier;                         /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    float *uo;                       /* Pointer to the returned data. Space for m floats is allocated for uo by dspnt23.
                                        uo[i] is the interpolated value at point (xo[i], yo[i], zo[i]), for i = 0,m-1. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                        /* array object to accept the data and return it to Python */
    int dims[1];                                      /* used in creating object_out */



    /* declarations for writes to a file */

    FILE *fp;                                                                           /* File used in ascii write */ 
    char *title[8] = { "x", "y ", "z", "u", "xo", "yo", "zo", "result"};                /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iOOOOiOOO", &n, &object_x, &object_y, &object_z, &object_u, &m,
                                                                &object_xo, &object_yo, &object_zo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgrid3s is wrong.\n");
        return NULL;
    }

    if( !(uo  = (float *)malloc(m * sizeof(float))) ) {
        printf("memory request failed for uo");
        exit(1);
    }

    c_dspnt3s(n, (float *)object_x->data, (float *)object_y->data, (float *)object_z->data,
                 (float *)object_u->data,  m, (float *)object_xo->data, (float *)object_yo->data,
                 (float *)object_zo->data, (float *)uo, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */

    dims[0] = m;

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(1, dims, PyArray_FLOAT, (char *)uo);

    if (PRINTDSPNT == 1) {
        /* -------- print data to the screen ---------- */
        printf("n: %d\n", n);
        printf("m: %d\n", m);
        print_float(n, title[0], (float *)object_x->data);
        print_float(n, title[1], (float *)object_x->data);
        print_float(n, title[2], (float *)object_z->data);
        print_float(n, title[3], (float *)object_u->data);
        print_float(m, title[4], (float *)object_xo->data);
        print_float(m, title[5], (float *)object_yo->data);
        print_float(m, title[6], (float *)object_zo->data);
        print_float(m, title[7], (float *)object_out->data);
    }

    if (WRITEDSPNT == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dspnt3s.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "n: %d\n", n);
        fprintf(fp, "m: %d\n", m);
        write_float(n, title[0], fp, (float *)object_x->data);
        write_float(n, title[1], fp, (float *)object_y->data);
        write_float(n, title[2], fp, (float *)object_z->data);
        write_float(n, title[3], fp, (float *)object_u->data);
        write_float(m, title[4], fp, (float *)object_xo->data);
        write_float(m, title[5], fp, (float *)object_yo->data);
        write_float(m, title[6], fp, (float *)object_zo->data);
        write_float(m, title[7], fp, (float *)object_out->data);

        fclose(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char ds_c_dsgrid2d__doc__[] = 
"\n" 
"                          grid2d - Primary gridding function\n"
"\n" 
" grid2d is the C double-precision function that does an interpolation from 2D random data\n"
" to a output grid. grid2d is called after all the desired values for the control parameters\n"
" have been set using the procedures seti, setrd and setc.\n"
"\n" 
" grid2d returns a pointer to a linear array of data that is the interpolated grid stored in\n"
" row-major order. That is, if out is declared as\n"
"           float *out;\n"
" and we set:\n"
"           out = grid2d(npnts, x, y, z, numxout, numyout, xo, yo, &ier);\n"
" then out[i*numyout + j] is the interpolated value at coordinate point (xo[i], y[j]) for\n"
" 0 <= i < numxout and 0 <= j < numyout. The space for out is allocated internal to grid2d\n"
" and is numxout*numyout doubles in size.\n"
"\n" 
"  ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n" 
" Prototype:\n"
"\n" 
"   extern double *c_dsgrid2d(int npnts, double x[], double y[], double z[], int numxout,\n"
"                                         int numyout, double xo[], double yo[], int *ier);\n"
"\n" 
" Call from Python:\n"
"\n" 
"   out, ier = dsgrid2d(npnts, x, y, z, numxout, numyout, xo, yo)\n"
"\n" 
" where:\n"
"\n" 
"    npnts -- the number of input data points\n"
"\n" 
"    x -- array of size npnts contaning the x coordinates of the input data points\n"
"\n" 
"    y -- array of size npnts contaning the y coordinates of the input data points\n"
"\n" 
"    z -- array of size npnts contaning the functional values of the input data points. That is,\n" 
"         z[j] is the value of the input function at coordinate (x[j], y[j]), for 0 <= j < npnts.\n" 
"\n" 
"    numxout -- the number of x values in the output grid.\n"
"\n" 
"    numyout -- the number of y values in the output grid.\n"
"\n" 
"    xo -- array of size numxout contaning the x coordinates of the output data grid. The values\n"
"          of xo must be increasing, but need not be equally spaced.\n"
"\n" 
"    yo -- array of size numxout contaning the y coordinates of the output data grid. The values\n"
"          of yo must be increasing, but need not be equally spaced.\n"
"\n" 
"\n"; 

static PyObject *ds_c_dsgrid2d(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* the number of input data points */
    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the z coordinates of the input data points */

    int numxout;                     /* the number of x values in the output grid */
    int numyout;                     /* the number of y values in the output grid */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */

    /* fields required by call to c function*/

    double *out;                     /* a pointer to a linear array of the interpolated data */

    int ier;                         /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                        /* array object to accept the data and return it to Python */
    int dims[2];                                      /* used in creating object_out */


    /* declarations for writes to a file */

    FILE *fp;                                                                    /* File used in ascii write */ 
    char *title[6] = {"x", "y ", "z", "xo", "yo", "result"};                     /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iOOOiiOO", &npnts, &object_x, &object_y, &object_z, 
                                           &numxout, &numyout,  &object_xo, &object_yo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgrid2d is wrong.\n");
        return NULL;
    }

    out = (double *)c_dsgrid2d(npnts, (double *)object_x->data, (double *)object_y->data, (double *)object_z->data,
                                           numxout, numyout, (double *)object_xo->data, (double *)object_yo->data, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */
    dims[0] = numxout;
    dims[1] = numyout; 

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(2, dims, PyArray_DOUBLE, (char *)out);


    if (PRINTDSGRID == 1) {
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

    if (WRITEDSGRID == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dsgrid2d.asc", "w")) == NULL) {
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

        fclose(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char ds_c_dsgrid3d__doc__[] = 
"\n" 
"                          grid3d - Primary gridding function\n"
"\n" 
"  grid3d is the C double-precision function that does an interpolation from 3D random data\n"
"  to a output grid. grid3d is called after all the desired values for the control parameters\n"
"  have been set using the procedures seti, setr and setc.\n"
"\n" 
" grid3s returns a pointer to a linear array of data that is the interpolated grid stored in\n"
" row-major order. That is, if out is declared as\n"
"           float *out;\n"
" and we set:\n"
"           out = grid3s(npnts, x, y, z, u, numxout, numyout, numzout, xo, yo, zo, &ier);\n"
" then out[numzout*numyout*i + numzout*j + k] is the interpolated value at coordinate point\n"
" (xo[i], y[j], z[k]) for 0 <= i < numxout, 0 <= j < numyout and 0 <= k < numzout.The space for\n"
"  out is allocated internal to grid3d and is numxout*numyout*numzout doubles in size.\n"
"\n" 
"  ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n" 
" Prototype:\n"
"\n" 
"   extern double *c_dsgrid3d(int npnts, double x[], double y[], double z[], double u[],\n"
"       int numxout, int numyout, int numzout, double xo[], double yo[], double zo[], *int *ier);\n"
"\n" 
" Call from Python:\n"
"\n" 
"   out,  ier = grid3d(npnts, x, y, z, u, numxout, numyout, numzout, xo, yo, zo)\n"
"\n" 
" where:\n"
"\n" 
"    npnts -- the number of input data points\n"
"\n" 
"    x -- array of size npnts contaning the x coordinates of the input data points\n"
"\n" 
"    y -- array of size npnts contaning the y coordinates of the input data points\n"
"\n" 
"    z -- array of size npnts contaning the z coordinates of the input data points\n"
"\n" 
"    u -- array of size npnts contaning the functional values of the input data points. That is,\n"
"         u[n] is the value of the input function at the coordinates (x[n], y[n], z[n]), for\n"
"         0 <= n < npnts.\n"
"\n" 
"    numxout -- the number of x values in the output grid.\n"
"\n" 
"    numyout -- the number of y values in the output grid.\n"
"\n" 
"    numzout -- the number of z values in the output grid.\n"
"\n" 
"    xo -- array of size numxout contaning the x coordinates of the output data grid. The values\n"
"          of xo must be increasing, but need not be equally spaced.\n"
"\n" 
"    yo -- array of size numxout contaning the y coordinates of the output data grid. The values\n"
"          of yo must be increasing, but need not be equally spaced.\n"
"\n" 
"    zo -- array of size numxout contaning the z coordinates of the output data grid. The values\n"
"          of zo must be increasing, but need not be equally spaced.\n"
"\n" 
"\n"; 

static PyObject *ds_c_dsgrid3d(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int npnts;                       /* the number of input data points */
    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the z coordinates of the input data points */
    PyArrayObject *object_u;         /* object pointer contaning the functional values of the input data */

    int numxout;                     /* the number of x values in the output grid */
    int numyout;                     /* the number of y values in the output grid */
    int numzout;                     /* the number of z values in the output grid */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */
    PyArrayObject *object_zo;        /* object pointer contaning the z coordinates of the output data grid */

    /* fields required by call to c function*/

    double *out;                     /* a pointer to a linear array of the interpolated data */

    int ier;                         /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                  /* array object to accept the data and return it to Python */
    int dims[3];                                      /* used in creating object_out */


    /* declarations for writes to a file */

    FILE *fp;                                                                       /* File used in ascii write */ 
    char *title[8] = { "x", "y ", "z", "u", "xo", "yo", "zo", "result"};             /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iOOOOiiiOOO", &npnts, &object_x, &object_y, &object_z, &object_u,
                                 &numxout, &numyout, &numzout, &object_xo, &object_yo, &object_zo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgrid3d is wrong.\n");
        return NULL;
    }

    out = (double *)c_dsgrid3d(npnts, (double *)object_x->data, (double *)object_y->data, (double *)object_z->data,
                                (double *)object_u->data,  numxout, numyout, numzout, (double *)object_xo->data,
                                          (double *)object_yo->data, (double *)object_zo->data, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */

    dims[0] = numxout;
    dims[1] = numyout; 
    dims[2] = numzout; 

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(3, dims, PyArray_DOUBLE, (char *)out);


    if (PRINTDSGRID == 1) {
        /* -------- print data to the screen ---------- */
        printf("npnts: %d\n", npnts);
        printf("numxout: %d\n", numxout);
        printf("numyout: %d\n", numyout);
        print_double(npnts, title[0], (double *)object_x->data);
        print_double(npnts, title[1], (double *)object_x->data);
        print_double(npnts, title[2], (double *)object_z->data);
        print_double(npnts, title[3], (double *)object_u->data);
        print_double(numxout, title[4], (double *)object_xo->data);
        print_double(numyout, title[5], (double *)object_yo->data);
        print_double(numyout, title[6], (double *)object_zo->data);
        print_double(numxout*numyout, title[7], (double *)object_out->data);
    }

    if (WRITEDSGRID == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dsgrid3d.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "npnts: %d\n", npnts);
        fprintf(fp, "numxout: %d\n", numxout);
        fprintf(fp, "numyout: %d\n", numyout);
        write_double(npnts, title[0], fp, (double *)object_x->data);
        write_double(npnts, title[1], fp, (double *)object_y->data);
        write_double(npnts, title[2], fp, (double *)object_z->data);
        write_double(npnts, title[3], fp, (double *)object_u->data);
        write_double(numxout, title[4], fp, (double *)object_xo->data);
        write_double(numyout, title[5], fp, (double *)object_yo->data);
        write_double(numyout, title[6], fp, (double *)object_zo->data);
        write_double(numxout*numyout, title[7], fp, (double *)object_out->data);

        fclose(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);
} 


static char ds_c_dssetrd__doc__[] =
"\n" 
"                          setrd - Set double-precision parameters values\n"
"\n" 
"  setrd is used to set values for any of the control parameters that take double-precision\n"
"  values. The values set by setd remain in effect until changed by subsequent calls to\n"
"  setrd.\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void c_nnsetrd(char *pnam, double dval);\n"
"\n" 
" Call from Python:\n"
"\n" 
"   setrd(pnam, dval)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter to be assigned an double-precision value.\n"
"\n" 
"   dval -- the value to be assigned to the control parameter whose name is pointed to by pnam.\n"    
"\n" 
"\n"; 

static PyObject *ds_c_dssetrd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter to be assigned an int value */
    double dval;               /* value to be assigned to the control parameter whose name is pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "sd", &pnam, &dval))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dssetrd is wrong.\n");
        return NULL;
    }

    c_dssetrd(pnam, dval);

    Py_INCREF(Py_None);
    return Py_None;
} 

static char ds_c_dsgetrd__doc__[] =
"\n" 
"                          getrd - Retrieve an double-precision parameter\n"
"\n" 
"  getrd is called to obtain current values for any of the double-precision control parameters.\n"
"\n" 
" Prototype:\n"
"\n" 
"    extern void c_nngetrd(char *pnam, int *dval);\n"
"\n" 
" Call from Python:\n"
"\n" 
"    dval = getrd(pnam)\n"
"\n" 
" where:\n"
"\n" 
"   pnam -- the name of the control parameter to be assigned a double-precision value.\n"
"\n" 
"   dval -- the value currently assigned to the control parameter whose name is pointed to by pnam.\n"    
"\n" 
"\n"; 

static PyObject *ds_c_dsgetrd(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    char *pnam;               /* the name of the control parameter whose value is to be retrieved */

    /* fields required by call to c function*/

    double dval;               /* the value currently assigned to the control parameter whose name is
                                  pointed to by pnam */

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "s", &pnam))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dsgetrd is wrong.\n");
        return NULL;
    }

    c_dsgetrd(pnam, &dval);

    return Py_BuildValue("d", dval);
} 

static char ds_c_dspnt2d__doc__[] = 
"\n" 
"                          pnt2d - Interpolate at specified points\n"
"\n" 
" This function is called to interpolate double precision 2D data at specified points.\n"
"\n" 
" Prototype:\n"
"\n" 
"   extern void c_dspnt2d(int n, double x[], double y[], double z[], int m, double xo[],\n"
"                                                        double yo[], double *zo, int *ier);\n"
"\n" 
" ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n" 
" Call from Python:\n"
"\n" 
"   zo, ier = pnt2d(n, x, y, z, m, xo, yo)\n"
"\n" 
" where:\n"
"\n" 
"    n -- the number of input data points\n"
"\n" 
"    x -- array of size n contaning the x coordinates of the input data points\n"
"\n" 
"    y -- array of size n contaning the y coordinates of the input data points\n"
"\n" 
"    z -- array of size n contaning the functional values of the input data points. That is,\n"
"         z[j] is the value of the input function at coordinate (x[j], y[j]), for j = 0,n-1.\n" 
"\n" 
"    m -- the number of output data points (this may be 1).\n"
"\n" 
"    xo -- array of size n contaning the x coordinates of the output data grid. The values of xo\n"
"          may be in any order.\n"
"\n" 
"    yo -- array of size n contaning the y coordinates of the output data grid. The values of yo\n"
"          may be in any order.\n"
"\n" 
"    zo -- a pointer to a float. Space for m doubles must be allocated for zo before calling\n"
"          dspnt2d.  zo[i] is the interpolated value at point (xo[j], yo[j]), for i = 0,m-1.\n"
"\n" 
"\n"; 

static PyObject *ds_c_dspnt2d(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int n;                           /* the number of input data points */
    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the functional values of the input data points */

    int m;                           /* the number of output data points (this may be 1) */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */

    /* fields required by call to c function*/

    double *zo;                      /* Pointer to the returned data. Space for m floats is allocated for zo by dspnt2s.
                                        zo[i] is the interpolated value at point (xo[i], yo[i]), for i = 0,m-1. */

    int ier;                         /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                        /* array object to accept the data and return it to Python */
    int dims[1];                                      /* used in creating object_out */



    /* declarations for writes to a file */

    FILE *fp;                                                                  /* File used in ascii write */ 
    char *title[6] = { "x", "y ", "z", "xo", "yo", "result"};                   /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iOOOiOO", &n, &object_x, &object_y, &object_z, &m, &object_xo, &object_yo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dspnt2d is wrong.\n");
        return NULL;
    }

    if( !(zo  = (double *)malloc(m * sizeof(double))) ) {
        printf("memory request failed for zo");
        exit(1);
    }

    c_dspnt2d(n, (double *)object_x->data, (double *)object_y->data, (double *)object_z->data,
               m, (double *)object_xo->data, (double *)object_yo->data, (double *)zo, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */
    dims[0] = m;

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(1, dims, PyArray_DOUBLE, (char *)zo);

    if (PRINTDSPNT == 1) {
        /* -------- print data to the screen ---------- */
        printf("n: %d\n", n);
        printf("m: %d\n", m);
        print_double(n, title[0], (double *)object_x->data);
        print_double(n, title[1], (double *)object_x->data);
        print_double(n, title[2], (double *)object_z->data);
        print_double(m, title[3], (double *)object_xo->data);
        print_double(m, title[4], (double *)object_yo->data);
        print_double(m, title[5], (double *)object_out->data);
    }

    if (WRITEDSPNT == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dspnt2d.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "n: %d\n", n);
        fprintf(fp, "m: %d\n", m);
        write_double(n, title[0], fp, (double *)object_x->data);
        write_double(n, title[1], fp, (double *)object_y->data);
        write_double(n, title[2], fp, (double *)object_z->data);
        write_double(m, title[3], fp, (double *)object_xo->data);
        write_double(m, title[4], fp, (double *)object_yo->data);
        write_double(m, title[5], fp, (double *)object_out->data);

        fclose(fp);
    }


    return Py_BuildValue(("Oi"), object_out, ier);
} 

static char ds_c_dspnt3d__doc__[] = 
"\n" 
"                          pnt3d - Interpolate at specified points\n"
"\n" 
" This function is called to interpolate double-precision 3D data at specified points.\n"
"\n" 
" Prototype:\n"
"\n" 
"   extern void c_dspnt3d(int n, double x[], double y[], double z[], double u[], int m,\n"
"                            double xo[], double yo[], double zo[], double *uo, int *ier);\n"
"\n" 
" ier is an error return value. If ier is returned as 0, then no errors were detected.\n"
"\n" 
" Call from Python:\n"
"\n" 
"   uo, ier = dspnt3d(n, x, y, z, u, m, xo, yo, zo)\n"
"\n" 
" where:\n"
"\n" 
"    n -- the number of input data points\n"
"\n" 
"    x -- array  contaning the x coordinates of the input data points\n"
"\n" 
"    y -- array contaning the y coordinates of the input data points\n"
"\n" 
"    z -- array contaning the z coordinates of the input data points\n"
"\n" 
"    u -- array of size n contaning the functional values of the input data points. That is,\n"
"         u[j] is the value of the input function at coordinate (x[j], y[j], z[j]), for j = 0,n-1\n"
"\n" 
"    m -- the number of output data points (this may be 1).\n"
"\n" 
"    xo -- array of dimension m contaning the x coordinates of the output data grid. The values\n"
"          of xo may be in any order.\n"
"\n" 
"    yo -- array of dimension m contaning the y coordinates of the output data grid. The values\n"
"          of yo may be in any order.\n"
"\n" 
"    zo -- array of dimension m contaning the z coordinates of the output data grid. The values\n"
"          of zo may be in any order.\n"
"\n" 
"    uo -- a pointer to a float. Space for m doubles must be allocated for uo before calling\n"
"          dspnt3d. uo[i] is the interpolated value at point (xo[i], yo[i], zo[i]), for i = 0,m-1.\n"
"\n" 
"\n"; 

static PyObject *ds_c_dspnt3d(PyObject *self, PyObject *args) 
{
    /* ----------------------- Declarations ------------------------------------*/

    /* fields which are passed from Python in the args tuple */

    int n;                           /* the number of input data points */
    PyArrayObject *object_x;         /* object pointer contaning the x coordinates of the input data points */
    PyArrayObject *object_y;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_z;         /* object pointer contaning the y coordinates of the input data points */
    PyArrayObject *object_u;         /* object pointer contaning the functional values of the input data points */

    int m;                           /* the number of output data points (this may be 1) */
    PyArrayObject *object_xo;        /* object pointer contaning the x coordinates of the output data grid */
    PyArrayObject *object_yo;        /* object pointer contaning the y coordinates of the output data grid */
    PyArrayObject *object_zo;        /* object pointer contaning the y coordinates of the output data grid */

    /* fields required by call to c function*/

    double *uo;                      /* Pointer to the returned data. Space for m floats is allocated for uo by dspnt23.
                                        uo[i] is the interpolated value at point (xo[i], yo[i], zo[i]), for i = 0,m-1. */

    int ier;                         /* An error return value. If ier is returned as 0, then no errors were detected.
                                        If ier is non-zero, then refer to the list in the error table for details. */

    /* fields required to construct the return of result to python */

    PyArrayObject *object_out;                        /* array object to accept the data and return it to Python */
    int dims[1];                                      /* used in creating object_out */


    /* declarations for writes to a file */

    FILE *fp;                                                                       /* File used in ascii write */ 
    char *title[8] = { "x", "y ", "z", "u", "xo", "yo", "zo", "result"};            /* Titles for print to file */ 

    /* ----------------------- Start Execution ------------------------------------*/

 
    if(!PyArg_ParseTuple(args, "iOOOOiOOO", &n, &object_x, &object_y, &object_z, &object_u, &m,
                                                                &object_xo, &object_yo, &object_zo))
    {
        PyErr_SetString(PyExc_TypeError, "Pass to dspnt3d is wrong.\n");
        return NULL;
    }

    if( !(uo  = (double *)malloc(m * sizeof(double))) ) {
        printf("memory request failed for uo");
        exit(1);
    }

    c_dspnt3d(n, (double *)object_x->data, (double *)object_y->data, (double *)object_z->data,
                 (double *)object_u->data,  m, (double *)object_xo->data, (double *)object_yo->data, 
                 (double *)object_zo->data, (double *)uo, &ier); 

    /* -------- create a NumPy array housing the C language data out ----------- */
    dims[0] = m;

    object_out = (PyArrayObject *)PyArray_FromDimsAndData(1, dims, PyArray_DOUBLE, (char *)uo);

    if (PRINTDSPNT == 1) {
        /* -------- print data to the screen ---------- */
        printf("n: %d\n", n);
        printf("m: %d\n", m);
        print_double(n, title[0], (double *)object_x->data);
        print_double(n, title[1], (double *)object_x->data);
        print_double(n, title[2], (double *)object_z->data);
        print_double(n, title[3], (double *)object_u->data);
        print_double(m, title[4], (double *)object_xo->data);
        print_double(m, title[5], (double *)object_yo->data);
        print_double(m, title[6], (double *)object_zo->data);
        print_double(m, title[7], (double *)object_out->data);
    }

    if (WRITEDSPNT == 1) {
        /* -------- write data to a file ----------- */
        if((fp = fopen("dspnt3d.asc", "w")) == NULL) {
            PyErr_SetString(PyExc_IOError, "Can not open file to write checks");
            return NULL;
        }

        fprintf(fp, "n: %d\n", n);
        fprintf(fp, "m: %d\n", m);
        write_double(n, title[0], fp, (double *)object_x->data);
        write_double(n, title[1], fp, (double *)object_y->data);
        write_double(n, title[2], fp, (double *)object_z->data);
        write_double(n, title[3], fp, (double *)object_u->data);
        write_double(m, title[4], fp, (double *)object_xo->data);
        write_double(m, title[5], fp, (double *)object_yo->data);
        write_double(m, title[6], fp, (double *)object_zo->data);
        write_double(m, title[7], fp, (double *)object_out->data);

        fclose(fp);
    }

    return Py_BuildValue(("Oi"), object_out, ier);

} 

 /*************************************************************************
 *                                                                        *
 * METHOD REGISTRATION TABLE: NAME-STRING -> FUNCTION-POINTER
 * 
 *                                                                        *
\**************************************************************************/

static struct PyMethodDef ds_methods[] = {

  { "grid2s",  (PyCFunction)ds_c_dsgrid2s,    METH_VARARGS,   ds_c_dsgrid2s__doc__  },
  { "grid3s",  (PyCFunction)ds_c_dsgrid3s,    METH_VARARGS,   ds_c_dsgrid3s__doc__  },
  { "seti",    (PyCFunction)ds_c_dsseti,      METH_VARARGS,   ds_c_dsseti__doc__    },
  { "geti",    (PyCFunction)ds_c_dsgeti,      METH_VARARGS,   ds_c_dsgeti__doc__    },
  { "setr",    (PyCFunction)ds_c_dssetr,      METH_VARARGS,   ds_c_dssetr__doc__    },
  { "getr",    (PyCFunction)ds_c_dsgetr,      METH_VARARGS,   ds_c_dsgetr__doc__    },
  { "setc",    (PyCFunction)ds_c_dssetc,      METH_VARARGS,   ds_c_dssetc__doc__    },
  { "getc",    (PyCFunction)ds_c_dsgetc,      METH_VARARGS,   ds_c_dsgetc__doc__    },
  { "pnt2s",   (PyCFunction)ds_c_dspnt2s,     METH_VARARGS,   ds_c_dspnt2s__doc__   },
  { "pnt3s",   (PyCFunction)ds_c_dspnt3s,     METH_VARARGS,   ds_c_dspnt3s__doc__   },

  { "grid2d",  (PyCFunction)ds_c_dsgrid2d,    METH_VARARGS,   ds_c_dsgrid2d__doc__  },
  { "grid3d",  (PyCFunction)ds_c_dsgrid3d,    METH_VARARGS,   ds_c_dsgrid3d__doc__  },
  { "setrd",   (PyCFunction)ds_c_dssetrd,     METH_VARARGS,   ds_c_dssetrd__doc__   },
  { "getrd",   (PyCFunction)ds_c_dsgetrd,     METH_VARARGS,   ds_c_dsgetrd__doc__   },
  { "pnt2d",   (PyCFunction)ds_c_dspnt2d,     METH_VARARGS,   ds_c_dspnt2d__doc__   },
  { "pnt3d",   (PyCFunction)ds_c_dspnt3d,     METH_VARARGS,   ds_c_dspnt3d__doc__   },

  {  NULL,      NULL,     0,     NULL }
};

 /*************************************************************************
 *                                                                        *
 * INITIALIZATION FUNCTION        
 * 
 *                                                                        *
\**************************************************************************/

void initdsgridmodule()
{
  PyObject *m, *d;
  
  /* create this module and add the functions */
  m = Py_InitModule("dsgridmodule", ds_methods);
  import_array();

  /* add symbolic constants to the module */
  d = PyModule_GetDict(m);
  ErrorObject = Py_BuildValue("s", "dsgridmodule.error"); 
  PyDict_SetItemString(d, "error", ErrorObject);  

  /* check for errors */
  if(PyErr_Occurred())
    Py_FatalError("can't initialize module dsgridmodule");
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

