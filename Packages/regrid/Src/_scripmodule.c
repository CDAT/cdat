#ifdef __CPLUSPLUS__
extern "C" {
#endif
#include "Python.h"
#include "Numeric/arrayobject.h"
 
static PyObject *ErrorObject;

static int array_really_contiguous(PyArrayObject *ap) {
      int sd;
      int i;

      sd = ap->descr->elsize;
      for (i = ap->nd-1; i >= 0; --i) {
              if (ap->dimensions[i] == 0) return 1; /* contiguous by definition */
              if (ap->strides[i] != sd) return 0;
              sd *= ap->dimensions[i];
      }
      return 1;
}

struct fcomplex {
    float r;
    float i;
    };
typedef struct fcomplex Py_complex_float;
#define TRANSPOSE_OPTION 0
#define MIRROR_OPTION 0
#define get_fortran_dim(v,n) v->dimensions[(n)-1]

/* 
    Built by PyFort for C compiler.
*/

static int default_option = TRANSPOSE_OPTION;
static PyObject*
set_pyfort_option (PyObject* unused, PyObject* args) {
    if(!PyArg_ParseTuple(args, "i", &default_option)) return NULL;
    Py_INCREF(Py_None);
    return Py_None;
}

static void
set_pyfort_error (char* routine, char* var, char* problem) {
    char buf[512];
    sprintf(buf, "%s, argument %s: %s", routine, var, problem);
    PyErr_SetString (ErrorObject, buf);
}

static void set_transposed_strides (PyArrayObject* ar)
{
    int m1, n1, itmp; 
    n1 = ar->nd; 
    if (n1 < 2) return;
    m1 = ar->strides[n1-1];   /* stride for one element */ 
    for(itmp=0; itmp < n1 ; itmp++) { 
        ar->strides[itmp] = m1; 
        m1 *= ar->dimensions[itmp]; 
    } 
    ar->flags &= ~CONTIGUOUS; 
}


static PyArrayObject*
transpose_array (char* rname, char* vname, PyArrayObject* ap) {
/* return transpose of ap, possibly not contiguous so as to avoid copy if we
   are transposing a previously transposed contiguous array
   This means with the transpose option on the output of one call might
   not need any copying if used as input to another call. I.e., Fortran 
   arrays stay in row-major order.
*/
    int i, n;
    PyArrayObject *ret;
    n  = ap->nd;

    /* this allocates memory for dimensions and strides (but fills them
           incorrectly), sets up descr, and points data at ap->data. */
    ret = (PyArrayObject *)PyArray_FromDimsAndData(n, ap->dimensions,
                                                    ap->descr->type_num,
                                                    ap->data);
    if (!ret) {
        set_pyfort_error (rname, vname, "Could not create descriptors for transpose.");
        return NULL;
    }
       
    /* point at true owner of memory: */
    ret->base = (PyObject *)ap;
    Py_INCREF(ap);
    for(i=0; i<n; i++) {
        ret->dimensions[i] = ap->dimensions[n - i - 1];
        ret->strides[i] = ap->strides[n - i - 1];
    }
    if (array_really_contiguous(ret)) {
        ret->flags |= CONTIGUOUS;
    } else {
        ret->flags &= ~CONTIGUOUS;
    }
    return ret;
}

static PyArrayObject* make_contiguous(char* rname, char* vname, PyArrayObject* ap)
{
/* return an owned ref to a contiguous version of ap */
    PyArrayObject* result;
    if (ap->flags & CONTIGUOUS) {
        Py_INCREF (ap);
        return ap;
    } else {
        result = (PyArrayObject *) PyArray_ContiguousFromObject(
			(PyObject*) ap, ap->descr->type_num, 0, 0);
        if(!result) set_pyfort_error(rname, vname, "Failed making object contiguous.");
        return result;
    }
}

static int do_size_check (char* rname, char* vname, PyArrayObject *av, int rank,  int extents[], int mirror)
{
    int size1;
    int i, j;
    char buf[512];

    size1 = av->nd;
    
    if( size1 == rank) {
        for(i=0; i < rank; ++i) {
            /* no checking on last dimension of expected size 1 */
            if (i == size1-1) {
               if (extents[i] == 1) break;
            }
            j = mirror ? size1 - 1 - i : i;
            if(av->dimensions[j] != extents[i]) 
            {
               sprintf(buf, "Incorrect extent in dimension %d (%d expected %d)",
                       i+1, av->dimensions[j], extents[i]);
               set_pyfort_error(rname, vname, buf);
               return 0;
            }
        } 
    } else {
        if (rank != 1 || 
            size1 > 0 ||
            extents[0] != 1) 
        {    
           sprintf(buf, "Incorrect rank (%d expected %d)",
                       size1, rank);
           set_pyfort_error(rname, vname, buf);
           return 0;
        }
    }
    return 1; /* size ok */
}

static PyArrayObject*
do_array_in (char* rname, char* vname, PyObject *v, 
    enum PyArray_TYPES python_array_type)
{
    PyArrayObject* av;
    PyArrayObject* t;

    if(!PyArray_Check (v)) {
        t = (PyArrayObject *) PyArray_ContiguousFromObject(v, PyArray_NOTYPE, 0, 0);
        if (!t) {
            set_pyfort_error(rname, vname, "Argument cannot be converted to needed array.");
            goto err;
        }
    } else {
        t = (PyArrayObject*) v;
        Py_INCREF((PyObject*) t);
    }
    if (t->descr->type_num != python_array_type) {
        av = (PyArrayObject*) PyArray_Cast (t, python_array_type);
        Py_DECREF((PyObject*) t);
        t = av;
        if (!t) {
            set_pyfort_error(rname, vname, "Argument cannot be cast to needed type.");
            goto err;
        }
    } 
    return t;

err:
   return (PyArrayObject*) 0;
}

static PyArrayObject*
do_array_inout (char* rname, char* vname, PyObject *v, 
    enum PyArray_TYPES python_array_type)
{
    PyArrayObject* av;

   if (!PyArray_Check (v)) {
        set_pyfort_error(rname, vname, "Argument intent(inout) must be an array.");
        goto err;
   }
   av = (PyArrayObject*) v;
   if (av->descr->type_num != python_array_type) {
        set_pyfort_error(rname, vname, "Argument intent(inout) must be of correct type.");
        goto err;
   }
   if (!(av->flags & CONTIGUOUS))  {
       set_pyfort_error(rname, vname, "Argument intent(inout) must be contiguous.");
       goto err;
   }
   Py_INCREF(av);
   return av;
err:
   return (PyArrayObject*) 0;
}

static PyArrayObject*
do_array_create (char* rname, char* vname, enum PyArray_TYPES python_array_type, 
    int rank, int extents[], int mirror)
{
    PyArrayObject* av;
    int i, dims[7];
    if (rank > 7) {
        set_pyfort_error(rname, vname, "Too many dimensions -- limit is 7.");
        goto err;
    }
    if(mirror) {
        for(i=0; i < rank; ++i) dims[i] = extents[rank-1-i];
    } else {
        for(i=0; i < rank; ++i) dims[i] = extents[i];
    }
    av = (PyArrayObject*) PyArray_FromDims(rank, dims, python_array_type);
    if (!av) {
        set_pyfort_error(rname, vname, "Could not create array -- too big?");
        goto err;
    }
    return av;
err:
    return (PyArrayObject*) 0;
}
/* Methods */

/* conserv_regrid */
static char _scrip_conserv_regrid__doc__[] =
"conserv_regrid(noutput, input, remap_matrix, src_address, dst_address)";

extern void conserv_regrid(long NUM_LINKS, long NEXTRA, long NINPUT, long NOUTPUT, double** INPUT, double** OUTPUT, double** REMAP_MATRIX, long* SRC_ADDRESS, long* DST_ADDRESS);

static PyObject*
_scrip_conserv_regrid (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long NUM_LINKS;
    long NEXTRA;
    long NINPUT;
    long NOUTPUT;
    PyArrayObject* pyarray_value;
    PyObject* INPUT;
    PyArrayObject* aINPUT;
    int eINPUT[2];
    PyArrayObject* aOUTPUT;
    PyObject* rOUTPUT;
    int eOUTPUT[2];
    PyObject* REMAP_MATRIX;
    PyArrayObject* aREMAP_MATRIX;
    int eREMAP_MATRIX[2];
    PyObject* SRC_ADDRESS;
    PyArrayObject* aSRC_ADDRESS;
    int eSRC_ADDRESS[1];
    PyObject* DST_ADDRESS;
    PyArrayObject* aDST_ADDRESS;
    int eDST_ADDRESS[1];
    aINPUT = (PyArrayObject*) 0;
    aOUTPUT = (PyArrayObject*) 0;
    aREMAP_MATRIX = (PyArrayObject*) 0;
    aSRC_ADDRESS = (PyArrayObject*) 0;
    aDST_ADDRESS = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "lOOOO|i", &NOUTPUT, &INPUT, &REMAP_MATRIX, &SRC_ADDRESS, &DST_ADDRESS, &keyoption)) {
        return NULL;
    }
    if (!(aINPUT = do_array_in ("conserv_regrid", "INPUT", INPUT, PyArray_DOUBLE))) goto err;
    if (!(aREMAP_MATRIX = do_array_in ("conserv_regrid", "REMAP_MATRIX", REMAP_MATRIX, PyArray_DOUBLE))) goto err;
    if (!(aSRC_ADDRESS = do_array_in ("conserv_regrid", "SRC_ADDRESS", SRC_ADDRESS, PyArray_LONG))) goto err;
    if (!(aDST_ADDRESS = do_array_in ("conserv_regrid", "DST_ADDRESS", DST_ADDRESS, PyArray_LONG))) goto err;
    NUM_LINKS = get_fortran_dim(aSRC_ADDRESS, 1);
    NEXTRA = get_fortran_dim(aINPUT, 1);
    NINPUT = get_fortran_dim(aINPUT, 2);
    eINPUT[0] = NEXTRA;
    eINPUT[1] = NINPUT;
    eOUTPUT[0] = NEXTRA;
    eOUTPUT[1] = NOUTPUT;
    eREMAP_MATRIX[0] = NUM_LINKS;
    eREMAP_MATRIX[1] = 3;
    eSRC_ADDRESS[0] = NUM_LINKS;
    eDST_ADDRESS[0] = NUM_LINKS;
    if (!do_size_check ("conserv_regrid", "INPUT", aINPUT, 2, eINPUT, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aINPUT->nd > 1)) {
        pyarray_value = aINPUT;
        aINPUT = transpose_array ("conserv_regrid", "INPUT", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aINPUT) goto err;
    }
    pyarray_value = aINPUT;
    aINPUT = make_contiguous ("conserv_regrid", "INPUT", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aINPUT) goto err;
    if (!do_size_check ("conserv_regrid", "REMAP_MATRIX", aREMAP_MATRIX, 2, eREMAP_MATRIX, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aREMAP_MATRIX->nd > 1)) {
        pyarray_value = aREMAP_MATRIX;
        aREMAP_MATRIX = transpose_array ("conserv_regrid", "REMAP_MATRIX", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aREMAP_MATRIX) goto err;
    }
    pyarray_value = aREMAP_MATRIX;
    aREMAP_MATRIX = make_contiguous ("conserv_regrid", "REMAP_MATRIX", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aREMAP_MATRIX) goto err;
    if (!do_size_check ("conserv_regrid", "SRC_ADDRESS", aSRC_ADDRESS, 1, eSRC_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_ADDRESS->nd > 1)) {
        pyarray_value = aSRC_ADDRESS;
        aSRC_ADDRESS = transpose_array ("conserv_regrid", "SRC_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_ADDRESS) goto err;
    }
    pyarray_value = aSRC_ADDRESS;
    aSRC_ADDRESS = make_contiguous ("conserv_regrid", "SRC_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_ADDRESS) goto err;
    if (!do_size_check ("conserv_regrid", "DST_ADDRESS", aDST_ADDRESS, 1, eDST_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aDST_ADDRESS->nd > 1)) {
        pyarray_value = aDST_ADDRESS;
        aDST_ADDRESS = transpose_array ("conserv_regrid", "DST_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aDST_ADDRESS) goto err;
    }
    pyarray_value = aDST_ADDRESS;
    aDST_ADDRESS = make_contiguous ("conserv_regrid", "DST_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aDST_ADDRESS) goto err;
    if (!(aOUTPUT = do_array_create ("conserv_regrid", "OUTPUT", PyArray_DOUBLE, 2, eOUTPUT, keyoption&MIRROR_OPTION))) goto err;
    conserv_regrid(NUM_LINKS, 
        NEXTRA, 
        NINPUT, 
        NOUTPUT, 
        (double**) (aINPUT->data), 
        (double**) (aOUTPUT->data), 
        (double**) (aREMAP_MATRIX->data), 
        (long*) (aSRC_ADDRESS->data), 
        (long*) (aDST_ADDRESS->data));
    rOUTPUT = PyArray_Return(aOUTPUT);
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);

    pyfort_result = Py_BuildValue("O",rOUTPUT);

    Py_XDECREF(rOUTPUT);
    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aOUTPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    return NULL;
} 
/* end of wrapper for conserv_regrid */

/* conserv_regrid_normal */
static char _scrip_conserv_regrid_normal__doc__[] =
"conserv_regrid_normal(noutput, input, remap_matrix, src_address, dst_address, normal)";

extern void conserv_regrid_normal(long NUM_LINKS, long NEXTRA, long NINPUT, long NOUTPUT, double** INPUT, double** OUTPUT, double** REMAP_MATRIX, long* SRC_ADDRESS, long* DST_ADDRESS, double* NORMAL);

static PyObject*
_scrip_conserv_regrid_normal (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long NUM_LINKS;
    long NEXTRA;
    long NINPUT;
    long NOUTPUT;
    PyArrayObject* pyarray_value;
    PyObject* INPUT;
    PyArrayObject* aINPUT;
    int eINPUT[2];
    PyArrayObject* aOUTPUT;
    PyObject* rOUTPUT;
    int eOUTPUT[2];
    PyObject* REMAP_MATRIX;
    PyArrayObject* aREMAP_MATRIX;
    int eREMAP_MATRIX[2];
    PyObject* SRC_ADDRESS;
    PyArrayObject* aSRC_ADDRESS;
    int eSRC_ADDRESS[1];
    PyObject* DST_ADDRESS;
    PyArrayObject* aDST_ADDRESS;
    int eDST_ADDRESS[1];
    PyObject* NORMAL;
    PyArrayObject* aNORMAL;
    int eNORMAL[1];
    aINPUT = (PyArrayObject*) 0;
    aOUTPUT = (PyArrayObject*) 0;
    aREMAP_MATRIX = (PyArrayObject*) 0;
    aSRC_ADDRESS = (PyArrayObject*) 0;
    aDST_ADDRESS = (PyArrayObject*) 0;
    aNORMAL = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "lOOOOO|i", &NOUTPUT, &INPUT, &REMAP_MATRIX, &SRC_ADDRESS, &DST_ADDRESS, &NORMAL, &keyoption)) {
        return NULL;
    }
    if (!(aINPUT = do_array_in ("conserv_regrid_normal", "INPUT", INPUT, PyArray_DOUBLE))) goto err;
    if (!(aREMAP_MATRIX = do_array_in ("conserv_regrid_normal", "REMAP_MATRIX", REMAP_MATRIX, PyArray_DOUBLE))) goto err;
    if (!(aSRC_ADDRESS = do_array_in ("conserv_regrid_normal", "SRC_ADDRESS", SRC_ADDRESS, PyArray_LONG))) goto err;
    if (!(aDST_ADDRESS = do_array_in ("conserv_regrid_normal", "DST_ADDRESS", DST_ADDRESS, PyArray_LONG))) goto err;
    if (!(aNORMAL = do_array_in ("conserv_regrid_normal", "NORMAL", NORMAL, PyArray_DOUBLE))) goto err;
    NUM_LINKS = get_fortran_dim(aSRC_ADDRESS, 1);
    NEXTRA = get_fortran_dim(aINPUT, 1);
    NINPUT = get_fortran_dim(aINPUT, 2);
    eINPUT[0] = NEXTRA;
    eINPUT[1] = NINPUT;
    eOUTPUT[0] = NEXTRA;
    eOUTPUT[1] = NOUTPUT;
    eREMAP_MATRIX[0] = NUM_LINKS;
    eREMAP_MATRIX[1] = 3;
    eSRC_ADDRESS[0] = NUM_LINKS;
    eDST_ADDRESS[0] = NUM_LINKS;
    eNORMAL[0] = NOUTPUT;
    if (!do_size_check ("conserv_regrid_normal", "INPUT", aINPUT, 2, eINPUT, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aINPUT->nd > 1)) {
        pyarray_value = aINPUT;
        aINPUT = transpose_array ("conserv_regrid_normal", "INPUT", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aINPUT) goto err;
    }
    pyarray_value = aINPUT;
    aINPUT = make_contiguous ("conserv_regrid_normal", "INPUT", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aINPUT) goto err;
    if (!do_size_check ("conserv_regrid_normal", "REMAP_MATRIX", aREMAP_MATRIX, 2, eREMAP_MATRIX, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aREMAP_MATRIX->nd > 1)) {
        pyarray_value = aREMAP_MATRIX;
        aREMAP_MATRIX = transpose_array ("conserv_regrid_normal", "REMAP_MATRIX", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aREMAP_MATRIX) goto err;
    }
    pyarray_value = aREMAP_MATRIX;
    aREMAP_MATRIX = make_contiguous ("conserv_regrid_normal", "REMAP_MATRIX", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aREMAP_MATRIX) goto err;
    if (!do_size_check ("conserv_regrid_normal", "SRC_ADDRESS", aSRC_ADDRESS, 1, eSRC_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_ADDRESS->nd > 1)) {
        pyarray_value = aSRC_ADDRESS;
        aSRC_ADDRESS = transpose_array ("conserv_regrid_normal", "SRC_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_ADDRESS) goto err;
    }
    pyarray_value = aSRC_ADDRESS;
    aSRC_ADDRESS = make_contiguous ("conserv_regrid_normal", "SRC_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_ADDRESS) goto err;
    if (!do_size_check ("conserv_regrid_normal", "DST_ADDRESS", aDST_ADDRESS, 1, eDST_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aDST_ADDRESS->nd > 1)) {
        pyarray_value = aDST_ADDRESS;
        aDST_ADDRESS = transpose_array ("conserv_regrid_normal", "DST_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aDST_ADDRESS) goto err;
    }
    pyarray_value = aDST_ADDRESS;
    aDST_ADDRESS = make_contiguous ("conserv_regrid_normal", "DST_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aDST_ADDRESS) goto err;
    if (!do_size_check ("conserv_regrid_normal", "NORMAL", aNORMAL, 1, eNORMAL, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aNORMAL->nd > 1)) {
        pyarray_value = aNORMAL;
        aNORMAL = transpose_array ("conserv_regrid_normal", "NORMAL", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aNORMAL) goto err;
    }
    pyarray_value = aNORMAL;
    aNORMAL = make_contiguous ("conserv_regrid_normal", "NORMAL", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aNORMAL) goto err;
    if (!(aOUTPUT = do_array_create ("conserv_regrid_normal", "OUTPUT", PyArray_DOUBLE, 2, eOUTPUT, keyoption&MIRROR_OPTION))) goto err;
    conserv_regrid_normal(NUM_LINKS, 
        NEXTRA, 
        NINPUT, 
        NOUTPUT, 
        (double**) (aINPUT->data), 
        (double**) (aOUTPUT->data), 
        (double**) (aREMAP_MATRIX->data), 
        (long*) (aSRC_ADDRESS->data), 
        (long*) (aDST_ADDRESS->data), 
        (double*) (aNORMAL->data));
    rOUTPUT = PyArray_Return(aOUTPUT);
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    Py_XDECREF((PyObject*) aNORMAL);

    pyfort_result = Py_BuildValue("O",rOUTPUT);

    Py_XDECREF(rOUTPUT);
    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aOUTPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    Py_XDECREF((PyObject*) aNORMAL);
    return NULL;
} 
/* end of wrapper for conserv_regrid_normal */

/* conserv_regrid2 */
static char _scrip_conserv_regrid2__doc__[] =
"conserv_regrid2(noutput, input, remap_matrix, src_address, dst_address, src_grad1, src_grad2)";

extern void conserv_regrid2(long NUM_LINKS, long NEXTRA, long NINPUT, long NOUTPUT, double** INPUT, double** OUTPUT, double** REMAP_MATRIX, long* SRC_ADDRESS, long* DST_ADDRESS, double** SRC_GRAD1, double** SRC_GRAD2);

static PyObject*
_scrip_conserv_regrid2 (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long NUM_LINKS;
    long NEXTRA;
    long NINPUT;
    long NOUTPUT;
    PyArrayObject* pyarray_value;
    PyObject* INPUT;
    PyArrayObject* aINPUT;
    int eINPUT[2];
    PyArrayObject* aOUTPUT;
    PyObject* rOUTPUT;
    int eOUTPUT[2];
    PyObject* REMAP_MATRIX;
    PyArrayObject* aREMAP_MATRIX;
    int eREMAP_MATRIX[2];
    PyObject* SRC_ADDRESS;
    PyArrayObject* aSRC_ADDRESS;
    int eSRC_ADDRESS[1];
    PyObject* DST_ADDRESS;
    PyArrayObject* aDST_ADDRESS;
    int eDST_ADDRESS[1];
    PyObject* SRC_GRAD1;
    PyArrayObject* aSRC_GRAD1;
    int eSRC_GRAD1[2];
    PyObject* SRC_GRAD2;
    PyArrayObject* aSRC_GRAD2;
    int eSRC_GRAD2[2];
    aINPUT = (PyArrayObject*) 0;
    aOUTPUT = (PyArrayObject*) 0;
    aREMAP_MATRIX = (PyArrayObject*) 0;
    aSRC_ADDRESS = (PyArrayObject*) 0;
    aDST_ADDRESS = (PyArrayObject*) 0;
    aSRC_GRAD1 = (PyArrayObject*) 0;
    aSRC_GRAD2 = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "lOOOOOO|i", &NOUTPUT, &INPUT, &REMAP_MATRIX, &SRC_ADDRESS, &DST_ADDRESS, &SRC_GRAD1, &SRC_GRAD2, &keyoption)) {
        return NULL;
    }
    if (!(aINPUT = do_array_in ("conserv_regrid2", "INPUT", INPUT, PyArray_DOUBLE))) goto err;
    if (!(aREMAP_MATRIX = do_array_in ("conserv_regrid2", "REMAP_MATRIX", REMAP_MATRIX, PyArray_DOUBLE))) goto err;
    if (!(aSRC_ADDRESS = do_array_in ("conserv_regrid2", "SRC_ADDRESS", SRC_ADDRESS, PyArray_LONG))) goto err;
    if (!(aDST_ADDRESS = do_array_in ("conserv_regrid2", "DST_ADDRESS", DST_ADDRESS, PyArray_LONG))) goto err;
    if (!(aSRC_GRAD1 = do_array_in ("conserv_regrid2", "SRC_GRAD1", SRC_GRAD1, PyArray_DOUBLE))) goto err;
    if (!(aSRC_GRAD2 = do_array_in ("conserv_regrid2", "SRC_GRAD2", SRC_GRAD2, PyArray_DOUBLE))) goto err;
    NUM_LINKS = get_fortran_dim(aSRC_ADDRESS, 1);
    NEXTRA = get_fortran_dim(aINPUT, 1);
    NINPUT = get_fortran_dim(aINPUT, 2);
    eINPUT[0] = NEXTRA;
    eINPUT[1] = NINPUT;
    eOUTPUT[0] = NEXTRA;
    eOUTPUT[1] = NOUTPUT;
    eREMAP_MATRIX[0] = NUM_LINKS;
    eREMAP_MATRIX[1] = 3;
    eSRC_ADDRESS[0] = NUM_LINKS;
    eDST_ADDRESS[0] = NUM_LINKS;
    eSRC_GRAD1[0] = NEXTRA;
    eSRC_GRAD1[1] = NINPUT;
    eSRC_GRAD2[0] = NEXTRA;
    eSRC_GRAD2[1] = NINPUT;
    if (!do_size_check ("conserv_regrid2", "INPUT", aINPUT, 2, eINPUT, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aINPUT->nd > 1)) {
        pyarray_value = aINPUT;
        aINPUT = transpose_array ("conserv_regrid2", "INPUT", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aINPUT) goto err;
    }
    pyarray_value = aINPUT;
    aINPUT = make_contiguous ("conserv_regrid2", "INPUT", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aINPUT) goto err;
    if (!do_size_check ("conserv_regrid2", "REMAP_MATRIX", aREMAP_MATRIX, 2, eREMAP_MATRIX, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aREMAP_MATRIX->nd > 1)) {
        pyarray_value = aREMAP_MATRIX;
        aREMAP_MATRIX = transpose_array ("conserv_regrid2", "REMAP_MATRIX", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aREMAP_MATRIX) goto err;
    }
    pyarray_value = aREMAP_MATRIX;
    aREMAP_MATRIX = make_contiguous ("conserv_regrid2", "REMAP_MATRIX", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aREMAP_MATRIX) goto err;
    if (!do_size_check ("conserv_regrid2", "SRC_ADDRESS", aSRC_ADDRESS, 1, eSRC_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_ADDRESS->nd > 1)) {
        pyarray_value = aSRC_ADDRESS;
        aSRC_ADDRESS = transpose_array ("conserv_regrid2", "SRC_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_ADDRESS) goto err;
    }
    pyarray_value = aSRC_ADDRESS;
    aSRC_ADDRESS = make_contiguous ("conserv_regrid2", "SRC_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_ADDRESS) goto err;
    if (!do_size_check ("conserv_regrid2", "DST_ADDRESS", aDST_ADDRESS, 1, eDST_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aDST_ADDRESS->nd > 1)) {
        pyarray_value = aDST_ADDRESS;
        aDST_ADDRESS = transpose_array ("conserv_regrid2", "DST_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aDST_ADDRESS) goto err;
    }
    pyarray_value = aDST_ADDRESS;
    aDST_ADDRESS = make_contiguous ("conserv_regrid2", "DST_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aDST_ADDRESS) goto err;
    if (!do_size_check ("conserv_regrid2", "SRC_GRAD1", aSRC_GRAD1, 2, eSRC_GRAD1, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_GRAD1->nd > 1)) {
        pyarray_value = aSRC_GRAD1;
        aSRC_GRAD1 = transpose_array ("conserv_regrid2", "SRC_GRAD1", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_GRAD1) goto err;
    }
    pyarray_value = aSRC_GRAD1;
    aSRC_GRAD1 = make_contiguous ("conserv_regrid2", "SRC_GRAD1", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_GRAD1) goto err;
    if (!do_size_check ("conserv_regrid2", "SRC_GRAD2", aSRC_GRAD2, 2, eSRC_GRAD2, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_GRAD2->nd > 1)) {
        pyarray_value = aSRC_GRAD2;
        aSRC_GRAD2 = transpose_array ("conserv_regrid2", "SRC_GRAD2", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_GRAD2) goto err;
    }
    pyarray_value = aSRC_GRAD2;
    aSRC_GRAD2 = make_contiguous ("conserv_regrid2", "SRC_GRAD2", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_GRAD2) goto err;
    if (!(aOUTPUT = do_array_create ("conserv_regrid2", "OUTPUT", PyArray_DOUBLE, 2, eOUTPUT, keyoption&MIRROR_OPTION))) goto err;
    conserv_regrid2(NUM_LINKS, 
        NEXTRA, 
        NINPUT, 
        NOUTPUT, 
        (double**) (aINPUT->data), 
        (double**) (aOUTPUT->data), 
        (double**) (aREMAP_MATRIX->data), 
        (long*) (aSRC_ADDRESS->data), 
        (long*) (aDST_ADDRESS->data), 
        (double**) (aSRC_GRAD1->data), 
        (double**) (aSRC_GRAD2->data));
    rOUTPUT = PyArray_Return(aOUTPUT);
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    Py_XDECREF((PyObject*) aSRC_GRAD1);
    Py_XDECREF((PyObject*) aSRC_GRAD2);

    pyfort_result = Py_BuildValue("O",rOUTPUT);

    Py_XDECREF(rOUTPUT);
    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aOUTPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    Py_XDECREF((PyObject*) aSRC_GRAD1);
    Py_XDECREF((PyObject*) aSRC_GRAD2);
    return NULL;
} 
/* end of wrapper for conserv_regrid2 */

/* bilinear_regrid */
static char _scrip_bilinear_regrid__doc__[] =
"bilinear_regrid(noutput, input, remap_matrix, src_address, dst_address)";

extern void bilinear_regrid(long NUM_LINKS, long NEXTRA, long NINPUT, long NOUTPUT, double** INPUT, double** OUTPUT, double** REMAP_MATRIX, long* SRC_ADDRESS, long* DST_ADDRESS);

static PyObject*
_scrip_bilinear_regrid (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long NUM_LINKS;
    long NEXTRA;
    long NINPUT;
    long NOUTPUT;
    PyArrayObject* pyarray_value;
    PyObject* INPUT;
    PyArrayObject* aINPUT;
    int eINPUT[2];
    PyArrayObject* aOUTPUT;
    PyObject* rOUTPUT;
    int eOUTPUT[2];
    PyObject* REMAP_MATRIX;
    PyArrayObject* aREMAP_MATRIX;
    int eREMAP_MATRIX[2];
    PyObject* SRC_ADDRESS;
    PyArrayObject* aSRC_ADDRESS;
    int eSRC_ADDRESS[1];
    PyObject* DST_ADDRESS;
    PyArrayObject* aDST_ADDRESS;
    int eDST_ADDRESS[1];
    aINPUT = (PyArrayObject*) 0;
    aOUTPUT = (PyArrayObject*) 0;
    aREMAP_MATRIX = (PyArrayObject*) 0;
    aSRC_ADDRESS = (PyArrayObject*) 0;
    aDST_ADDRESS = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "lOOOO|i", &NOUTPUT, &INPUT, &REMAP_MATRIX, &SRC_ADDRESS, &DST_ADDRESS, &keyoption)) {
        return NULL;
    }
    if (!(aINPUT = do_array_in ("bilinear_regrid", "INPUT", INPUT, PyArray_DOUBLE))) goto err;
    if (!(aREMAP_MATRIX = do_array_in ("bilinear_regrid", "REMAP_MATRIX", REMAP_MATRIX, PyArray_DOUBLE))) goto err;
    if (!(aSRC_ADDRESS = do_array_in ("bilinear_regrid", "SRC_ADDRESS", SRC_ADDRESS, PyArray_LONG))) goto err;
    if (!(aDST_ADDRESS = do_array_in ("bilinear_regrid", "DST_ADDRESS", DST_ADDRESS, PyArray_LONG))) goto err;
    NUM_LINKS = get_fortran_dim(aSRC_ADDRESS, 1);
    NEXTRA = get_fortran_dim(aINPUT, 1);
    NINPUT = get_fortran_dim(aINPUT, 2);
    eINPUT[0] = NEXTRA;
    eINPUT[1] = NINPUT;
    eOUTPUT[0] = NEXTRA;
    eOUTPUT[1] = NOUTPUT;
    eREMAP_MATRIX[0] = NUM_LINKS;
    eREMAP_MATRIX[1] = 1;
    eSRC_ADDRESS[0] = NUM_LINKS;
    eDST_ADDRESS[0] = NUM_LINKS;
    if (!do_size_check ("bilinear_regrid", "INPUT", aINPUT, 2, eINPUT, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aINPUT->nd > 1)) {
        pyarray_value = aINPUT;
        aINPUT = transpose_array ("bilinear_regrid", "INPUT", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aINPUT) goto err;
    }
    pyarray_value = aINPUT;
    aINPUT = make_contiguous ("bilinear_regrid", "INPUT", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aINPUT) goto err;
    if (!do_size_check ("bilinear_regrid", "REMAP_MATRIX", aREMAP_MATRIX, 2, eREMAP_MATRIX, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aREMAP_MATRIX->nd > 1)) {
        pyarray_value = aREMAP_MATRIX;
        aREMAP_MATRIX = transpose_array ("bilinear_regrid", "REMAP_MATRIX", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aREMAP_MATRIX) goto err;
    }
    pyarray_value = aREMAP_MATRIX;
    aREMAP_MATRIX = make_contiguous ("bilinear_regrid", "REMAP_MATRIX", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aREMAP_MATRIX) goto err;
    if (!do_size_check ("bilinear_regrid", "SRC_ADDRESS", aSRC_ADDRESS, 1, eSRC_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_ADDRESS->nd > 1)) {
        pyarray_value = aSRC_ADDRESS;
        aSRC_ADDRESS = transpose_array ("bilinear_regrid", "SRC_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_ADDRESS) goto err;
    }
    pyarray_value = aSRC_ADDRESS;
    aSRC_ADDRESS = make_contiguous ("bilinear_regrid", "SRC_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_ADDRESS) goto err;
    if (!do_size_check ("bilinear_regrid", "DST_ADDRESS", aDST_ADDRESS, 1, eDST_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aDST_ADDRESS->nd > 1)) {
        pyarray_value = aDST_ADDRESS;
        aDST_ADDRESS = transpose_array ("bilinear_regrid", "DST_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aDST_ADDRESS) goto err;
    }
    pyarray_value = aDST_ADDRESS;
    aDST_ADDRESS = make_contiguous ("bilinear_regrid", "DST_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aDST_ADDRESS) goto err;
    if (!(aOUTPUT = do_array_create ("bilinear_regrid", "OUTPUT", PyArray_DOUBLE, 2, eOUTPUT, keyoption&MIRROR_OPTION))) goto err;
    bilinear_regrid(NUM_LINKS, 
        NEXTRA, 
        NINPUT, 
        NOUTPUT, 
        (double**) (aINPUT->data), 
        (double**) (aOUTPUT->data), 
        (double**) (aREMAP_MATRIX->data), 
        (long*) (aSRC_ADDRESS->data), 
        (long*) (aDST_ADDRESS->data));
    rOUTPUT = PyArray_Return(aOUTPUT);
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);

    pyfort_result = Py_BuildValue("O",rOUTPUT);

    Py_XDECREF(rOUTPUT);
    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aOUTPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    return NULL;
} 
/* end of wrapper for bilinear_regrid */

/* bicubic_regrid */
static char _scrip_bicubic_regrid__doc__[] =
"bicubic_regrid(noutput, input, remap_matrix, src_address, dst_address, src_grad1, src_grad2, src_grad3)";

extern void bicubic_regrid(long NUM_LINKS, long NEXTRA, long NINPUT, long NOUTPUT, double** INPUT, double** OUTPUT, double** REMAP_MATRIX, long* SRC_ADDRESS, long* DST_ADDRESS, double** SRC_GRAD1, double** SRC_GRAD2, double** SRC_GRAD3);

static PyObject*
_scrip_bicubic_regrid (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long NUM_LINKS;
    long NEXTRA;
    long NINPUT;
    long NOUTPUT;
    PyArrayObject* pyarray_value;
    PyObject* INPUT;
    PyArrayObject* aINPUT;
    int eINPUT[2];
    PyArrayObject* aOUTPUT;
    PyObject* rOUTPUT;
    int eOUTPUT[2];
    PyObject* REMAP_MATRIX;
    PyArrayObject* aREMAP_MATRIX;
    int eREMAP_MATRIX[2];
    PyObject* SRC_ADDRESS;
    PyArrayObject* aSRC_ADDRESS;
    int eSRC_ADDRESS[1];
    PyObject* DST_ADDRESS;
    PyArrayObject* aDST_ADDRESS;
    int eDST_ADDRESS[1];
    PyObject* SRC_GRAD1;
    PyArrayObject* aSRC_GRAD1;
    int eSRC_GRAD1[2];
    PyObject* SRC_GRAD2;
    PyArrayObject* aSRC_GRAD2;
    int eSRC_GRAD2[2];
    PyObject* SRC_GRAD3;
    PyArrayObject* aSRC_GRAD3;
    int eSRC_GRAD3[2];
    aINPUT = (PyArrayObject*) 0;
    aOUTPUT = (PyArrayObject*) 0;
    aREMAP_MATRIX = (PyArrayObject*) 0;
    aSRC_ADDRESS = (PyArrayObject*) 0;
    aDST_ADDRESS = (PyArrayObject*) 0;
    aSRC_GRAD1 = (PyArrayObject*) 0;
    aSRC_GRAD2 = (PyArrayObject*) 0;
    aSRC_GRAD3 = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "lOOOOOOO|i", &NOUTPUT, &INPUT, &REMAP_MATRIX, &SRC_ADDRESS, &DST_ADDRESS, &SRC_GRAD1, &SRC_GRAD2, &SRC_GRAD3, &keyoption)) {
        return NULL;
    }
    if (!(aINPUT = do_array_in ("bicubic_regrid", "INPUT", INPUT, PyArray_DOUBLE))) goto err;
    if (!(aREMAP_MATRIX = do_array_in ("bicubic_regrid", "REMAP_MATRIX", REMAP_MATRIX, PyArray_DOUBLE))) goto err;
    if (!(aSRC_ADDRESS = do_array_in ("bicubic_regrid", "SRC_ADDRESS", SRC_ADDRESS, PyArray_LONG))) goto err;
    if (!(aDST_ADDRESS = do_array_in ("bicubic_regrid", "DST_ADDRESS", DST_ADDRESS, PyArray_LONG))) goto err;
    if (!(aSRC_GRAD1 = do_array_in ("bicubic_regrid", "SRC_GRAD1", SRC_GRAD1, PyArray_DOUBLE))) goto err;
    if (!(aSRC_GRAD2 = do_array_in ("bicubic_regrid", "SRC_GRAD2", SRC_GRAD2, PyArray_DOUBLE))) goto err;
    if (!(aSRC_GRAD3 = do_array_in ("bicubic_regrid", "SRC_GRAD3", SRC_GRAD3, PyArray_DOUBLE))) goto err;
    NUM_LINKS = get_fortran_dim(aSRC_ADDRESS, 1);
    NEXTRA = get_fortran_dim(aINPUT, 1);
    NINPUT = get_fortran_dim(aINPUT, 2);
    eINPUT[0] = NEXTRA;
    eINPUT[1] = NINPUT;
    eOUTPUT[0] = NEXTRA;
    eOUTPUT[1] = NOUTPUT;
    eREMAP_MATRIX[0] = NUM_LINKS;
    eREMAP_MATRIX[1] = 4;
    eSRC_ADDRESS[0] = NUM_LINKS;
    eDST_ADDRESS[0] = NUM_LINKS;
    eSRC_GRAD1[0] = NEXTRA;
    eSRC_GRAD1[1] = NINPUT;
    eSRC_GRAD2[0] = NEXTRA;
    eSRC_GRAD2[1] = NINPUT;
    eSRC_GRAD3[0] = NEXTRA;
    eSRC_GRAD3[1] = NINPUT;
    if (!do_size_check ("bicubic_regrid", "INPUT", aINPUT, 2, eINPUT, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aINPUT->nd > 1)) {
        pyarray_value = aINPUT;
        aINPUT = transpose_array ("bicubic_regrid", "INPUT", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aINPUT) goto err;
    }
    pyarray_value = aINPUT;
    aINPUT = make_contiguous ("bicubic_regrid", "INPUT", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aINPUT) goto err;
    if (!do_size_check ("bicubic_regrid", "REMAP_MATRIX", aREMAP_MATRIX, 2, eREMAP_MATRIX, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aREMAP_MATRIX->nd > 1)) {
        pyarray_value = aREMAP_MATRIX;
        aREMAP_MATRIX = transpose_array ("bicubic_regrid", "REMAP_MATRIX", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aREMAP_MATRIX) goto err;
    }
    pyarray_value = aREMAP_MATRIX;
    aREMAP_MATRIX = make_contiguous ("bicubic_regrid", "REMAP_MATRIX", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aREMAP_MATRIX) goto err;
    if (!do_size_check ("bicubic_regrid", "SRC_ADDRESS", aSRC_ADDRESS, 1, eSRC_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_ADDRESS->nd > 1)) {
        pyarray_value = aSRC_ADDRESS;
        aSRC_ADDRESS = transpose_array ("bicubic_regrid", "SRC_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_ADDRESS) goto err;
    }
    pyarray_value = aSRC_ADDRESS;
    aSRC_ADDRESS = make_contiguous ("bicubic_regrid", "SRC_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_ADDRESS) goto err;
    if (!do_size_check ("bicubic_regrid", "DST_ADDRESS", aDST_ADDRESS, 1, eDST_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aDST_ADDRESS->nd > 1)) {
        pyarray_value = aDST_ADDRESS;
        aDST_ADDRESS = transpose_array ("bicubic_regrid", "DST_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aDST_ADDRESS) goto err;
    }
    pyarray_value = aDST_ADDRESS;
    aDST_ADDRESS = make_contiguous ("bicubic_regrid", "DST_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aDST_ADDRESS) goto err;
    if (!do_size_check ("bicubic_regrid", "SRC_GRAD1", aSRC_GRAD1, 2, eSRC_GRAD1, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_GRAD1->nd > 1)) {
        pyarray_value = aSRC_GRAD1;
        aSRC_GRAD1 = transpose_array ("bicubic_regrid", "SRC_GRAD1", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_GRAD1) goto err;
    }
    pyarray_value = aSRC_GRAD1;
    aSRC_GRAD1 = make_contiguous ("bicubic_regrid", "SRC_GRAD1", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_GRAD1) goto err;
    if (!do_size_check ("bicubic_regrid", "SRC_GRAD2", aSRC_GRAD2, 2, eSRC_GRAD2, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_GRAD2->nd > 1)) {
        pyarray_value = aSRC_GRAD2;
        aSRC_GRAD2 = transpose_array ("bicubic_regrid", "SRC_GRAD2", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_GRAD2) goto err;
    }
    pyarray_value = aSRC_GRAD2;
    aSRC_GRAD2 = make_contiguous ("bicubic_regrid", "SRC_GRAD2", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_GRAD2) goto err;
    if (!do_size_check ("bicubic_regrid", "SRC_GRAD3", aSRC_GRAD3, 2, eSRC_GRAD3, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_GRAD3->nd > 1)) {
        pyarray_value = aSRC_GRAD3;
        aSRC_GRAD3 = transpose_array ("bicubic_regrid", "SRC_GRAD3", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_GRAD3) goto err;
    }
    pyarray_value = aSRC_GRAD3;
    aSRC_GRAD3 = make_contiguous ("bicubic_regrid", "SRC_GRAD3", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_GRAD3) goto err;
    if (!(aOUTPUT = do_array_create ("bicubic_regrid", "OUTPUT", PyArray_DOUBLE, 2, eOUTPUT, keyoption&MIRROR_OPTION))) goto err;
    bicubic_regrid(NUM_LINKS, 
        NEXTRA, 
        NINPUT, 
        NOUTPUT, 
        (double**) (aINPUT->data), 
        (double**) (aOUTPUT->data), 
        (double**) (aREMAP_MATRIX->data), 
        (long*) (aSRC_ADDRESS->data), 
        (long*) (aDST_ADDRESS->data), 
        (double**) (aSRC_GRAD1->data), 
        (double**) (aSRC_GRAD2->data), 
        (double**) (aSRC_GRAD3->data));
    rOUTPUT = PyArray_Return(aOUTPUT);
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    Py_XDECREF((PyObject*) aSRC_GRAD1);
    Py_XDECREF((PyObject*) aSRC_GRAD2);
    Py_XDECREF((PyObject*) aSRC_GRAD3);

    pyfort_result = Py_BuildValue("O",rOUTPUT);

    Py_XDECREF(rOUTPUT);
    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aOUTPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    Py_XDECREF((PyObject*) aSRC_GRAD1);
    Py_XDECREF((PyObject*) aSRC_GRAD2);
    Py_XDECREF((PyObject*) aSRC_GRAD3);
    return NULL;
} 
/* end of wrapper for bicubic_regrid */

/* distwgt_regrid */
static char _scrip_distwgt_regrid__doc__[] =
"distwgt_regrid(noutput, input, remap_matrix, src_address, dst_address)";

extern void distwgt_regrid(long NUM_LINKS, long NEXTRA, long NINPUT, long NOUTPUT, double** INPUT, double** OUTPUT, double** REMAP_MATRIX, long* SRC_ADDRESS, long* DST_ADDRESS);

static PyObject*
_scrip_distwgt_regrid (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long NUM_LINKS;
    long NEXTRA;
    long NINPUT;
    long NOUTPUT;
    PyArrayObject* pyarray_value;
    PyObject* INPUT;
    PyArrayObject* aINPUT;
    int eINPUT[2];
    PyArrayObject* aOUTPUT;
    PyObject* rOUTPUT;
    int eOUTPUT[2];
    PyObject* REMAP_MATRIX;
    PyArrayObject* aREMAP_MATRIX;
    int eREMAP_MATRIX[2];
    PyObject* SRC_ADDRESS;
    PyArrayObject* aSRC_ADDRESS;
    int eSRC_ADDRESS[1];
    PyObject* DST_ADDRESS;
    PyArrayObject* aDST_ADDRESS;
    int eDST_ADDRESS[1];
    aINPUT = (PyArrayObject*) 0;
    aOUTPUT = (PyArrayObject*) 0;
    aREMAP_MATRIX = (PyArrayObject*) 0;
    aSRC_ADDRESS = (PyArrayObject*) 0;
    aDST_ADDRESS = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "lOOOO|i", &NOUTPUT, &INPUT, &REMAP_MATRIX, &SRC_ADDRESS, &DST_ADDRESS, &keyoption)) {
        return NULL;
    }
    if (!(aINPUT = do_array_in ("distwgt_regrid", "INPUT", INPUT, PyArray_DOUBLE))) goto err;
    if (!(aREMAP_MATRIX = do_array_in ("distwgt_regrid", "REMAP_MATRIX", REMAP_MATRIX, PyArray_DOUBLE))) goto err;
    if (!(aSRC_ADDRESS = do_array_in ("distwgt_regrid", "SRC_ADDRESS", SRC_ADDRESS, PyArray_LONG))) goto err;
    if (!(aDST_ADDRESS = do_array_in ("distwgt_regrid", "DST_ADDRESS", DST_ADDRESS, PyArray_LONG))) goto err;
    NUM_LINKS = get_fortran_dim(aSRC_ADDRESS, 1);
    NEXTRA = get_fortran_dim(aINPUT, 1);
    NINPUT = get_fortran_dim(aINPUT, 2);
    eINPUT[0] = NEXTRA;
    eINPUT[1] = NINPUT;
    eOUTPUT[0] = NEXTRA;
    eOUTPUT[1] = NOUTPUT;
    eREMAP_MATRIX[0] = NUM_LINKS;
    eREMAP_MATRIX[1] = 1;
    eSRC_ADDRESS[0] = NUM_LINKS;
    eDST_ADDRESS[0] = NUM_LINKS;
    if (!do_size_check ("distwgt_regrid", "INPUT", aINPUT, 2, eINPUT, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aINPUT->nd > 1)) {
        pyarray_value = aINPUT;
        aINPUT = transpose_array ("distwgt_regrid", "INPUT", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aINPUT) goto err;
    }
    pyarray_value = aINPUT;
    aINPUT = make_contiguous ("distwgt_regrid", "INPUT", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aINPUT) goto err;
    if (!do_size_check ("distwgt_regrid", "REMAP_MATRIX", aREMAP_MATRIX, 2, eREMAP_MATRIX, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aREMAP_MATRIX->nd > 1)) {
        pyarray_value = aREMAP_MATRIX;
        aREMAP_MATRIX = transpose_array ("distwgt_regrid", "REMAP_MATRIX", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aREMAP_MATRIX) goto err;
    }
    pyarray_value = aREMAP_MATRIX;
    aREMAP_MATRIX = make_contiguous ("distwgt_regrid", "REMAP_MATRIX", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aREMAP_MATRIX) goto err;
    if (!do_size_check ("distwgt_regrid", "SRC_ADDRESS", aSRC_ADDRESS, 1, eSRC_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aSRC_ADDRESS->nd > 1)) {
        pyarray_value = aSRC_ADDRESS;
        aSRC_ADDRESS = transpose_array ("distwgt_regrid", "SRC_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aSRC_ADDRESS) goto err;
    }
    pyarray_value = aSRC_ADDRESS;
    aSRC_ADDRESS = make_contiguous ("distwgt_regrid", "SRC_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aSRC_ADDRESS) goto err;
    if (!do_size_check ("distwgt_regrid", "DST_ADDRESS", aDST_ADDRESS, 1, eDST_ADDRESS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aDST_ADDRESS->nd > 1)) {
        pyarray_value = aDST_ADDRESS;
        aDST_ADDRESS = transpose_array ("distwgt_regrid", "DST_ADDRESS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aDST_ADDRESS) goto err;
    }
    pyarray_value = aDST_ADDRESS;
    aDST_ADDRESS = make_contiguous ("distwgt_regrid", "DST_ADDRESS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aDST_ADDRESS) goto err;
    if (!(aOUTPUT = do_array_create ("distwgt_regrid", "OUTPUT", PyArray_DOUBLE, 2, eOUTPUT, keyoption&MIRROR_OPTION))) goto err;
    distwgt_regrid(NUM_LINKS, 
        NEXTRA, 
        NINPUT, 
        NOUTPUT, 
        (double**) (aINPUT->data), 
        (double**) (aOUTPUT->data), 
        (double**) (aREMAP_MATRIX->data), 
        (long*) (aSRC_ADDRESS->data), 
        (long*) (aDST_ADDRESS->data));
    rOUTPUT = PyArray_Return(aOUTPUT);
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);

    pyfort_result = Py_BuildValue("O",rOUTPUT);

    Py_XDECREF(rOUTPUT);
    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aINPUT);
    Py_XDECREF((PyObject*) aOUTPUT);
    Py_XDECREF((PyObject*) aREMAP_MATRIX);
    Py_XDECREF((PyObject*) aSRC_ADDRESS);
    Py_XDECREF((PyObject*) aDST_ADDRESS);
    return NULL;
} 
/* end of wrapper for distwgt_regrid */

static struct PyMethodDef _scrip_methods[] = {
   {"conserv_regrid", (PyCFunction) _scrip_conserv_regrid, METH_VARARGS, _scrip_conserv_regrid__doc__},
   {"conserv_regrid_normal", (PyCFunction) _scrip_conserv_regrid_normal, METH_VARARGS, _scrip_conserv_regrid_normal__doc__},
   {"conserv_regrid2", (PyCFunction) _scrip_conserv_regrid2, METH_VARARGS, _scrip_conserv_regrid2__doc__},
   {"bilinear_regrid", (PyCFunction) _scrip_bilinear_regrid, METH_VARARGS, _scrip_bilinear_regrid__doc__},
   {"bicubic_regrid", (PyCFunction) _scrip_bicubic_regrid, METH_VARARGS, _scrip_bicubic_regrid__doc__},
   {"distwgt_regrid", (PyCFunction) _scrip_distwgt_regrid, METH_VARARGS, _scrip_distwgt_regrid__doc__},
   {"set_pyfort_option", (PyCFunction) set_pyfort_option, METH_VARARGS, 
           "set_pyfort_option (value) sets default value of option keyword."},
   {NULL,          NULL, 0, NULL}/* sentinel */
};

static char _scrip_module_documentation[] =
"Fortran interface module _scrip";

void init_scrip(void)
{
        PyObject *m, *d, *j;
 
        import_array ();
        m = Py_InitModule4("_scrip", _scrip_methods,
                _scrip_module_documentation,
                (PyObject*)NULL,PYTHON_API_VERSION);
 
        d = PyModule_GetDict(m);
        ErrorObject = PyString_FromString("_scrip.error");
        PyDict_SetItemString(d, "error", ErrorObject);
        j = PyInt_FromLong((long) TRANSPOSE_OPTION);
        PyDict_SetItemString(d, "TRANSPOSE", j);
        Py_XDECREF(j);
        j = PyInt_FromLong((long) MIRROR_OPTION);
        PyDict_SetItemString(d, "MIRROR", j);
        Py_XDECREF(j);
        j = PyInt_FromLong(0L);
        PyDict_SetItemString(d, "NONE", j);
        Py_XDECREF(j);

        if (PyErr_Occurred()) {
            Py_FatalError("can't initialize module _scrip");
        }
}

/* C++ trailer */
#ifdef __CPLUSCPLUS__
}
#endif
