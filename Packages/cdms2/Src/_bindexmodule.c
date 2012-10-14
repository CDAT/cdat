#ifdef __CPLUSPLUS__
extern "C" {
#endif
#include "Python.h"
#include "numpy/arrayobject.h"
 
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
    ar->flags &= ~NPY_CONTIGUOUS; 
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
    ret = (PyArrayObject *)PyArray_SimpleNewFromData(n, ap->dimensions,
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
        ret->flags |= NPY_CONTIGUOUS;
    } else {
        ret->flags &= ~NPY_CONTIGUOUS;
    }
    return ret;
}

static PyArrayObject* make_contiguous(char* rname, char* vname, PyArrayObject* ap)
{
/* return an owned ref to a contiguous version of ap */
    PyArrayObject* result;
    if (ap->flags & NPY_CONTIGUOUS) {
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
   if (!(av->flags & NPY_CONTIGUOUS))  {
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
    int i;
    npy_intp dims[7];
    if (rank > 7) {
        set_pyfort_error(rname, vname, "Too many dimensions -- limit is 7.");
        goto err;
    }
    if(mirror) {
        for(i=0; i < rank; ++i) dims[i] = (npy_intp) extents[rank-1-i];
    } else {
        for(i=0; i < rank; ++i) dims[i] = (npy_intp) extents[i];
    }
    av = (PyArrayObject*) PyArray_SimpleNew(rank, dims, python_array_type);
    if (!av) {
        set_pyfort_error(rname, vname, "Could not create array -- too big?");
        goto err;
    }
    return av;
err:
    return (PyArrayObject*) 0;
}
/* Methods */

/* bindex */
static char _bindex_bindex__doc__[] =
"bindex(lats, lons, head, next)";

extern void bindex(long NGRID, long NBINS, double* LATS, double* LONS, long* HEAD, long* NEXT);

static PyObject*
_bindex_bindex (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long NGRID;
    long NBINS;
    PyArrayObject* pyarray_value;
    PyObject* LATS;
    PyArrayObject* aLATS;
    int eLATS[1];
    PyObject* LONS;
    PyArrayObject* aLONS;
    int eLONS[1];
    PyObject* HEAD;
    PyArrayObject* aHEAD;
    int eHEAD[1];
    PyObject* NEXT;
    PyArrayObject* aNEXT;
    int eNEXT[1];
    aLATS = (PyArrayObject*) 0;
    aLONS = (PyArrayObject*) 0;
    aHEAD = (PyArrayObject*) 0;
    aNEXT = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "OOOO|i", &LATS, &LONS, &HEAD, &NEXT, &keyoption)) {
        return NULL;
    }
    if (!(aLATS = do_array_in ("bindex", "LATS", LATS, PyArray_DOUBLE))) goto err;
    if (!(aLONS = do_array_in ("bindex", "LONS", LONS, PyArray_DOUBLE))) goto err;
    if (!(aHEAD = do_array_inout ("bindex", "HEAD", HEAD, PyArray_LONG))) goto err;
    if (!(aNEXT = do_array_inout ("bindex", "NEXT", NEXT, PyArray_LONG))) goto err;
    NGRID = get_fortran_dim(aLATS, 1);
    NBINS = get_fortran_dim(aHEAD, 1);
    eLATS[0] = NGRID;
    eLONS[0] = NGRID;
    eHEAD[0] = NBINS;
    eNEXT[0] = NGRID;
    if (!do_size_check ("bindex", "LATS", aLATS, 1, eLATS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aLATS->nd > 1)) {
        pyarray_value = aLATS;
        aLATS = transpose_array ("bindex", "LATS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aLATS) goto err;
    }
    pyarray_value = aLATS;
    aLATS = make_contiguous ("bindex", "LATS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aLATS) goto err;
    if (!do_size_check ("bindex", "LONS", aLONS, 1, eLONS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aLONS->nd > 1)) {
        pyarray_value = aLONS;
        aLONS = transpose_array ("bindex", "LONS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aLONS) goto err;
    }
    pyarray_value = aLONS;
    aLONS = make_contiguous ("bindex", "LONS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aLONS) goto err;
    if (!do_size_check ("bindex", "HEAD", aHEAD, 1, eHEAD, keyoption & MIRROR_OPTION)) goto err;
    if (!do_size_check ("bindex", "NEXT", aNEXT, 1, eNEXT, keyoption & MIRROR_OPTION)) goto err;
    bindex(NGRID, 
        NBINS, 
        (double*) (aLATS->data), 
        (double*) (aLONS->data), 
        (long*) (aHEAD->data), 
        (long*) (aNEXT->data));
    Py_XDECREF((PyObject*) aLATS);
    Py_XDECREF((PyObject*) aLONS);

    pyfort_result = Py_BuildValue("");

    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aLATS);
    Py_XDECREF((PyObject*) aLONS);
    return NULL;
} 
/* end of wrapper for bindex */

/* deltas */
  static char _bindex_setDeltas__doc__[] =
  "setDeltas(dX,dY)";

  extern void setDeltas(double dX, double dY);
  
static PyObject*
_bindex_setDeltas (PyObject* unused, PyObject* args) {
  double dX,dY;
  if(!PyArg_ParseTuple(args, "dd", &dX,&dY)) {
    return NULL;
  }
  setDeltas(dX,dY);
  Py_INCREF(Py_None);
  return Py_None;
}
  static char _bindex_getLens__doc__[] =
  "setDeltas(dX,dY)";

  extern void getLens(int *nX, int *nY);
  
static PyObject*
_bindex_getLens (PyObject* unused, PyObject* args) {
  double dX,dY;
  if(!PyArg_ParseTuple(args, "")) {
    return NULL;
  }
  int NI,NJ;
  getLens(&NI,&NJ);
  return Py_BuildValue("ii",NI,NJ);
}

/* intersect */
static char _bindex_intersect__doc__[] =
"intersect(slat, slon, elat, elon, lats, lons, head, next, points, latind, lonind)";

extern long intersect(long NGRID, long NBINS, double SLAT, double SLON, double ELAT, double ELON, double* LATS, double* LONS, long* HEAD, long* NEXT, long* POINTS, char* LATIND, char* LONIND, int nLATIND, int nLONIND);

static PyObject*
_bindex_intersect (PyObject* unused, PyObject* args) {

    PyObject *pyfort_result;
    int keyoption;
    long fortran_result;
    long NGRID;
    long NBINS;
    double SLAT;
    double SLON;
    double ELAT;
    double ELON;
    PyArrayObject* pyarray_value;
    PyObject* LATS;
    PyArrayObject* aLATS;
    int eLATS[1];
    PyObject* LONS;
    PyArrayObject* aLONS;
    int eLONS[1];
    PyObject* HEAD;
    PyArrayObject* aHEAD;
    int eHEAD[1];
    PyObject* NEXT;
    PyArrayObject* aNEXT;
    int eNEXT[1];
    PyObject* POINTS;
    PyArrayObject* aPOINTS;
    int ePOINTS[1];
    char* LATIND;
    int nLATIND;
    char* LONIND;
    int nLONIND;
    aLATS = (PyArrayObject*) 0;
    aLONS = (PyArrayObject*) 0;
    aHEAD = (PyArrayObject*) 0;
    aNEXT = (PyArrayObject*) 0;
    aPOINTS = (PyArrayObject*) 0;
    keyoption = default_option;

    if(!PyArg_ParseTuple(args, "ddddOOOOOs#s#|i", &SLAT, &SLON, &ELAT, &ELON, &LATS, &LONS, &HEAD, &NEXT, &POINTS, &LATIND, &nLATIND, &LONIND, &nLONIND, &keyoption)) {
        return NULL;
    }
    if (!(aLATS = do_array_in ("intersect", "LATS", LATS, PyArray_DOUBLE))) goto err;
    if (!(aLONS = do_array_in ("intersect", "LONS", LONS, PyArray_DOUBLE))) goto err;
    if (!(aHEAD = do_array_in ("intersect", "HEAD", HEAD, PyArray_LONG))) goto err;
    if (!(aNEXT = do_array_in ("intersect", "NEXT", NEXT, PyArray_LONG))) goto err;
    if (!(aPOINTS = do_array_inout ("intersect", "POINTS", POINTS, PyArray_LONG))) goto err;
    NGRID = get_fortran_dim(aLATS, 1);
    NBINS = get_fortran_dim(aHEAD, 1);
    eLATS[0] = NGRID;
    eLONS[0] = NGRID;
    eHEAD[0] = NBINS;
    eNEXT[0] = NGRID;
    ePOINTS[0] = NGRID;
    if (!do_size_check ("intersect", "LATS", aLATS, 1, eLATS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aLATS->nd > 1)) {
        pyarray_value = aLATS;
        aLATS = transpose_array ("intersect", "LATS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aLATS) goto err;
    }
    pyarray_value = aLATS;
    aLATS = make_contiguous ("intersect", "LATS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aLATS) goto err;
    if (!do_size_check ("intersect", "LONS", aLONS, 1, eLONS, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aLONS->nd > 1)) {
        pyarray_value = aLONS;
        aLONS = transpose_array ("intersect", "LONS", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aLONS) goto err;
    }
    pyarray_value = aLONS;
    aLONS = make_contiguous ("intersect", "LONS", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aLONS) goto err;
    if (!do_size_check ("intersect", "HEAD", aHEAD, 1, eHEAD, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aHEAD->nd > 1)) {
        pyarray_value = aHEAD;
        aHEAD = transpose_array ("intersect", "HEAD", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aHEAD) goto err;
    }
    pyarray_value = aHEAD;
    aHEAD = make_contiguous ("intersect", "HEAD", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aHEAD) goto err;
    if (!do_size_check ("intersect", "NEXT", aNEXT, 1, eNEXT, keyoption & MIRROR_OPTION)) goto err;
    if ((keyoption & TRANSPOSE_OPTION) && (aNEXT->nd > 1)) {
        pyarray_value = aNEXT;
        aNEXT = transpose_array ("intersect", "NEXT", pyarray_value);
        Py_DECREF(pyarray_value);
        if(!aNEXT) goto err;
    }
    pyarray_value = aNEXT;
    aNEXT = make_contiguous ("intersect", "NEXT", pyarray_value);
    Py_DECREF(pyarray_value);
    if(!aNEXT) goto err;
    if (!do_size_check ("intersect", "POINTS", aPOINTS, 1, ePOINTS, keyoption & MIRROR_OPTION)) goto err;
    fortran_result = intersect(NGRID, 
        NBINS, 
        SLAT, 
        SLON, 
        ELAT, 
        ELON, 
        (double*) (aLATS->data), 
        (double*) (aLONS->data), 
        (long*) (aHEAD->data), 
        (long*) (aNEXT->data), 
        (long*) (aPOINTS->data), 
        LATIND, 
        LONIND, 
        nLATIND, 
        nLONIND);
    Py_XDECREF((PyObject*) aLATS);
    Py_XDECREF((PyObject*) aLONS);
    Py_XDECREF((PyObject*) aHEAD);
    Py_XDECREF((PyObject*) aNEXT);

    pyfort_result = Py_BuildValue("l",fortran_result);

    return pyfort_result;
err:
    Py_XDECREF((PyObject*) aLATS);
    Py_XDECREF((PyObject*) aLONS);
    Py_XDECREF((PyObject*) aHEAD);
    Py_XDECREF((PyObject*) aNEXT);
    return NULL;
} 
/* end of wrapper for intersect */

static struct PyMethodDef _bindex_methods[] = {
   {"bindex", (PyCFunction) _bindex_bindex, METH_VARARGS, _bindex_bindex__doc__},
   {"intersect", (PyCFunction) _bindex_intersect, METH_VARARGS, _bindex_intersect__doc__},
   {"setDeltas", (PyCFunction) _bindex_setDeltas, METH_VARARGS, _bindex_setDeltas__doc__},
   {"getLens", (PyCFunction) _bindex_getLens, METH_VARARGS, _bindex_getLens__doc__},
   {"set_pyfort_option", (PyCFunction) set_pyfort_option, METH_VARARGS, 
           "set_pyfort_option (value) sets default value of option keyword."},
   {NULL,          NULL, 0, NULL}/* sentinel */
};

static char _bindex_module_documentation[] =
"Fortran interface module _bindex";

void init_bindex(void)
{
        PyObject *m, *d, *j;
 
        import_array ();
        m = Py_InitModule4("_bindex", _bindex_methods,
                _bindex_module_documentation,
                (PyObject*)NULL,PYTHON_API_VERSION);
 
        d = PyModule_GetDict(m);
        ErrorObject = PyString_FromString("_bindex.error");
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
            Py_FatalError("can't initialize module _bindex");
        }
}

/* C++ trailer */
#ifdef __CPLUSCPLUS__
}
#endif
