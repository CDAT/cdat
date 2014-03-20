#ifdef MS_WIN32
#undef DL_IMPORT
#define DL_IMPORT(RTYPE) __declspec(dllexport) RTYPE
#endif

#include "Python.h"
#include "numpy/arrayobject.h"
#include <assert.h>
#include "slabapi.h"
static void slabErrorSet (char* message) 
{
   PyErr_SetString(PyExc_RuntimeError, message);
}

/* this pointer must be used immediately, and not be retained  */
char* 
slabAttribute (PyObject* slab, char* attribute, char* default_value) 
{
    PyObject* attr;
    PyObject* t;
    char* result;
    attr = PyObject_GetAttrString (slab, attribute);
    if (!attr) {
        PyErr_Clear();
        return default_value;
    }
    t = PyObject_Str (attr);
    if (!t) {
        slabErrorSet("Slab attribute could not be converted to string.");
        Py_DECREF(attr);
        return (char*) 0;
    }
    result = PyString_AsString (t);
    Py_DECREF(attr);
    Py_DECREF(t);
    return result;
}

PyObject* 
slabDimension (PyObject* slab, int i)
{
    /* returns the axis object */
    PyObject* axis;
    axis = PyObject_CallMethod(slab, "getAxis", "i", i);
    if(PyErr_Occurred()) return NULL;
    return axis;
}

int 
slabRank (PyObject* slab)
{
    PyObject* result;
    int iresult;
    result = PyObject_GetAttrString(slab, "ndim");
    if(PyErr_Occurred()) return -1;
    iresult = (int) PyInt_AsLong(result);
    if(PyErr_Occurred()) return -1;
    Py_DECREF(result);
    return iresult;
}

int 
slabCheck (PyObject* slab) /* 1 if slab is a variable, else 0 */
{
     PyObject *cdmsmodule;
     PyObject *dict;
     PyObject *isfcn;
     PyObject* resultobj;
     int result;

     cdmsmodule = (PyObject*)0;
     dict = (PyObject*)0;
     isfcn = (PyObject*)0;
     resultobj = (PyObject*)0;

     cdmsmodule = PyImport_ImportModule("cdms2");
     if(!cdmsmodule) goto err;
     if(!PyModule_Check(cdmsmodule)) goto err;
     dict = PyModule_GetDict(cdmsmodule); /* borrowed ref */
     if(!dict) goto err;
     if(!PyDict_Check(dict)) goto err;
     isfcn = PyDict_GetItemString(dict, "isVariable"); /* borrowed ref */
     if(PyErr_Occurred()) goto err;
     resultobj = PyObject_CallFunction (isfcn, "O", slab);
     if(PyErr_Occurred()) goto err;
     result = (int) PyInt_AsLong (resultobj);
     if(PyErr_Occurred()) goto err;
     return result;
err:
     Py_XDECREF(cdmsmodule);
     Py_XDECREF(resultobj);
     return 0;
}

char 
slabType (PyObject* slab)
{
    PyObject* typecode,*typecode2;
    char* tcode;
    char result;
    typecode = PyObject_GetAttrString(slab, "dtype");
    if(PyErr_Occurred()) goto err;
    typecode2 = PyObject_GetAttrString(typecode,"char");
    if(PyErr_Occurred()) goto err;
    tcode = PyString_AsString(typecode2);
    if(PyErr_Occurred()) goto err;
    result = tcode[0];
    Py_DECREF(typecode);
    Py_DECREF(typecode2);
    return result;
err:
    slabErrorSet("Slab object internal error, typecode unobtainable.");
    return ' ';
}

PyArrayObject*
slabData (PyObject* slab)
{
    PyArrayObject* result;
    result = (PyArrayObject*) PyObject_CallMethod(slab, "filled", (char*) 0);
    if(PyErr_Occurred()) {
        return (PyArrayObject*) 0;
    }
    return result;
}

PyArrayObject*
slabRaw (PyObject* slab)
{
    /* slab.raw_data() */
    PyArrayObject* result;
    result = (PyArrayObject*) PyObject_CallMethod(slab, "raw_data", (char*) 0);
    if(PyErr_Occurred()) {
        return (PyArrayObject*) 0;
    }
    return result;
}
PyObject*
slabMask (PyObject* slab)
{
    PyObject* result;
    PyObject* numpy;
    PyObject* numpyma;
    PyObject* nomask;
    numpy = PyImport_ImportModule("numpy");
    if (PyErr_Occurred()) {
      printf("error happened importing numpy\n");
      slabErrorSet("Slab internal error, could not access mask.");
      return NULL;
    }
    /* slab.filled() */
/*     result = PyObject_CallMethod (slab, "mask", (char*)0); */
    result = PyObject_GetAttrString (slab, "mask");
    if (PyErr_Occurred()) {
      printf("error happened getting mask attribute\n");
      slabErrorSet("Slab internal error, could not access mask.");
      return NULL;
    }
    numpyma = PyObject_GetAttrString (numpy, "ma");
    if (PyErr_Occurred()) {
      printf("error happened getting numpy.ma\n");
      slabErrorSet("Slab internal error, could not load numpy.ma.");
      return NULL;
    }
    nomask = PyObject_GetAttrString (numpyma, "nomask");
    if (PyErr_Occurred()) {
      printf("error happened getting nomask object\n");
      slabErrorSet("Slab internal error, could not access nomask object.");
      return NULL;
    }
    if (result == nomask) {
      Py_XDECREF(numpy);
      Py_XDECREF(nomask);
      Py_XDECREF(result);
      return Py_None;
    }
    Py_XDECREF(numpy);
    Py_XDECREF(nomask);
    return result;
}

void
freePyObject( char* raw)
{ Py_DECREF((PyObject *) raw); }


PyObject* 
slabDimensionKey (PyObject* slab, int idim, char* key)
{
    PyObject* result;
    char buf[120];

    assert(slab != 0);
    assert(key != 0);
    assert(idim >= 0 && idim < slabRank(slab));
        
    result = PyObject_CallMethod (slab, "getdimattribute", "is", idim, key);
    if(!result) {
        sprintf(buf, "Slab internal error, getdimattribute call failed, key = %s", key);
        slabErrorSet(buf);
        return (PyObject*) 0;
    }
    return result;
}

/* Return the shape of the slab as an integer array. */
int*
slabShape (PyObject* slab)
{
    PyObject* result;
    char buf[120];
    int  *return_int, rank, i;

    assert(slab != 0);

    result = PyObject_GetAttrString (slab, "shape");
    if(!result) {
        sprintf(buf, "Slab has no shape, it must be a scalar value.");
        slabErrorSet(buf);
        return NULL;
    }
    rank = PyTuple_Size( result );
    if ((return_int = (int *) malloc( rank * sizeof(int) ))==NULL) {
        sprintf(buf, "Not enough memory to store dimension lengths.");
        slabErrorSet(buf);
        return NULL;
    }
    for (i=0; i<rank; i++)
        return_int[i] = (int) PyInt_AsLong(PyTuple_GetItem(result, i));
    
    return return_int;
}

int
slabDimensionLength (PyObject* slab, int idim) 
{
    int result;
    PyObject* obj;
    obj = slabDimensionKey(slab, idim, "length");
    if(!obj) return -1;
    if(!PyInt_Check(obj)) return -1;
    result = (int) PyInt_AsLong(obj);
    Py_DECREF(obj);
    return result;
}

char*          
slabDimensionName (PyObject* slab, int idim, int* isLongLat)
{
    char* result;
    int flg;
    PyObject* obj;
    PyObject* axis = NULL;
    obj = slabDimensionKey(slab, idim, "name");
    if(!obj) return (char*) 0;
    result = PyString_AsString(obj);
    Py_DECREF(obj);

    *isLongLat = 0;
    axis = slabDimension(slab, idim);
    obj = PyObject_CallMethod (axis, "isLongitude", (char*) 0);
    flg = (int ) PyInt_AsLong(obj);
    if (flg == 1) *isLongLat = 2;
    Py_DECREF( obj );
    if (flg != 1) {
       obj = PyObject_CallMethod (axis, "isLatitude", (char*) 0);
       *isLongLat = (int ) PyInt_AsLong(obj);
       if (flg == 1) *isLongLat = 1;
       Py_DECREF( obj );
    }
    Py_DECREF( axis );

    return result;
}

char*          
slabDimensionUnits (PyObject* slab, int idim)
{
    char* result;
    PyObject* obj;
    obj = slabDimensionKey(slab, idim, "units");
    if(!obj) return (char*) 0;
    result = PyString_AsString(obj);
    Py_DECREF(obj);
    return result;
}

PyArrayObject*   /* of type float */
slabDimensionValues (PyObject* slab, int idim)
{
    PyObject* result;
    PyObject* obj;

    obj = slabDimensionKey(slab, idim, "values");
    if(PyErr_Occurred()) return NULL;
    result = PyObject_CallMethod (obj, "astype", "s", "f");
    Py_DECREF(obj);
    if(PyErr_Occurred()) {
        printf("Failed making float array of dimension values.\n");
        return NULL;
    }
    return (PyArrayObject*) result;
}

PyArrayObject*
slabDimensionBounds (PyObject* slab, int idim)
{
/* this always returns some bounds, and they are of typcode 'f' */
    PyObject *cdms2cu_func = NULL;
    PyObject* getcdmsbounds = NULL;
    PyObject* axis = NULL;
    PyArrayObject* cdmsbounds = NULL;
    PyArrayObject* cubounds=NULL;
    PyObject *cdms2cu_module=NULL;

    axis = slabDimension(slab, idim);
    if(PyErr_Occurred()) goto err;
    getcdmsbounds = PyObject_CallMethod(axis, "getBounds", (char*) 0);
    if(PyErr_Occurred()) goto err;
    if(getcdmsbounds == Py_None) { /* there is a switch in axis.py, not good */
        getcdmsbounds = PyObject_CallMethod(axis, "genGenericBounds", (char*) 0);
        if(PyErr_Occurred()) goto err;
    }
    cdmsbounds = (PyArrayObject*) PyObject_CallMethod(getcdmsbounds,"astype","s","f")
;
    if(PyErr_Occurred()) goto err;
    cdms2cu_module = PyImport_ImportModule("cdms2.slabinterface");
    if(!cdms2cu_module) goto err;
    cdms2cu_func = PyObject_GetAttrString( cdms2cu_module,
                        "cdms_bounds2cu_bounds" );
    if (cdms2cu_func == NULL) goto err;
    cubounds = (PyArrayObject*) PyObject_CallFunction(cdms2cu_func, "O", cdmsbounds);
    Py_DECREF(axis);
    Py_DECREF(getcdmsbounds);
    Py_DECREF(cdmsbounds);
    Py_DECREF(cdms2cu_module);
    Py_DECREF(cdms2cu_func);
    return cubounds;
err:
    Py_XDECREF(getcdmsbounds);
    Py_XDECREF(axis);
    Py_XDECREF(cdmsbounds);
    Py_XDECREF(cdms2cu_module);
    Py_XDECREF(cdms2cu_func);
    return NULL;
}

PyObject*
slabDimensionWeights (PyObject* slab, int idim)
{
    PyObject *cdmsmodule = (PyObject*)0;
    PyObject *dict = (PyObject*)0;
    PyObject *isfcn = (PyObject*)0;
    PyObject *bdsobj = (PyObject*)0;
    PyObject *obj = (PyObject*)0;
    PyObject *result = (PyObject*)0;
    int auto_result = 0;

    /* Check to make sure that the CDMS's auto bounds is turned on. If it 
     * is not, the return from this function Py_None. VCS will generate 
     * its own weights if setAutoBounds is off. Without this check, VCS  
     * will not generate useful weights and seg. fault.
     */
    cdmsmodule = PyImport_ImportModule("cdms2");
    if(!cdmsmodule) goto err;
    if(!PyModule_Check(cdmsmodule)) goto err;
    dict = PyModule_GetDict(cdmsmodule); /* borrowed ref */
    if(!dict) goto err;
    if(!PyDict_Check(dict)) goto err;
    isfcn = PyDict_GetItemString(dict, "getAutoBounds"); /* borrowed ref */
    if(PyErr_Occurred()) goto err;
    bdsobj = PyObject_CallFunction (isfcn, "");
    if(PyErr_Occurred()) goto err;
    auto_result = (int) PyInt_AsLong (bdsobj);
    if(PyErr_Occurred()) goto err;

    if (auto_result == 0) return Py_None;

   /* they are of typcode 'f'  or are None 
    * get weights from the transient variable
    */
    obj = slabDimensionKey(slab, idim, "weights");
    if(PyErr_Occurred()) return NULL;
    if(obj == Py_None) {
        return obj;
    }
    result = PyObject_CallMethod (obj, "astype", "s", "f");
    Py_DECREF(obj);
    if(PyErr_Occurred()) return NULL;
    return result;

err:
     Py_XDECREF(cdmsmodule);
     Py_XDECREF(bdsobj);
     return Py_None;
}

int          
slabDimensionIsCircular (PyObject* slab, int idim)
{
    PyObject* obj;
    PyObject* resobj;
    long result;
    obj = slabDimension(slab, idim);
    if(PyErr_Occurred()) return 0;
    resobj = PyObject_CallMethod (obj, "isCircular", (char*) 0);
    Py_DECREF(obj);
    if(PyErr_Occurred()) return 0;
    result = PyInt_AsLong (resobj);
    Py_DECREF(resobj);
    return (int) result;
}

/* Reset the date and time of the slab. This is primarily used for animation. */
void
slabDateAndTime (PyObject* slab, int count)
{
     PyObject *vcs_legacymodule=NULL;
     PyObject *dict=NULL;
     PyObject *date_time=NULL;

     vcs_legacymodule = PyImport_ImportModule("vcs_legacy.Canvas");
     if(!vcs_legacymodule) goto err;
     if(!PyModule_Check(vcs_legacymodule)) goto err;
     dict = PyModule_GetDict(vcs_legacymodule); /* borrowed ref */
     if(!dict) goto err;
     if(!PyDict_Check(dict)) goto err;
     date_time = PyDict_GetItemString(dict, "change_date_time"); /* borrowed ref */
     if(PyErr_Occurred()) goto err;
     PyObject_CallFunction (date_time, "Oi", slab, count);
     if(PyErr_Occurred()) goto err;
     Py_DECREF(vcs_legacymodule);
/*DNW - 8/22/02 - This DECREF causes memory problems for Python if the
	user chooses to generate more than 140 GIFs in a Python loop.
	That is, needed memory is removed then overwritten, which causes
	Python to give the undefined global name error for "range", "len",
 	or some other globally defined Python key word.
     Py_DECREF(dict);*/
     return;
err:
     Py_XDECREF( vcs_legacymodule );
     Py_XDECREF( dict );
     Py_XDECREF( date_time );
}

PyObject* 
slabSetDimensionKey (PyObject* slab, int idim, char* key, char *value)
{
    char buf[120];
    PyObject* result=NULL;

    assert(slab != 0);
    assert(key != 0);
    assert(idim >= 0 && idim < slabRank(slab));
    assert(value != 0);
        
    result = PyObject_CallMethod (slab, "setdimattribute", "iss", idim, key, value);
    if(!result) {
        sprintf(buf, "Slab internal error, setdimattribute call failed, key = %s", key);
        slabErrorSet(buf);
        return (PyObject*) 0;
    }
    return result;
}

PyObject*
slabSetKey (PyObject* slab, char* key, char *value)
{
    char buf[120];
    PyObject* result=NULL;

    assert(slab != 0);
    assert(key != 0);
    assert(value != 0);

    result = PyObject_CallMethod (slab, "setattribute", "ss", key, value);
    if(!result) {
        sprintf(buf, "Slab internal error, setattribute call failed, key = %s", key);
        slabErrorSet(buf);
        return (PyObject*) 0;
    }
    return result;
}
