#ifdef MS_WIN32
#undef DL_IMPORT
#define DL_IMPORT(RTYPE) __declspec(dllexport) RTYPE
#endif

#include "Python.h"
#include "assert.h"
#include "numpy/arrayobject.h"
/* #include <assert.h> */
#include "slabapi.h"
#include "vcs_legacy_names_length.h"

/* ----------------------------------------------------- */
/* slabapi module -- for testing the CAPI to slabs */
static PyObject *ErrorObject;

static char slabapi_attribute__doc__[] =
"attribute(slab, name) returns string form of slab attribute"
;

static PyObject *
slabapi_attribute(self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        char *optarg;
        char *a;
        optarg = (char*) 0;
	if (!PyArg_ParseTuple(args, "Os;s", &slab, &a, &optarg))
		return NULL;
        if (!optarg) optarg="default";
	return Py_BuildValue("s", slabAttribute(slab, a, optarg));
}

static char slabapi_data__doc__[] =
"data(slab) = pointer to the raw data for the slab"
;

static PyArrayObject *
slabapi_data(self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
	if (!PyArg_ParseTuple(args, "O", &slab))
		return NULL;
        return slabData(slab);
}

static char slabapi_mask__doc__[] =
"mask(slab) = pointer to the raw data for the slab mask, or 0 if none."
;

static PyObject *
slabapi_mask(self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
	if (!PyArg_ParseTuple(args, "O", &slab))
		return NULL;
        return slabMask(slab);
}

static char slabapi_typecode__doc__[] =
"typecode(slab) = type code of the slab"
;
static PyObject *
slabapi_typecode(self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        char t;
	if (!PyArg_ParseTuple(args, "O", &slab))
		return NULL;
        t = slabType(slab);
        if(t=='\0') return (PyObject*) 0;
        return PyString_FromStringAndSize(&t, 1);
}

static char slabapi_dimension__doc__[] =
"dimension(slab, i) = ith dimension object of the slab"
;
static PyObject *
slabapi_dimension(self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int j;
	if (!PyArg_ParseTuple(args, "Oi", &slab, &j))
		return NULL;
        return slabDimension (slab, j);
}

static char slabapi_rank__doc__[] =
"rank(slab) = rank (# of dimensions) of the slab"
;
static PyObject *
slabapi_rank(self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int rank;

	if (!PyArg_ParseTuple(args, "O", &slab))
		return NULL;
        rank = slabRank(slab);
        if (PyErr_Occurred()) return NULL;
        return PyInt_FromLong ((long) slabRank(slab));
}

static char slabapi_is_slab__doc__[] =
"is_slab(slab) = test to see if slab is a cu.Slab"
;
static PyObject *
slabapi_is_slab(self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int result;
        PyObject* resobj;

	if (!PyArg_ParseTuple(args, "O", &slab))
		return NULL;
        result = slabCheck(slab);
        if(PyErr_Occurred()) return (PyObject*) 0;
        resobj = PyInt_FromLong((long)result);
        return resobj;
}

static char slabapi_dimension_length__doc__[] =
"dimension_length(slab, idim) = length of the idim'th dimension object."
;

static PyObject *
slabapi_dimension_length (self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int idim;
        int result;

	if (!PyArg_ParseTuple(args, "Oi", &slab, &idim))
		return NULL;
        result = slabDimensionLength(slab, idim);
        if(PyErr_Occurred()) return NULL;
        return Py_BuildValue ("i", result);
}

static char slabapi_dimension_name__doc__[] =
"dimension_name(slab, idim) = name of the idim'th dimension object."
;

static PyObject *
slabapi_dimension_name (self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int idim, isLongLat;
        char* result;

	if (!PyArg_ParseTuple(args, "Oi", &slab, &idim))
		return NULL;
        result = slabDimensionName(slab, idim, &isLongLat);
        if(PyErr_Occurred()) return NULL;
        return Py_BuildValue ("s", result);
}

static char slabapi_dimension_units__doc__[] =
"dimension_units(slab, idim) = name of the idim'th dimension object."
;

static PyObject *
slabapi_dimension_units (self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int idim;
        char* result;

	if (!PyArg_ParseTuple(args, "Oi", &slab, &idim))
		return NULL;
        result = slabDimensionUnits(slab, idim);
        if(PyErr_Occurred()) return NULL;
        return Py_BuildValue ("s", result);
}

static char slabapi_dimension_weights__doc__[] =
"dimension_weights(slab, idim) = weights of the idim'th dimension object, or None"
;

static PyObject*
slabapi_dimension_weights (self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int idim;
        PyObject* result;

	if (!PyArg_ParseTuple(args, "Oi", &slab, &idim))
		return NULL;
        result = slabDimensionWeights(slab, idim);
        if(PyErr_Occurred()) return NULL;
        return result;
}

static char slabapi_dimension_bounds__doc__[] =
"dimension_weights(slab, idim) = bounds of the idim'th dimension object"
;

static PyObject*
slabapi_dimension_bounds (self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int idim;
        PyArrayObject* result;

	if (!PyArg_ParseTuple(args, "Oi", &slab, &idim))
		return NULL;
        result = slabDimensionBounds(slab, idim);
        if(PyErr_Occurred()) return NULL;
        return (PyObject*) result;
}

static char slabapi_dimension_values__doc__[] =
"dimension_values(slab, idim) = values of the idim'th dimension object"
;

static PyArrayObject*
slabapi_dimension_values (self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int idim;
        PyArrayObject* result;

	if (!PyArg_ParseTuple(args, "Oi", &slab, &idim))
		return NULL;
        result = slabDimensionValues(slab, idim);
        if(PyErr_Occurred()) return NULL;
        return result;
}

static char slabapi_dimension_is_circular__doc__[] =
"dimension_is_circular(slab, idim) = values of the idim'th dimension object"
;

static PyObject*
slabapi_dimension_is_circular (self, args)
	PyObject *self;	/* Not used */
	PyObject *args;
{
        PyObject* slab;
        int idim, result;

	if (!PyArg_ParseTuple(args, "Oi", &slab, &idim))
		return NULL;
        result = slabDimensionIsCircular(slab, idim);
        if(PyErr_Occurred()) return NULL;
        return Py_BuildValue("i", result);
}

static PyObject *
PyVCS_getdotdirectory(self,args)
  PyObject *self;
  PyObject *args;
{
    char a[1024],b[1024];
    strcpy(a,DOT_DIRECTORY);
    strcpy(b,DOT_DIRECTORY_ENV);
    return Py_BuildValue("ss",a,b);
}


/* List of methods defined in the module */

static struct PyMethodDef slabapi_methods[] = {
	{"attribute",	(PyCFunction)slabapi_attribute,	METH_VARARGS,	slabapi_attribute__doc__},
 {"data",	(PyCFunction)slabapi_data,	METH_VARARGS,	slabapi_data__doc__},
 {"mask",	(PyCFunction)slabapi_mask,	METH_VARARGS,	slabapi_mask__doc__},
 {"typecode",	(PyCFunction)slabapi_typecode,	METH_VARARGS,	slabapi_typecode__doc__},
 {"rank",	(PyCFunction)slabapi_rank,	METH_VARARGS,	slabapi_rank__doc__},
 {"is_slab",	(PyCFunction)slabapi_is_slab,	METH_VARARGS,	slabapi_is_slab__doc__},
 {"dimension",	(PyCFunction)slabapi_dimension,	METH_VARARGS,	slabapi_dimension__doc__},
 {"dimension_length",	(PyCFunction)slabapi_dimension_length,	METH_VARARGS,	slabapi_dimension_length__doc__},
 {"dimension_name",	(PyCFunction)slabapi_dimension_name,	METH_VARARGS,	slabapi_dimension_name__doc__},
 {"dimension_units",	(PyCFunction)slabapi_dimension_units,	METH_VARARGS,	slabapi_dimension_units__doc__},
 {"dimension_weights",	(PyCFunction)slabapi_dimension_weights,	METH_VARARGS,	slabapi_dimension_weights__doc__},
 {"dimension_bounds",	(PyCFunction)slabapi_dimension_bounds,	METH_VARARGS,	slabapi_dimension_bounds__doc__},
 {"dimension_values",	(PyCFunction)slabapi_dimension_values,	METH_VARARGS,	slabapi_dimension_values__doc__},
 {"dimension_is_circular",	(PyCFunction)slabapi_dimension_is_circular,	METH_VARARGS,	slabapi_dimension_is_circular__doc__},
 {"getdotdirectory", (PyCFunction)PyVCS_getdotdirectory, 1}, 
	{NULL,	 (PyCFunction)NULL, 0, NULL}		/* sentinel */
};


/* Initialization function for the module (*must* be called initslabapi) */

static char slabapi_module_documentation[] = 
""
;

void
initslabapi()
{
	PyObject *m, *d;

	/* Create the module and add the functions */
	m = Py_InitModule4("slabapi", slabapi_methods,
		slabapi_module_documentation,
		(PyObject*)NULL,PYTHON_API_VERSION);
        import_array()
	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);
	ErrorObject = PyString_FromString("slabapi.error");
	PyDict_SetItemString(d, "error", ErrorObject);

	/* XXXX Add constants here */
	
	/* Check for errors */
	if (PyErr_Occurred())
		Py_FatalError("can't initialize module slabapi");
}

