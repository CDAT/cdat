#include <float.h>
#include <limits.h>
#include "Python.h"

static char _kinds_module_documentation[] = 
"Constants from C standard header files to support kinds module"
;
static struct PyMethodDef _kinds_methods[] = {
    	
    {NULL,		NULL, 0}		/* sentinel */
};

DL_EXPORT(void)
init_kinds()
{
	PyObject *m, *d;
    PyObject *v;
	/* Create the module and add the functions */
	m = Py_InitModule4("_kinds", 
	                    _kinds_methods,
		                _kinds_module_documentation,
		                (PyObject*)NULL,PYTHON_API_VERSION);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	d = PyModule_GetDict(m);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
/* C FLT */
	v = Py_BuildValue("d", FLT_MAX);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_MAX", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("d", FLT_MIN);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_MIN", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("d", FLT_EPSILON);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_EPSILON", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_ROUNDS);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_ROUNDS", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_RADIX);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_RADIX", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_DIG);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_DIG", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_MANT_DIG);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_MANT_DIG", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_MAX_10_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_MAX_10_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_MIN_10_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_MIN_10_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_MAX_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_MAX_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", FLT_MIN_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "FLT_MIN_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
/* C DOUBLE */
	v = Py_BuildValue("d", DBL_MAX);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_MAX", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("d", DBL_MIN);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_MIN", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("d", DBL_EPSILON);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_EPSILON", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", DBL_DIG);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_DIG", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", DBL_MANT_DIG);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_MANT_DIG", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", DBL_MAX_10_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_MAX_10_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", DBL_MIN_10_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_MIN_10_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", DBL_MAX_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_MAX_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", DBL_MIN_EXP);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "DBL_MIN_EXP", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);
/* C INT */
	v = Py_BuildValue("i", INT_MAX);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "INT_MAX", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("i", INT_MIN);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "INT_MIN", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);
/* C LONG INT */
	v = Py_BuildValue("L", LONG_MAX);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "LONG_MAX", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);

	v = Py_BuildValue("L", LONG_MIN);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	PyDict_SetItemString(d, "LONG_MIN", v);
	if (PyErr_Occurred())
		Py_FatalError("Cannot initialize the _kinds module");
	Py_DECREF(v);
}

   
