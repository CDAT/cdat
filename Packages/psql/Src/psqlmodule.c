/************************************************************************
 *          Python module interface to the PCMDI Standard Query         *
 *                       Language (PSQL) library                        *
 *                                                                      *
 *                                  for                                 *
 *             PCMDI's Climate Data Analysis Tool - (CDAT)              *
 *              Command Line User Interface Popup Window                *
 *                         Developed at LLNL                            *
 *                                                                      *
 *                                                                      *
 *      Copyright (C) 1997. The Regents of the University of California.*
 *      All rights reserved.                                            *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      Authors: Charles O'Connor and  Dean N. Williams                 *
 *                                                                      *
 *      Date: 05/26/98                                                  *
 *                                                                      *
 *      File name: psqlmodule.c                                         *
 *                                                                      *
 *                                                                      *
 *      Langague: ANSI C/Python                                         *
 *                                                                      *
 *                                                                      *
 *      Comments:                                                       *
 *               This software uses ANSI C and Python to provide the    *
 *               CDAT user with a command line user interface to PSQL.  *
 *                                                                      *
 *      Modifications:                                                  *
 *                                                                      *
 *                                                                      *
 *      Contact:                                                        *
 *                                                                      *
 *              Charles O'Connor    Dean N. Williams                    *
 *                                                                      *
 *              LLNL                LLNL                                *
 *              PO Box 808, L-264   PO Box 808, L-264                   *
 *              Livermore, CA       Livermore, CA                       *
 *              94550               94550                               *
 *                                                                      *
 *              (510) 422-1627      (510) 423-0145                      *
 *                                                                      *
 *                                                                      *
 ************************************************************************
 */

#include "Python.h"
#include <string.h>
#define PSQL
#include "cdmsint_new.h"

#define PyInit_PSQL initpsql_interface

static PyObject *PyPSQL_Error;

/* Structure for PSQL object */
typedef struct {
	PyObject_HEAD
  	long ldb;
} PyPSQL_Object;

staticforward PyTypeObject PyPSQL_Type;

static PyObject *
psql_err(message)
char *message;
{
	PyErr_SetString(PyPSQL_Error, message);
	return NULL;
}

static void
PyPSQL_Dealloc(self)
PyPSQL_Object *self;
{
	/* Delete PSQL object */
        PyMem_DEL(self);
}

/* Initialize the PSQL object. */
static PyObject *
PyPSQL_init(self, args)
  PyObject *self;
  PyObject *args;
{
	PyPSQL_Object *psql_object;

        /* Initialize the PSQL as a new Python object */
        psql_object = PyObject_NEW(PyPSQL_Object, &PyPSQL_Type);

	/* This function initializes the PSQL module */
  	psql_object->ldb = py_psql_init( );

	/* Return the PSQL object to python */
        return (PyObject *)psql_object;
}

/* Initialize the PSQL object as a list. */
static PyObject *
PyPSQL_list(self, args)
  PyPSQL_Object *self;
  PyObject *args;
{
	PyObject *psql_list;
	char *psql_command=NULL;
	char *psql_str, *pstr;
	int  i=0, newline_ct=0;

	if(PyArg_ParseTuple(args, "s", &psql_command)) {
           if ((psql_command == NULL) || (psql_command[0] == '\0')) {
	      psql_err("Error1 - No PSQL command given.");
              return NULL;
           } else {
              /* Pass the PSQL command string down to the PSQL interpreter */
	      psql_str = py_psql_execute(self->ldb, psql_command);
	      if (psql_str[0] != '\0') {
	          /* Count the number of new lines */
	          while (psql_str[i] != '\0')  {
		        if (psql_str[i] == '\n')
                           ++newline_ct;
                        ++i;
	          }
                  psql_list = PyList_New(newline_ct);
                  pstr = strtok(psql_str, "\n");
	          for (i=0; i<newline_ct; i++) {
	             if (pstr != NULL)
	                PyList_SetItem(psql_list, i, Py_BuildValue("s",pstr));
                     pstr = strtok(NULL, "\n");
	          }
                  return (PyObject *) psql_list;
	      } else {
	         /* return null python object */
                 Py_INCREF ((PyObject *)Py_None);
                 return Py_None; 
	      }
           }
        }

	/* return null python object */
        Py_INCREF ((PyObject *)Py_None);
        return Py_None; 
}

/* Initialize the PSQL object as a string. */
static PyObject *
PyPSQL_string(self, args)
  PyPSQL_Object *self;
  PyObject *args;
{
	PyObject *psql_list;
	char *psql_command=NULL;
	char *psql_str, *pstr;
	int  i=0, newline_ct=0;

	if(PyArg_ParseTuple(args, "s", &psql_command)) {
           if ((psql_command == NULL) || (psql_command[0] == '\0')) {
	      psql_err("Error1 - No PSQL command given.");
              return NULL;
           } else {
              /* Pass the PSQL command string down to the PSQL interpreter */
	      psql_str = py_psql_execute(self->ldb, psql_command);
	      if (psql_str[0] != '\0') {
                  PySys_WriteStdout("%s\n", psql_str);
	      }
	      /* return null python object */
              Py_INCREF ((PyObject *)Py_None);
              return Py_None; 
           }
        }

	/* return null python object */
        Py_INCREF ((PyObject *)Py_None);
        return Py_None; 
}

static PyMethodDef PyPSQL_methods[] =
{
	{"init", PyPSQL_init, 1},
	{"list", (PyCFunction)PyPSQL_list, 1},
	{"string", (PyCFunction)PyPSQL_string, 1},

	{0, 0}
};

static PyObject *
PyPSQL_getattr(self, name)
PyPSQL_Object *self;
char *name;
{
	PyObject *method;

	method = Py_FindMethod(PyPSQL_methods, (PyObject *)self, name);
	if (method != NULL)
		return method;
	PyErr_Clear();

	PyErr_SetString(PyExc_AttributeError, name);
	return NULL;
}

static PyTypeObject PyPSQL_Type = {
	PyObject_HEAD_INIT(&PyType_Type)
	0,						/* ob_size */
	"PSQL Command",					/* tp_name */
	sizeof(PyPSQL_Object),				/* tp_basicsize */
	0,						/* tp_itemsize */
	/* methods */
	(destructor)PyPSQL_Dealloc,			/* tp_dealloc */
	0,						/* tp_print */
	(getattrfunc)PyPSQL_getattr,			/* tp_getattr */
	(setattrfunc)0,					/* tp_setattr */
	0,						/* tp_compare */
	0,						/* tp_repr */
	0,						/* tp_as_number */
	0,                       	 		/* tp_as_sequence */
	0,				      		/* tp_as_mapping */
	0,                                      	/* tp_hash */
};


void
PyInit_PSQL()
{
	PyObject *m, *d;

	m = Py_InitModule("psql_interface", PyPSQL_methods);

	d = PyModule_GetDict(m);
	PyPSQL_Error = Py_BuildValue("s", "psql_interface.error");
	PyDict_SetItemString(d,"error", PyPSQL_Error);
	
	if (PyErr_Occurred())
	   Py_FatalError("Cannot initialize module psql_interface.");
}
