/***********************************************************************
 * AN ENHANCED PYTHON EMBEDDED-CALL INTERFACE
 * Wraps Python's run-time embedding API functions for easy use.
 * Most (except Run_Method) assume the call's qualified by a module
 * (namespace). The module can be a file-name reference or a dummy module
 * created to provide a namespace for file-less strings. These routines
 * provide debugging, module (re)loading, input/output conversions, etc.
 *
 * Python is automatically initialized when the first call occurs.
 * Input/output conversions use the standard conversion format-codes.
 * Errors are flagged as either a -1 int, or a NULL pointer result.
 * Exported names use upper-case to minimize clashes [but this scheme 
 * isn't fool-proof: there's no "import" in C, just "from*"]. Also
 * note that the varargs code here may not be portable to all C's.
 ***********************************************************************/

#ifndef PYEMBED_H
#define PYEMBED_H

#include <stdio.h>
#include <Python.h>
#include <import.h>
#include <pythonrun.h>
#include <graminit.h>

/* from Python/modsupport.c: does input-conversion */
/* but not debugging, reloading, output-conversion */

extern 
PyAPI_FUNC(PyObject*) PyEval_CallFunction(PyObject *obj, const char *format, ...);
extern 
PyAPI_FUNC(PyObject*) PyEval_CallMethod(PyObject *obj, const char *methodname, const char *format, ...);


extern int PY_RELOAD;    /* reload modules when attributes are referenced? */
extern int PY_DEBUG;     /* start debugger when string/function/member run? */

typedef enum {
     PY_EXPRESSION,      /* what is a code-string? */
     PY_STATEMENT        /* expr's and statements differ */
} StringModes;


/** PYEMBED0.C: MODULES **/
extern char     *Init(char *modname);
extern int       Make_Dummy_Module(char *modname);
extern PyObject *Load_Module(char *modname);
extern PyObject *Load_Attribute(char *modname, char *attrname);
extern int       Run_Command_Line(char *prompt);


/** PYEMBED1.C: MODULE-VARIABLES **/
extern int
    Convert_Result(PyObject *presult, char *resFormat, void *resTarget);
extern int 
    Get_Global(char *modname, char *varname, char *resfmt, void *cresult);
extern int
    Set_Global(char *modname, char *varname, char *valfmt, ... /*val*/);


/** PYEMBED2.C: CODE-STRINGS **/
extern PyObject*
    Debug_Codestr(StringModes mode, 
                  char *codestring, PyObject *moddict);
extern int 
    Run_Codestr(StringModes mode,                  /* expr or stmt? */
                char *code,   char *modname,       /* code, module  */
                char *resfmt, void *cresult);      /* output result */


/** PYEMBED3.C: FUNCTIONS/CLASSES **/
extern PyObject*
    Debug_Function(PyObject *func, PyObject *args);
extern int 
    Run_Function(char *modname, char *funcname,           /* or classname */
                 char *resfmt,  void *cresult,            /* output */
                 char *argfmt,  ... /* arg, arg... */ );  /* input args */


/** PYEMBED4.C: OBJECT-METHODS/MEMBERS **/
extern int 
    Run_Method(PyObject *pobject, char *method,        /* uses Debug_Function */
                   char *resfmt,  void *cresult,              /* output */
                   char *argfmt,  ... /* arg, arg... */ );    /* inputs */
extern int 
    Get_Member(PyObject *pobject, char *attrname,
                   char *resfmt,  void *cresult);             /* output */
extern int 
    Set_Member(PyObject *pobject, char *attrname,
                   char *valfmt,  ... /* val, val... */ );    /* input */

#endif 
