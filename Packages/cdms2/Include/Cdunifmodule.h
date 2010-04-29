#ifndef Py_CDUNIFMODULE_H
#define Py_CDUNIFMODULE_H
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Include file for cdunif files and variables.
 *
 * Adapted from code written by Konrad Hinsen
 * last revision: 2001-5-4
 */


#include <stdio.h>
#include "cdunif.h"

/* CdunifFile object */

typedef struct {
  PyObject_HEAD
  PyObject *dimensions;   /* dictionary */
  PyObject *variables;    /* dictionary */
  PyObject *attributes;   /* dictionary */
  PyObject *name;         /* string */
  PyObject *mode;         /* string */
  int id;
  char open;
  char define;
  char write;
  int recdim;
  CuFileType filetype;			     /* CuNetcdf, CdGrads, etc. */
  PyObject *diminfo;			     /* {name:(units,typecode,fileName,relatedVar,dimensionType,internalId) */
} PyCdunifFileObject;


/* CdunifVariable object */

typedef struct {
  PyObject_HEAD
  PyCdunifFileObject *file;
  PyObject *attributes;   /* dictionary */
  char *name;
  int *dimids;
  size_t *dimensions;
  int type;               /* same as array types */
  int nd;
  int id;
  char unlimited;
} PyCdunifVariableObject;


/* Variable index structure */

typedef struct {
  Py_ssize_t start;
  Py_ssize_t stop;
  Py_ssize_t stride;
  Py_ssize_t item;
} PyCdunifIndex;

/*
 * C API functions
 */

/* Type definitions */
#define PyCdunifFile_Type_NUM 0
#define PyCdunifVariable_Type_NUM 1

/* Open a Cdunif file (i.e. create a new file object) */
#define PyCdunifFile_Open_RET PyCdunifFileObject *
#define PyCdunifFile_Open_PROTO Py_PROTO((char *filename, char *mode))
#define PyCdunifFile_Open_NUM 2

/* Close a Cdunif file. Returns -1 if there was an error. */
#define PyCdunifFile_Close_RET int
#define PyCdunifFile_Close_PROTO Py_PROTO((PyCdunifFileObject *file))
#define PyCdunifFile_Close_NUM 3

/* Ensure that all data is written to the disk file.
   Returns 0 if there was an error. */
#define PyCdunifFile_Sync_RET int
#define PyCdunifFile_Sync_PROTO Py_PROTO((PyCdunifFileObject *file))
#define PyCdunifFile_Sync_NUM 4

/* Create a new dimension. Returns -1 if there was an error. */
#define PyCdunifFile_CreateDimension_RET int
#define PyCdunifFile_CreateDimension_PROTO \
        Py_PROTO((PyCdunifFileObject *file, char *name, long size))
#define PyCdunifFile_CreateDimension_NUM 5

/* Create a Cdunif variable and return the variable object */
#define PyCdunifFile_CreateVariable_RET PyCdunifVariableObject *
#define PyCdunifFile_CreateVariable_PROTO \
      Py_PROTO((PyCdunifFileObject *file, char *name, int typecode, \
                char **dimension_names, int ndim))
#define PyCdunifFile_CreateVariable_NUM 6

/* Return an object referring to an existing variable */
#define PyCdunifFile_GetVariable_RET PyCdunifVariableObject *
#define PyCdunifFile_GetVariable_PROTO \
	  Py_PROTO((PyCdunifFileObject *file, char *name))
#define PyCdunifFile_GetVariable_NUM 7

/* Get variable rank */
#define PyCdunifVariable_GetRank_RET int
#define PyCdunifVariable_GetRank_PROTO Py_PROTO((PyCdunifVariableObject *var))
#define PyCdunifVariable_GetRank_NUM 8

/* Get variable shape */
#define PyCdunifVariable_GetShape_RET size_t *
#define PyCdunifVariable_GetShape_PROTO Py_PROTO((PyCdunifVariableObject *var))
#define PyCdunifVariable_GetShape_NUM 9

/* Allocate and initialize index structures for reading/writing data */
#define PyCdunifVariable_Indices_RET PyCdunifIndex *
#define PyCdunifVariable_Indices_PROTO Py_PROTO((PyCdunifVariableObject *var))
#define PyCdunifVariable_Indices_NUM 10

/* Read data and return an array object */
#define PyCdunifVariable_ReadAsArray_RET PyArrayObject *
#define PyCdunifVariable_ReadAsArray_PROTO \
	  Py_PROTO((PyCdunifVariableObject *var, PyCdunifIndex *indices))
#define PyCdunifVariable_ReadAsArray_NUM 11

/* Write array. Returns -1 if there was an error.  */
#define PyCdunifVariable_WriteArray_RET int
#define PyCdunifVariable_WriteArray_PROTO \
	  Py_PROTO((PyCdunifVariableObject *var, PyCdunifIndex *indices, \
		    PyObject *array))
#define PyCdunifVariable_WriteArray_NUM 12

/* Get file attribute */
#define PyCdunifFile_GetAttribute_RET PyObject *
#define PyCdunifFile_GetAttribute_PROTO \
	  Py_PROTO((PyCdunifFileObject *var, char *name))
#define PyCdunifFile_GetAttribute_NUM 13

/* Set file attribute */
#define PyCdunifFile_SetAttribute_RET int
#define PyCdunifFile_SetAttribute_PROTO \
	  Py_PROTO((PyCdunifFileObject *var, char *name, PyObject *value))
#define PyCdunifFile_SetAttribute_NUM 14

/* Set file attribute to string value */
#define PyCdunifFile_SetAttributeString_RET int
#define PyCdunifFile_SetAttributeString_PROTO \
	  Py_PROTO((PyCdunifFileObject *var, char *name, char *value))
#define PyCdunifFile_SetAttributeString_NUM 15

/* Get variable attribute */
#define PyCdunifVariable_GetAttribute_RET PyObject *
#define PyCdunifVariable_GetAttribute_PROTO \
	  Py_PROTO((PyCdunifVariableObject *var, char *name))
#define PyCdunifVariable_GetAttribute_NUM 16

/* Set variable attribute */
#define PyCdunifVariable_SetAttribute_RET int
#define PyCdunifVariable_SetAttribute_PROTO \
	  Py_PROTO((PyCdunifVariableObject *var, char *name, PyObject *value))
#define PyCdunifVariable_SetAttribute_NUM 17

/* Set variable attribute to string value */
#define PyCdunifVariable_SetAttributeString_RET int
#define PyCdunifVariable_SetAttributeString_PROTO \
	  Py_PROTO((PyCdunifVariableObject *var, char *name, char *value))
#define PyCdunifVariable_SetAttributeString_NUM 18

/* Add entry to the history */
#define PyCdunifFile_AddHistoryLine_RET int
#define PyCdunifFile_AddHistoryLine_PROTO \
	  Py_PROTO((PyCdunifFileObject *self, char *text))
#define PyCdunifFile_AddHistoryLine_NUM 19

/* Write string. Returns -1 if there was an error.  */
#define PyCdunifVariable_WriteString_RET int
#define PyCdunifVariable_WriteString_PROTO \
	  Py_PROTO((PyCdunifVariableObject *var, PyStringObject *value))
#define PyCdunifVariable_WriteString_NUM 20

/* Read string  */
#define PyCdunifVariable_ReadAsString_RET PyStringObject *
#define PyCdunifVariable_ReadAsString_PROTO \
	  Py_PROTO((PyCdunifVariableObject *var))
#define PyCdunifVariable_ReadAsString_NUM 21

/* Total number of C API pointers */
#define PyCdunif_API_pointers 22



#ifdef _CDUNIF_MODULE

/* Type object declarations */
#if defined(__CYGWIN__)
extern PyTypeObject PyCdunifFile_Type;
extern PyTypeObject PyCdunifVariable_Type;
#else
staticforward PyTypeObject PyCdunifFile_Type;
staticforward PyTypeObject PyCdunifVariable_Type;
#endif

/* Type check macros */
#define PyCdunifFile_Check(op) ((op)->ob_type == &PyCdunifFile_Type)
#define PyCdunifVariable_Check(op) ((op)->ob_type == &PyCdunifVariable_Type)

/* C API function declarations */
static PyCdunifFile_Open_RET PyCdunifFile_Open PyCdunifFile_Open_PROTO;
static PyCdunifFile_Close_RET PyCdunifFile_Close PyCdunifFile_Close_PROTO;
static PyCdunifFile_Sync_RET PyCdunifFile_Sync PyCdunifFile_Sync_PROTO;
static PyCdunifFile_CreateDimension_RET PyCdunifFile_CreateDimension \
  PyCdunifFile_CreateDimension_PROTO;
static PyCdunifFile_CreateVariable_RET PyCdunifFile_CreateVariable \
  PyCdunifFile_CreateVariable_PROTO;
static PyCdunifFile_GetVariable_RET PyCdunifFile_GetVariable \
  PyCdunifFile_GetVariable_PROTO;
static PyCdunifVariable_GetRank_RET PyCdunifVariable_GetRank \
  PyCdunifVariable_GetRank_PROTO;
static PyCdunifVariable_GetShape_RET PyCdunifVariable_GetShape \
  PyCdunifVariable_GetShape_PROTO;
static PyCdunifVariable_Indices_RET PyCdunifVariable_Indices \
  PyCdunifVariable_Indices_PROTO;
static PyCdunifVariable_ReadAsArray_RET PyCdunifVariable_ReadAsArray \
  PyCdunifVariable_ReadAsArray_PROTO;
static PyCdunifVariable_ReadAsString_RET PyCdunifVariable_ReadAsString \
  PyCdunifVariable_ReadAsString_PROTO;
static PyCdunifVariable_WriteArray_RET PyCdunifVariable_WriteArray \
  PyCdunifVariable_WriteArray_PROTO;
static PyCdunifVariable_WriteString_RET PyCdunifVariable_WriteString \
  PyCdunifVariable_WriteString_PROTO;
static PyCdunifFile_GetAttribute_RET PyCdunifFile_GetAttribute \
  PyCdunifFile_GetAttribute_PROTO;
static PyCdunifFile_SetAttribute_RET PyCdunifFile_SetAttribute \
  PyCdunifFile_SetAttribute_PROTO;
static PyCdunifFile_SetAttributeString_RET PyCdunifFile_SetAttributeString \
  PyCdunifFile_SetAttributeString_PROTO;
static PyCdunifVariable_GetAttribute_RET PyCdunifVariable_GetAttribute \
  PyCdunifVariable_GetAttribute_PROTO;
static PyCdunifVariable_SetAttribute_RET PyCdunifVariable_SetAttribute \
  PyCdunifVariable_SetAttribute_PROTO;
static PyCdunifVariable_SetAttributeString_RET \
  PyCdunifVariable_SetAttributeString \
  PyCdunifVariable_SetAttributeString_PROTO;
static PyCdunifFile_AddHistoryLine_RET PyCdunifFile_AddHistoryLine \
  PyCdunifFile_AddHistoryLine_PROTO;

#else

/* C API address pointer */ 
static void **PyCdunif_API;

/* Type check macros */
#define PyCdunifFile_Check(op) \
   ((op)->ob_type == (PyTypeObject *)PyCdunif_API[PyCdunifFile_Type_NUM])
#define PyCdunifVariable_Check(op) \
   ((op)->ob_type == (PyTypeObject *)PyCdunif_API[PyCdunifVariable_Type_NUM])

/* C API function declarations */
#define PyCdunifFile_Open \
  (*(PyCdunifFile_Open_RET (*)PyCdunifFile_Open_PROTO) \
   PyCdunif_API[PyCdunifFile_Open_NUM])
#define PyCdunifFile_Close \
  (*(PyCdunifFile_Close_RET (*)PyCdunifFile_Close_PROTO) \
   PyCdunif_API[PyCdunifFile_Close_NUM])
#define PyCdunifFile_Sync \
  (*(PyCdunifFile_Sync_RET (*)PyCdunifFile_Sync_PROTO) \
   PyCdunif_API[PyCdunifFile_Sync_NUM])
#define PyCdunifFile_CreateDimension \
  (*(PyCdunifFile_CreateDimension_RET (*)PyCdunifFile_CreateDimension_PROTO) \
   PyCdunif_API[PyCdunifFile_CreateDimension_NUM])
#define PyCdunifFile_CreateVariable \
  (*(PyCdunifFile_CreateVariable_RET (*)PyCdunifFile_CreateVariable_PROTO) \
   PyCdunif_API[PyCdunifFile_CreateVariable_NUM])
#define PyCdunifFile_GetVariable \
  (*(PyCdunifFile_GetVariable_RET (*)PyCdunifFile_GetVariable_PROTO) \
   PyCdunif_API[PyCdunifFile_GetVariable_NUM])
#define PyCdunifVariable_GetRank \
  (*(PyCdunifVariable_GetRank_RET (*)PyCdunifVariable_GetRank_PROTO) \
   PyCdunif_API[PyCdunifVariable_GetRank_NUM])
#define PyCdunifVariable_GetShape \
  (*(PyCdunifVariable_GetShape_RET (*)PyCdunifVariable_GetShape_PROTO) \
   PyCdunif_API[PyCdunifVariable_GetShape_NUM])
#define PyCdunifVariable_Indices \
  (*(PyCdunifVariable_Indices_RET (*)PyCdunifVariable_Indices_PROTO) \
   PyCdunif_API[PyCdunifVariable_Indices_NUM])
#define PyCdunifVariable_ReadAsArray \
  (*(PyCdunifVariable_ReadAsArray_RET (*)PyCdunifVariable_ReadAsArray_PROTO) \
   PyCdunif_API[PyCdunifVariable_ReadAsArray_NUM])
#define PyCdunifVariable_ReadAsString \
  (*(PyCdunifVariable_ReadAsString_RET (*)PyCdunifVariable_ReadAsString_PROTO) \
   PyCdunif_API[PyCdunifVariable_ReadAsString_NUM])
#define PyCdunifVariable_WriteArray \
  (*(PyCdunifVariable_WriteArray_RET (*)PyCdunifVariable_WriteArray_PROTO) \
   PyCdunif_API[PyCdunifVariable_WriteArray_NUM])
#define PyCdunifVariable_WriteString \
  (*(PyCdunifVariable_WriteString_RET (*)PyCdunifVariable_WriteString_PROTO) \
   PyCdunif_API[PyCdunifVariable_WriteString_NUM])
#define PyCdunifFile_GetAttribute \
  (*(PyCdunifFile_GetAttribute_RET (*)PyCdunifFile_GetAttribute_PROTO) \
   PyCdunif_API[PyCdunifFile_GetAttribute_NUM])
#define PyCdunifFile_SetAttribute \
  (*(PyCdunifFile_SetAttribute_RET (*)PyCdunifFile_SetAttribute_PROTO) \
   PyCdunif_API[PyCdunifFile_SetAttribute_NUM])
#define PyCdunifFile_SetAttributeString \
  (*(PyCdunifFile_SetAttributeString_RET \
     (*)PyCdunifFile_SetAttributeString_PROTO) \
   PyCdunif_API[PyCdunifFile_SetAttributeString_NUM])
#define PyCdunifVariable_GetAttribute \
  (*(PyCdunifVariable_GetAttribute_RET (*)PyCdunifVariable_GetAttribute_PROTO) \
   PyCdunif_API[PyCdunifVariable_GetAttribute_NUM])
#define PyCdunifVariable_SetAttribute \
  (*(PyCdunifVariable_SetAttribute_RET (*)PyCdunifVariable_SetAttribute_PROTO) \
   PyCdunif_API[PyCdunifVariable_SetAttribute_NUM])
#define PyCdunifVariable_SetAttributeString \
  (*(PyCdunifVariable_SetAttributeString_RET \
     (*)PyCdunifVariable_SetAttributeString_PROTO) \
   PyCdunif_API[PyCdunifVariable_SetAttributeString_NUM])
#define PyCdunifFile_AddHistoryLine \
  (*(PyCdunifFile_AddHistoryLine_RET \
     (*)PyCdunifFile_AddHistoryLine_PROTO) \
   PyCdunif_API[PyCdunifFile_AddHistoryLine_NUM])

#define import_cdunif() \
{ \
  PyObject *module = PyImport_ImportModule("Cdunif"); \
  if (module != NULL) { \
    PyObject *module_dict = PyModule_GetDict(module); \
    PyObject *c_api_object = PyDict_GetItemString(module_dict, "_C_API"); \
    if (PyCObject_Check(c_api_object)) { \
      PyCdunif_API = (void **)PyCObject_AsVoidPtr(c_api_object); \
    } \
  } \
}

#endif



#ifdef __cplusplus
}
#endif
#endif /* Py_CDUNIFMODULE_H */
