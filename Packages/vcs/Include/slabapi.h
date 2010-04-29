#ifndef __slabapi__H
#define __slabapi__H
#ifdef __cplusplus
extern "C" {
#endif
#include "Python.h"
#include "numpy/arrayobject.h"

char*          slabAttribute (PyObject* slab, char* attribute, char* default_value);
PyObject*      slabDimension (PyObject* slab, int i); /* the i'th dimension object */
int            slabRank (PyObject* slab); /* number of dimensions */
int            slabCheck(PyObject* slab);
char           slabType (PyObject* slab); /* type code */
PyArrayObject* slabData (PyObject* slab); /* slab.__data */
PyObject*      slabMask (PyObject* slab); /* None or array of mask values */
double         slabMissingValue(PyObject* slab);

/* Dimension object queries */
PyObject*      slabDimensionKey (PyObject* slab, int idim, char* key);
int            slabDimensionLength (PyObject* slab, int idim);
char*          slabDimensionName (PyObject* slab, int idim, int *isLongLat);
char*          slabDimensionUnits (PyObject* slab, int idim);
PyArrayObject* slabDimensionValues (PyObject* slab, int idim); /* type float */
PyObject*      slabDimensionWeights (PyObject* slab, int idim); /* possibly None */
PyArrayObject* slabDimensionBounds (PyObject* slab, int idim);
/* 0 if not circcular, 360.0 if circular */
int            slabDimensionIsCircular (PyObject* slab, int idim); 

#ifdef __cplusplus
}
#endif
#endif /* __slabapi__H */
