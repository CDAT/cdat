#ifndef PY_ARRAY_UNIQUE_INCLUDED
#define PY_ARRAY_UNIQUE_INCLUDED

// This file must be included BEFORE arrayobject.h is included in every
// file that includes arrayobject.h

// Whenever using Numeric or NumPy for an extension that contains multiple
// files which include arrayobject.h, we must define a unique name for
// the table of functions defined in arrayobject.h.  If we do not, then
// each source file including arrayobject.h will get a local static table
// which can cause segfaults.

#define PY_ARRAY_UNIQUE_SYMBOL numpyPV

//!!!!! IMPORTANT !!!!!!
// 
// Code that uses PyArray functions, but that does not define a module
// via Py_InitModule(), must also specify
//
// #define NO_IMPORT_ARRAY
//
// Basically, swig wrapper files should only include this header and arrayobject.
// Any hand written module code should include this header and arrayobject.
// Any other files using numeric API functions must define NO_IMPORT_ARRAY.

#endif
