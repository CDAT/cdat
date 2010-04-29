/***********************************************************************
*
* Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
* Produced at the Lawrence Livermore National Laboratory  
* Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
* LLNL-CODE-406031.  
* All rights reserved.  
*   
* This file is part of "Simple and Flexible Scene Graph Version 2.0."
* Please also read BSD_ADDITIONAL.txt.
*   
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met:
*   
* @ Redistributions of source code must retain the above copyright
*   notice, this list of conditions and the disclaimer below.
* @ Redistributions in binary form must reproduce the above copyright
*   notice, this list of conditions and the disclaimer (as noted below) in
*   the documentation and/or other materials provided with the
*   distribution.
* @ Neither the name of the LLNS/LLNL nor the names of its contributors
*   may be used to endorse or promote products derived from this software
*   without specific prior written permission.
*   
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
* LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING
*
***********************************************************************/


#include <cstdlib>
#include "numpyconversion.h"

#include "pyarray_unique_api.h"
#include "numpy/arrayobject.h"

#include "PvDataType.h"

char *encodeArray(PyObject *o, char* name, int dimX, int dimY, int dimZ, int dimT)
{

  PyArrayObject *A = NULL; // casted pointer

  // Set Field Name
  char fieldName[1024] = "Unknown";
  if (name != NULL)
    strcpy(fieldName, name);
 
  //printf("%s:%d First argument OK o=%lu\n",__FILE__,__LINE__, o);fflush(stdout);
  
  // Here we would really like to check whether the object is indeed a
  // PyArray. However, at least on tux the check itself results in a
  // seg fault so we can't (ptb)
  /* 
  if (!PyArray_Check(o)) {
    printf("%s:%d Object not a PyArray",__FILE__,__LINE__);fflush(stdout);
    PyErr_SetString(PyExc_TypeError,"Error:pypython_encodeArray:: First argument must be of type PyArray\n");
    return NULL;
  }
  */
  //printf("%s:%d First argument is PyArray o=%lu\n",__FILE__,__LINE__, o);fflush(stdout);
  
  A = (PyArrayObject *)o;

  //printf("%s:%d Cast object to PyArray pointer\n",__FILE__,__LINE__);fflush(stdout);
  
  // At some point in the future the contigous restriction should be
  // lifted and visus be adapted to deal with a strides
  // array. However, for now we need something working first
  // (05/24/07) (ptb)
  
  // Check whether the array in a valid C-array which includes being
  // aligned, of the correct byteorder, and contiguous
  if (!PyArray_ISCARRAY(A)) { 
    PyErr_SetString(PyExc_ValueError,"Error:encodeArray currently only c-style arrays are supported\n");
    return NULL;
  }
  

  // Array dimensions are passed to the function explicitly. The
  // encodeArray function will shape the array accordingly and compute
  // the samplesize and pvtype accordingly. This allows to build up
  // arrays on non-trivial data types something not directly possible
  // with numpy arrays. 
  // Example 2D rgb image: 
  //
  // The original array is of shape [height]x[width]x[3] of type Int8
  // and would normally be interpreted as a 3D array. Passing the
  // explicit dimensions dimX = width, dimY = height, dimZ = 1, dimT =
  // 1 allows us to treat it as image with samplesize 3 bytes.

  // First we calculate the total number of elements in the array
  int total = 1;
  for (int i=0;i<PyArray_NDIM(A);i++) 
    total *= PyArray_DIM(A,i);

  // If there are fewer elements than our explicit shape demands
  if (total < dimX*dimY*dimZ*dimT) {
    PyErr_SetString(PyExc_ValueError,"Error:encodeArray: Not enough elements for the given dimensions \n");fflush(stdout);
    return NULL;
  }

  // Now we determine the type and sample size of a SINGLE array entry
  int size = -1;
  char type = PyArray_DESCR(A)->type;
  char errmsg[512];
  PvDataType pvtype = PV_CHAR;  // At least with this default we won't
                                // access data past the end of the array.
  switch (type) {
  case 'c': // char 
  case 'b': // Int8
  case '1': // ??? 
    size = 1;
    // Here we need some way to differentiate between actual arrays of bytes
    // and arrays of raw values (PV_RAW) for which the user must specify a
    // field value size in bytes.  For now, we assume a byte array is just
    // byte values.
    pvtype = PV_UCHAR;
    break;
  case 'd': // double 
    size = 8;
    pvtype = PV_FLOAT64;
    break;
  case 'f': // float 
    size = 4;
    pvtype = PV_FLOAT;
    break;
  case 'l':
  case 'D':
  case 'F':
    sprintf(errmsg,"Error:encodeArray: typecode \"%c\" not supported\n",type);
    PyErr_SetString(PyExc_ValueError,errmsg);
    size = -1;
    break;
  default:
    sprintf(errmsg,"Error:encodeArray: typecode \"%c\" unknowd\n",type);
    PyErr_SetString(PyExc_ValueError,errmsg);
    size = -1;
    break;
  }
  
  if ((size != 1) && (size != 4) && (size != 8)) {
      // This also triggers the exception set above  in the switch statement.
    return NULL;
  }


  // Now adjust for potentially non-trivial data types
  
  if (total > dimX*dimY*dimZ*dimT) {
    int factor = total / (dimX*dimY*dimZ*dimT); 
    
    //fprintf(stderr,"Factor %d\n",factor);
    // special case for RGB images
    if ((size == 1) && (factor == 3)) {
      size *= factor;
      pvtype = PV_RGB;
    }
    else {
      size *= factor;
      pvtype = PV_RAW;
    }
  }

  char *dataset = new char[1024];
  
  if (dataset == NULL) {
    PyErr_SetString(PyExc_MemoryError,"Error:encodeArray could not allocate result \n");
    return NULL;
  }

  // When using the IncoreDataSource object, we now have the ability to more 
  // easily specify multiple named fields for the data source.
  // This routine generates a descriptor for a single field, which needs to
  // be incorporated into the complete data source descriptor.
  // The dimensions are not encoded in this descriptor, since the dimensions
  // are common to all fields of a DataSource object.
  //
  // See src/pv/IncoreDataSource.h for the full format.
  //
  assert(sizeof(unsigned long) >= sizeof(unsigned char*));

  sprintf(dataset,"%s %d %d %d ",
          fieldName,               // Name of field
          PyArray_DATA(A),   // Pointer to base field address
          pvtype,                  // Type of field data
          size                     // Size of data
          );

  // Test Accessing The Array Before Returning
  switch(pvtype) {
 	case PV_FLOAT32:
  		{
			float* data = (float*) PyArray_DATA(A);
			float sum = 0.0;
			
			for (int i=0; i<dimX; ++i) {
				int ioffset = i * dimY * dimX;
				for (int j=0; j<dimY; ++j) {
					int joffset = j * dimY;
					for (int k=0; k<dimZ; ++k)
						sum += data[k+joffset+ioffset];		
				}
			}
  		}
  		break;
  	case PV_FLOAT64:
  		{
			double* data = (double*) PyArray_DATA(A);
			double sum = 0.0;
			
			for (int i=0; i<dimX; ++i) {
				int ioffset = i * dimY * dimX;
				for (int j=0; j<dimY; ++j) {
					int joffset = j * dimY;
					for (int k=0; k<dimZ; ++k)
						sum += data[k+joffset+ioffset];		
				}
			}
  		}
  		break;
  	default:
  		break;
  }

  return dataset;
}

int dumpArray(PyArrayObject *A, PyObject* f)
{
  PyErr_SetString(PyExc_ValueError,"Error: dumpArray: not implemented yet \n");
  return NULL;
}

int arrayPointer(PyObject *o)
{
  PyArrayObject *A = NULL; // casted pointer

  /*
  if (!PyArray_Check(o)) {
    printf("%s:%d Object not a PyArray",__FILE__,__LINE__);fflush(stdout);
    PyErr_SetString(PyExc_TypeError,"Error:pypython_encodeArray:: First argument must be of type PyArray\n");
    return 0;
  }
  */
  
  A = (PyArrayObject *)o;

  if (!PyArray_ISCARRAY(A)) { 
  //if (!PyArray_ISCONTIGUOUS(A)) { 
    PyErr_SetString(PyExc_ValueError,"Error:encodeArray currently only c-style arrays are supported\n");
    fprintf(stderr,"invalid array\n");
    return 0;
  }
  
  /*
  float* f = (float*)PyArray_DATA(A); 
  for (int i=0;i<25*126*256;i++) {
    if (f[i] > 0)
      fprintf(stderr,"%f\n",f[i]);
  }
  */
  int address = 0;
  char val[100];
  sprintf(val, "%x", PyArray_DATA(A));
  sscanf(val, "%x", &address); 
 
  return address;
}

int isCArray(PyObject* o)
{
  //if (!PyArray_Check(o))
  //  return 0; 
  
  PyArrayObject *A = (PyArrayObject *)o;

  return PyArray_ISCARRAY(A);
}

    
