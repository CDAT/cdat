#ifndef VISUSSWIGSUPPORT_H
#define VISUSSWIGSUPPORT_H

//#include <Python.h>
#include "pyarray_unique_api.h"
#include "numpy/arrayobject.h"

// Attach the new_result to the given result
static void attachResult(PyObject** result, PyObject* new_result)
{
  // If the previous result is empty
  if ((!(*result)) || (*result == Py_None)) {
    // We simply pass on the new result 
    *result = new_result;
  }
  else {
    // If result contains a single object not a tuple 
    if (!PyTuple_Check(*result)) {
      // Create a new tuple with a single value
      PyObject* tmp = *result;
      *result = PyTuple_New(1);
      PyTuple_SetItem(*result,0,tmp); // and set its entry to the old result
    }

    // Now *result holds a pointer to a tuple

    // Create yet another tuple holding new_result
    PyObject* tuple = PyTuple_New(1);
    PyTuple_SetItem(tuple,0,new_result);

    // Make a copy of the tuple containing the old result
    PyObject* org_tuple = *result;

    // Concatenate the old and new results
    *result = PySequence_Concat(org_tuple,tuple);

    // Make sure to tell python that the temporary tuples can be
    // deleted
    Py_DECREF(org_tuple);
    Py_DECREF(tuple);
  }
}

static PyObject* convertBlockDataNumpy(VisusBlockData* data)
{
  std::vector<int> samples; // The number of samples in the VisusBlockData
  int ndim = 4; // The number of dimensions of the output array
  
  int dim[4]; // The number of samples in each dimension. Currently 
              // there cannot be more than three spatial dimensions.
              // However, rgb values, for example will be returned 
              // as an array of uchar[3] which can create 4D arrays
    
  int type_num = -1; // The type code for the new array

  samples = data->samples();
  dim[0] = samples[0];
  dim[1] = samples[1];
  dim[2] = samples[2];
  dim[3] = 1; // until we find otherwise we assume we have no sub-array


  // Here we determine the python type codes for the given data. Note
  // that python does not support most of the unsigned type (which we
  // ignore) nor long long 64 bit integers for which we produce an
  // error. Raw's and rgb values will be interpreted as sub-arrays of
  // unsigned bytes of the appropriate length.
  switch (data->dataType()) {
  
  case PV_RAW:
    // We treat a raw array as an array of arrays where each data point
    // stores an array of sampleSize() many unsigned chars
    type_num = PyArray_UBYTE;
    dim[3] = data->sampleSize();
    break;
  case PV_CHAR:
    type_num = PyArray_CHAR;
    break;
  case PV_UCHAR:
    type_num = PyArray_UBYTE;
    break;
  case PV_INT:
    type_num = PyArray_INT;
    break;
  case PV_INT16:
  case PV_UINT16:
    type_num = PyArray_SHORT;
    break;
  case PV_INT32:
  case PV_UINT32:
    type_num = PyArray_INT;
    break;
  case PV_INT64:
  case PV_UINT64:
    PyErr_SetString(PyExc_TypeError, "64-bit interger currently not suppoted in numpy");
    break;
  case PV_FLOAT:
  case PV_FLOAT32:
    type_num = PyArray_FLOAT;
    break;
  case PV_FLOAT64:
    type_num = PyArray_DOUBLE;
    break;
  case PV_RGB:
    type_num = PyArray_UBYTE;
    dim[3] = 3;
    break;
  case PV_RGBA:
    type_num = PyArray_UBYTE;
    dim[3] = 4;
    break;
  }

  // Now we want to compactify the potentially 4D array if
  // possible. Each dimension containing only a single sample should
  // be ignored and we need to handle potential sub_arrays (stored in
  // dim[3]
  
  if (dim[3] > 1) { // If we have a sub-array
    
    // We need to test whether we can compactify our representation
    int i = 2;
    while ((dim[i] == 1) && (i >= 0)) {
      std::swap(dim[i],dim[i+1]);
      i--;
    }
  }

  for (int i=1;i<4;i++) {
    if (dim[i] == 1) {
      ndim = i;
      break;
    }
  }

  // Allocate the new array
  PyArrayObject* array = (PyArrayObject*)PyArray_FromDims(ndim,dim,type_num);

  // Copy the data 
  memcpy(array->data, data->data(),data->dataSize());

  return PyArray_Return(array);
}


#endif
