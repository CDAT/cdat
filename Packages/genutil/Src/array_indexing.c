#include <Python.h>
#include "numpy/arrayobject.h"



int extract_function_double(int n1, int n2 ,
		      double *input, int *indices, 
		      double *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    output[j]=input[l];
    }
  return 0;
}
int extract_function_float(int n1, int n2 ,
		      float *input, int *indices, 
		      float *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
/*     printf("output[j] will be set to input[l] j,l,k,input[l] are: %d,%d,%d,%f\n",j,l,k,input[l]); */
    output[j]=input[l];
    }
  return 0;
}
int extract_function_int(int n1, int n2 ,
		      int *input, int *indices, 
		      int *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    output[j]=input[l];
    }
  return 0;
}

int extract_function_long(int n1, int n2 ,
		      long *input, int *indices, 
		      long *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    output[j]=input[l];
    }
  return 0;
}
int extract_function_short(int n1, int n2 ,
		      short *input, int *indices, 
		      short *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    output[j]=input[l];
    }
  return 0;
}
int extract_function_char(int n1, int n2 ,
		      char *input, int *indices, 
		      char *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    output[j]=input[l];
    }
  return 0;
}
int extract_function_uchar(int n1, int n2 ,
		      unsigned char *input, int *indices, 
		      unsigned char *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    output[j]=input[l];
    }
  return 0;
}
int extract_function_schar(int n1, int n2 ,
		      signed char *input, int *indices, 
		      signed char *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    output[j]=input[l];
    }
  return 0;
}

int set_function_char(int n1, int n2 , 
		      char *input, int *indices, 
		      char *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
  return 0;
}

int set_function_uchar(int n1, int n2 , 
		      unsigned char *input, int *indices, 
		      unsigned char *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
  return 0;
}

int set_function_schar(int n1, int n2 , 
		      signed char *input, int *indices, 
		      signed char *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
  return 0;
}

int set_function_short(int n1, int n2 , 
		      short *input, int *indices, 
		      short *output)
{
  int j,k,l;
   char msg[256];
 for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
}

int set_function_int(int n1, int n2 , 
		      int *input, int *indices, 
		      int *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
  return 0;
}

int set_function_long(int n1, int n2 , 
		      long *input, int *indices, 
		      long *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
  return 0;
}

int set_function_float(int n1, int n2 , 
		      float *input, int *indices, 
		      float *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
  return 0;
}

int set_function_double(int n1, int n2 , 
		      double *input, int *indices, 
		      double *output)
{
  int j,k,l;
  char msg[256];
  for (j=0 ; j<n2 ; j++ )
    {
    k=((int *)indices)[j];
    if (k>=0) l=k*n2+j;
    else      l=(n1+k)*n2+j;
    if (l>(n2*n1-1)) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l<0) 
      {
	sprintf(msg,"Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
	PyErr_SetString(PyExc_ValueError,msg);
	return 1;
      }
    if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
    if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
/*     printf("assigning %i, to %f, %f\n",l,output[j],input[l]); */
    input[l]=output[j];
/*     printf("assigned  %i, to %f, %f\n",l,output[j],input[l]); */
    }
  return 0;
}


static PyObject *  
extract(self,args) 
     PyObject *self;
     PyObject *args;
{
  PyObject *input,*indices;
  PyArrayObject *array,*result,*indices_array;
  int j,n1,n2,type_num,nloop,ioffset,ioffset2,dim,ierr;
  if (!PyArg_ParseTuple(args,"OO",&input,&indices))
    return NULL;
  array=(PyArrayObject *)
    PyArray_ContiguousFromObject(input,PyArray_NOTYPE,1,0);
  if (array==NULL) return NULL;
  indices_array=(PyArrayObject *)
    PyArray_ContiguousFromObject(indices,PyArray_INT,0,0);
  if (indices_array==NULL) {
    Py_DECREF(array);
    return NULL;}
  if (indices_array->nd!=array->nd-1 )
    {
      nloop=array->dimensions[0];
      n2=1;
      for (j=1;j< indices_array->nd; j++) {
	n2=n2*indices_array->dimensions[j];}
    }
  else 
    {
      nloop=1;
      n1=1;
      n2=1;
      for (j=1;j< array->nd; j++) 
	{n1=n1*array->dimensions[j];
	}
      for (j=0;j< indices_array->nd; j++) {
	n2=n2*indices_array->dimensions[j];}
      if (n1!=n2) {
    PyErr_SetString(PyExc_ValueError,"Length of second array must match length of first array without the first dimension");
    return NULL;}
    }
  n1=array->dimensions[0];
  type_num=array->descr->type_num;
  result=(PyArrayObject *)(PyArray_SimpleNew(indices_array->nd,indices_array->dimensions,type_num));
  for (dim=0;dim<nloop;dim++)
    {
      ioffset=indices_array->strides[0]*dim;
      ioffset2=result->strides[0]*dim;
      ierr=0;
/*       printf("dim,nloop,offset: %d, %d, %d\n",dim,nloop,ioffset); */
      if (type_num==PyArray_FLOAT){
	ierr=extract_function_float(n1,n2,(float *)array->data,(int *)(indices_array->data+ioffset), (float *)(result->data+ioffset2));
      }
      else if (type_num==PyArray_CHAR){
	ierr=extract_function_char(n1,n2,array->data,(int *)indices_array->data+ioffset, (result->data+ioffset2));
      }
      else if (type_num==PyArray_UBYTE){
	ierr=extract_function_uchar(n1,n2,(unsigned char *)array->data,(int *)(indices_array->data+ioffset),(unsigned char *)(result->data+ioffset2));
      }
      else if (type_num==PyArray_BYTE){
	ierr=extract_function_schar(n1,n2,(signed char *)array->data,(int *)(indices_array->data+ioffset),(signed char *)(result->data+ioffset2));
      }
      else if (type_num==PyArray_SHORT){
	ierr=extract_function_short(n1,n2,(short *)array->data,(int *)(indices_array->data+ioffset),(short *)(result->data+ioffset2));
      }
      else if (type_num==PyArray_INT){
	ierr=extract_function_int(n1,n2,(int *)array->data,(int *)(indices_array->data+ioffset),(int *)(result->data+ioffset2));
      }
      else if (type_num==PyArray_LONG){
	ierr=extract_function_long(n1,n2,(long *)array->data,(int *)(indices_array->data+ioffset),(long *)(result->data+ioffset2));
      }
      else if (type_num==PyArray_DOUBLE){
	ierr=extract_function_double(n1,n2,(double *)array->data,(int *)(indices_array->data+ioffset),(double *)(result->data+ioffset2));
      }
      else {
	ierr=1;
	PyErr_SetString(PyExc_ValueError,"Unsupported Array Type !");
      }
      if (ierr==1)
	{
	  Py_DECREF(array); 
	  Py_DECREF(indices_array); 
	  Py_DECREF(result);
	  return NULL;
	}
    }
  Py_DECREF(array); 
  Py_DECREF(indices_array); 
  return (PyObject *)result;
}

static PyObject *  
set(self,args) 
     PyObject *self;
     PyObject *args;
{
  PyObject *array,*indices,*values;
  PyArrayObject *array_array,*indices_array,*values_array;
  int j,n1,n2,nloop,aoffset,ioffset,type_num,dim,ierr;
  if (!PyArg_ParseTuple(args,"OOO",&array,&indices,&values))
    return NULL;
  array_array=(PyArrayObject *)
    PyArray_ContiguousFromObject(array,PyArray_NOTYPE,1,0);
  if (array_array==NULL) return NULL;
  indices_array=(PyArrayObject *)
    PyArray_ContiguousFromObject(indices,PyArray_INT,0,0);
  if (indices_array==NULL) {
    Py_DECREF(array_array);
    return NULL;}
  values_array=(PyArrayObject *)
    PyArray_ContiguousFromObject(values,PyArray_NOTYPE,0,0);
  if (values_array==NULL) {
    Py_DECREF(array_array);
    Py_DECREF(indices_array);
    return NULL;}
  if (array_array->descr->type_num != values_array->descr->type_num)
    {
      Py_DECREF(array_array);
      Py_DECREF(indices_array);
      Py_DECREF(values_array);
      PyErr_SetString(PyExc_ValueError,"Set array and source array must be of same type !");
      return NULL;
    }
  
  if (indices_array->nd!=array_array->nd-1 )
    {
      nloop=array_array->dimensions[0];
      n2=1;
      for (j=1;j< indices_array->nd; j++) {
	n2=n2*indices_array->dimensions[j];}
    }
  else 
    {
      nloop=1;
      n1=1;
      n2=1;
      for (j=1;j< array_array->nd; j++) 
	{n1=n1*array_array->dimensions[j];
	}
      for (j=0;j< indices_array->nd; j++) {
	n2=n2*indices_array->dimensions[j];}
      if (n1!=n2) 
	{
	  Py_DECREF(array_array);
	  Py_DECREF(indices_array);
	  Py_DECREF(values_array);
	  PyErr_SetString(PyExc_ValueError,"Length of second array must match length of first array without the first dimension");
	  return NULL;
	} 
      if (values_array->nd!=array_array->nd-1 )
	{
	  Py_DECREF(array_array);
	  Py_DECREF(indices_array);
	  Py_DECREF(values_array);
	  PyErr_SetString(PyExc_ValueError,"Third (values) array must be one dimension less than first (data) arrray");
	  return NULL;
	}
    }
  n1=array_array->dimensions[0];
  type_num=array_array->descr->type_num;
  for (dim=0;dim<nloop;dim++)
    {
      aoffset=array_array->strides[0]*dim;
      if (indices_array->nd!=0) {
	ioffset=indices_array->strides[0]*dim;
      }
      else {
	ioffset = 0;
      }
      ierr=0;
      if (type_num==PyArray_FLOAT){
	ierr=set_function_float(n1,n2,(float *)array_array->data,(int *)(indices_array->data+ioffset), (float *)(values_array->data+aoffset));
      }
      else if (type_num==PyArray_CHAR){
	ierr=set_function_char(n1,n2,array_array->data,(int *)indices_array->data+ioffset, values_array->data+aoffset);
      }
      else if (type_num==PyArray_UBYTE){
	ierr=set_function_uchar(n1,n2,(unsigned char *)array_array->data,(int *)(indices_array->data+ioffset),(unsigned char *)(values_array->data+aoffset));
      }
      else if (type_num==PyArray_BYTE){
	ierr=set_function_schar(n1,n2,(signed char *)array_array->data,(int *)(indices_array->data+ioffset),(signed char *)(values_array->data+aoffset));
      }
      else if (type_num==PyArray_SHORT){
	ierr=set_function_short(n1,n2,(short *)array_array->data,(int *)(indices_array->data+ioffset),(short *)(values_array->data+aoffset));
      }
      else if (type_num==PyArray_INT){
	ierr=set_function_int(n1,n2,(int *)array_array->data,(int *)(indices_array->data+ioffset),(int *)(values_array->data+aoffset));
      }
      else if (type_num==PyArray_LONG){
	ierr=set_function_long(n1,n2,(long *)array_array->data,(int *)(indices_array->data+ioffset),(long *)(values_array->data+aoffset));
      }
      else if (type_num==PyArray_DOUBLE){
	ierr=set_function_double(n1,n2,(double *)array_array->data,(int *)(indices_array->data+ioffset),(double *)(values_array->data+aoffset));
      }
      else {
	ierr=1;
	PyErr_SetString(PyExc_ValueError,"Unsupported Array Type !");
      }
      if (ierr==1)
	{
	  Py_DECREF(array); 
	  Py_DECREF(indices_array); 
	  Py_DECREF(values_array);
	  return NULL;
	}
    }
  Py_DECREF(values_array); 
  Py_DECREF(indices_array); 
  return (PyObject *)array_array;
}

void set_function_fixed_value(int n1, int n2 , 
		      char *input, char *indices, 
		      float value)
{
  int j,k,l;
/*   printf(" Value: %f\n",value); */
  for (j=0 ; j<n2 ; j++ )
    {
/*       printf("j is: %i of %i\n",j,n2); */
      k=((int *)indices)[j];
/*       printf("k is: %i\n",k); */
      if (k>=0) l=k*n2+j;
      else      l=(n1+k)*n2+j;
      if (l>(n2*n1-1)) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
      if (l<0) printf("Index Out of range you want %i which corresponds to %i, maximum is %i\n",k,l,n2*n1-1);
      ((float *)input)[l]=(float)value;
    }
}

static PyObject *  
rank(self,args) 
     PyObject *self;
     PyObject *args;
{
  PyObject *array,*indices;
  PyArrayObject *array_array,*indices_array;
  int j,n1,n2,ic;
  if (!PyArg_ParseTuple(args,"OO",&array,&indices))
    return NULL;
  array_array=(PyArrayObject *)
    PyArray_ContiguousFromObject(array,PyArray_FLOAT32,1,0);
  if (array_array==NULL) return NULL;
  indices_array=(PyArrayObject *)
    PyArray_ContiguousFromObject(indices,PyArray_INT32,0,0);
  if (indices_array==NULL) {
    Py_DECREF(array_array);
    return NULL;}
  if (indices_array->nd!=array_array->nd ){
    PyErr_SetString(PyExc_ValueError,"Second (indices) array and first (data) arrray must have same dims");
    return NULL;
  }
  /* Check that array and indices dimensions match*/
  n1=1;
  n2=1;
  for (j=0;j< array_array->nd; j++) 
    {n1=n1*array_array->dimensions[j];
    }
  for (j=0;j< indices_array->nd; j++) {
    n2=n2*indices_array->dimensions[j];}
  if (n1!=n2) {
    PyErr_SetString(PyExc_ValueError,"Length of second array must match length of first array");
  return NULL;}
  n1=array_array->dimensions[0];
  /* Computes length of indices array */
  n2=1;
  for (j=1;j< indices_array->nd; j++) {
    n2=n2*indices_array->dimensions[j];}
  /* loop through n1 */
/*   printf("LOOP LENGTH: %i\n",n1); */
  for (j=0;j<n1;j++)
    {
      ic=(int) array_array->strides[0];
      ic=ic*j;
/*       tmp=(int *)indices_array->data+ic; */
      set_function_fixed_value(n1,n2,array_array->data,indices_array->data+ic, (float)j);
    }
  Py_DECREF(indices_array); 
  /*Py_DECREF(array_array); */
  return (PyObject *)array_array;
  }



static PyMethodDef MyExtractMethods[]= {
  {"extract", extract , METH_VARARGS},
  {"set", set , METH_VARARGS},
  {"rank",rank,METH_VARARGS},
  {NULL, NULL} /*sentinel */
};

void 
initarray_indexing()
{
  (void) Py_InitModule("array_indexing", MyExtractMethods);
  import_array()
  
}

int main(int argc,char **argv)
{
  Py_SetProgramName(argv[0]);
  Py_Initialize();
  initarray_indexing();}

