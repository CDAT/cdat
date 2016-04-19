#include <Python.h>
#include <udunits.h>

static ut_system *ut_read = NULL;

void check_ut_read(char *xmlFile) {
  extern ut_system *ut_read;
  int status=0;
  if (ut_read != NULL) {
    return;
  }
  ut_set_error_message_handler(ut_ignore);
  ut_read = ut_read_xml(xmlFile);
  if (ut_read == NULL) {
    status = ut_get_status() == UT_PARSE
      ? UT_ESYNTAX
      : UT_EIO;
  }
  return;
}

static PyObject *
init(self,args)
     PyObject *self;
     PyObject *args;
{
  char *file=NULL;
  check_ut_read(file);
  Py_INCREF ((PyObject *)Py_None); return Py_None;
}

static PyObject *
addBaseUnit(self,args)
  PyObject *self;
  PyObject *args;
{
  extern ut_system *ut_read;
  char msg[256],*unit;
  int MAX_STRING=256;
  ut_unit *newequnit=NULL;
  ut_status myutstatus;

  /* read in unit name */
  if (!PyArg_ParseTuple(args,"s",&unit))
    return NULL;

  newequnit = ut_new_base_unit(ut_read);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: creating %s base unit",unit);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  myutstatus = ut_map_name_to_unit(unit,UT_ASCII,newequnit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error mapping %s unit",unit);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  Py_INCREF ((PyObject *)Py_None); return Py_None;
}

static PyObject *
addDimensionlessUnit(self,args)
  PyObject *self;
  PyObject *args;
{
  extern ut_system *ut_read;
  char msg[256],*unit;
  int MAX_STRING=256;
  ut_unit *dimlessunit=NULL;
  ut_status myutstatus;

  /* read in unit name */
  if (!PyArg_ParseTuple(args,"s",&unit))
    return NULL;
  if (dimlessunit!= NULL) ut_free(dimlessunit);
  dimlessunit = ut_new_dimensionless_unit(ut_read);
  if (ut_get_status() != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: creating dimless unit %s",unit);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  myutstatus = ut_map_name_to_unit(unit,UT_ASCII,dimlessunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error mapping dimless uniti to %s",unit);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  Py_INCREF ((PyObject *)Py_None); return Py_None; 
}

static PyObject *
addOffsettedUnit(self,args)
  PyObject *self;
  PyObject *args;
{
  extern ut_system *ut_read;
  char msg[256],*newunit, *oldunit;
  int MAX_STRING=256;
  double offset;
  ut_unit *offsettedunit=NULL,*originalunit;
  ut_status myutstatus;

  /* read in unit name */
  if (!PyArg_ParseTuple(args,"sds",&newunit,&offset,&oldunit))
    return NULL;

  originalunit = ut_parse(ut_read, oldunit, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(msg,"UDUNITS Error: invalid udunits: %s",oldunit);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  offsettedunit = ut_offset(originalunit,offset);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error offsetting %s unit by %g",oldunit,offset);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  myutstatus = ut_map_name_to_unit(newunit,UT_ASCII,offsettedunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error mapping name %s unit to offsetted unit %s (by %g)",newunit,oldunit,offset);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }

  Py_INCREF ((PyObject *)Py_None); return Py_None;

}
static PyObject *
addScaledUnit(self,args)
  PyObject *self;
  PyObject *args;
{
  extern ut_system *ut_read;
  char msg[256],*newunit, *oldunit, err[256];
  int MAX_STRING=256;
  double scale;
  ut_unit *scaledunit=NULL,*originalunit;
  ut_status myutstatus;

  /* read in unit name */
  if (!PyArg_ParseTuple(args,"sds",&newunit,&scale,&oldunit))
    return NULL;

  originalunit = ut_parse(ut_read, oldunit, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",oldunit);
    PyErr_SetString(PyExc_TypeError, err);
    return NULL;
  }
  scaledunit = ut_scale(scale,originalunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error scaling %s unit by %g",oldunit,scale);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  myutstatus = ut_map_name_to_unit(newunit,UT_ASCII,scaledunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error mapping name %s unit to scaled unit %s (by %g)",newunit,oldunit,scale);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }

  Py_INCREF ((PyObject *)Py_None); return Py_None;

}

static PyObject *
addMultipliedUnits(self,args)
  PyObject *self;
  PyObject *args;
{
  extern ut_system *ut_read;
  char msg[256],*newunit, *unit1, *unit2, err[256];
  int MAX_STRING=256;
  ut_unit *outunit=NULL,*uunit1,*uunit2;
  ut_status myutstatus;

  /* read in unit name */
  if (!PyArg_ParseTuple(args,"sss",&newunit,&unit1,&unit2))
    return NULL;

  uunit1 = ut_parse(ut_read, unit1, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",unit1);
    PyErr_SetString(PyExc_TypeError, err);
    return NULL;
  }
  uunit2 = ut_parse(ut_read, unit2, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",unit2);
    PyErr_SetString(PyExc_TypeError, err);
    return NULL;
  }
  outunit = ut_multiply(uunit1,uunit2);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error multiplying units %s and %s",unit1,unit2);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  myutstatus = ut_map_name_to_unit(newunit,UT_ASCII,outunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error mapping name %s unit to multiplied units %s and %s",newunit,unit1,unit2);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }

  Py_INCREF ((PyObject *)Py_None); return Py_None;

}
static PyObject *
addDividedUnits(self,args)
  PyObject *self;
  PyObject *args;
{
  extern ut_system *ut_read;
  char msg[256],*newunit, *unit1, *unit2, err[256];
  int MAX_STRING=256;
  ut_unit *outunit=NULL,*uunit1,*uunit2;
  ut_status myutstatus;

  /* read in unit name */
  if (!PyArg_ParseTuple(args,"sss",&newunit,&unit1,&unit2))
    return NULL;

  uunit1 = ut_parse(ut_read, unit1, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",unit1);
    PyErr_SetString(PyExc_TypeError, err);
    return NULL;
  }
  uunit2 = ut_parse(ut_read, unit2, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",unit2);
    PyErr_SetString(PyExc_TypeError, err);
    return NULL;
  }
  outunit = ut_divide(uunit1,uunit2);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error dividing units %s and %s",unit1,unit2);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  myutstatus = ut_map_name_to_unit(newunit,UT_ASCII,outunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error mapping name %s unit to divided units %s and %s",newunit,unit1,unit2);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }

  Py_INCREF ((PyObject *)Py_None); return Py_None;

}
static PyObject *
addInvertedUnit(self,args)
  PyObject *self;
  PyObject *args;
{
  extern ut_system *ut_read;
  char msg[256],*newunit, *oldunit, err[256];
  int MAX_STRING=256;
  ut_unit *invertedunit=NULL,*originalunit;
  ut_status myutstatus;

  /* read in unit name */
  if (!PyArg_ParseTuple(args,"ss",&newunit,&oldunit))
    return NULL;

  originalunit = ut_parse(ut_read, oldunit, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",oldunit);
    PyErr_SetString(PyExc_TypeError, err);
    return NULL;
  }
  invertedunit = ut_invert(originalunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error inverting %s unit",oldunit);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }
  myutstatus = ut_map_name_to_unit(newunit,UT_ASCII,invertedunit);
  if (myutstatus != UT_SUCCESS) {
    snprintf(msg,MAX_STRING,"Udunits: Error mapping name %s unit to inverted unit %s",newunit,oldunit);
    PyErr_SetString(PyExc_TypeError, msg);
    return NULL;
  }

  Py_INCREF ((PyObject *)Py_None); return Py_None;

}

  static PyObject *
convert(self,args)
  PyObject *self;
  PyObject *args;
{
  char *unit1,*unit2;
  extern ut_system *ut_read;
  ut_unit *udunit1 = NULL, *udunit2 = NULL;
  double Slope,Intercept;
  char err[256];

  if (!PyArg_ParseTuple(args,"ss",&unit1,&unit2))
    return NULL;

  udunit1 = ut_parse(ut_read, unit1, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",unit1);
    PyErr_SetString(PyExc_TypeError, err);
    return NULL;
  }
  udunit2 = ut_parse(ut_read, unit2, UT_ASCII);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: invalid udunits: %s",unit2);
    PyErr_SetString(PyExc_TypeError, err);
    ut_free(udunit1);
    if (udunit2 != NULL) {
      ut_free(udunit2);
    }
    return NULL;
  }
  cv_converter*   converter = ut_get_converter(udunit1, udunit2);
  if ( ut_get_status() != UT_SUCCESS) {
    sprintf(err,"UDUNITS Error: cannot convert from %s to %s",unit1,unit2);
    PyErr_SetString(PyExc_TypeError, "Error Converting.");
    ut_free(udunit1);
    ut_free(udunit2);
    if (converter != NULL) {
      cv_free(converter);
    }
    return NULL;
  }
  Intercept = cv_convert_double(converter, 0.0);
  Slope = cv_convert_double(converter, 1.0) - Intercept;
  ut_free(udunit1);
  ut_free(udunit2);
  cv_free(converter);

  return Py_BuildValue("dd", Slope, Intercept);
}


static PyObject *
term(self,args)
     PyObject *self;
     PyObject *args;
{
  extern ut_system *ut_read;
  ut_free_system(ut_read);
  ut_read = NULL;
}
static PyMethodDef MyUdunitsMethods[]= {
  {"init", init , METH_VARARGS},
  {"addDimensionlessUnit", addDimensionlessUnit , METH_VARARGS},
  {"addBaseUnit", addBaseUnit , METH_VARARGS},
  {"addScaledUnit", addScaledUnit , METH_VARARGS},
  {"addOffsettedUnit", addOffsettedUnit , METH_VARARGS},
  {"addInvertedUnit", addInvertedUnit , METH_VARARGS},
  {"addMultipliedUnits", addMultipliedUnits , METH_VARARGS},
  {"addDividedUnits", addDividedUnits , METH_VARARGS},
  {"convert", convert , METH_VARARGS},
  {"term", term , METH_VARARGS},
  {NULL, NULL} /*sentinel */
};

void 
initudunits_wrap()
{
  (void) Py_InitModule("udunits_wrap", MyUdunitsMethods);
}

int main(int argc,char **argv)
{
  Py_SetProgramName(argv[0]);
  Py_Initialize();
  initudunits_wrap();}

