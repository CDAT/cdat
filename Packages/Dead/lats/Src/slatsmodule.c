/* Wrap LATS library */

#include "Python.h"             /* Python header files */
#include "Numeric/arrayobject.h"
#include <stdio.h>              /* C header files */
#include <string.h>
#include "latsint.h"
#include "netcdf.h"

PyObject *SlatsErrorObject;   /* locally-raised exception */
PyObject *SlatsModuleDict;		     /* module dictionary */

#define onError(message) \
   { PyErr_SetString(SlatsErrorObject, message); return NULL; }

#define DefineLongConstant(dict, name,value) \
   { PyDict_SetItemString(dict, name, PyInt_FromLong(value)); }

/******************************************************************************
* LOCAL LOGIC/DATA
******************************************************************************/

/******************************************************************************
* EXPORTED MODULE METHOD-FUNCTIONS
*
* Define 1 static (local) C function per module method. Each method-function 
* gets 2 'PyObject*' Python-object arguments when called:
* 'self'-- only used for C extension data-types (not here)
* 'args'-- a 'tuple' holding the argument objects passed from Python code.
* Each returns a Python object: a result, None, or NULL to raise an exception.
******************************************************************************/

static char doc_basetime[] = "err = basetime(integer fileid, integer year, integer month, integer day, integer hour)";

static PyObject *
SlatsBaseTime(PyObject *self, PyObject *args)
{
    PyObject *result;

    int fileid, year, month, day, hour;

    int err;

    if (!PyArg_ParseTuple(args, "iiiii", &fileid, &year, &month, &day, &hour))
	    onError("Error parsing basetime args");

    if (!(err = lats_basetime(fileid, year, month, day, hour)))
	    onError("Error setting base time");
    
    result = Py_BuildValue("i", err);
    
    return result;
}

static char doc_close[] = "err = close(integer fileid)";

static PyObject *
SlatsClose(PyObject *self, PyObject *args)
{
    PyObject *result;

    int fileid;

    int err;

    if (!PyArg_ParseTuple(args, "i", &fileid))
	return NULL;

    err = lats_close(fileid);
    result = Py_BuildValue("i", err);
    
    return result;
}

static char doc_create[] = "err = create(string pathname, integer convention, integer calendar, integer frequency, integer delta, string center, string model, string comments)";

static PyObject *
SlatsCreate(PyObject *self, PyObject *args)
{
    PyObject *result;

    char *pathname;
    int convention;
    int calendar;
    int frequency;
    int delta;
    char *center;
    char *model;
    char *comments;

    int err;

    if (!PyArg_ParseTuple(args, "siiiisss", &pathname, &convention, &calendar, &frequency, &delta, &center, &model, &comments))
	return NULL;

    err = lats_create(pathname, convention, calendar, frequency, delta, center, model, comments);
    result = Py_BuildValue("i", err);
    
    return result;
}

					     /* Initialize a GDT file */
static char doc_initGDT[] = "(ncid,alttimeid,timeid) = initGDT(integer fileid)";

static PyObject *
SlatsInitGDT(PyObject *self, PyObject *args)
{
    PyObject *result;
    int fileid, ncid, varid, alttimeid;
    int dimids[MAX_VAR_DIMS], ndims;

    if (!PyArg_ParseTuple(args, "i", &fileid))
	return NULL;
    
					     /* Get ncid */
    if ((ncid=lats_ncid(fileid))==-1)
	    onError("Not a netCDF file");

					     /* Get the time id */
    if((varid=ncvarid(ncid,"time"))==-1)
	    onError("Cannot find time variable");

					     /* Set time:associate = "time_absolute" */
    if(ncattput(ncid,varid,"associate",NC_CHAR,strlen("time_absolute")+1,"time_absolute")==-1)
	    onError("Cannot put associate attribute");

					     /* Get the unlimited time dimension */
    if(ncvarinq(ncid,varid,(char *)0,(nc_type *)0,&ndims, dimids,(int *)0)==-1)
	    onError("Cannot get unlimited dimension ID");

					     /* Create time_absolute */
    if((alttimeid=ncvardef(ncid,"time_absolute",NC_DOUBLE,1,dimids))==-1)
	    onError("Cannot create alternate time variable");

    result = Py_BuildValue("(iii)", ncid, alttimeid, varid);

    return result;
}

					     /* Write an alternate time value */
static char doc_writeAltTime[] = "err = writeAltTime(integer ncid, integer alttimeid, integer index, fp value)";

static PyObject *
SlatsWriteAltTime(PyObject *self, PyObject *args)
{
    PyObject *result;

    int ncid, alttimeid, index;
    double value;
    long start, count;
    int err;

    if (!PyArg_ParseTuple(args, "iiid", &ncid, &alttimeid, &index, &value))
	    return NULL;

    start=index;
    count=1;
    if((err=ncvarput(ncid,alttimeid,&start,&count,&value))==-1)
	    onError("Cannot write alternate time value");

    result = Py_BuildValue("i", err);
    return result;
}
					     /* Create a time bounds variable */

static char doc_createTimeBounds[] = "(ncid,boundid,timeid) = createTimeBounds(integer fileid)";

static PyObject *
SlatsCreateTimeBounds(PyObject *self, PyObject *args)
{
    PyObject *result;
    int fileid;
    int ncid, varid, boundid, bounddim;
    int dimids[MAX_VAR_DIMS], ndims;

    if (!PyArg_ParseTuple(args, "i", &fileid))
	return NULL;

					     /* Get the netCDF ID */
    if ((ncid=lats_ncid(fileid))==-1)
	    onError("Not a netCDF file");
					     /* Get the time varid */
    if((varid=ncvarid(ncid,"time"))==-1)
	    onError("Cannot find time variable");

					     /* Get the unlimited time dimension */
    if(ncvarinq(ncid,varid,(char *)0,(nc_type *)0,&ndims, dimids,(int *)0)==-1)
	    onError("Cannot get unlimited dimension ID");

					     /* Create a time bounds dimension ID */
    if((bounddim =ncdimdef(ncid,"bound_t",2L))==-1)
	    onError("Cannot create time bound dimension");

					     /* Add bounds attribute to time var */
    if(ncattput(ncid,varid,"bounds",NC_CHAR,strlen("bounds_time")+1,"bounds_time")==-1)
	    onError("Cannot put boundsattribute");

					     /* Create the time bounds var */
    dimids[1]=bounddim;
    if((boundid=ncvardef(ncid,"bounds_time",NC_DOUBLE,2,dimids))==-1)
	    onError("Cannot create time bounds variable");

    result = Py_BuildValue("(iii)", ncid, boundid, varid);

    return result;
}

					     /* Set time bounds at the current timepoint */
static char doc_setTimeBounds[] = "err = setTimeBounds(integer ncid, integer boundid, integer index, fp lower, fp upper)";

static PyObject *
SlatsSetTimeBounds(PyObject *self, PyObject *args)
{
    PyObject *result;
    
    int ncid, boundid, index;
    double lower, upper, bounds[2];
    int err;
    long start[2], count[2];

    err = 0;
    if (!PyArg_ParseTuple(args, "iiidd", &ncid, &boundid, &index, &lower, &upper))
	    return NULL;

    bounds[0]=lower;
    bounds[1]=upper;
    start[0]=index;
    start[1]=0;
    count[0]=1;
    count[1]=2;
    if((err=ncvarput(ncid,boundid,start,count,bounds))==-1)
	    onError("Cannot write time bounds");
    
    result = Py_BuildValue("i", err);
    return result;
}

static char doc_getTimeUnits[] = "timeunits = getTimeUnits(ncid, timeid)";

static PyObject *
SlatsGetTimeUnits(PyObject *self, PyObject *args)
{
    PyObject *result;
    int ncid, timeid;
    char timeunits[LATS_MAX_RELUNITS];

    if (!PyArg_ParseTuple(args, "ii", &ncid, &timeid))
	return NULL;

    if (ncattget(ncid, timeid, "units", timeunits)==-1)
	    onError("Cannot get time units");

    result = Py_BuildValue("s",timeunits);

    return result;
}

static char doc_grid[] = "err = grid(string gridname, integer gridtype, array<fp> lons, array<fp> lats)";

static PyObject *
SlatsGrid(PyObject *self, PyObject *args)
{
    PyObject *result;

    char *gridname;
    int gridtype, nlons, nlats;
    PyArrayObject *lons, *lats, *tolons, *tolats;
    PyArrayObject *lonbounds, *latbounds, *tolonbounds, *tolatbounds;
    double *lonboundsdata, *latboundsdata;
    char tfrom;
    
    int err;

    tolonbounds = tolatbounds = lonbounds = latbounds = (PyArrayObject *)0;
    lonboundsdata = latboundsdata = (double *)0;
    if (!PyArg_ParseTuple(args, "siO!O!|O!O!", &gridname, &gridtype, &PyArray_Type, &lons, &PyArray_Type, &lats, &PyArray_Type, &lonbounds, &PyArray_Type, &latbounds))
	return NULL;

    nlons = lons->dimensions[0];
    tfrom = lons->descr->type_num;
    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
	    if (!(tolons = (PyArrayObject *)PyArray_Cast(lons, PyArray_DOUBLE)))
		onError("Cannot cast longitude array to doubles");
    }

    nlats = lats->dimensions[0];
    tfrom = lats->descr->type_num;
    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
	    if (!(tolats = (PyArrayObject *)PyArray_Cast(lats, PyArray_DOUBLE)))
		onError("Cannot cast longitude array to doubles");
    }
					     /* Only cast lonbounds and latbounds if both are defined */
    if(lonbounds && latbounds){
	    tfrom = lonbounds->descr->type_num;
	    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
		    if (!(tolonbounds = (PyArrayObject *)PyArray_Cast(lonbounds, PyArray_DOUBLE)))
			    onError("Cannot cast longitude array to doubles");
	    }
	    tfrom = latbounds->descr->type_num;
	    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
		    if (!(tolatbounds = (PyArrayObject *)PyArray_Cast(latbounds, PyArray_DOUBLE)))
			    onError("Cannot cast longitude array to doubles");
	    }
	    lonboundsdata = (double *)tolonbounds->data;
	    latboundsdata = (double *)tolatbounds->data;
    }

    if (!(err = lats_grid_bounds(gridname, gridtype, nlons, (double *)tolons->data, nlats, (double *)tolats->data, lonboundsdata, latboundsdata)))
	    onError("Cannot create grid");
    Py_DECREF(tolons);
    Py_DECREF(tolats);
    Py_XDECREF(tolonbounds);
    Py_XDECREF(tolatbounds);
    
    result = Py_BuildValue("i", err);
    
    return result;
}

static char doc_gridDelete[] = "err = gridDelete(integer gridid)";

static PyObject *
SlatsGridDelete(PyObject *self, PyObject *args)
{
    PyObject *result;
    int gridid;
    
    int err;

    if (!PyArg_ParseTuple(args, "i", &gridid))
	    return NULL;

    if (!(err = latsGridDeleteEntry(gridid)))
	    onError("Cannot delete grid");

    result = Py_BuildValue("i", err);

    return result;
}

static char doc_defineGridWeights[] = "(ncid,weightid) = defineGridWeights(integer fileid, string latitudeName, string weightsName)";

static PyObject *
SlatsDefineGridWeights(PyObject *self, PyObject *args)
{
    PyObject *result;

    int fileid;
    char *latname;
    char *weightname;
    int ncid, varid, weightid;
    int dimids[MAX_VAR_DIMS], ndims;

    int err;

    if (!PyArg_ParseTuple(args, "iss", &fileid, &latname, &weightname))
	    return NULL;

					     /* Get the netCDF ID */
    if ((ncid=lats_ncid(fileid))==-1)
	    onError("Not a netCDF file");

					     /* Get the varid of latname */
    if((varid=ncvarid(ncid,latname))==-1)
	    onError("Cannot find latitude variable");

					     /* Get the dimid of latname */
    if(ncvarinq(ncid,varid,(char *)0,(nc_type *)0,&ndims, dimids,(int *)0)==-1)
	    onError("Cannot get dimension IDs");

					     /* Add 'weights' attribute to var */
    if(ncattput(ncid,varid,"weights",NC_CHAR,strlen(weightname)+1,weightname)==-1)
	    onError("Cannot put weights attribute");

					     /* Create the weights var */
    if((weightid=ncvardef(ncid,weightname,NC_DOUBLE,1,dimids))==-1)
	    onError("Cannot create weights variable");

    result = Py_BuildValue("(ii)", ncid, weightid);

    return result;
}

static char doc_writeGridWeights[] = "err = writeGridWeights(integer ncfileid, integer weightid, array<fp> weights)";

static PyObject *
SlatsWriteGridWeights(PyObject *self, PyObject *args)
{
    PyObject *result;

    int ncid, weightid;
    int nweights;
    PyArrayObject *weights, *toweights;
    char tfrom;
    long start, count;

    int err;

    err = 0;
    if (!PyArg_ParseTuple(args, "iiO!", &ncid, &weightid, &PyArray_Type, &weights))
	    return NULL;
					     /* Cast weights to double */
    nweights = weights->dimensions[0];
    tfrom = weights->descr->type_num;
    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
	    if (!(toweights = (PyArrayObject *)PyArray_Cast(weights, PyArray_DOUBLE)))
		onError("Cannot cast weight array to doubles");
    }

					     /* Write the weights */
    start = 0;
    count = nweights;
    if((err=ncvarput(ncid,weightid,&start,&count,(double *)toweights->data))==-1)
	    onError("Cannot write weights");

    Py_DECREF(toweights);

    result = Py_BuildValue("i", err);
    return result;
}

static char doc_missingFloat[] = "err = missingFloat(integer fileid, integer varid, fp missing_value, fp delta)";

static PyObject *
SlatsMissingFloat(PyObject *self, PyObject *args)
{
    PyObject *result;
    int fileid, varid;
    double missingD, deltaD;
    int err;

    if (!PyArg_ParseTuple(args, "iidd", &fileid, &varid, &missingD, &deltaD))
	    return NULL;

    if (!(err=lats_miss_float(fileid, varid, (float)missingD, (float)deltaD)))
	    onError("Cannot set floating-point missing value");
    
    result = Py_BuildValue("i", err);
    
    return result;
}

static char doc_missingInt[] = "err = missingInt(integer fileid, integer varid, integer missing_value)";

static PyObject *
SlatsMissingInt(PyObject *self, PyObject *args)
{
    PyObject *result;
    int fileid, varid;
    int missing;
    int err;

    if (!PyArg_ParseTuple(args, "iii", &fileid, &varid, &missing))
	    return NULL;

    if (!(err=lats_miss_int(fileid, varid, missing)))
	    onError("Cannot set integer missing value");
    
    result = Py_BuildValue("i", err);
    
    return result;
}

static char doc_parmtab[] = "err = parmtab(string parm_file_path)";

static PyObject *
SlatsParmTab(PyObject *self, PyObject *args)
{
    PyObject *result;

    char *parmFilePath;

    int err;

    if (!PyArg_ParseTuple(args, "s", &parmFilePath))
	return NULL;

    if (!(err = lats_parmtab(parmFilePath)))
	    onError("Cannot set parameter file path");
    
    result = Py_BuildValue("i", err);
    
    return result;
}

static char doc_vertdim[] = "err = vertdim(string name, string type, array<fp> levs)";

static PyObject *
SlatsVertDim(PyObject *self, PyObject *args)
{
    PyObject *result;

    char *name, *type;
    PyArrayObject *levs, *tolevs;
    PyArrayObject *bounds, *tobounds;
    double *boundsdata;
    char tfrom;
    int nlevs;

    int err;

    bounds = tobounds = (PyArrayObject *)0;
    boundsdata = (double *)0;
    if (!PyArg_ParseTuple(args, "ssO!|O!", &name, &type, &PyArray_Type, &levs, &PyArray_Type, &bounds))
	return NULL;

    nlevs = levs->dimensions[0];
    tfrom = levs->descr->type_num;
    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
	    if (!(tolevs = (PyArrayObject *)PyArray_Cast(levs, PyArray_DOUBLE)))
		onError("Cannot cast vertical dimension array to doubles");
    }
    if(bounds){
	    tfrom = bounds->descr->type_num;
	    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
		    if (!(tobounds = (PyArrayObject *)PyArray_Cast(bounds, PyArray_DOUBLE)))
			    onError("Cannot cast vertical boundary array to doubles");
	    }
	    boundsdata = (double *)tobounds->data;
    }

    if (!(err = lats_vert_dim_bounds(name, type, nlevs, (double *)tolevs->data, boundsdata)))
	    onError("Cannot create vertical dimensions");
    Py_DECREF(tolevs);

    result = Py_BuildValue("i", err);
    
    return result;
}

/* Define hybrid vertical dimension components */

static char doc_defineVertDimComp[] = "(ncid,compid1,compid2,boundid1,boundid2,boundvid) = defineVertDimComp(integer fileid, string vertname, string name1, string name2, string combname, fp p0, string comment=None, string boundname1=None, string boundname2=None)";

static PyObject *
SlatsDefineVertDimComp(PyObject *self, PyObject *args)
{

    PyObject *result;

    int err;
    int fileid;
    int ncid, vertid, compid1, compid2, ndims, boundid1, boundid2, bounddim;
    int dimids[MAX_VAR_DIMS];
    char *vertname, *name1, *name2, *combname, *comment, *boundname1, *boundname2;
    double p0;

    boundname1 = boundname2 = comment = (char *)0;
    if (!PyArg_ParseTuple(args, "issssd|sss", &fileid, &vertname, &name1, &name2, &combname, &p0, &comment, &boundname1, &boundname2))
	    return NULL;

					     /* Get the netCDF ID */
    if ((ncid=lats_ncid(fileid))==-1)
	    onError("Not a netCDF file");

					     /* Get the vertical dimension varid */
    if ((vertid=ncvarid(ncid,vertname))==-1)
	    onError("Cannot find vertical dimension variable");

					     /* Get the dimid of vertdim */
    if(ncvarinq(ncid,vertid,(char *)0,(nc_type *)0,&ndims, dimids,(int *)0)==-1)
	    onError("Cannot get vertical dimension IDs");

					     /* Add 'component' attribute to vertdim */
    if(ncattput(ncid,vertid,"component",NC_CHAR,strlen(combname)+1,combname)==-1)
	    onError("Cannot put component attribute");

					     /* Add 'p0' attribute to vertdim */
    if(ncattput(ncid,vertid,"p0",NC_DOUBLE,1,&p0)==-1)
	    onError("Cannot put p0 attribute");

    if(comment){
	    if(ncattput(ncid,vertid,"comment",NC_CHAR,strlen(comment)+1,comment)==-1)
		    onError("Cannot put comment attribute");
    }

					     /* Create component vars */
    if(((compid1=ncvardef(ncid,name1,NC_DOUBLE,1,dimids))==-1) ||
       ((compid2=ncvardef(ncid,name2,NC_DOUBLE,1,dimids))==-1)
       )
	    onError("Cannot create component variable");

					     /* Create boundary vars */
    boundid1 = boundid2 = bounddim = -1;
    if(boundname1 && boundname2){
					     /* Create a vertdim component bounds dimension ID */
	    if((bounddim =ncdimdef(ncid,"bound_v",2L))==-1)
		    onError("Cannot create vertical dimension bound dimension");
	    dimids[1]=bounddim;
	    if(((boundid1=ncvardef(ncid,boundname1,NC_DOUBLE,2,dimids))==-1) ||
	       ((boundid2=ncvardef(ncid,boundname2,NC_DOUBLE,2,dimids))==-1)
	       )
		    onError("Cannot create component variable");
	    if((ncattput(ncid,compid1,"bounds",NC_CHAR,strlen(boundname1)+1,boundname1)==-1) ||
	       (ncattput(ncid,compid2,"bounds",NC_CHAR,strlen(boundname1)+1,boundname2)==-1))
		    onError("Cannot put vertdim component bounds attribute");
    }
    result = Py_BuildValue("(iiiiii)", ncid, compid1, compid2, boundid1, boundid2, bounddim);

    return result;
}

/* Write the vertical dimension components */

static char doc_writeVertDimComp[] = "err = writeVertDimComp(integer ncid, integer compid1, integer compid2, array<fp> comp1, array<fp> comp2, integer boundid1=None, integer boundid2=None, array<fp> bound1=None, array<fp> bound2=None)";

static PyObject *
SlatsWriteVertDimComp(PyObject *self, PyObject *args)
{
    PyObject *result;
    int ncid, compid1, compid2, ncomp, boundid1, boundid2;
    PyArrayObject *comp1, *comp2, *tocomp1, *tocomp2;
    PyArrayObject *bound1, *bound2, *tobound1, *tobound2;
    char tfrom;
    long start[2], count[2];
    int err;

    err = 0;
    bound1 = bound2 = (PyArrayObject *)0;
    if (!PyArg_ParseTuple(args, "iiiO!O!|iiO!O!", &ncid, &compid1, &compid2, &PyArray_Type, &comp1, &PyArray_Type, &comp2, &boundid1, &boundid2, &PyArray_Type, &bound1, &PyArray_Type, &bound2))
	    return NULL;

					     /* Cast arrays to double */
    ncomp = comp1->dimensions[0];
    tfrom = comp1->descr->type_num;
    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
	    if (!(tocomp1 = (PyArrayObject *)PyArray_Cast(comp1, PyArray_DOUBLE)))
		onError("Cannot cast component array to doubles");
    }

    ncomp = comp2->dimensions[0];
    tfrom = comp2->descr->type_num;
    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
	    if (!(tocomp2 = (PyArrayObject *)PyArray_Cast(comp2, PyArray_DOUBLE)))
		onError("Cannot cast component array to doubles");
    }

    if(bound1 && bound2){
	    tfrom = bound1->descr->type_num;
	    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
		    if (!(tobound1 = (PyArrayObject *)PyArray_Cast(bound1, PyArray_DOUBLE)))
			    onError("Cannot cast component boundary array to doubles");
	    }
	    tfrom = bound2->descr->type_num;
	    if (PyArray_CanCastSafely(PyArray_DOUBLE, tfrom)){
		    if (!(tobound2 = (PyArrayObject *)PyArray_Cast(bound2, PyArray_DOUBLE)))
			    onError("Cannot cast component boundary array to doubles");
	    }
    }

					     /* Write the components */
    start[0] = 0;
    count[0] = ncomp;
    if(((err=ncvarput(ncid,compid1,start,count,(double *)tocomp1->data))==-1) ||
       ((err=ncvarput(ncid,compid2,start,count,(double *)tocomp2->data))==-1))
	    onError("Cannot write component arrays");

    if(bound1 && bound2){
	    start[0] = start[1] = 0;
	    count[0] = ncomp;
	    count[1] = 2;
	    if(((err=ncvarput(ncid,boundid1,start,count,(double *)tobound1->data))==-1) ||
	       ((err=ncvarput(ncid,boundid2,start,count,(double *)tobound2->data))==-1))
		    onError("Cannot write component boundary arrays");
    }

    Py_DECREF(tocomp1);
    Py_DECREF(tocomp2);

    result = Py_BuildValue("i", err);
    return result;
}

static char doc_vertdimDelete[] = "err = vertdimDelete(integer id)";

static PyObject *
SlatsVertDimDelete(PyObject *self, PyObject *args)
{
    PyObject *result;
    int vertid;

    int err;

    if (!PyArg_ParseTuple(args, "i", &vertid))
	    return NULL;

    if (!(err = latsVertDeleteEntry(vertid)))
	    onError("Cannot delete vertical dimension");

    result = Py_BuildValue("i", err);

    return result;
}

static char doc_var[] = "err = var(integer fileid, string varname, integer datatype, integer timestat, integer gridid, integer levid, string comments)";

static PyObject *
SlatsVar(PyObject *self, PyObject *args)
{
    PyObject *result;

    int fileid, datatype, timestat, gridid, levid;
    char *varname;
    char *comments;

    int err;

    if (!PyArg_ParseTuple(args, "isiiiis", &fileid, &varname, &datatype, &timestat, &gridid, &levid, &comments))
	return NULL;

    err = lats_var(fileid, varname, datatype, timestat, gridid, levid, comments);
    result = Py_BuildValue("i", err);
    
    return result;
}

static char doc_write[] = "err = write(integer fileid, integer varid, fp lev, integer year, integer month, integer day, integer hour, array data)";

static PyObject *
SlatsWrite(PyObject *self, PyObject *args)
{
    PyObject *result;
    int fileid, varid, year, month, day, hour;
    double lev;
    PyArrayObject *data, *todata;
    char tfrom, tto;
    latsVar *var;

    int err;

    if (!PyArg_ParseTuple(args, "iidiiiiO!", &fileid, &varid, &lev, &year, &month, &day, &hour, &PyArray_Type, &data))
	return NULL;

    tfrom = data->descr->type_num;

					     /* Get the target datatype */
    if (!(var = latsVarLookup(fileid, varid)))
	    onError("Cannot find variable id");

    tto = (var->parm->datatype==LATS_FLOAT ? PyArray_FLOAT : PyArray_LONG);

    if (PyArray_CanCastSafely(tto, tfrom)){
	    if (!(todata = (PyArrayObject *)PyArray_Cast(data, tto)))
		onError("Cannot recast data array");
    }

    if (!(err = lats_write(fileid, varid, lev, year, month, day, hour, (void *)todata->data)))
	    onError("Cannot write data");
    Py_DECREF(todata);
    
    result = Py_BuildValue("i", err);
    
    return result;
}

/******************************************************************************
* METHOD REGISTRATION TABLE: NAME-STRING -> FUNCTION-POINTER
*
* List of functions defined in the module. A name->address method map, used
* to build-up the module's dictionary in "Py_InitModule". Once imported, this
* module acts just like it's coded in Python. The method functions handle
* converting data from/to python objects, and linkage to other C functions.
******************************************************************************/


static struct PyMethodDef slats_methods[] = {
	{"basetime", SlatsBaseTime, 1, doc_basetime}, /* Set the base time */
	{"close", SlatsClose, 1, doc_close}, /* Close the file */
	{"create",  SlatsCreate,  1, doc_create}, /* Create a LATS file */
	{"initGDT",SlatsInitGDT, 1, doc_initGDT}, /* Initialize a GDT file */
	{"writeAltTime", SlatsWriteAltTime, 1, doc_writeAltTime}, /* Write an alternate time value */
	{"createTimeBounds", SlatsCreateTimeBounds, 1,  doc_createTimeBounds}, /* Create time bounds */
	{"setTimeBounds", SlatsSetTimeBounds, 1, doc_setTimeBounds}, /* Set time boundary data at current index */
	{"getTimeUnits", SlatsGetTimeUnits, 1, doc_getTimeUnits}, /* Get the time units */
	{"grid", SlatsGrid, 1, doc_grid},    /* Create a grid */
	{"gridDelete", SlatsGridDelete, 1, doc_gridDelete}, /* Delete a grid */
	{"defineGridWeights", SlatsDefineGridWeights, 1, doc_defineGridWeights}, /* Define grid weights */
	{"writeGridWeights", SlatsWriteGridWeights, 1, doc_writeGridWeights}, /* Write grid weights */
	{"missingFloat", SlatsMissingFloat, 1, doc_missingFloat}, /* Set a float missing value */
	{"parmtab", SlatsParmTab, 1, doc_parmtab}, /* Set the parameter file path */
	{"var", SlatsVar, 1, doc_var},	     /* Define a variable */
	{"vertdim", SlatsVertDim, 1, doc_vertdim}, /* Create a vertical dimension */
	{"defineVertDimComp", SlatsDefineVertDimComp, 1, doc_defineVertDimComp}, /* Define vertdim components */
	{"writeVertDimComp", SlatsWriteVertDimComp, 1, doc_writeVertDimComp}, /* Write the vertdim components */
	{"vertdimDelete", SlatsVertDimDelete, 1, doc_vertdimDelete}, /* Delete a vertical dimension */
	{"write", SlatsWrite, 1, doc_write}, /* Write the field */
	{NULL,         NULL}                              /* end, for initmodule */
};


/******************************************************************************
* INITIALIZATION FUNCTION (IMPORT-TIME)
*
* Initialization function for the module. Called on first "import slats" in 
* a Python program. The function is usually called "initslats": this name's
* added to the built-in module table in config.c statically (if added to file
* Module/Setup), or called when the module's loaded dynamically as a shareable 
* object-file found on PYTHONPATH. File and function names matter if dynamic.
******************************************************************************/

void
initslats()
{
    PyObject *m, *d;
    import_array();
    /* create the module and add the functions */
    m = Py_InitModule("slats", slats_methods);        /* registration hook */

    /* add symbolic constants to the module */
    d = SlatsModuleDict = PyModule_GetDict(m);
    SlatsErrorObject = Py_BuildValue("s", "LATS error");
    PyDict_SetItemString(d, "error", SlatsErrorObject);      /* export exception */

    DefineLongConstant(d, "Pcmdi", (long)LATS_GDT);
    DefineLongConstant(d, "GDT", (long)LATS_GDT);
    DefineLongConstant(d, "GDTRel", (long)LATS_GDT_REL);
    DefineLongConstant(d, "GradsGrib", (long)LATS_GRADS_GRIB);
    DefineLongConstant(d, "GribOnly", (long)LATS_GRIB_ONLY);
    DefineLongConstant(d, "Coards", (long)LATS_COARDS);

    DefineLongConstant(d, "Standard", (long)LATS_STANDARD);
    DefineLongConstant(d, "Gregorian", (long)LATS_STANDARD);
    DefineLongConstant(d, "Julian", (long)LATS_JULIAN);
    DefineLongConstant(d, "Noleap", (long)LATS_NOLEAP);
    DefineLongConstant(d, "Calendar360", (long)LATS_360);
    DefineLongConstant(d, "Clim", (long)LATS_CLIM);
    DefineLongConstant(d, "ClimLeap", (long)LATS_CLIMLEAP);
    DefineLongConstant(d, "Clim360", (long)LATS_CLIM360);
    
    DefineLongConstant(d, "Yearly", (long)LATS_YEARLY);
    DefineLongConstant(d, "Monthly", (long)LATS_MONTHLY);
    DefineLongConstant(d, "MonthlyTableComp", (long)LATS_MONTHLY_TABLE_COMP);
    DefineLongConstant(d, "Weekly", (long)LATS_WEEKLY);
    DefineLongConstant(d, "Daily", (long)LATS_DAILY);
    DefineLongConstant(d, "Hourly", (long)LATS_HOURLY);
    DefineLongConstant(d, "ForecastHourly", (long)LATS_FORECAST_HOURLY);
    DefineLongConstant(d, "Fixed", (long)LATS_FIXED);

    DefineLongConstant(d, "LatsFloat", (long)LATS_FLOAT);
    DefineLongConstant(d, "LatsInt", (long)LATS_FIXED);

    DefineLongConstant(d, "Average", (long)LATS_AVERAGE);
    DefineLongConstant(d, "Instant", (long)LATS_INSTANT);
    DefineLongConstant(d, "Accum", (long)LATS_ACCUM);
    DefineLongConstant(d, "OtherTimeStat", (long)LATS_OTHER_TIME_STAT);
    
    DefineLongConstant(d, "Gaussian", (long)LATS_GAUSSIAN);
    DefineLongConstant(d, "Linear", (long)LATS_LINEAR);
    DefineLongConstant(d, "Generic", (long)LATS_GENERIC);
    
    DefineLongConstant(d, "SingleLevel", (long)LATS_SINGLE_LEVEL);
    DefineLongConstant(d, "MultiLevel", (long)LATS_MULTI_LEVEL);

    DefineLongConstant(d, "UpDirection", (long)LATS_UP);
    DefineLongConstant(d, "DownDirection", (long)LATS_DOWN);

    /* check for errors */
    if (PyErr_Occurred())
        Py_FatalError("can't initialize module slats");

    /* Return on fatal errors */
    lats_fatal=0;
}
