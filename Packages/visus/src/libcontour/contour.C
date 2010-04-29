//------------------------------------------------------------------------
//
// libcontour.C - isocontouring library
//
// Copyright (c) 1998 Emilio Camahort
//
//------------------------------------------------------------------------

// $Id: contour.C,v 1.1.2.4 2008/09/30 19:09:54 cottom1 Exp $

#include <time.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
using namespace std;

#include "data.h"			// basic data and algorithm clases
#include "dataset.h"
#include "conplot.h"
#include "contour.h"

#include "conplot2d.h"			// isocontouring algorithms and data
#include "conplot3d.h"
#include "conplotreg2.h"
#include "conplotreg3.h"

#include "datasetslc.h"			// time-varying data set classes
#include "datasetvol.h"
#include "datasetreg2.h"
#include "datasetreg3.h"

//------------------------------------------------------------------------
//
// ConDataset - the contour dataset structure is defined here
//
//------------------------------------------------------------------------

typedef struct ConDataset	// structure to store and entire dataset
{
    int		nsfun;			// number of signature functions
    Signature	***sfun;		// signature functions

    Dataset	*data;			// input data/dataset
    Conplot	*plot;			// isocontouring algorithm and data
} ConDataset;

//------------------------------------------------------------------------
//
// setPreprocessing() - display the progress of preprocessing
//
//------------------------------------------------------------------------

void    setPreprocessing(int percent, void *data)
{
#ifdef VERBOSE

    fprintf(stderr, "Preprocessing: %d done\n", percent);
#endif
}

//------------------------------------------------------------------------
//
// newDatasetUnstr() - create a new dataset for unstructured data
//
//------------------------------------------------------------------------

ConDataset	*newDatasetUnstr(int datatype, int meshtype, int nvars,
				 int ntime, int nverts, int ncells,
				 double *verts, u_int *cells,
				 int *celladj, u_char *data)
{
    int		t;			// a timestep index variable
    int		var;			// a variable index variable

    Data::DataType	type;		// data set parmeters

    Datasetslc	*slc;			// different dataset pointers
    Datasetvol	*vol;

    ConDataset	*dataset;


    type    = Data::DataType(datatype);	// miscellaneous initializations
    dataset = new ConDataset;


    switch (meshtype)			// create big objects: data and `plot'
      {
      case CONTOUR_2D :
	 dataset->data = slc = new Datasetslc(type, nvars, ntime, nverts,
				    ncells, verts, cells, celladj, data);
         // create the plot
         dataset->plot = new Conplot2d(slc);
         break;
      case CONTOUR_3D :
         dataset->data = vol = new Datasetvol(type, nvars, ntime, nverts,
				    ncells, verts, cells, celladj, data);
         // create the plot
         dataset->plot = new Conplot3d(vol);
         break;
      default :
#ifdef VERBOSE
	 fprintf(stderr, "libcontour:loadDataset: incorrect mesh type\n");
#endif
	 return NULL;
      };
					// allocate and init signature data

    dataset->sfun = new (Signature**[dataset->data->nData()]);
    for (var = 0; var < dataset->data->nData(); var++)
	{
	dataset->sfun[var] = new (Signature*[dataset->data->nTime()]);
	for (t = 0; t < dataset->data->nTime(); t++)
	    dataset->sfun[var][t] = NULL;
	}

					// check for errors, return
    if (!dataset->plot)
	{
#ifdef VERBOSE
	fprintf(stderr, "libcontour:loadDataset: couldn't create plot\n");
#endif
	return NULL;
	}

    if (!dataset->data)
	{
#ifdef VERBOSE
	fprintf(stderr, "libcontour:loadDataset: couldn't create data\n");
#endif
	return NULL;
	}
#ifdef VERBOSE
    printf("libcontour:newDatasetUnstr: data set created\n");
#endif
    return dataset;
}

//------------------------------------------------------------------------
//
// newDatasetReg() - create a new dataset structure for a regular grid
//
//------------------------------------------------------------------------

ConDataset	*newDatasetReg(int datatype, int meshtype, int nvars,
			       int ntime, int *dim, u_char *data)
{
    int		t;			// a timestep index variable
    int		var;			// a variable index variable

    Data::DataType	type;		// data set parmeters

    Datasetreg2	*reg2;			// different dataset pointers
    Datasetreg3	*reg3;

    ConDataset	*dataset;


    type    = Data::DataType(datatype);	// miscellaneous initializations
    dataset = new ConDataset;


    switch (meshtype)			// create big objects: data and `plot'
      {
      case CONTOUR_REG_2D :
         dataset->data = reg2 = new Datasetreg2(type, nvars, ntime, dim, data);
         // create the plot
         dataset->plot = new Conplotreg2(reg2);
         break;
      case CONTOUR_REG_3D :
         dataset->data = reg3 = new Datasetreg3(type, nvars, ntime, dim, data);
         // create the plot
	     dataset->plot = new Conplotreg3(reg3);
         break;
      default :
#ifdef VERBOSE
	 fprintf(stderr, "libcontour:loadDataset: incorrect mesh type\n");
#endif
	 return NULL;
      };
					// allocate and init signature data

    dataset->sfun = new (Signature**[dataset->data->nData()]);
    for (var = 0; var < dataset->data->nData(); var++)
	{
	dataset->sfun[var] = new (Signature*[dataset->data->nTime()]);
	for (t = 0; t < dataset->data->nTime(); t++)
	    dataset->sfun[var][t] = NULL;
	}

					// check for errors, return
    if (!dataset->plot)
	{
#ifdef VERBOSE
	fprintf(stderr, "libcontour:loadDataset: couldn't create plot\n");
#endif
	return NULL;
	}

    if (!dataset->data)
	{
#ifdef VERBOSE
	fprintf(stderr, "libcontour:loadDataset: couldn't create data\n");
#endif
	return NULL;
	}

#ifdef VERBOSE
    printf("libcontour:newDatasetReg: data set created\n");
#endif
    return dataset;
}

//------------------------------------------------------------------------
//
// loadDataset() - load data set from disk
//
//------------------------------------------------------------------------

ConDataset	*loadDataset(int datatype, int meshtype, int nvars,
			     int ntime, char **files)
{
    int		t;			// a timestep index variable
    int		var;			// a variable index variable

    Data::DataType	type;		// data set parmeters

    Datasetslc	*slc;			// different dataset pointers
    Datasetvol	*vol;
    Datasetreg2	*reg2;
    Datasetreg3	*reg3;

    ConDataset	*dataset;


    type    = Data::DataType(datatype);	// miscellaneous initializations
    dataset = new ConDataset;


    switch (meshtype)			// create big objects: data and `plot'
      {
      case CONTOUR_2D :
	 dataset->data = slc = new Datasetslc(type, nvars, ntime, files);
         // create the plot
         dataset->plot = new Conplot2d(slc);
         break;
      case CONTOUR_3D :
         dataset->data = vol = new Datasetvol(type, nvars, ntime, files);
         // create the plot
         dataset->plot = new Conplot3d(vol);
         break;
      case CONTOUR_REG_2D :
         dataset->data = reg2 = new Datasetreg2(type, nvars, ntime, files);
         // create the plot
         dataset->plot = new Conplotreg2(reg2);
         break;
      case CONTOUR_REG_3D :
         dataset->data = reg3 = new Datasetreg3(type, nvars, ntime, files);
         // create the plot
	 dataset->plot = new Conplotreg3(reg3);
         break;
      default :
#ifdef VERBOSE
	 fprintf(stderr, "libcontour:loadDataset: incorrect mesh type\n");
#endif
	 return NULL;
      };
					// allocate and init signature data

    dataset->sfun = new (Signature**[dataset->data->nData()]);
    for (var = 0; var < dataset->data->nData(); var++)
	{
	dataset->sfun[var] = new (Signature*[dataset->data->nTime()]);
	for (t = 0; t < dataset->data->nTime(); t++)
	    dataset->sfun[var][t] = NULL;
	}

					// check for errors, return
    if (!dataset->plot)
	{
#ifdef VERBOSE
	fprintf(stderr, "libcontour:loadDataset: couldn't create plot\n");
#endif
	return NULL;
	}

    if (!dataset->data)
	{
#ifdef VERBOSE
	fprintf(stderr, "libcontour:loadDataset: couldn't create data\n");
#endif
	return NULL;
	}

#ifdef VERBOSE
    printf("libcontour:loadDataset: Data set loaded\n");

#endif
    return dataset;
}

//------------------------------------------------------------------------
//
// getSeedCells() - get seed cell data
//
//------------------------------------------------------------------------

SeedData	*getSeedCells(ConDataset *dataset, int variable, int timestep)
{
    SeedData	*seeddata;		// pointer to seed data structure

						// sanity checks

    if (!dataset || !dataset->data || !dataset->plot)
	{
	fprintf(stderr, "libcontour:getSeedCells: Couldn't find dataset\n");
	return NULL;
	}
    if (variable < 0 || variable >= dataset->data->nData())
	{
	fprintf(stderr, "libcontour:getSeedCells: variable out of range\n");
	return NULL;
	}
    if (timestep < 0 || timestep >= dataset->data->nTime())
	{
	fprintf(stderr, "libcontour:getSeedCells: timestep out of range\n");
	return NULL;
	}

						// extract seeds
    seeddata = new SeedData;

    dataset->data->getData(timestep)->setContourFun(variable);
    //plot[id]->setContourFun(variable, timestep);
    //Conplot::setContourFun(variable);
    dataset->plot->setTime(timestep);

						// determine if seeds computed
    if (dataset->plot->getSeeds()->getNCells() == 0)
	dataset->plot->Preprocess(timestep, setPreprocessing, NULL);

    seeddata->nseeds = dataset->plot->getSeeds()->getNCells();
    seeddata->seeds  = (Seed *)dataset->plot->getSeeds()->getCellPointer();

#ifdef VERBOSE
    //    for (int i = 0; i < seeddata->nseeds; i++)
    //	printf("seed cell %d --> min = %f max = %f  id = %d\n",
    //	i, seeds[i].min, seeds[i].max, seeds[i].cell_id);
#endif /* of VERBOSE */

#ifdef VERBOSE
    printf("libcontour:getSeedCells: seed data extracted\n");
#endif

    return seeddata;
}

//------------------------------------------------------------------------
//
// getNumberOfSignatures() - get number of signature functions
//
//------------------------------------------------------------------------

int	getNumberOfSignatures(ConDataset *dataset)
{
    if (!dataset)
	{
	fprintf(stderr, "libcontour:getNumberOfSignatures: invalid dataset\n");
	return NULL;
	}

    return dataset->data->getData(0)->getNFunctions();
}

//------------------------------------------------------------------------
//
// getSignatureFunctions() - get signature functions
//
//------------------------------------------------------------------------

Signature	*getSignatureFunctions(ConDataset *dataset, int variable,
				       int timestep)
{
    int			t;		// a timestep index variable
    int			fun;		// signature function index

						// sanity checks

    if (!dataset || !dataset->data || !dataset->plot)
	{
	fprintf(stderr, "libcontour:getSignatureFunctions: Couldn't find dataset\n");
	return NULL;
	}
    if (variable < 0 || variable >= dataset->data->nData())
	{
	fprintf(stderr, "libcontour:getSignatureFunctions: variable out of range\n");
	return NULL;
	}
    if (timestep < 0 || timestep >= dataset->data->nTime())
	{
	fprintf(stderr, "libcontour:getSignatureFunctions: timestep out of range\n");
	return NULL;
	}

    						// obtain signature functions

    dataset->data->getData(timestep)->setContourFun(variable);
    //plot[id]->setContourFun(variable, timestep);
    //Conplot::setContourFun(variable);
    dataset->plot->setTime(timestep);
					    // compute signature functions

    printf("Server: computing signature functions ...\n");

    dataset->nsfun = dataset->data->getData(0)->getNFunctions();

#ifdef COMPUTE_FUNCTIONS_FOR_ALL_TIMES
    for (t = 0; t < dataset->data->nTime(); t++)	// per time step t
	{
	dataset->sfun[variable][t] = new Signature[dataset->nsfun];
	for (fun = 0; fun < dataset->nsfun; fun++)
	    {
	    dataset->sfun[variable][t][fun].name = 
			    strdup(dataset->data->getData(0)->fName(fun));
	    dataset->sfun[variable][t][fun].fy   = 
			    dataset->data->getData(t)->compFunction(fun, 
			    dataset->sfun[variable][t][fun].nval, 
			    &dataset->sfun[variable][t][fun].fx);
	    }
	}
#endif /* of COMPUTE_FUNCTIONS_FOR_ALL_TIMES */

    if (!dataset->sfun[variable][timestep])	// have signatures already?
	{
	t = timestep;
	dataset->sfun[variable][t] = new Signature[dataset->nsfun];
	for (fun = 0; fun < dataset->nsfun; fun++)
	    {
	    dataset->sfun[variable][t][fun].name = 
				strdup(dataset->data->getData(0)->fName(fun));
	    dataset->sfun[variable][t][fun].fy   = 
				dataset->data->getData(t)->compFunction(fun, 
				dataset->sfun[variable][t][fun].nval,
				&dataset->sfun[variable][t][fun].fx);
	    }
	}

    //sfundata.minvalue = data[id]->getMinFun(variable);
    //sfundata.maxvalue = data[id]->getMaxFun(variable);

    printf("Server::getSignatureData: signature data sent\n");

    return dataset->sfun[variable][timestep];
}

//------------------------------------------------------------------------
//
// getSignatureValues() - get signature values for isovalue
//
//------------------------------------------------------------------------

float	*getSignatureValues(ConDataset *dataset, int variable, int timestep,
			    float isovalue)
{
    int			t;		// a timestep index variable
    int			fun;		// signature function index
    float		*svalues;	// signature values

					    	// sanity checks

    if (!dataset || !dataset->data || !dataset->plot)
	{
	fprintf(stderr, "libcontour:getSignatureValues: Couldn't find dataset\n");
	return NULL;
	}
    if (variable < 0 || variable >= dataset->data->nData())
	{
	fprintf(stderr, "libcontour:getSignatureValues: variable out of range\n");
	return NULL;
	}
    if (timestep < 0 || timestep >= dataset->data->nTime())
	{
	fprintf(stderr, "libcontour:getSignatureValues: timestep out of range\n");
	return NULL;
	}

    						// obtain signature values
	
    dataset->data->getData(timestep)->setContourFun(variable);
    //plot[id]->setContourFun(variable, timestep);
    //Conplot::setContourFun(variable);
    dataset->plot->setTime(timestep);

    dataset->nsfun = dataset->data->getData(0)->getNFunctions();

					// do we have signatures for timestep?

    if (!dataset->sfun[variable][timestep])
	{
	t = timestep;
	dataset->sfun[variable][t] = new Signature[dataset->nsfun];
	for (fun = 0; fun < dataset->nsfun; fun++)
	    {
	    dataset->sfun[variable][t][fun].name = 
			    strdup(dataset->data->getData(0)->fName(fun));
	    dataset->sfun[variable][t][fun].fy   = 
			    dataset->data->getData(t)->compFunction(fun, 
			    dataset->sfun[variable][t][fun].nval, 
			    &dataset->sfun[variable][t][fun].fx);
	    }
	}

    svalues = new float[dataset->nsfun];

    for (fun = 0; fun < dataset->nsfun; fun++)
	{
	int	l, r, m;		// binary search from SIoXtSpectrum.h
	l = 0;
	r = dataset->sfun[variable][timestep][fun].nval;
	while (l < r)
	    {
	    m = (l + r) >> 1;
	    if (isovalue < dataset->sfun[variable][timestep][fun].fx[m])
		r = m - 1;
	    else
		l = m + 1;
	    }
	svalues[fun] = dataset->sfun[variable][timestep][fun].fy[m];
#ifdef VERBOSE
	/*	printf("function %d %s\t --> %d values: (55, %f)\n", fun,
	       sfun[id][variable][timestep][vfun].name,
	       sfun[id][variable][timestep][vfun].nval, 
	       sfun[id][variable][timestep][vfun].fy[55]); */
#endif /* of VERBOSE */
	}

    printf("libcontour:getSignatureValues: signature values computed\n");

    return svalues;
}

//------------------------------------------------------------------------
//
// getSlice() - extract a 2d slice from a 3d regular data grid
//
//------------------------------------------------------------------------

SliceData	*getSlice(ConDataset *dataset, int variable, int timestep,
			  char axis, int index)
{
    u_int		dim[3];		// dataset dimensions
    SliceData		*slice;		// slice data
    Data::datatypes	buffer;		// buffer to hold actual slice

						// sanity checks

    if (!dataset || !dataset->data || !dataset->plot)
	{
	fprintf(stderr, "libcontour:getSlice: Couldn't find dataset\n");
	return NULL;
	}
    if (dataset->data->meshType() != CONTOUR_REG_3D)
	{
	fprintf(stderr, "libcontour:getSlice: invalid mesh type\n");
	return NULL;
	}
    if (variable < 0 || variable >= dataset->data->nData())
	{
	fprintf(stderr, "libcontour:getSlice: variable out of range\n");
	return NULL;
	}
    if (timestep < 0 || timestep >= dataset->data->nTime())
	{
	fprintf(stderr, "libcontour:getSlice: timestep out of range\n");
	return NULL;
	}
    if (axis != 'x' && axis != 'y' && axis != 'z')
	{
	fprintf(stderr, "libcontour:getSlice: invalid slice axis\n");
	return NULL;
	}

    slice = new SliceData;
    ((Datareg3 *)dataset->data->getData(0))->getDim(dim);

						// check index range and
    switch (axis)				// determine width and height
	{
	case 'x' :  if (index < 0 || index >= dim[0]) {
			fprintf(stderr, "libcontour:getSlice: x-index out of range\n");
			return NULL;
		    } else {
			slice->width  = dim[1];
			slice->height = dim[2]; }
		    break;

	case 'y' :  if (index < 0 || index >= dim[1]) {
			fprintf(stderr, "libcontour:getSlice: y-index out of range\n");
			return NULL;
		    } else {
			slice->width  = dim[2];
			slice->height = dim[0]; }
		    break;

	case 'z' :  if (index < 0 || index >= dim[2]) {
			fprintf(stderr, "libcontour:getSlice: z-index out of range\n");
			return NULL;
		    } else {
			slice->width  = dim[0];
			slice->height = dim[1]; }
		    break;
	}

    dataset->data->getData(timestep)->setContourFun(variable);
    //plot[id]->setContourFun(variable, timestep);
    //Conplot::setContourFun(variable);
    dataset->plot->setTime(timestep);

    slice->datatype = int(dataset->data->dataType());

						// allocate memory for slice
    switch (slice->datatype)
	{
	case CONTOUR_UCHAR :	
		buffer.ucdata = new u_char[slice->width * slice->height];
		break;
	case CONTOUR_USHORT :
		buffer.usdata = new u_short[slice->width * slice->height];
		break;
	case CONTOUR_FLOAT :
		buffer.fdata = new float[slice->width * slice->height];
		break;
	case CONTOUR_DOUBLE :
		buffer.ddata = new double[slice->width * slice->height];
		break;
	}
					    // extract slice from dataset

    if (((Datareg3 *)dataset->data->getData(timestep))->getSlice(
				    variable, axis, index, &buffer))
	{
	fprintf(stderr, "Datareg3::getSlice(): Couldn't extract slice\n");
	return NULL;
	}

    //
    // void		getSlice(int variable, char axis, int index, 
    //			 Data::datatypes *buffer)

    printf("libcontour::extractSlice: slice %d along axis %c \n", index, axis);

    switch (slice->datatype)		    // assign buffer to slice data
	{
	case CONTOUR_UCHAR :	slice->ucdata = buffer.ucdata;
	    			break;
	case CONTOUR_USHORT :	slice->usdata = buffer.usdata;
	    			break;
	case CONTOUR_FLOAT :	slice->fdata = buffer.fdata;
	    			break;
    case CONTOUR_DOUBLE :	slice->ddata = buffer.ddata;
	    			break;

	}

    printf("Server::extractSlice: sent new slice to client\n");

    return slice;
}

//------------------------------------------------------------------------
//
// getContour2d() - extract a 2d isocontour from a 2d data set
//
//------------------------------------------------------------------------

Contour2dData	*getContour2d(ConDataset *dataset, int variable, int timestep,
			      float isovalue)
{
    Contour2d		*isocontour;	// new isocontour
    Contour2dData	*contour2d;	// 2d isocontour data structure

						// sanity checks

    if (!dataset || !dataset->data || !dataset->plot)
	{
	fprintf(stderr, "libcontour:getContour2d: Couldn't find dataset\n");
	return NULL;
	}
    if (dataset->data->meshType() != CONTOUR_2D &&
    	dataset->data->meshType() != CONTOUR_REG_2D)
	{
	fprintf(stderr, "libcontour:getContour2d: invalid mesh type\n");
	return NULL;
	}
    if (variable < 0 || variable >= dataset->data->nData())
	{
	fprintf(stderr, "libcontour:getContour2d: variable out of range\n");
	return NULL;
	}
    if (timestep < 0 || timestep >= dataset->data->nTime())
	{
	fprintf(stderr, "libcontour:getContour2d: timestep out of range\n");
	return NULL;
	}

    dataset->data->getData(timestep)->setContourFun(variable);
    //plot[id]->setContourFun(variable, timestep);
    //Conplot::setContourFun(variable); 	--> should depend on dataset
    dataset->plot->setTime(timestep);

    contour2d = new Contour2dData;
    printf("libcontou:getContour2d: isovalue = %f\n", isovalue);

						// determine if seeds computed
    if (dataset->plot->getSeeds()->getNCells() == 0)
	dataset->plot->Preprocess(timestep, setPreprocessing, NULL);

						// extract isocontour
    dataset->plot->ResetAll();
    dataset->plot->Extract(isovalue);
    isocontour = dataset->plot->getContour2d();

    contour2d->nvert = isocontour->getNVert();
    contour2d->nedge = isocontour->getNEdge();
    contour2d->vert  = isocontour->vert;
    contour2d->edge  = isocontour->edge;

    printf("libcontour:getContour2d: nr of vertices: %d\n", contour2d->nvert);
    printf("libcontour:getContour2d: nr of edges: %d\n", contour2d->nedge);

    return contour2d;
}

//------------------------------------------------------------------------
//
// getContour3d() - extract a 3d isocontour from a 3d data set
//
//------------------------------------------------------------------------

Contour3dData	*getContour3d(ConDataset *dataset, int variable, int timestep,
                              float isovalue, int colorvar)
{
    Contour3d		*isocontour;	// new isocontour
    Contour3dData	*contour3d;	// 3d isocontour data structure

					    	// sanity checks

    if (!dataset || !dataset->data || !dataset->plot)
	{
	fprintf(stderr, "libcontour:getContour3d: Couldn't find dataset\n");
	return NULL;
	}
    if (dataset->data->meshType() != CONTOUR_3D &&
    	dataset->data->meshType() != CONTOUR_REG_3D)
	{
	fprintf(stderr, "libcontour:getContour3d: invalid mesh type\n");
	return NULL;
	}
    if (variable < 0 || variable >= dataset->data->nData())
	{
	fprintf(stderr, "libcontour:getContour3d: variable out of range\n");
	return NULL;
	}
    if (colorvar != NO_COLOR_VARIABLE)
	if (colorvar < 0 || colorvar >= dataset->data->nData())
	{
	fprintf(stderr, "libcontour:getContour3d: invalid color variable\n");
	return NULL;
	}
    if (timestep < 0 || timestep >= dataset->data->nTime())
	{
	fprintf(stderr, "libcontour:getContour3d: timestep out of range\n");
	return NULL;
	}

    dataset->data->getData(timestep)->setContourFun(variable);
    //plot[id]->setContourFun(variable, timestep);
    //Conplot::setContourFun(variable); 	--> should depend on dataset
    dataset->data->getData(timestep)->setColorFun(colorvar);
    //plot[id]->setColorFun(colorvar, timestep);
    //Conplot::setColorFun(colorvar); 	--> should depend on dataset
    dataset->plot->setTime(timestep);

    contour3d = new Contour3dData;
#ifdef VERBOSE
    printf("licontour::getContour3d: isovalue = %f\n", isovalue);
#endif

    // determine if seeds computed
    if (dataset->plot->getSeeds()->getNCells() == 0)
      dataset->plot->Preprocess(timestep, setPreprocessing, NULL);
    
    // extract isocontour
    dataset->plot->ResetAll();
    dataset->plot->Extract(isovalue);
    isocontour = dataset->plot->getContour3d();

    contour3d->nvert    = isocontour->getNVert();
    contour3d->ntri     = isocontour->getNTri();

    contour3d->vert     = isocontour->vert;
    contour3d->vnorm    = isocontour->vnorm;
    contour3d->vfun     = isocontour->vfun;
    contour3d->tri      = isocontour->tri;

    contour3d->colorvar = colorvar;
    contour3d->fmin     = isocontour->fmin;
    contour3d->fmax     = isocontour->fmax;

    
    return contour3d;
}

//------------------------------------------------------------------------
//
// clearDataset() - clear (remove) dataset from memory
//
//------------------------------------------------------------------------

void	clearDataset(ConDataset *dataset)
{
    int		t;			// timestep index variable
    int		v;			// variable index variable


    if (dataset->data && dataset->plot)		// sanity check
	{
	for (v = 0; v < dataset->data->nData(); v++)	// delete signatures
	    {
	    for (t = 0; t < dataset->data->nTime(); t++)
		if (dataset->sfun[v][t])
		    delete [] dataset->sfun[v][t];
	    delete [] dataset->sfun[v];
	    }
	delete [] dataset->sfun;

	delete dataset->data;			// delete data, set to NULL
	delete dataset->plot;
	}
}
