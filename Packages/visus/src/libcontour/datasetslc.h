//--------------------------------------------------------------------
//
// Datasetslc - representation for a time-varying volume
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999
//
//--------------------------------------------------------------------

// $Id: datasetslc.h,v 1.1.2.1 2007/05/18 00:57:50 dlaney Exp $

#ifndef DATASET_SLC_H
#define DATASET_SLC_H

#include "dataset.h"
#include "dataslc.h"

//--------------------------------------------------------------------
//
// Datasetslc - a scalar time-varying dataset
//
//--------------------------------------------------------------------
class Datasetslc : public Dataset {
   public:
      // constructors and destructors

      Datasetslc(Data::DataType t, int ndata, int ntime, char *files[]);
      Datasetslc(Data::DataType t, int ndata, int ntime, int nverts, int ncells,
	         double *verts, u_int *cells, int *celladj, u_char *data);
      ~Datasetslc() {}

      // member access methods

      float getMin(int t) const { return(slc[t]->getMin()); }
      float getMax(int t) const { return(slc[t]->getMax()); }
      float getMin()      const { return(min); }
      float getMax()      const { return(max); }

      // add by fan
      float getMinFun(int j)      const { return(min); }
      float getMaxFun(int j)      const { return(max); }


      Data *getData(int i) { return(slc[i]); }
      Dataslc *getSlc(int i) { return(slc[i]); }

   private:

      Dataslc	**slc;
      float	min, max;
};

//------------------------------------------------------------------------
//
// Datasetslc() - usual constructor, reads data from one or more files
//
//------------------------------------------------------------------------

inline Datasetslc::Datasetslc(Data::DataType t, int nd, int nt, char *fn[])
		       : Dataset(t, nd, nt, fn)
{
   int		i;
   
   meshtype = 2;
   slc = (Dataslc **)malloc(sizeof(Dataslc *)*nt);
   min = 1e10;
   max = -1e10;
   ncells = 0;
   for (i=0; i<nt; i++) {
printf("loading file: %s\n", fn[i]);
      slc[i] = new Dataslc(t, nd, fn[i]);
      if (slc[i]->getMin() < min)
         min = slc[i]->getMin();
      if (slc[i]->getMax() > max)
         max = slc[i]->getMax();
      if (slc[i]->getNCells() > ncells)
         ncells = slc[i]->getNCells();
printf("step %d: min : %f max : %f\n", i, min, max);
printf("step %d: tmin : %f tmax : %f\n", i, slc[i]->getMin(), slc[i]->getMax());
   }
   maxcellindex=ncells;
printf("min : %f max : %f\n", min, max);
}

//------------------------------------------------------------------------
//
// Datasetslc() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

inline Datasetslc::Datasetslc(Data::DataType t, int ndata, int ntime, 
			      int nverts, int ncells, double *verts,
			      u_int *cells, int *celladj, u_char *data)
			      : Dataset(t, ndata, ntime, data)
{
   int	i;				// timestep index variable
   int	size;				// size of single timestep of data
   
    meshtype = 2;
    slc = (Dataslc **)malloc(sizeof(Dataslc *)*ntime);
    min = 1e10;
    max = -1e10;
    //   ncells = 0;	this was here to allow different ncells for different
    //			times, for now we don't allow it with this constructor
    Datasetslc::ncells = ncells;

    switch (t)
	{
	case Data::UCHAR : 	size = nverts * ndata * sizeof(u_char);
				break;
	case Data::USHORT :	size = nverts * ndata * sizeof(u_short);
				break;
	case Data::FLOAT :	size = nverts * ndata * sizeof(float);
				break;
	case Data::DOUBLE:	size = nverts * ndata * sizeof(double);
				break;

	}

    for (i = 0; i < ntime; i++)
	{
	slc[i] = new Dataslc(t, ndata, nverts, ncells, verts, cells, celladj,
							      data + i*size);
	if (slc[i]->getMin() < min)
            min = slc[i]->getMin();
	if (slc[i]->getMax() > max)
            max = slc[i]->getMax();
	if (slc[i]->getNCells() > ncells)
            ncells = slc[i]->getNCells();
printf("step %d: min : %f max : %f\n", i, min, max);
printf("step %d: tmin : %f tmax : %f\n", i, slc[i]->getMin(), slc[i]->getMax());
      }

   maxcellindex=ncells;

printf("min : %f max : %f\n", min, max);
}

#endif
