//--------------------------------------------------------------------
//
// Datasetvol - representation for a time-varying volume
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999
//
//--------------------------------------------------------------------

// $Id: datasetvol.h,v 1.1.2.1 2007/05/18 00:57:50 dlaney Exp $

#ifndef DATASET_VOL_H
#define DATASET_VOL_H

#include "data.h"
#include "dataset.h"
#include "datavol.h"

//--------------------------------------------------------------------
//
// Datasetvol - a scalar time-varying dataset
//
//--------------------------------------------------------------------

class Datasetvol : public Dataset {
   public:
      // constructors and destructors

      Datasetvol(Data::DataType t, int ndata, int ntime, char *files[]);
      Datasetvol(Data::DataType t, int ndata, int ntime, int nverts, int ncells,
	         double *verts, u_int *cells, int *celladj, u_char *data);
      ~Datasetvol() {}

      // member access methods

      float getMin(int t) const { return(vol[t]->getMin()); }
      float getMax(int t) const { return(vol[t]->getMax()); }
      float getMin()      const { return(min); }
      float getMax()      const { return(max); }

      // add by fan
      float getMinFun(int j)      const { return(min); }
      float getMaxFun(int j)      const { return(max); }


      Data *getData(int i) { return(vol[i]); }
      Datavol *getVol(int i) { return(vol[i]); }

   private:

      Datavol	**vol;
      float	min, max;
};

//------------------------------------------------------------------------
//
// Datasetvol() - usual constructor, reads data from one or more files
//
//------------------------------------------------------------------------

inline Datasetvol::Datasetvol(Data::DataType t, int nd, int nt, char *fn[])
			      : Dataset(t, nd, nt, fn)
{
   int i;

   meshtype = 3;
   vol = (Datavol **)malloc(sizeof(Datavol *)*nt);
   min = 1e10;
   max = -1e10;
   ncells = 0;
   for (i=0; i<nt; i++) {
      vol[i] = new Datavol(t, nd, fn[i]);
      if (vol[i]->getMin() < min)
         min = vol[i]->getMin();
      if (vol[i]->getMax() > max)
         max = vol[i]->getMax();
      if (vol[i]->getNCells() > ncells)
         ncells = vol[i]->getNCells();
   }
   maxcellindex=ncells;
}

//------------------------------------------------------------------------
//
// Datasetvol() - called by the constructors to initialize the data
//
//------------------------------------------------------------------------

inline Datasetvol::Datasetvol(Data::DataType t, int ndata, int ntime,
			      int nverts, int ncells, double *verts,
			      u_int *cells, int *celladj, u_char *data)
			      : Dataset(t, ndata, ntime, data)
{
    int		i;			// timestep index variable
    int		size;			// size of single timestep of data

    meshtype = 3;
    vol = (Datavol **)malloc(sizeof(Datavol *)*ntime);
    min = 1e10;
    max = -1e10;
    // ncells = 0;	this was here to allow different ncells for different
    //			times, for now we don't allow it with this constructor
    Datasetvol::ncells = ncells;

    switch (t)
	{
	case Data::UCHAR :	size = nverts * ndata * sizeof(u_char);
				break;
	case Data::USHORT :	size = nverts * ndata * sizeof(u_short);
				break;
	case Data::FLOAT :	size = nverts * ndata * sizeof(float);
				break;
	case Data::DOUBLE :	size = nverts * ndata * sizeof(double);
				break;
	}

    for (i=0; i<ntime; i++)
	{
	vol[i] = new Datavol(t, ndata, nverts, ncells, verts, cells, celladj,
							      data + i*size);
	if (vol[i]->getMin() < min)
            min = vol[i]->getMin();
	if (vol[i]->getMax() > max)
            max = vol[i]->getMax();
	if (vol[i]->getNCells() > ncells)
            ncells = vol[i]->getNCells();
      }

   maxcellindex=ncells;
}

#endif

