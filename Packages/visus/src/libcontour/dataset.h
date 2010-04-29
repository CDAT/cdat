//--------------------------------------------------------------------
//
// Dataset - representation for a time-varying dataset
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999
//
//--------------------------------------------------------------------

// $Id: dataset.h,v 1.1 2003/09/02 17:27:16 scorzell Exp $

#ifndef DATASET_H
#define DATASET_H

#include "data.h"

//--------------------------------------------------------------------
//
// Dataset - a scalar time-varying dataset
//
//--------------------------------------------------------------------
class Dataset {
   public:
      // constructors and destructors

      Dataset(Data::DataType t, int ndata, int ntime, char *files[]);
      Dataset(Data::DataType t, int ndata, int ntime, u_char *data);
      ~Dataset() {}

      // member access methods

      Data::DataType	dataType(void) const	{ return(type); }
      int		meshType(void) const	{ return(meshtype); }
      int		nTime(void) const	{ return(ntime); }
      int		nData(void) const	{ return(ndata); }
      char		**fileNames(void) const	{ return(filenames); }

      virtual float getMin(int t) const = 0;
      virtual float getMax(int t) const = 0;
      virtual float getMin() const = 0;
      virtual float getMax() const = 0;

      // add by fan
      virtual float getMinFun(int f) const = 0;
      virtual float getMaxFun(int f) const = 0;
      // end fan

      virtual Data *getData(int i) = 0;

      int           getNCells(void) { return(ncells); }
      int           maxCellIndex(void) { return(maxcellindex); }

   protected:

      int ncells;     			// number of cells
      int meshtype;			// 2d unstr, reg2, 3d unstr, reg3
      int maxcellindex;			// maximum number of cells

   private:

      Data::DataType	type;		// data type: uchar, ushort, float
      int		ntime;		// number of timesteps
      int		ndata;		// add by fan
      char		**filenames;	// data filenames
//    float		min, max;	// min/max of data over time
};

//------------------------------------------------------------------------
//
// Dataset() - the usual constructor, initializes some data
//
//------------------------------------------------------------------------

inline Dataset::Dataset(Data::DataType t, int nd, int nt, char *fn[])
{
    type      = t;
    ntime     = nt;	// number of time step
    ndata     = nd;	// number of data: add by fan
    filenames = fn;
}

//------------------------------------------------------------------------
//
// Dataset() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

inline Dataset::Dataset(Data::DataType t, int nd, int nt, u_char *data)
{
    type      = t;
    ntime     = nt;
    ndata     = nd;
    filenames = NULL;
}

#endif
