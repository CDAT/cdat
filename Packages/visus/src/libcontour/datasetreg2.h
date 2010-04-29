//--------------------------------------------------------------------
//
// Datasetreg2 - representation for a 2D time-varying regular grid
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999 
//
//--------------------------------------------------------------------

// $Id: datasetreg2.h,v 1.1.2.1 2007/05/18 00:57:49 dlaney Exp $

#ifndef DATASET_REG2_H
#define DATASET_REG2_H

#include "dataset.h"
#include "datareg2.h"

//--------------------------------------------------------------------
//
// Datasetreg2 - a scalar time-varying dataset
//
//--------------------------------------------------------------------
class Datasetreg2 : public Dataset {
   public:
      // constructors and destructors

      Datasetreg2(Data::DataType t, int ndata, int ntime, char *files[]);
      Datasetreg2(Data::DataType t, int ndata, int ntime, int *dim, 
		  u_char *data);
      ~Datasetreg2() {}

      // member access methods

      float getMin(int t) const { return(reg2[t]->getMin()); }
      float getMax(int t) const { return(reg2[t]->getMax()); }
      float getMin()      const { return(min); }
      float getMax()      const { return(max); }

      // add by fan
      float getMinFun(int j)      const { return(min); }
      float getMaxFun(int j)      const { return(max); }


      Data	*getData(int i) { return(reg2[i]); }
      Datareg2	*getMesh(int i) { return(reg2[i]); }

   private:

      Datareg2	**reg2;
      float	min, max;
};

//------------------------------------------------------------------------
//
// Datasetreg2() - usual constructor, reads data from one or more files
//
//------------------------------------------------------------------------

inline Datasetreg2::Datasetreg2(Data::DataType t, int nd, int nt, char *fn[])
		         : Dataset(t, nd, nt, fn)
{
   int i;

   meshtype = 4;
   reg2 = (Datareg2 **)malloc(sizeof(Datareg2 *)*nt);
   min = 1e10;
   max = -1e10;
   ncells = 0;
   maxcellindex = 0;
   for (i=0; i<nt; i++) {
printf("loading file: %s\n", fn[i]);
      reg2[i] = new Datareg2(t, nd, fn[i]);
      if (reg2[i]->getMin() < min)
         min = reg2[i]->getMin();
      if (reg2[i]->getMax() > max)
         max = reg2[i]->getMax();
      if (reg2[i]->getNCells() > ncells)
         ncells = reg2[i]->getNCells();
      if (reg2[i]->maxCellIndex() > maxcellindex)
         maxcellindex = reg2[i]->maxCellIndex();
   }
}

//------------------------------------------------------------------------
//
// Datasetreg2() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

inline Datasetreg2::Datasetreg2(Data::DataType t, int ndata, int ntime,
		    		int *dim, u_char *data)
				: Dataset(t, ndata, ntime, data)
{
    int	i;				// timestep index variable
    int	size;				// size of single timestep of data

    meshtype = 4;
    reg2 = (Datareg2 **)malloc(sizeof(Datareg2 *)*ntime);
    min = 1e10;
    max = -1e10;
    ncells = 0;
    maxcellindex = 0;

    switch (t)
	{
	case Data::UCHAR :  size = dim[0] * dim[1] * ndata * sizeof(u_char);
			    break;
	case Data::USHORT : size = dim[0] * dim[1] * ndata * sizeof(u_short);
			    break;
	case Data::FLOAT :  size = dim[0] * dim[1] * ndata * sizeof(float);
			    break;
	case Data::DOUBLE:  size = dim[0] * dim[1] * ndata * sizeof(double);
			    break;

	}

   for (i=0; i<ntime; i++)
	{
	reg2[i] = new Datareg2(t, ndata, dim, data + i*size);
	if (reg2[i]->getMin() < min)
	    min = reg2[i]->getMin();
	if (reg2[i]->getMax() > max)
	    max = reg2[i]->getMax();
	if (reg2[i]->getNCells() > ncells)
	    ncells = reg2[i]->getNCells();
	if (reg2[i]->maxCellIndex() > maxcellindex)
	    maxcellindex = reg2[i]->maxCellIndex();
      }
}

#endif
