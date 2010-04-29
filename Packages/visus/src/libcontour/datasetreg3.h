//--------------------------------------------------------------------
//
// Datasetreg3 - representation for a 3D time-varying regular grid
//
// Updated by Emilio Camahort, 1999
//
//--------------------------------------------------------------------

// $Id: datasetreg3.h,v 1.1.2.1 2008/09/30 19:09:54 cottom1 Exp $

#ifndef DATASET_REG3_H
#define DATASET_REG3_H

#include "dataset.h"
#include "datareg3.h"

//--------------------------------------------------------------------
//
// Datasetreg3 - a 3D scalar time-varying regular grid of data
//
//--------------------------------------------------------------------
class Datasetreg3 : public Dataset {
   public:
      // constructors and destructors

      Datasetreg3(Data::DataType t, int ndata, int ntime, char *files[]);
      Datasetreg3(Data::DataType t, int ndata, int ntime, int *dim, 
		  u_char *data);
      ~Datasetreg3() {}

      // member access methods

      int getNData(void) { return(reg3[0]->getNData()); }

      //  min, max for "0" variable at time step "t"
      float getMin(int t) const { return(reg3[t]->getMin()); }
      float getMax(int t) const { return(reg3[t]->getMax()); }

      // min, max for "0" variable, over all time
      float getMin()      const { return(min[0]); }
      float getMax()      const { return(max[0]); }

      // min, max for "j" variable, over all time
      float getMinFun(int j)      const { return(min[j]); }
      float getMaxFun(int j)      const { return(max[j]); }

      // "i" time step 
      Data	*getData(int i) { return(reg3[i]); } 
      Datareg3	*getMesh(int i) { return(reg3[i]); }

   private:

      float	*min, *max;	// min, max for each variable over all times
      Datareg3	**reg3;
};

//------------------------------------------------------------------------
//
// Datasetreg3() - usual constructor, reads data from one or more files
//
//------------------------------------------------------------------------

inline Datasetreg3::Datasetreg3(Data::DataType t, int nd, int nt, char *fn[])
			 : Dataset(t, nd, nt, fn)
{
    int i, j;

    meshtype = 5;

   // may be bug here: Fan modify
   //   min = (float *)malloc(sizeof(float)*nt);
   //   max = (float *)malloc(sizeof(float)*nt);

    min = (float *)malloc(sizeof(float)*nd);
    max = (float *)malloc(sizeof(float)*nd);
    for (i=0; i<nd; i++)
	{
	min[i] = 1e10;
	max[i] = -1e10;
	}
   // end fan

    reg3 = (Datareg3 **)malloc(sizeof(Datareg3 *)*nt);
    ncells = 0;
    maxcellindex=0;

    for (i = 0; i < nt; i++)				// timestep loop
	{
	// comment by fan
	// min[i] = 1e10;
	// max[i] = -1e10;

#ifdef VERBOSE
printf("loading file: %s\n", fn[i]);
#endif
	reg3[i] = new Datareg3(t, nd, fn[i]);

	for (j=0; j<nd; j++)				// per variable loop
	    {
	     if (reg3[i]->getMin(j) < min[j])
	       min[j] = reg3[i]->getMin(j);
	     if (reg3[i]->getMax(j) > max[j])
	       max[j] = reg3[i]->getMax(j);  // modify fan
	       // May be bug fan
	       // max[j] = reg3[i]->getMax();
	    }

	if (reg3[i]->getNCells() > ncells)
	    ncells = reg3[i]->getNCells();

	if (reg3[i]->maxCellIndex() > maxcellindex)
	    maxcellindex = reg3[i]->maxCellIndex();
	}

#ifdef VERBOSE
    for (i = 0; i < nd; i++)
	printf("variable[%d]: min=%f, max=%f\n",i, min[i],max[i]);
#endif
}

//------------------------------------------------------------------------
//
// Datasetreg3() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

inline Datasetreg3::Datasetreg3(Data::DataType t, int ndata, int ntime,
				int *dim, u_char *data)
				: Dataset(t, ndata, ntime, data)
{
    int i, j;				// timestep and variable indices
    int	size;				// size of single timestep of data

    meshtype = 5;

    // may be bug here: Fan modify
    //   min = (float *)malloc(sizeof(float)*ntime);
    //   max = (float *)malloc(sizeof(float)*ntime);

    min = (float *)malloc(sizeof(float)*ndata);
    max = (float *)malloc(sizeof(float)*ndata);
    for (i=0; i<ndata; i++)
	{
	min[i] = 1e10;
	max[i] = -1e10;
	}
    // end fan

    reg3 = (Datareg3 **)malloc(sizeof(Datareg3 *)*ntime);
    ncells = 0;
    maxcellindex=0;

    switch (t)
	{
	case Data::UCHAR : 
		size = dim[0]*dim[1]*dim[2] * ndata * sizeof(u_char); 
		break;
	case Data::USHORT :	
		size = dim[0]*dim[1]*dim[2] * ndata * sizeof(u_short); 
		break;
	case Data::FLOAT :	
		size = dim[0]*dim[1]*dim[2] * ndata * sizeof(float); 
		break;
	case Data::DOUBLE :	
		size = dim[0]*dim[1]*dim[2] * ndata * sizeof(double); 
		break;
	}

    for (i = 0; i < ntime; i++)				// timestep loop
	{
	// comment by fan
	// min[i] = 1e10;
	// max[i] = -1e10;

	reg3[i] = new Datareg3(t, ndata, dim, data + i*size);

	for (j=0; j<ndata; j++)				// per variable loop
	    {
	     if (reg3[i]->getMin(j) < min[j])
	       min[j] = reg3[i]->getMin(j);
	     if (reg3[i]->getMax(j) > max[j])
	       max[j] = reg3[i]->getMax(j);  // modify fan
	       // May be bug fan
	       // max[j] = reg3[i]->getMax();
	    }

	if (reg3[i]->getNCells() > ncells)
	    ncells = reg3[i]->getNCells();

	if (reg3[i]->maxCellIndex() > maxcellindex)
	    maxcellindex = reg3[i]->maxCellIndex();
	}

#ifdef VERBOSE
    for (i = 0; i < ndata; i++)
	printf("variable[%d]: min=%f, max=%f\n",i, min[i],max[i]);
#endif

}

#endif
