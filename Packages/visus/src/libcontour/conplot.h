//------------------------------------------------------------------------
//
// conplot.h - class for preprocessing and extraction of isocontours
//
// Copyright (c) 1997 Dan Schikore
//------------------------------------------------------------------------

// $Id: conplot.h,v 1.1.2.2 2007/09/04 17:35:46 tbremer Exp $

#ifndef CONPLOT_H
#define CONPLOT_H

#include <sys/types.h>

#include "dataset.h"
#include "segTree.h"
#include "intTree.h"
#include "bucketSearch.h"
#include "seedCells.h"
#include "cellQueue.h"
#include "edgeHash.h"
#include "range.h"
#include "contour2d.h"
#include "contour3d.h"

//#define USE_SEG_TREE
#define USE_INT_TREE
//#define USE_BUCKETS

//------------------------------------------------------------------------
//
// conplot.h
//
//------------------------------------------------------------------------
class Conplot {
   public:
      Conplot(Dataset *v);
      virtual ~Conplot();

      // preprocess the volume to compute the seed set
      void Preprocess(int t, void (*func)(int, void*) = NULL, void * = NULL);

      // extract an isosurface of the given value
      u_int Extract(float isovalue) { return(ExtractAll(isovalue)); }

      // select a timestep
      void  setTime(int t);

      // mark a cell or test whether a cell is marked
      inline void TouchCell(u_int);
      inline int CellTouched(u_int);
      inline void ClearTouched(void);

      void ResetAll(void)
              { for (int t=0; t<data->nTime(); t++) Reset(t); }

      int getCells(float val) { return(tree[curtime].getCells(val, int_cells)); }

      SeedCells	*getSeeds()	{ return &seeds[curtime]; }

      Contour2d	*getContour2d()	{ return &contour2d[curtime]; }
      Contour3d	*getContour3d()	{ return &contour3d[curtime]; }

   protected:
      // extract an isosurface
      u_int ExtractAll(float isovalue);

      // build the segment tree for the seed set
      void BuildSegTree(int t);

      virtual void Reset(int) = 0;
      virtual int  Size(int)  = 0;
      virtual int  isDone(int)  = 0;
      virtual void Done(int)  = 0;

      // track a contour from a seed cell
      virtual void TrackContour(float, int)  = 0;

      Dataset	*data;
      CellQueue queue;
      SeedCells *seeds;

      Contour2d *contour2d;
      Contour3d *contour3d;

      int curtime;

   private:
//      CellSearch *tree;
#ifdef USE_SEG_TREE
      SegTree *tree;
#elif defined USE_INT_TREE
      IntTree *tree;
#elif defined USE_BUCKETS
      BucketSearch *tree;
#endif
      u_int	*int_cells;
      u_char	*touched;
};


//------------------------------------------------------------------------
//
// CellTouched() - test if a cell has been visited
//
//------------------------------------------------------------------------
int
Conplot::CellTouched(u_int id)
{
   int byte;
   int bit;

   byte = id>>3;
   bit  = id &0x7;
   return(touched[byte] & (1<<bit));
}

//------------------------------------------------------------------------
//
// TouchCell() - mark a cell as visited
//
//------------------------------------------------------------------------
void
Conplot::TouchCell(u_int id)
{
   int byte;
   int bit;

   byte = id>>3;
   bit  = id &0x7;
   touched[byte] |= (1<<bit);
}

//------------------------------------------------------------------------
//
// ClearTouched() - clear the bit array of 'touched' cells
//
//------------------------------------------------------------------------
void
Conplot::ClearTouched(void)
{
   memset(touched, 0, sizeof(char)*((data->maxCellIndex())>>3));
}

#endif
