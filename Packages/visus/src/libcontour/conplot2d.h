//------------------------------------------------------------------------
//
// conplot2d.h - class for preprocessing and extraction of isocurves from
//             2d data
//
// Copyright (c) 1997 Dan Schikore
//------------------------------------------------------------------------

// $Id: conplot2d.h,v 1.1 2003/09/02 17:27:15 scorzell Exp $

#ifndef CONPLOT_2D_H
#define CONPLOT_2D_H

#include <sys/types.h>
#include "contour2d.h"
#include "dataset.h"
#include "segTree.h"
#include "seedCells.h"
#include "cellQueue.h"
#include "edgeHash.h"
#include "range.h"
#include "datasetslc.h"

#include "conplot.h"

//------------------------------------------------------------------------
//
// conplot2d.h
//
//------------------------------------------------------------------------
class Conplot2d : public Conplot {
   public:
      Conplot2d(Datasetslc *d);
      ~Conplot2d();

   protected:
      // extract in 3d (from memory) or slice-by-slice (swap from disk)
      u_int ExtractAll(float isovalue);

      int InterpEdge(int, float *, u_int *, float, int);

      // track a contour from a seed cell
      void TrackContour(float, int);

      // enqueue faces for propagation of surface
      inline void EnqueueFaces(int, int, CellQueue &);

      void Reset(int t)   { con2[t].Reset();           }
      int  Size(int t)    { return(con2[t].getSize()); }
      int  isDone(int t)  { return(con2[t].isDone());  }
      void Done(int t)    { con2[t].Done(); }

   private:
      Datasetslc *slc;
      Dataslc *curslc;
      Contour2d *con2, *curcon;
};

#endif
