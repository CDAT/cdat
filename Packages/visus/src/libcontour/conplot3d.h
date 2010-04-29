//------------------------------------------------------------------------
//
// conplot3d.h - class for preprocessing and extraction of surfaces from
//             3d data
//
// Copyright (c) 1997 Dan Schikore
//------------------------------------------------------------------------

// $Id: conplot3d.h,v 1.1 2003/09/02 17:27:15 scorzell Exp $

#ifndef CONPLOT_3D_H
#define CONPLOT_3D_H

#include <sys/types.h>
#include "contour3d.h"
#include "dataset.h"
#include "segTree.h"
#include "seedCells.h"
#include "cellQueue.h"
#include "edgeHash.h"
#include "range.h"
#include "datasetvol.h"

#include "conplot.h"

//------------------------------------------------------------------------
//
// conplot3d.h
//
//------------------------------------------------------------------------
class Conplot3d : public Conplot {
   public:
      Conplot3d(Datasetvol *d);
      ~Conplot3d();

   protected:
      // extract in 3d (from memory) or slice-by-slice (swap from disk)
      u_int ExtractAll(float isovalue);

      int InterpEdge(int, float *, u_int *, float, int);

      // track a contour from a seed cell
      void TrackContour(float, int);

      // enqueue faces for propagation of surface
      inline void EnqueueFaces(int, int, CellQueue &);

      void Reset(int t)   { con3[t].Reset();           }
      int  Size(int t)    { return(con3[t].getSize()); }
      int  isDone(int t)  { return(con3[t].isDone());  }
      void Done(int t)    { con3[t].Done(); }

   private:
      Datasetvol *vol;
      Datavol *curvol;
      Contour3d *con3, *curcon;
};

#endif
