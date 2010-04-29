//------------------------------------------------------------------------
//
// conplotreg2.h - class for preprocessing and extraction of isocurves
//             	from 2d rectilinear data
//
// Copyright (c) 1997 Dan Schikore
//------------------------------------------------------------------------

// $Id: conplotreg2.h,v 1.1 2003/09/02 17:27:15 scorzell Exp $

#ifndef CONPLOT_RECT_2D_H
#define CONPLOT_RECT_2D_H

#include <sys/types.h>
#include "contour2d.h"
#include "dataset.h"
#include "segTree.h"
#include "seedCells.h"
#include "cellQueue.h"
#include "edgeHash.h"
#include "range.h"
#include "datasetreg2.h"

#include "conplot.h"

//------------------------------------------------------------------------
//
// conplotreg2.h
//
//------------------------------------------------------------------------
class Conplotreg2 : public Conplot {
   public:
      Conplotreg2(Datasetreg2 *d);
      ~Conplotreg2();

   protected:
      // extract in 3d (from memory) or slice-by-slice (swap from disk)
      u_int ExtractAll(float isovalue);

      int InterpEdge(int, float *, float, int, int);

      // track a contour from a seed cell
      void TrackContour(float, int);

      // enqueue faces for propagation of surface
      inline void EnqueueFaces(int, int, CellQueue &);

      void Reset(int t)   { con2[t].Reset();           }
      int  Size(int t)    { return(con2[t].getSize()); }
      int  isDone(int t)  { return(con2[t].isDone());  }
      void Done(int t)    { con2[t].Done(); }

   private:
      Datasetreg2 *reg2;
      Datareg2 *curreg2;
      Contour2d *con2, *curcon;
};

#endif
