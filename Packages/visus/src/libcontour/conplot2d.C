//------------------------------------------------------------------------
//
// conPlot2d.C - preprocess and extract contours from 2d scalar data
//
// Copyright (c) 1997 Dan Schikore
//------------------------------------------------------------------------

// $Id: conplot2d.C,v 1.2.2.1 2007/09/04 17:37:02 tbremer Exp $

#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <memory.h>
#include <string.h>

#include "conplot.h"
#include "contour2d.h"
#include "range.h"
#include "segTree.h"
#include "conplot2d.h"

static int unst2d[8][3] = {
   {0},
   {1, 2, 0},
   {1, 0, 1},
   {1, 1, 2},
   {1, 2, 1},
   {1, 1, 0},
   {1, 0, 2},
   {0}
};


//------------------------------------------------------------------------
//
// bitsize - return the number of bits in an int (easier way?)
//
//------------------------------------------------------------------------
static int
bitsize(unsigned int i)
{
   int b = 1, size=0;

   while (b <= i) {
      b<<=1;
      size++;
   }
   return(size);
}

//------------------------------------------------------------------------
//
// Conplot() - create a contour plot for the given image.
//
//------------------------------------------------------------------------
Conplot2d::Conplot2d(Datasetslc *d) : Conplot(d)
{
   float min[3], max[3];
   int i;

   slc = d;

#ifdef VERBOSE
   printf("***** Data Characteristics\n");
   //   printf("cells: %d\n", vol->getNCells());
   printf("*****\n");
#endif

   contour2d = con2 = new Contour2d[slc->nTime()];
   contour3d = NULL;
   data->getData(0)->getExtent(min, max);
printf("minextent: %f %f %f\n", min[0], min[1], min[2]);
printf("maxextent: %f %f %f\n", max[0], max[1], max[2]);

   for (i=0; i<slc->nTime(); i++)
      con2[i].setExtent(min,max);

#ifdef VERBOSE
   printf("contour3d is %x, contour2d is %x\n", contour3d, con2);
#endif
}

//------------------------------------------------------------------------
//
// ~Conplot2d() - destroy a plot
//
//------------------------------------------------------------------------
Conplot2d::~Conplot2d()
{
}

//------------------------------------------------------------------------
//
// InterpEdge() - interpolate ....
//
//------------------------------------------------------------------------
int
Conplot2d::InterpEdge(int edge, float *val, u_int *v, float isovalue, int cell)
{
   float ival;
   float pt[2];

   switch (edge) {
      case 0:
         ival = (isovalue-val[1])/(val[0]-val[1]);
         pt[0] = curslc->getVert(v[1])[0]*(1.0-ival) +
                 curslc->getVert(v[0])[0]*ival;
         pt[1] = curslc->getVert(v[1])[1]*(1.0-ival) +
                 curslc->getVert(v[0])[1]*ival;
         break;
      case 1:
         ival = (isovalue-val[2])/(val[1]-val[2]);
         pt[0] = curslc->getVert(v[2])[0]*(1.0-ival) +
                 curslc->getVert(v[1])[0]*ival;
         pt[1] = curslc->getVert(v[2])[1]*(1.0-ival) +
                 curslc->getVert(v[1])[1]*ival;
         break;
      case 2:
         ival = (isovalue-val[0])/(val[2]-val[0]);
         pt[0] = curslc->getVert(v[0])[0]*(1.0-ival) +
                 curslc->getVert(v[2])[0]*ival;
         pt[1] = curslc->getVert(v[0])[1]*(1.0-ival) +
                 curslc->getVert(v[2])[1]*ival;
         break;
   }

//printf("pt %f %f\n", pt[0], pt[1]);

   return(curcon->AddVert(pt));
}

//------------------------------------------------------------------------
//
// EnqueueFaces() - enqueue adjacent faces for propagation of contour
//             code      = case table lookup code for current cell
//             i,j,k     = index of current cell
//             thisslice = queue of cells to be processed on current slice
//             nextslice = queue of cells to be processed on next slice
//             trackinz  = flag indicating whether to propagate in z-dir
//                         0 -> don't track in z, but queue cells in nextslice
//                         1 -> propagate contour in all 3 dim
//
//------------------------------------------------------------------------
inline void
Conplot2d::EnqueueFaces(int code, int c, CellQueue &thisslice)
{
   switch (code) {
      case 0:
         break;
      case 1:
         break;
      case 2:
         break;
   }
}

//------------------------------------------------------------------------
//
// TrackContour() - compute and track a contour by table lookup and propagation
//                  through adjacent cells
//            isovalue  = surface value of interest
//            i,j,k     = index of seed cell
//            nextslice = queue of seeds for next slice (used only if tracking in 2d)
//
//------------------------------------------------------------------------
void
Conplot2d::TrackContour(float isovalue, int cell)
{
   float val[3];
   u_int *verts;
   u_int v1, v2;
   int code;
   int adj;
   int e;

   queue.Add(cell);

   curslc = (Dataslc*)data->getData(curtime);
   curcon = &con2[curtime];

   while (queue.Get(cell) > 0) {
      curslc->getCellValues(cell, val);
      verts = curslc->getCellVerts(cell);

      code = 0;
      if (val[0] < isovalue) code += 0x01;
      if (val[1] < isovalue) code += 0x02;
      if (val[2] < isovalue) code += 0x04;

      for (e=0; e<unst2d[code][0]; e++) {
         v1 = InterpEdge(unst2d[code][2*e+1], val, verts, isovalue, cell);
         v2 = InterpEdge(unst2d[code][2*e+2], val, verts, isovalue, cell);
         curcon->AddEdge(v1, v2);
         adj = curslc->getCellAdj(cell, unst2d[code][2*e+1]);
         if (adj != -1 && !CellTouched(adj)) {
            TouchCell(adj);
            queue.Add(adj);
         }
         adj = curslc->getCellAdj(cell, unst2d[code][2*e+2]);
         if (adj != -1 && !CellTouched(adj)) {
            TouchCell(adj);
            queue.Add(adj);
         }
      }

      EnqueueFaces(code, cell, queue);
   }

}
