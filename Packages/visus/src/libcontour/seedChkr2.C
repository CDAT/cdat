//------------------------------------------------------------------------
//
// seedChkr2.C - preprocessing of 2d volumes for seed set extraction
//
//------------------------------------------------------------------------

// $Id: seedChkr2.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <memory.h>
#include "seedChkr2.h"
#include "datareg2.h"

//#define VERBOSE
#define DEBUGNo

//------------------------------------------------------------------------
//
//------------------------------------------------------------------------
void
seedChkr2::compSeeds(void)
{
   Datareg2 &reg2 = (Datareg2&)data;
   int i, j;
   int xdim, ydim;
   float val[4];
   float min4, max4;
   int nseed;

#ifdef VERBOSE
   printf("***** Seed Creation\n");
#endif

   xdim = reg2.dim[0];
   ydim = reg2.dim[1];

   // proceed through the slices computing seeds
   nseed=0;

   // process the k'th slab
   for (i=0; i<xdim-1; i+=2)
      for (j=0; j<ydim-1; j+=2) {

         // load the voxel data
         reg2.getCellValues(i, j, val);

         min4 = MIN4(val[0], val[1], val[2], val[3]);
         max4 = MAX4(val[0], val[1], val[2], val[3]);

         seeds.AddSeed(reg2.index2cell(i,j), min4, max4);

         nseed++;
      }

   for (i=1; i<xdim-1; i+=2)
      for (j=1; j<ydim-1; j+=2) {

         // load the voxel data
         reg2.getCellValues(i, j, val);

         min4 = MIN4(val[0], val[1], val[2], val[3]);
         max4 = MAX4(val[0], val[1], val[2], val[3]);

         seeds.AddSeed(reg2.index2cell(i,j), min4, max4);

         nseed++;
      }

#ifdef VERBOSE
   printf("computed %d seeds\n", nseed);
#endif
}
