//------------------------------------------------------------------------
//
// seedChkr3.C - preprocessing of 3d volumes for seed set extraction
//
//------------------------------------------------------------------------

// $Id: seedChkr3.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif

#include <memory.h>
#include "seedChkr3.h"
#include "datareg3.h"

//#define VERBOSE
#define DEBUGNo

//------------------------------------------------------------------------
//
//------------------------------------------------------------------------
void
seedChkr3::compSeeds(void)
{
   Datareg3 &reg3 = (Datareg3&)data;
   int i, j, k;
   int xdim, ydim, zdim;
   float val[8];
   float min8, max8;
   int nseed;

#ifdef VERBOSE
   printf("***** Seed Creation\n");
#endif

   xdim = reg3.dim[0];
   ydim = reg3.dim[1];
   zdim = reg3.dim[2];

   // proceed through the slices computing seeds
   nseed=0;

   // process the k'th slab
   for (i=0; i<xdim-1; i+=2)
      for (j=0; j<ydim-1; j+=2)
         for (k=0; k<zdim-1; k+=2) {

            // load the voxel data
            reg3.getCellValues(i, j, k, val);

            MIN8(min8, val[0], val[1], val[2], val[3], val[4], val[5], val[6], val[7]);
            MAX8(max8, val[0], val[1], val[2], val[3], val[4], val[5], val[6], val[7]);

            seeds.AddSeed(reg3.index2cell(i,j,k), min8, max8);

            nseed++;
         }

   for (i=1; i<xdim-1; i+=2)
      for (j=1; j<ydim-1; j+=2)
         for (k=1; k<zdim-1; k+=2) {

            // load the voxel data
            reg3.getCellValues(i, j, k, val);

            MIN8(min8, val[0], val[1], val[2], val[3], val[4], val[5], val[6], val[7]);
            MAX8(max8, val[0], val[1], val[2], val[3], val[4], val[5], val[6], val[7]);

            seeds.AddSeed(reg3.index2cell(i,j,k), min8, max8);

            nseed++;
         }

#ifdef VERBOSE
   printf("computed %d seeds\n", nseed);
#endif
}
