//------------------------------------------------------------------------
//
// seedAll.C - preprocessing of 2d volumes for seed set extraction
//
//------------------------------------------------------------------------

// $Id: seedAll.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <memory.h>
#include "seedAll.h"
#include "datareg2.h"

//#define VERBOSE
#define DEBUGNo

//------------------------------------------------------------------------
//
//------------------------------------------------------------------------
void
seedAll::compSeeds(void)
{
   int c;
   float min, max;
   int nseed;

#ifdef VERBOSE
   printf("***** Seed Creation\n");
#endif

   // proceed through the slices computing seeds
   nseed=0;

   for (c=0; c<data.getNCells(); c++) {

         // load the voxel data
         data.getCellRange(c, min, max);

         seeds.AddSeed(c, min, max);

         nseed++;
   }

#ifdef VERBOSE
   printf("computed %d seeds\n", nseed);
#endif
}
