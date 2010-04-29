//------------------------------------------------------------------------
//
// regProp.C - preprocessing of 3d volumes for seed set extraction
//
//------------------------------------------------------------------------

// $Id: regProp.C,v 1.2.2.1 2007/09/04 17:30:17 tbremer Exp $

#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <memory.h>
#include "regProp.h"
#include "datareg3.h"

//#define VERBOSE
#define DEBUGNo

//------------------------------------------------------------------------
//
// Preprocess() - build a segment tree for O(log n) queries
//
//------------------------------------------------------------------------
void
regProp::compSeeds(void)
{
   Datareg3 &reg3 = (Datareg3&)data;
   int i, j, k;
   int xdim, ydim, zdim;
   float val[8];
   Range *_prop_z, *prop_z;
   Range *_prop_y, *prop_y;
   Range prop_x;
   Range propagated;
   Range c_prop;
   Range responsibility, c_respons;
   Range delay;
   Range x_comp;
   float min_x, min_y, min_z, max_x, max_y, max_z;
   float min_in, max_in, min8, max8;
   int nseed;

#ifdef VERBOSE
   printf("***** Seed Creation\n");
#endif

   xdim = reg3.dim[0];
   ydim = reg3.dim[1];
   zdim = reg3.dim[2];
   _prop_z = new Range[xdim * ydim];
   _prop_y = new Range[xdim];

   // proceed through the slices computing seeds
   nseed=0;
   for (k=0; k<zdim-1; k++) {
#ifdef VERBOSE
      if (k % 10 == 0)
         printf("slice %d, %d seeds\n", k, nseed);
#endif

      // process the k'th slab
      for (j=0; j<ydim-1; j++)
         for (i=0; i<xdim-1; i++) {
            prop_y = &_prop_y[i];
            prop_z = &_prop_z[j*(xdim-1)+i];
            // load the voxel data
            reg3.getCellValues(i, j, k, val);
#ifdef DEBUG
printf("cell %d %d %d\n", i, j, k);
printf("   %f %f %f %f %f %f %f %f\n", val[0], val[1], val[2], val[3],
       val[4], val[5], val[6], val[7]);
#endif


            min_x = MIN4(val[0], val[3], val[4], val[7]);
            max_x = MAX4(val[0], val[3], val[4], val[7]);
            min_y = MIN4(val[0], val[1], val[2], val[3]);
            max_y = MAX4(val[0], val[1], val[2], val[3]);
            min_z = MIN4(val[0], val[1], val[4], val[5]);
            max_z = MAX4(val[0], val[1], val[4], val[5]);

           // set the incoming values if on a border
           if (i==0)
              prop_x.Set(min_x, max_x);
           if (j==0)
              prop_y->Set(min_y, max_y);
           if (k==0)
              prop_z->Set(min_z, max_z);

#ifdef DEBUG
printf("prop_x: ");
prop_x.Print();
printf("prop_y: ");
(*prop_y).Print();
printf("prop_z: ");
(*prop_z).Print();
#endif

           // merge incoming information
           x_comp = prop_x.Complement(min_x, max_x);
         propagated = prop_x + ((*prop_y)+(*prop_z)-x_comp);
//           propagated = prop_x;
//           propagated += *prop_y;
//           propagated += *prop_z;
//           propagated -= x_comp;

#ifdef DEBUG
printf("x_comp: ");
x_comp.Print();
printf("total propagated: ");
propagated.Print();
#endif


           // compute complement of incoming ranges
           min_in = MIN3(min_x, min_y, min_z);
//           max_in = MAX3(min_x, min_y, min_z);
           max_in = MAX3(max_x, max_y, max_z);
//         c_prop = Range(min_in,max_in) - propagated;
           c_prop.Set(min_in,max_in);
           c_prop -= propagated;

#ifdef DEBUG
printf("complement: ");
c_prop.Print();
#endif


           // compute responsibility ranges
           min8 = MIN2(min_in, val[6]);
           max8 = MAX2(max_in, val[6]);
//         responsibility = Range(min8, max8) - c_prop;
           responsibility.Set(min8, max8);
           responsibility-=c_prop;
           c_respons = responsibility.Complement(min8, max8);

#ifdef DEBUG
printf("respon: ");
responsibility.Print();
printf("c_respon: ");
c_respons.Print();
#endif

           // determine range which can be delayed
           delay.MakeEmpty();
           if (i < xdim-2)
              delay+=Range(MIN4(val[1], val[2], val[5], val[6]),
                           MAX4(val[1], val[2], val[5], val[6]));
           if (j < ydim-2)
              delay+=Range(MIN4(val[4], val[5], val[6], val[7]),
                           MAX4(val[4], val[5], val[6], val[7]));
           if (k < zdim-2)
              delay+=Range(MIN4(val[2], val[3], val[6], val[7]),
                           MAX4(val[2], val[3], val[6], val[7]));

#ifdef DEBUG
printf("delay: ");
delay.Print();
#endif

           // test for propagation of entire responsibility range
           if (responsibility.Empty() || (!delay.Empty() &&
               delay.MinAll() <= responsibility.MinAll() &&
               delay.MaxAll() >= responsibility.MaxAll())) {

#ifdef DEBUG
printf("DELAY ALL\n");
#endif

              // propagate first to the next z-slice
              if (k == zdim-2)
                 prop_z->MakeEmpty();
              else {
                 prop_z->Set(MIN4(val[2], val[3], val[6], val[7]),
                            MAX4(val[2], val[3], val[6], val[7]));
                 *prop_z -= c_respons;
              }

              c_respons += *prop_z;

              // propagate in y-direction next
              if (j == ydim-2)
                 prop_y->MakeEmpty();
              else {
                 prop_y->Set(MIN4(val[4], val[5], val[6], val[7]),
                            MAX4(val[4], val[5], val[6], val[7]));
                 *prop_y-=c_respons;
              }

              c_respons += *prop_y;

              // all remaining propagated in x-dir
              if (i == xdim-2)
                 prop_x.MakeEmpty();
              else {
                 prop_x.Set(MIN4(val[1], val[2], val[5], val[6]),
                                MAX4(val[1], val[2], val[5], val[6]));
                 prop_x-= c_respons;
              }
           }
           else {
#ifdef DEBUG
printf("ADD SEED\n");
#endif
              // can't propagate all responsiblity, cell must be a seed
              seeds.AddSeed(reg3.index2cell(i,j,k), responsibility.MinAll(),
                            responsibility.MaxAll());

#ifdef DEBUG
printf("seed at %d %d %d\n", i, j, k);
#endif

              nseed++;

              prop_z->MakeEmpty();
              prop_y->MakeEmpty();
              prop_x.MakeEmpty();
           }

#ifndef WIN32
#ifdef DEBUG
sleep(1);
#endif
#endif

        }
   }

   // Freeing memory that seems to be no longer in use
   // 08/28/07 ptb
   delete[] _prop_z;
   delete[] _prop_y;

#ifdef VERBOSE
   printf("computed %d seeds\n", nseed);
#endif
}
