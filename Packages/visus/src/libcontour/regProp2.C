//------------------------------------------------------------------------
//
// regProp2.C - preprocessing of 2d volumes for seed set extraction
//
//------------------------------------------------------------------------

// $Id: regProp2.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <memory.h>
#include "regProp2.h"
#include "datareg2.h"

//#define VERBOSE
#define DEBUGNo

//------------------------------------------------------------------------
//
// Preprocess() - build a segment tree for O(log n) queries
//
//------------------------------------------------------------------------
void
regProp2::compSeeds(void)
{
   Datareg2 &reg2 = (Datareg2&)data;
   int i, j;
   int xdim, ydim;
   float val[4];
   Range *_prop_x, *prop_x;
   Range prop_y;
   Range propagated;
   Range c_prop;
   Range responsibility, c_respons;
   Range delay;
   Range y_comp;
   float min_x, min_y, max_x, max_y;
   float min_in, max_in, min4, max4;
   int nseed;

#ifdef VERBOSE
   printf("***** Seed Creation\n");
#endif

   xdim = reg2.dim[0];
   ydim = reg2.dim[1];
   _prop_x = new Range[ydim];

   // proceed through the slices computing seeds
   nseed=0;

   // process the k'th slab
   for (i=0; i<xdim-1; i++)
      for (j=0; j<ydim-1; j++) {
         prop_x = &_prop_x[j];

         // load the voxel data
         reg2.getCellValues(i, j, val);
#ifdef DEBUG
printf("cell %d %d %d\n", i, j, k);
printf("   %f %f %f %f %f %f %f %f\n", val[0], val[1], val[2], val[3],
       val[4], val[5], val[6], val[7]);
#endif


         min_x = MIN2(val[0], val[3]);
         max_x = MAX2(val[0], val[3]);
         min_y = MIN2(val[0], val[1]);
         max_y = MAX2(val[0], val[1]);

         // set the incoming values if on a border
         if (i==0)
            prop_x->Set(min_x, max_x);
         if (j==0)
            prop_y.Set(min_y, max_y);

#ifdef DEBUG
printf("prop_x: ");
prop_x.Print();
printf("prop_y: ");
(*prop_y).Print();
#endif

         // merge incoming information
         y_comp = prop_y.Complement(min_y, max_y);
         propagated = prop_y + ((*prop_x)-y_comp);
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
           min_in = MIN2(min_x, min_y);
//           max_in = MAX2(min_x, min_y);
           max_in = MAX2(max_x, max_y);
//         c_prop = Range(min_in,max_in) - propagated;
           c_prop.Set(min_in,max_in);
           c_prop -= propagated;

#ifdef DEBUG
printf("complement: ");
c_prop.Print();
#endif


           // compute responsibility ranges
           min4 = MIN2(min_in, val[2]);
           max4 = MAX2(max_in, val[2]);
//         responsibility = Range(min4, max4) - c_prop;
           responsibility.Set(min4, max4);
           responsibility-=c_prop;
           c_respons = responsibility.Complement(min4, max4);

#ifdef DEBUG
printf("respon: ");
responsibility.Print();
printf("c_respon: ");
c_respons.Print();
#endif

           // determine range which can be delayed
           delay.MakeEmpty();
           if (i < xdim-2)
              delay+=Range(MIN2(val[1], val[2]),
                           MAX2(val[1], val[2]));
           if (j < ydim-2)
              delay+=Range(MIN2(val[2], val[3]),
                           MAX2(val[2], val[3]));

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

              // propagate first to the next x-slice
              if (i == xdim-2)
                 prop_x->MakeEmpty();
              else {
                 prop_x->Set(MIN2(val[1], val[2]), MAX2(val[1], val[2]));
                 *prop_x-=c_respons;
              }

              c_respons += *prop_x;

              // all remaining propagated in y-dir
              if (j == ydim-2)
                 prop_y.MakeEmpty();
              else {
                 prop_y.Set(MIN2(val[2], val[3]), MAX2(val[2], val[3]));
                 prop_y-= c_respons;
              }
           }
           else {
#ifdef DEBUG
printf("ADD SEED\n");
#endif
              // can't propagate all responsiblity, cell must be a seed
              seeds.AddSeed(reg2.index2cell(i,j), responsibility.MinAll(),
                            responsibility.MaxAll());

#ifdef DEBUG
printf("seed at %d %d %d\n", i, j, k);
#endif

              nseed++;

              prop_y.MakeEmpty();
              prop_x->MakeEmpty();
           }
#ifndef WIN32
#ifdef DEBUG
sleep(1);
#endif
#endif
        }

#ifdef VERBOSE
   printf("computed %d seeds\n", nseed);
#endif
}
