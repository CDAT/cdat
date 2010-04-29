
// $Id: seedDirReg3.C,v 1.1 2003/09/02 17:27:18 scorzell Exp $

#include <stdio.h>
#include "seedDirReg3.h"
#include "datareg3.h"
#include "basic.h"

//#define DEBUG
//#define DEBUGSLEEP

#define sgn(x) ((x)>0 ? 1 : ((x)<0?-1:0))

void
seedDirReg3::dirSweep(Datareg3 &reg3)
{
   int i, j, k;
   Range resp;
   float min, max, t;
   float gradz;
   float grad1xa, grad1xb;
   float grad1ya, grad1yb;
   float grad2xa, grad2xb;
   float grad2ya, grad2yb;
   int keepflat, *keepflat_y;

   keepflat_y = (int *)malloc(sizeof(int)*reg3.dim[0]);

   for (k=0; k<reg3.dim[2]-1; k++) {
#ifdef DEBUG
printf("processing slice %d\n", k);
#endif
      for (i=0; i<reg3.dim[0]-1; i++)
         keepflat_y[i] = 1;
      for (j=0; j<reg3.dim[1]-1; j++) {
         keepflat = 1;
         for (i=0; i<reg3.dim[0]-1; i++) {
#ifdef DEBUG
printf("  processing cell (%d,%d,%d) keepflat (%d,y=%d)\n", i,j,k, keepflat, keepflat_y[i]);
#endif
            resp.MakeEmpty();

            // test responsiblity for each face

            // minimum z
            if (k == 0) {
               min = max = reg3.getValue(i,j,k);
               if ((t=reg3.getValue(i,j+1,k)) < min)
                  min = t;
               if (t > max)
                  max = t;
               if ((t=reg3.getValue(i+1,j+1,k)) < min)
                  min = t;
               if (t > max)
                  max = t;
               if ((t=reg3.getValue(i+1,j,k)) < min)
                  min = t;
               if (t > max)
                  max = t;
               if (min != max)
                  resp += Range(min,max);
#ifdef DEBUG
printf("  leftmost cell, responsibility (%f,%f)\n", min, max);
#endif
            }
            else {
               // never do anything other than on boundary
            }


            // maximum z
            if (k == reg3.dim[2]-2) {
               // never keep a top boundary seed
            }
            else {
               // never keep a top boundary seed
            }




            // general case: bottom edge in middle

            // cell (i,j) and (i,j-1) share this x-grad
            gradz = reg3.getValue(i,j,k+1) - reg3.getValue(i,j,k);

            // compute grad at (i,j,k) and (i,j,k+1)
            grad1xa = reg3.getValue(i+1,j,k) -
                      reg3.getValue(i,j,k);
            grad1xb = reg3.getValue(i+1,j,k+1) -
                      reg3.getValue(i,j,k+1);
            grad1ya = reg3.getValue(i,j+1,k) -
                      reg3.getValue(i,j,k);
            grad1yb = reg3.getValue(i,j+1,k+1) -
                      reg3.getValue(i,j,k+1);


            if (keepflat && keepflat_y[i]) {
#ifdef DEBUG
printf("at a flat: checking central edge\n");
#endif
               // check to see if gradient has 'turned'
               // only a seed if gradx & grady disagree in sign
               // note that 0 gradient is not considered opposite
               if (sgn(grad1xa) == 0 && sgn(grad1xb) == 0) {
                  // flat cell (in x dim) - continue
#ifdef DEBUG
printf("   still flat (x)\n");
#endif
               }
               else if (sgn(grad1ya) == 0 && sgn(grad1yb) == 0) {
                  // flat cell (in y dim) - continue
#ifdef DEBUG
printf("   still flat (y)\n");
#endif
               }
               else if ((sgn(gradz) == -sgn(grad1xa) && sgn(gradz) == -sgn(grad1ya)) ||
                        (sgn(gradz) == -sgn(grad1xb) && sgn(gradz) == -sgn(grad1yb))) {
                  // extreme occurs if y components oppose each other
                  // note that 0 gradient is not considered opposite
                  min = max = reg3.getValue(i,j,k);
                  if ((t=reg3.getValue(i,j,k+1)) < min)
                     min = t;
                  if (t > max)
                     max = t;
                  resp += Range(min,max);
#ifdef DEBUG
printf("   added resp (%f %f)\n", min, max);
#endif
                  keepflat = 0;
                  keepflat_y[i] = 0;
               }
#ifdef DEBUG
               else {
printf("   not a seed..  gradz=%f grad1xa=%f grad1ya=%f grad1xb=%f grad1yb=%f\n",
       gradz, grad1xa, grad1ya, grad1xb, grad1yb);
               }
#endif
            }
            else {
            }



            // top
            if (i == reg3.dim[0]-2) {
               if (keepflat && keepflat_y[i]) {
                  // reached end at a flat.. add the edge values
                  min = max = reg3.getValue(i+1,j,k);
                  if ((t=reg3.getValue(i+1,j,k+1)) < min)
                     min = t;
                  if (t > max)
                     max = t;
                  resp += Range(min,max);
               }

               if (j == reg3.dim[1]-2) {
                  if (keepflat && keepflat_y[i]) {
                     // reached end at a flat.. add the edge values
                     min = max = reg3.getValue(i+1,j+1,k);
                     if ((t=reg3.getValue(i+1,j+1,k+1)) < min)
                        min = t;
                     if (t > max)
                        max = t;
                     resp += Range(min,max);
                  }
               }
               else {
                  // do we need to set keepflat_y[i]?
                  gradz = reg3.getValue(i,j+1,k+1) -
                          reg3.getValue(i,j+1,k);
                  grad2xa = reg3.getValue(i+1,j+1,k+1) - reg3.getValue(i,j+1,k+1);
                  grad2xb = reg3.getValue(i+1,j+1,k)   - reg3.getValue(i,j+1,k);
                  grad2ya = reg3.getValue(i,j+1,k+1) - reg3.getValue(i,j,k+1);
                  grad2yb = reg3.getValue(i,j+1,k)   - reg3.getValue(i,j,k);
                  keepflat_y[i] = (sgn(gradz) != 0 &&
                     ((sgn(gradz) == -sgn(grad2xa) && sgn(gradz) == sgn(grad2ya))
                      ||
                      (sgn(gradz) == -sgn(grad2xb) && sgn(gradz) == sgn(grad2yb))));
#if 0
                  if (!keepflat_y[i]) {
                     if (sgn(gradz) != 0 &&
                        ((sgn(gradz) == -sgn(grad2xa) && sgn(gradz) == sgn(grad2ya)) ||
                         (sgn(gradz) == -sgn(grad2xb) && sgn(gradz) == sgn(grad2yb))))
                        keepflat_y[i]=1;
                  }
                  else {
                     if (sgn(gradz) != 0 &&
                         ((sgn(gradz) == sgn(grad2xa) && sgn(gradz) == sgn(grad2ya)) || 
                         (sgn(gradz) == -sgn(grad2xb) && sgn(gradz) == -sgn(grad2yb))))
                        keepflat_y[i]=0;
                  }
#endif
               }
            }
            else {
               if (j == reg3.dim[1]-2) {
                  if (keepflat && keepflat_y[i]) {
                     // reached end at a flat.. add the edge values
                     min = max = reg3.getValue(i+1,j+1,k);
                     if ((t=reg3.getValue(i+1,j+1,k+1)) < min)
                        min = t;
                     if (t > max)
                        max = t;
                     resp += Range(min,max);
                  }
               }
               else {
                  // do we need to set keepflat_y[i]?
                  gradz = reg3.getValue(i,j+1,k+1) -
                          reg3.getValue(i,j+1,k);
                  grad2xa = reg3.getValue(i+1,j+1,k+1) - reg3.getValue(i,j+1,k+1);
                  grad2xb = reg3.getValue(i+1,j+1,k)   - reg3.getValue(i,j+1,k);
                  grad2ya = reg3.getValue(i,j+1,k+1) - reg3.getValue(i,j,k+1);
                  grad2yb = reg3.getValue(i,j+1,k)   - reg3.getValue(i,j,k);
#ifdef DEBUG
printf("checking keepflat[%d]:\n", i);
printf("   gradz: %1.1f grad2xa: %1.1f grad2xb: %1.1f grad2ya: %1.1f grad2yb: %1.1f\n",
       gradz, grad2xa, grad2xb, grad2ya, grad2yb);
#endif
                  keepflat_y[i] = (sgn(gradz) != 0 &&
                     ((sgn(gradz) == -sgn(grad2xa) && sgn(gradz) == sgn(grad2ya))
                      ||
                      (sgn(gradz) == -sgn(grad2xb) && sgn(gradz) == sgn(grad2yb))));
#if 0
                  if (!keepflat_y[i]) {
                     if (sgn(gradz) != 0 &&
                        ((sgn(gradz) == -sgn(grad2xa) && sgn(gradz) == sgn(grad2ya)) ||
                         (sgn(gradz) == -sgn(grad2xb) && sgn(gradz) == sgn(grad2yb))))
                        keepflat_y[i]=1;
                  }
                  else {
                     if (sgn(gradz) != 0 &&
                         ((sgn(gradz) == sgn(grad2xa) && sgn(gradz) == sgn(grad2ya)) || 
                         (sgn(gradz) == -sgn(grad2xb) && sgn(gradz) == -sgn(grad2yb))))
                        keepflat_y[i]=0;
                  }
#endif
               }

               // do we need to set keepflat?
               gradz = reg3.getValue(i+1,j,k+1) -
                       reg3.getValue(i+1,j,k);
               grad2xa = reg3.getValue(i+1,j,k+1)   - reg3.getValue(i,j,k+1);
               grad2xb = reg3.getValue(i+1,j,k)     - reg3.getValue(i,j,k);
               grad2ya = reg3.getValue(i+1,j+1,k+1) - reg3.getValue(i+1,j,k+1);
               grad2yb = reg3.getValue(i+1,j+1,k)   - reg3.getValue(i+1,j,k);
#ifdef DEBUG
printf("checking keepflat:\n");
printf("   gradz: %1.1f grad2xa: %1.1f grad2xb: %1.1f grad2ya: %1.1f grad2yb: %1.1f\n",
       gradz, grad2xa, grad2xb, grad2ya, grad2yb);
#endif
               keepflat = (sgn(gradz) != 0 &&
                  ((sgn(gradz) == sgn(grad2xa) && sgn(gradz) == -sgn(grad2ya))
                   ||
                   (sgn(gradz) == sgn(grad2xb) && sgn(gradz) == -sgn(grad2yb))));
#if 0
               if (!keepflat) {
                  if (sgn(gradz) != 0 &&
                     ((sgn(gradz) == sgn(grad2xa) && sgn(gradz) == -sgn(grad2ya)) ||
                      (sgn(gradz) == sgn(grad2xb) && sgn(gradz) == -sgn(grad2yb))))
                     keepflat=1;
               }
               else {
                  if (sgn(gradz) != 0 &&
                      ((sgn(gradz) ==  sgn(grad2xa) && sgn(gradz) ==  sgn(grad2ya)) || 
                       (sgn(gradz) == -sgn(grad2xb) && sgn(gradz) == -sgn(grad2yb))))
                     keepflat=0;
               }
#endif
            }

            if (!resp.Empty())
               seeds.AddSeed(reg3.index2cell(i,j,k), resp.MinAll(), resp.MaxAll());
         }
      }
   }
}

void
seedDirReg3::compSeeds(void)
{
   printf("------- computing seeds\n");

   // clear the array of mark bits
   seeds.Clear();

   dirSweep((Datareg3&)data);

   printf("computed %d seeds\n", seeds.getNCells());
}
