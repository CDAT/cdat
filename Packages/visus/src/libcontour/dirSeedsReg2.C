
// $Id: dirSeedsReg2.C,v 1.1 2003/09/02 17:27:16 scorzell Exp $

#include <stdio.h>
#include "dirSeedsReg2.h"
#include "datareg2.h"
#include "basic.h"

#define DEBUGNo
#define DEBUGSLEEPNo

#define sgn(x) ((x)>0 ? 1 : ((x)<0?-1:0))

void
dirSeedsReg2::dirSweep(Datareg2 &reg2)
{
   int i, j;
   Range resp;
   float min, max, t;
   float gradx;
   float grad1ya, grad1yb;
   float grad2ya, grad2yb;
   int keepflat;
   int prev;

   for (i=0; i<reg2.dim[0]-1; i++) {
      keepflat = 1;
      prev = -1;
      for (j=0; j<reg2.dim[1]-1; j++) {
         resp.MakeEmpty();

         // test responsiblity for each face

         // left
         if (i == 0) {
            min = max = reg2.getValue(reg2.index2vert(i,j));
            if ((t=reg2.getValue(reg2.index2vert(i,j+1))) < min)
               min = t;
            if (t > max)
               max = t;
            if (min != max)
               resp += Range(min,max);
         }
         else {
            // never do anything other than on boundary
         }


         // right
         if (i == reg2.dim[0]-2) {
            // never keep a right boundary seed
         }
         else {
            // never keep a right boundary seed
         }



         // general case: bottom edge in middle

         // cell (i,j) and (i,j-1) share this x-grad
         gradx = reg2.getValue(reg2.index2vert(i+1,j)) -
                 reg2.getValue(reg2.index2vert(i,j));

         // compute y-grad at (i,j) and (i+1,j)
         grad1ya = reg2.getValue(reg2.index2vert(i,j+1)) -
                   reg2.getValue(reg2.index2vert(i,j));
         grad1yb = reg2.getValue(reg2.index2vert(i+1,j+1)) -
                   reg2.getValue(reg2.index2vert(i+1,j));

         if (keepflat) {
            // check to see if gradient has 'turned'
            // only a seed if gradx & grady disagree in sign
            // note that 0 gradient is not considered opposite
            if (sgn(grad1ya) == 0 && sgn(grad1yb) == 0) {
               // flat cell (in y dim) - continue
            }
            else if (sgn(gradx) == -sgn(grad1ya) || sgn(gradx) == -sgn(grad1yb)) {
               // extreme occurs if y components oppose each other
               // note that 0 gradient is not considered opposite
               min = max = reg2.getValue(reg2.index2vert(i,j));
               if ((t=reg2.getValue(reg2.index2vert(i+1,j))) < min)
                  min = t;
               if (t > max)
                  max = t;
               resp += Range(min,max);
               keepflat = 0;
            }
         }



         // top
         if (j == reg2.dim[1]-2) {
            if (keepflat) {
               min = max = reg2.getValue(reg2.index2vert(i,j+1));
               if ((t=reg2.getValue(reg2.index2vert(i+1,j+1))) < min)
                  min = t;
               if (t > max)
                  max = t;
               resp += Range(min,max);
            }
         }
         else {
            // only consider the top at the boundary
            if (!keepflat) {
               gradx = reg2.getValue(reg2.index2vert(i+1,j+1)) -
                       reg2.getValue(reg2.index2vert(i,j+1));
               grad2ya = reg2.getValue(reg2.index2vert(i,j+1)) -
                         reg2.getValue(reg2.index2vert(i,j));
               grad2yb = reg2.getValue(reg2.index2vert(i+1,j+1)) -
                         reg2.getValue(reg2.index2vert(i+1,j));
               if (sgn(gradx) != 0 && (sgn(gradx) == sgn(grad2ya) || sgn(gradx) == sgn(grad2yb)))
                  keepflat=1;
            }
            else {
               gradx = reg2.getValue(reg2.index2vert(i+1,j+1)) -
                       reg2.getValue(reg2.index2vert(i,j+1));
               grad2ya = reg2.getValue(reg2.index2vert(i,j+1)) -
                         reg2.getValue(reg2.index2vert(i,j));
               grad2yb = reg2.getValue(reg2.index2vert(i+1,j+1)) -
                         reg2.getValue(reg2.index2vert(i+1,j));
               if (sgn(gradx) == -sgn(grad2ya) || sgn(gradx) == -sgn(grad2yb))
                  keepflat=0;
            }
         }

         if (!resp.Empty()) {
            if (prev == -1) {
               if (i!=0)
                  prev = seeds.AddSeed(reg2.index2cell(i,j), resp.MinAll(),
                                       resp.MaxAll());
               else
                  seeds.AddSeed(reg2.index2cell(i,j), resp.MinAll(),
                                       resp.MaxAll());
            }
            else {
               seeds.AddToRange(prev, resp.MinAll(), resp.MaxAll());
               prev = -1;
            }
         }
         else
            prev = -1;
      }
   }
#if 0
   for (c=0; c<slc.getNCells(); c++) {
      resp.MakeEmpty();
      slc.getCellGrad(c, g1);

#ifdef DEBUG
printf("******\ncell %d: grad (%f %f)\n", g1[0], g1[1]);
#endif

      for (f=0; f<slc.getNCellFaces(); f++) {
         adjc = slc.getCellAdj(c,f);
         if (adjc != -1) {
            slc.normalToFace(c,f,norm);
#ifdef DEBUG
printf("adj cell: %d  norm (%f %f)\n", adjc, norm[0], norm[1]);
#endif
            if (norm[1] >= 0.0) {
               slc.getCellGrad(adjc, g2);
#ifdef DEBUG
printf("adj grad: %f %f\n", g2[0], g2[1]);
#endif
               if (sgn(g1[0])==sgn(g1[1]) && g1[1] * g2[1] < 0.0) {
                  slc.getFaceRange(c,f,min,max);
#ifdef DEBUG
printf("seed range: %f %f\n", min, max);
sleep(10);
#endif
                  resp += Range(min,max);
               }
            }
         }
         else {
            // boundary case... do something special?
            slc.normalToFace(c,f,norm);
// first condition:  all left boundary cells are selected
// second: top/bottom sides *may* be selected
//         right hand cells should never be selected (sgn==0.0)
            if ((fabs(norm[1]) < 0.0000001 && norm[0] < 0.0) ||
                (sgn(norm[1]) * (sgn(g1[0]*g1[1])) > 0.0)) {
               slc.getFaceRange(c,f,min,max);
               resp += Range(min,max);
            }
         }
      }
      if (!resp.Empty())
         seeds.AddSeed(c, resp.MinAll(), resp.MaxAll());
   }
#endif
}

void
dirSeedsReg2::compSeeds(void)
{
   printf("------- computing seeds\n");

   // clear the array of mark bits
   seeds.Clear();

   dirSweep((Datareg2&)data);

   printf("computed %d seeds\n", seeds.getNCells());
}
