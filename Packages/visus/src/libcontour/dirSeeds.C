
// $Id: dirSeeds.C,v 1.1 2003/09/02 17:27:16 scorzell Exp $

#include <stdio.h>
#include <string.h>

#include "dirSeeds.h"
#include "datavol.h"
#include "dataslc.h"

#define DEBUGNo
#define DEBUGSLEEPNo

#define sgn(x) ((x)>0 ? 1 : ((x)<0?-1:0))

void
dirSeeds::dirSweep(Dataslc &slc)
{
   int c, f;
   Range resp;
   float g1[3], g2[3];
   float norm[2];
   int adjc;
   float min, max;

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
//                (sgn(norm[1])*g1[0] > 0.0)) {
                (sgn(norm[1]) * (sgn(g1[0]*g1[1])) > 0.0)) {
               slc.getFaceRange(c,f,min,max);
               resp += Range(min,max);
            }
         }
      }
      if (!resp.Empty())
         seeds.AddSeed(c, resp.MinAll(), resp.MaxAll());
   }
}

void
dirSeeds::dirSweep(Datavol &vol)
{
   int c;

   for (c=0; c<vol.getNCells(); c++) {
   }
}

void
dirSeeds::compSeeds(void)
{
   printf("------- computing seeds\n");

   // clear the array of mark bits
   seeds.Clear();

   dirSweep((Dataslc&)data);

   printf("computed %d seeds\n", seeds.getNCells());
}
