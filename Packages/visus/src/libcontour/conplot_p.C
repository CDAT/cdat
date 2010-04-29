//------------------------------------------------------------------------
//
// conPlot_p.C - preprocessing of 3d volumes for seed set extraction
//
// Copyright (c) 1997 Dan Schikore
//------------------------------------------------------------------------

// $Id: conplot_p.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <memory.h>
#include <string.h>

#ifdef WIN32
#include <time.h>
#else
#include <sys/time.h>
#endif

#include "conplot.h"
#include "range.h"
#include "segTree.h"
#include "rangeProp.h"
#include "rangeSweep.h"
#include "dirSeeds.h"
#include "dirSeedsReg2.h"
#include "regProp.h"
#include "regProp2.h"
#include "respProp2.h"
#include "seedChkr2.h"
#include "seedChkr3.h"
#include "seedDirReg3.h"
#include "seedAll.h"

#define TREE_INFO
#define TREE_DUMPNo

#define VERBOSENo

//#define ALL
//#define RESPPROP2

//     general propagation
//#define RANGEPROP

//     regular propagation (regular grids 3d & 2d)
#define REGPROP
//#define REGPROP2

//     climbing (dimension independent)
//#define RANGESWEEP

//     checkerboard (regular grids 2d & 3d)
//#define CHKR2
//#define CHKR3

//     directional sweep (unst 2d, str 3d & 2d)
//#define DIRSEEDS
//#define DIRSEEDSREG3
//#define DIRSEEDSREG2

//------------------------------------------------------------------------
//
// fltcmp() - comparison function for two floats for sorting
//
//------------------------------------------------------------------------
static int
fltcmp(const void *v1, const void *v2)
{
   const float *f1 = (float *)v1;
   const float *f2 = (float *)v2;

   if (*f1 < *f2)
      return(-1);
   if (*f1 > *f2)
      return(1);
   return(0);
}



//------------------------------------------------------------------------
//
// BuildSegTree() - build a segment tree for O(log n) queries
//
//------------------------------------------------------------------------
void
Conplot::BuildSegTree(int t)
{
   float *val;
   u_int i, nval;
#ifdef USE_BUCKETS
   u_int totalsize=0;
#endif

   // get the full list of values
   val = (float *)malloc(sizeof(float) * seeds[t].getNCells()*2);
   for (i=0; i<seeds[t].getNCells(); i++) {
      val[i*2 + 0] = seeds[t].getMin(i);
      val[i*2 + 1] = seeds[t].getMax(i);
#ifdef USE_BUCKETS
      totalsize+=(val[i*2+1]-val[i*2+0]);
#endif
   }

#ifdef USE_BUCKETS
   printf("total cells stored will be: %d\n", totalsize);
#endif

   // sort the list
   qsort(val, seeds[t].getNCells()*2, sizeof(float), fltcmp);

#ifdef VERBOSE
   printf("minimum seed val: %f\n", val[0]);
   printf("maximum seed val: %f\n", val[seeds[t].getNCells()*2-1]);
#endif

   // get rid of duplicates
   nval=1;
   for (i=1; i<seeds[t].getNCells()*2; i++) {
      if (val[i] != val[nval-1])
         val[nval++] = val[i];
   }

#ifdef VERBOSE
   printf("there are %d distinct seed values\n", nval);
#endif

   // initialize the tree and add each segment
#ifdef VERBOSE
printf("initializing tree %d\n", t);
#endif
   tree[t].Init(nval, val);
   for (i=0; i<seeds[t].getNCells(); i++)
      tree[t].InsertSeg(seeds[t].getCellID(i),
                     seeds[t].getMin(i),
                     seeds[t].getMax(i));

   // notify that the tree is finished
   tree[t].Done();

#ifdef TREE_INFO
   // give information about the tree
   tree[t].Info();
#endif
#ifdef TREE_DUMP
   // give information about the tree
   tree[t].Dump();
#endif

   free(val);
}




//------------------------------------------------------------------------
//
// Preprocess() - build a segment tree for O(log n) queries
//
//------------------------------------------------------------------------
void
Conplot::Preprocess(int t, void (*cbfunc)(int, void*), void *cbdata)
{
   int first, last;


   first=clock();

#ifdef RANGESWEEP
   rangeSweep sweep(*(data->getData(t)), seeds[t], *this);
   sweep.compSeeds();
#elif defined DIRSEEDS
   dirSeeds dir(*(data->getData(t)), seeds[t], *this);
   dir.compSeeds();
#elif defined DIRSEEDSREG2
   dirSeedsReg2 dir(*(data->getData(t)), seeds[t], *this);
   dir.compSeeds();
#elif defined DIRSEEDSREG3
   seedDirReg3 dir(*(data->getData(t)), seeds[t], *this);
   dir.compSeeds();
#elif defined RANGEPROP
   rangeProp prop(*(data->getData(t)), seeds[t], *this);
   prop.compSeeds();
#elif defined REGPROP2
   regProp2 prop(*(data->getData(t)), seeds[t], *this);
   prop.compSeeds();
#elif defined REGPROP
   regProp prop(*(data->getData(t)), seeds[t], *this);
   prop.compSeeds();
#elif defined CHKR2
   seedChkr2 prop(*(data->getData(t)), seeds[t], *this);
   prop.compSeeds();
#elif defined CHKR3
   seedChkr3 prop(*(data->getData(t)), seeds[t], *this);
   prop.compSeeds();
#elif defined ALL
   seedAll prop(*(data->getData(t)), seeds[t], *this);
   prop.compSeeds();
#elif defined RESPPROP2
   respProp2 prop(*(data->getData(t)), seeds[t], *this);
   prop.compSeeds();
#endif

   last=clock();
#ifdef VERBOSE
   printf("seed search %d clocks, (%f sec)\n", (last-first),
          (last-first)/(float)CLOCKS_PER_SEC);
#endif
   first=clock();

   BuildSegTree(t);

   last=clock();
#ifdef VERBOSE
   printf("search build %d clocks, (%f sec)\n", (last-first),
          (last-first)/(float)CLOCKS_PER_SEC);
#endif
}
