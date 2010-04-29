//----------------------------------------------------------------------------
//
// intTree.h - interval tree data structure
//
// Copyright (c) 1997 Dan Schikore
//----------------------------------------------------------------------------

// $Id: intTree.h,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#ifndef INT_TREE_H
#define INT_TREE_H

#include <sys/types.h>

#include "cellSearch.h"

//----------------------------------------------------------------------------
//
// Interval Tree Class
//
//----------------------------------------------------------------------------
class IntTree : public CellSearch {
   public:
      IntTree(u_int n = 0, float *v = NULL);
      ~IntTree();

      void Init(u_int n, float *v);
      void InsertSeg(u_int cellid, float min, float max);
      void Dump(void);
      void Info(void);
      void Traverse(float, void (*f)(u_int, void*), void *);
      u_int getCells(float, u_int *);
      void Done(void);

   protected:
      u_int addSeed(u_int id, float mn, float mx) {
          u_int n = nseed++;
          if (n >= seedsize) {
             if (seedsize == 0) {
                seedsize=5;
                cellid = (u_int *)malloc(sizeof(u_int) * seedsize);
                min = (float *)malloc(sizeof(float) * seedsize);
                max = (float *)malloc(sizeof(float) * seedsize);
             }
             else {
                seedsize*=2;
                cellid = (u_int *)realloc(cellid, sizeof(u_int) * seedsize);
                min = (float *)realloc(min, sizeof(float) * seedsize);
                max = (float *)realloc(max, sizeof(float) * seedsize);
             }
          }
          cellid[n]=id;
          min[n]=mn;
          max[n]=mx;
          return(n);
      }

      u_int seedID(u_int n)  { return(cellid[n]); }
      float seedMin(u_int n) { return(min[n]); }
      float seedMax(u_int n) { return(max[n]); }

      static int mincmp(const void *, const void *);
      static int maxcmp(const void *, const void *);
      static void travFun(u_int n, void *data);

   private:
      u_int nseed;
      u_int seedsize;
      u_int *cellid;
      float *min;
      float *max;
     
      int nleaf;
      float *vals;
      CellBucket *minlist;
      CellBucket *maxlist;

      void (*travCB)(u_int, void *);
      void *travData;
};

#endif
