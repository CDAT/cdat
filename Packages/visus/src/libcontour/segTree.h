//----------------------------------------------------------------------------
//
// segTree.h - segment tree data structure
//
// A segment tree permits O(log n) query on a set of ranges (min_i,max_i)
// to determine all ranges which contain a given value of interest.  The
// endpoints of the ranges are sorted along the real line, segmenting the
// line into a number of discrete bins, for which a given cell either
// contains all of the bin or none of it (neglecting special endpoint cases).
// The segment tree is essentially a binary search tree over these segmented
// bins, in which each node corresponds to either a bin (at a leaf) or a
// contiguous region of bins (as you coallesce up the tree), with the root
// node represented the entire real line.  For each input range, a key value
// is stored in each node for which the range (min_i,max_i) contains the
// represented values.  Total storage is O(n log n) in the worst case, but
// typically much better.  The alternative interval tree has O(n) storage
// for the same function.
//
// Copyright (c) 1997 Dan Schikore
//----------------------------------------------------------------------------

// $Id: segTree.h,v 1.1 2003/09/02 17:27:18 scorzell Exp $

#ifndef SEG_TREE_H
#define SEG_TREE_H

#include <sys/types.h>

#define MIN2(x,y) ((x)<(y)?(x):(y))
#define MAX2(x,y) ((x)>(y)?(x):(y))

#define MIN3(x,y,z) ((x)<(y)?((x)<(z)?(x):(z)):((y)<(z)?(y):(z)))
#define MAX3(x,y,z) ((x)>(y)?((x)>(z)?(x):(z)):((y)>(z)?(y):(z)))

#define MIN4(x,y,z,w) ((x)<(y)?((x)<(z)?((x)<(w)?(x):(w)):((w)<(z)?(w):(z))):\
                       ((y)<(z)?((y)<(w)?(y):(w)):((z)<(w)?(z):(w))))
#define MAX4(x,y,z,w) ((x)>(y)?((x)>(z)?((x)>(w)?(x):(w)):((w)>(z)?(w):(z))):\
                       ((y)>(z)?((y)>(w)?(y):(w)):((z)>(w)?(z):(w))))

#define MIN6(min,a1,a2,a3,a4,a5,a6)       \
   do {                                   \
      float __min1, __min2;               \
      __min1 = MIN3(a1,a2,a3);            \
      __min2 = MIN3(a4,a5,a6);            \
      min = MIN2(__min1, __min2);         \
   } while (0)

#define MAX6(max,a1,a2,a3,a4,a5,a6)       \
   do {                                   \
      float __max1, __max2;               \
      __max1 = MAX3(a1,a2,a3);            \
      __max2 = MAX3(a4,a5,a6);            \
      max = MAX2(__max1, __max2);         \
   } while (0)

#define MIN7(min,a1,a2,a3,a4,a5,a6,a7)    \
   do {                                   \
      float __min1, __min2;               \
      __min1 = MIN3(a1,a2,a3);            \
      __min2 = MIN4(a4,a5,a6,a7);         \
      min = MIN2(__min1, __min2);         \
   } while (0)

#define MAX7(max,a1,a2,a3,a4,a5,a6,a7)    \
   do {                                   \
      float __max1, __max2;               \
      __max1 = MAX3(a1,a2,a3);            \
      __max2 = MAX4(a4,a5,a6,a7);         \
      max = MAX2(__max1, __max2);         \
   } while (0)

#define MIN8(min,a1,a2,a3,a4,a5,a6,a7,a8) \
   do {                                   \
      float __min1, __min2;               \
      __min1 = MIN4(a1,a2,a3,a4);         \
      __min2 = MIN4(a5,a6,a7,a8);         \
      min = MIN2(__min1, __min2);         \
   } while (0)

#define MAX8(max,a1,a2,a3,a4,a5,a6,a7,a8) \
   do {                                   \
      float __max1, __max2;               \
      __max1 = MAX4(a1,a2,a3,a4);         \
      __max2 = MAX4(a5,a6,a7,a8);         \
      max = MAX2(__max1, __max2);         \
   } while (0)

#define MIN4MERGE(min,a1,a2,a3,a4)        \
   do {                                   \
      float __min;                        \
      __min = MIN4(a1,a2,a3,a4);          \
      if (__min < min) min = __min;       \
   } while (0)

#define MAX4MERGE(max,a1,a2,a3,a4)        \
   do {                                   \
      float __max;                        \
      __max = MAX4(a1,a2,a3,a4);          \
      if (__max > max) max = __max;       \
   } while (0)

#include "cellSearch.h"

//----------------------------------------------------------------------------
//
// Segment Tree Class
//
//----------------------------------------------------------------------------
class SegTree : public CellSearch {
   public:
      SegTree(u_int n = 0, float *v = NULL);
      ~SegTree();

      void Done(void) {}
      void Init(u_int n, float *v);
      void Dump(void);
      void Info(void);
      void Traverse(float, void (*f)(u_int, void*), void *);
      u_int getCells(float, u_int *);
      void InsertSeg(u_int cellid, float min, float max)
           { InsertSegR(cellid, min, max, 0, nleaf-1, -1e10, 1e10); }

   protected:
      void InsertSegR(u_int, float, float, int, int, float, float);

   private:
      int nleaf;
      float *vals;
      CellBucket *leqthan;
      CellBucket *lessthan;
      CellBucket *grtrthan;
};

#endif
