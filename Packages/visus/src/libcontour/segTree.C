//----------------------------------------------------------------
//
// segTree.C - segment Tree manipulation
//
// Copyright (c) 1997 Dan Schikore
//
//----------------------------------------------------------------

// $Id: segTree.C,v 1.3 2005/05/23 17:13:36 rcook Exp $

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <memory.h>
#include "segTree.h"
#ifdef WIN32
#pragma warning (disable:4018)
#endif

//----------------------------------------------------------------
//
// SegTree() - construct a segment tree for the given range of values
//
//----------------------------------------------------------------
SegTree::SegTree(u_int n, float *val)
{
printf("seg tree constructor\n");
   if (n==0) {
      nleaf=0;
      vals=NULL;
      leqthan = NULL;
      lessthan = NULL;
      grtrthan = NULL;
      return;
   }

   Init(n, val);
}

//----------------------------------------------------------------
//
// Init() - Initialize the segment tree for the given set of values
//
//----------------------------------------------------------------
void
SegTree::Init(u_int n, float *val)
{
   nleaf = n;
   vals = (float *)malloc(sizeof(float)*nleaf);
   memcpy(vals, val, sizeof(float)*nleaf);
   leqthan = new CellBucket[nleaf];
   lessthan = new CellBucket[nleaf];
   grtrthan = new CellBucket[nleaf];
}

//----------------------------------------------------------------
//
// ~SegTree() - free storage for a segment tree
//
//----------------------------------------------------------------
SegTree::~SegTree()
{
printf("SegTree destructor\n");
   free(vals);
   /* should free inside buckets here */
   delete [] leqthan;
   delete [] lessthan;
   delete [] grtrthan;
}

//----------------------------------------------------------------
//
// leftmostbit() - find the leftmost bit of an int
//
//----------------------------------------------------------------
static int
leftmostbit(unsigned int i)
{
   int b = 1;

   while (b <= i)
      b<<=1;

   return(b>>1);
}

//----------------------------------------------------------------
//
// InsertSetR() - recursively split and insert a segment into
//                the tree
//
//----------------------------------------------------------------
void
SegTree::InsertSegR(u_int cellid, float min, float max,
                  int left, int right, float minval, float maxval)
{
   int root, diff;

#ifdef DEBUG_TREE
printf("inserting cell %d (%d %d) (%f %f) (%f %f)\n", cellid, left, right, min, max,
       minval, maxval);
#endif

   if (left == right) {
      if (min < maxval) {
#ifdef DEBUG_TREE
printf("   add to < bucket\n");
#endif
         /* insert in the lessthan bucket */
         lessthan[left].insert(cellid);
      }
      else {
#ifdef DEBUG_TREE
printf("   add to > bucket\n");
#endif
         /* insert in the grtrthan bucket */
         grtrthan[left].insert(cellid);
      }
      return;
   }

   /* compute the index of the root */
   diff = right-left;
   root = leftmostbit(diff) - 1;
   root += left;

#ifdef DEBUG_TREE
printf("root of tree is at %d\n", root);
#endif

   /* see if cell spans the current range */
   if (min <= minval && max >= maxval) {
#ifdef DEBUG_TREE
printf("    spans whole cell\n");
#endif
      leqthan[root].insert(cellid);
      return;
   }

   if (min <= vals[root])
      InsertSegR(cellid, min, MIN2(vals[root],max), left, root,
                        minval, vals[root]);
   if (max > vals[root])
      InsertSegR(cellid, MAX2(vals[root],min), max, root+1, right,
                        vals[root], maxval);
}

//----------------------------------------------------------------
//
// Traverse() - Traverse the tree, calling the given function for
//              each stored segment containing the given value
//
//----------------------------------------------------------------
void
SegTree::Traverse(float val, void (*f)(u_int, void *), void *data)
{
   int left, right, diff, root;

   left = 0;
   right = nleaf-1;

   while (left != right) {
      /* compute the index of the root */
      diff = right-left;
      root = leftmostbit(diff) - 1;
      root += left;
      leqthan[root].traverseCells(f, data);
      if (val <= vals[root])
         right=root;
      else
         left=root+1;
   }

   lessthan[left].traverseCells(f, data);
   if (val == vals[left])
      grtrthan[left].traverseCells(f, data);
}

//----------------------------------------------------------------
//
// getCells() - traverse the tree, storing the cell id's of all
//              segments containing the given value in a list
//
//----------------------------------------------------------------
u_int
SegTree::getCells(float val, u_int *cells)
{
   int left, right, diff, root;
   u_int ncells;

   left = 0;
   right = nleaf-1;

   ncells=0;
   while (left != right) {
      /* compute the index of the root */
      diff = right-left;
      root = leftmostbit(diff) - 1;
      root += left;
      leqthan[root].getCells(cells, ncells);
      if (val <= vals[root])
         right=root;
      else
         left=root+1;
   }

   lessthan[left].getCells(cells, ncells);
   if (val == vals[left])
      grtrthan[left].getCells(cells, ncells);

   return(ncells);
}

//----------------------------------------------------------------
//
// Dump() - dump the tree
//
//----------------------------------------------------------------
void
SegTree::Dump(void)
{
   int i;

   for (i=0; i<nleaf; i++) {
      printf("%d: value %f\n", i, vals[i]);
      leqthan[i].dump("   LEQ:");
      lessthan[i].dump("   LES:");
      grtrthan[i].dump("   GRT:");
   }
}

//----------------------------------------------------------------
//
// Info() - print some stats about the tree
//
//----------------------------------------------------------------
void
SegTree::Info(void)
{
   int i, total, max;

   printf("______SEGMENT TREE STATS______\n");

   printf("%d values in segment tree (%d buckets)\n", nleaf, nleaf*3);

   total = max = 0;
   for (i=0; i<nleaf; i++) {
      total += leqthan[i].nCells();
      total += lessthan[i].nCells();
      total += grtrthan[i].nCells();
      if (leqthan[i].nCells() > max)
          max = leqthan[i].nCells();
      if (lessthan[i].nCells() > max)
          max = lessthan[i].nCells();
      if (grtrthan[i].nCells() > max)
          max = grtrthan[i].nCells();
   }

   printf("total labels in tree: %d\n", total);
   printf("maximum labels in one list: %d\n", max);

   printf("______SEGMENT TREE STATS______\n");
}
