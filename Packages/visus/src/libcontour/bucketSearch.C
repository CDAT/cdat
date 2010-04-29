//----------------------------------------------------------------
//
// bucketSearch.C - bucket search
//
//----------------------------------------------------------------

// $Id: bucketSearch.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif

#include <memory.h>
#include "bucketSearch.h"

#define DEBUG_TREENo

//----------------------------------------------------------------
//
// BucketSearch() - construct a segment tree for the given range of values
//
//----------------------------------------------------------------
BucketSearch::BucketSearch(u_int n, float *val)
{
   if (n==0) {
      minval = maxval = 0;
      nbuckets = 0;
      buckets = NULL;

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
BucketSearch::Init(u_int n, float *val)
{
   minval = val[0];
   maxval = val[n-1];

   nbuckets = val[n-1] - val[0];
   buckets = new CellBucket[nbuckets];
}

//----------------------------------------------------------------
//
// ~BucketSearch() - free storage for a segment tree
//
//----------------------------------------------------------------
BucketSearch::~BucketSearch()
{
printf("BucketSearch destructor\n");
   /* should free inside buckets here */
   delete [] buckets;
}

void
BucketSearch::Done(void)
{
}

//----------------------------------------------------------------
//
// InsertSet() - recursively insert a segment into the tree
//
//----------------------------------------------------------------
void
BucketSearch::InsertSeg(u_int cellid, float min, float max)
{
   u_int first, last;
   u_int b;

   first = whichBucket(min);
   last = whichBucket(max);

   for (b=first; b<last; b++)
      buckets[b].insert(cellid);
}

//----------------------------------------------------------------
//
// Traverse() - Traverse the tree, calling the given function for
//              each stored segment containing the given value
//
//----------------------------------------------------------------
void
BucketSearch::Traverse(float val, void (*f)(u_int, void *), void *data)
{
   u_int b;

   b = whichBucket(val);
   buckets[b].traverseCells(f, data);
}

//----------------------------------------------------------------
//
// getCells() - traverse the tree, storing the cell id's of all
//              segments containing the given value in a list
//
//----------------------------------------------------------------
u_int
BucketSearch::getCells(float val, u_int *cells)
{
   u_int b;
   u_int ncells;

   ncells=0;
   b = whichBucket(val);
   buckets[b].getCells(cells, ncells);

   return(ncells);
}

//----------------------------------------------------------------
//
// Dump() - dump the tree
//
//----------------------------------------------------------------
void
BucketSearch::Dump(void)
{
   int i;

   for (i=0; i<nbuckets; i++) {
      printf("%d: value %f\n", i, minval+i);
      buckets[i].dump("   cells:");
      printf("\n");
   }
}

//----------------------------------------------------------------
//
// Info() - print some stats about the tree
//
//----------------------------------------------------------------
void
BucketSearch::Info(void)
{
   int i, total, max;

   printf("______BUCKET STATS_____\n");

   printf("%d buckets\n", nbuckets);

   total = max = 0;
   for (i=0; i<nbuckets; i++) {
      total += buckets[i].nCells();
      if (buckets[i].nCells() > max)
          max = buckets[i].nCells();
   }

   printf("total labels in buckets: %d\n", total);
   printf("maximum labels in one list: %d\n", max);
   printf("______BUCKET STATS_____\n");
}
