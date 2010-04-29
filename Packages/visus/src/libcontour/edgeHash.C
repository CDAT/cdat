//----------------------------------------------------------------------
//
// edgeHash.C - hash to lookup vertices which are already computed
//
// This is a *very* basic hash class to aid in finding vertices which
// have already been computed, avoiding recomputation and duplication
//
// Copyright (c) 1997 Dan Schikore
//----------------------------------------------------------------------

// $Id: edgeHash.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdio.h>
#include <math.h>
#include <memory.h>
#include <stdlib.h>
#include "edgeHash.h"

// use a small prime # of buckets
#define NBUCKETS 3001

//----------------------------------------------------------------------
//
// EdgeHash - construct a new hash
//
//----------------------------------------------------------------------
EdgeHash::EdgeHash()
{
   int b;

   nbuckets = NBUCKETS;
   nitems  = (int *)malloc(sizeof(int) * nbuckets);
   buckets  = (EdgeHashBucket *)malloc(sizeof(EdgeHashBucket) *
                                       nbuckets);

   // initialize each bucket
   for (b=0; b<nbuckets; b++) {
      nitems[b] = 0;
      buckets[b].elsize = 5;
      buckets[b].items = (EdgeHashEl *)malloc(sizeof(EdgeHashEl)*
                                              buckets[b].elsize);
   }
}

//----------------------------------------------------------------------
//
// LookupBucket - search a given bucket for a given key
//
//----------------------------------------------------------------------
int
EdgeHash::LookupBucket(int *nitems, EdgeHashBucket *b, int key)
{
   int i, vnum;

   // loop through the items
   for (i=0; i<(*nitems); i++) {

      if (b->items[i].key == key) {
         // found the requested key
         vnum = b->items[i].vnum;

         if (++(b->items[i].nref) == 4) {
            // edges referenced 4 times will not be used again
            if ((*nitems) > 1)
               b->items[i] = b->items[(*nitems)-1];
            (*nitems)--;
         }

         return(vnum);
      }

   }

   return(-1);
}


//----------------------------------------------------------------------
//
// InsertBucket - insert an item in the given bucket
//
//----------------------------------------------------------------------
void
EdgeHash::InsertBucket(int *nitems, EdgeHashBucket *b, int key, int vnum)
{
   int n = (*nitems)++;

   if (n >= b->elsize) {
      b->elsize*=2;
      b->items = (EdgeHashEl *)realloc(b->items, sizeof(EdgeHashEl)*
                                       b->elsize);
#ifdef VERBOSE
printf("hash size: %d\n", b->elsize);
#endif
   }

   b->items[n].key  = key;
   b->items[n].vnum = vnum;
   b->items[n].nref = 1;
}
