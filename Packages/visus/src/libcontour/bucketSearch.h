//----------------------------------------------------------------------------
//
// bucketSearch.h - segment tree data structure
//
//----------------------------------------------------------------------------

// $Id: bucketSearch.h,v 1.2 2003/09/04 18:50:52 scorzell Exp $

#ifndef BUCKET_SEARCH_H
#define BUCKET_SEARCH_H

#ifdef WIN32
#pragma warning (disable:4244)
#pragma warning (disable:4018)
#endif

#include <sys/types.h>

#include "cellSearch.h"

//----------------------------------------------------------------------------
//
// Bucket search structure
//
//----------------------------------------------------------------------------
class BucketSearch : public CellSearch {
   public:
      BucketSearch(u_int n = 0, float *v = NULL);
      ~BucketSearch();

      void Init(u_int n, float *v);
      void InsertSeg(u_int cellid, float min, float max);
      void Dump(void);
      void Info(void);
      void Traverse(float, void (*f)(u_int, void*), void *);
      u_int getCells(float, u_int *);
      void Done(void);

   protected:
      u_int whichBucket(float f) { return(f-minval); }

   private:
      int nbuckets;
      float minval, maxval;
      CellBucket *buckets;
};

#endif
