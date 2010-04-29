//-------------------------------------------------------------------------------
//
// range.h - class for performing range arithmetic.  A single range consists of
//           a sorted list of non-overlapping (min,max) pairs.  For simplificity,
//           we cap the number of pairs at 40.  This should be dynamic, though
//           for our usage ranges are not so fragmented.
//
//-------------------------------------------------------------------------------

// $Id: range.h,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#ifndef RANGE_H
#define RANGE_H

#include <memory.h>

class Range {
public:
   // default constructor
   Range(void)                    { nrange = 0; }

   // simple range constructor
   Range(float mn, float mx)      { nrange = 1;
                                    min[0] = mn;
                                    max[0] = mx;
                                  }

   // copy constructor
   Range(const Range &r)          {
                                    nrange = r.nrange;
                                    memcpy(min, r.min, sizeof(float)*nrange);
                                    memcpy(max, r.max, sizeof(float)*nrange);
                                  }

   // destructor
   virtual ~Range()               {}

   // return number of closed segments
   int   NSegs(void)              { return(nrange); }

   // return min value for a given segment
   float Min(int i)               { return(min[i]); }

   // return max value for a given segment
   float Max(int i)               { return(max[i]); }

   // return min value of all segments
   float MinAll(void)             { return(min[0]); }

   // return max value of all segments
   float MaxAll(void)             { return(max[nrange-1]); }

   // true if range is empty
   int   Empty(void)              { return(NSegs()==0); }

   // set the min value for a segment
   void  SetMin(int i, float m)   { min[i] = m; }

   // set the max value for a segment
   void  SetMax(int i, float m)   { max[i] = m; }

   // empty the range
   void  MakeEmpty(void)          { nrange=0; }

   // reset to a simple range
   void  Set(float mn, float mx)  { nrange=1, min[0]=mn, max[0]=mx; }

   // compress a list of segments which may contain (a,b),(b,c) -> (a,c)
   void  Compress(void);

   // print a range (useful for debugging)
   void  Print(void);

   // add the given (min,max) segment to the range
   void  AddRange(float, float);

   // union operator
   Range operator+(const Range &rhs) const
                   { Range result(*this); result+=rhs; return(result); }

   // in-place union operator
   Range& operator+=(const Range &);

   // eq
   Range& operator=(const Range &r) {
                                    nrange = r.nrange;
                                    memcpy(min, r.min, sizeof(float)*nrange);
                                    memcpy(max, r.max, sizeof(float)*nrange);
                                    return(*this);
                                   }

   // difference operator
   Range operator-(const Range &rhs) const
                   { Range result(*this); result-=rhs; return(result); }

   void diff(const Range &op1, const Range &op2)
                   { *this = op1; *this += op2; }

   // in-place difference operator
   Range& operator-=(const Range &);

   // intersection operator
   Range& operator^(const Range &);

   // equivalence
   int operator==(const Range &) const;

   // complement
   Range Complement(float min, float max);

   // Emilio: the following commented out, since it's poor style (gives a
   //	      warning and is not used anywhere in the code
#ifdef EMILIO
   // complement
   Range& Complement(const Range &r, float min, float max)
             { Range result(r); result.Complement(min,max); return(result); }
#endif /* of EMILIO */

   // true if the given range is disjoint from "this"
   int   Disjoint(const Range &) const;

protected:
   // check to see if the current range is "valid" (no min>max, etc)
   void Check(void);

private:
   // number of segments in the range
   int nrange;

   // sorted (min,max) for each segment
   float min[40], max[40];
};

#endif
