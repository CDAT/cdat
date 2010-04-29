
// $Id: rangeSweep.h,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#ifndef RANGE_SWEEP_H
#define RANGE_SWEEP_H

#include "range.h"
// #include "spqueue.h"
#include "ipqueue.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class RangeSweepRec {
   public:
//      operator <(RangeSweepRec&r2)  { return(this->cellid < r2.cellid); }
//      operator >(RangeSweepRec&r2)  { return(this->cellid > r2.cellid); }
//      operator ==(RangeSweepRec&r2) { return(this->cellid == r2.cellid); }

      int cellid;
      Range range;
};

class rangeSweep {
   public:
      rangeSweep(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~rangeSweep() {}

      void compSeeds(void);

   private:
      void PropagateRegion(int cellid, float min, float max);

//      SortedPriorityQueue<RangeSweepRec> queue;
      IndexedPriorityQueue<RangeSweepRec, double, int> queue;
      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
