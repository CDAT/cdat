
// $Id: seedChkr2.h,v 1.1 2003/09/02 17:27:18 scorzell Exp $

#ifndef SEED_CHKR2_H
#define SEED_CHKR2_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datavol;
class Dataslc;

class seedChkr2 {
   public:
      seedChkr2(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~seedChkr2() {}

      void compSeeds(void);

   private:

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
