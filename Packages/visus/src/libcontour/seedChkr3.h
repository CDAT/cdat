
// $Id: seedChkr3.h,v 1.1 2003/09/02 17:27:18 scorzell Exp $

#ifndef SEED_CHKR3_H
#define SEED_CHKR3_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datavol;
class Dataslc;

class seedChkr3 {
   public:
      seedChkr3(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~seedChkr3() {}

      void compSeeds(void);

   private:

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
