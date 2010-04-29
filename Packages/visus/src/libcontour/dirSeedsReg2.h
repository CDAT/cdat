
// $Id: dirSeedsReg2.h,v 1.1 2003/09/02 17:27:16 scorzell Exp $

#ifndef DIR_SEEDS_REG2_H
#define DIR_SEEDS_REG2_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datareg2;

class dirSeedsReg2 {
   public:
      dirSeedsReg2(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~dirSeedsReg2() {}

      void compSeeds(void);

   private:

      void dirSweep(Datareg2 &reg);

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
