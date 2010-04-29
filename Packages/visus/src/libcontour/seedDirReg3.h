
// $Id: seedDirReg3.h,v 1.1 2003/09/02 17:27:18 scorzell Exp $

#ifndef SEED_DIR_REG3_H
#define SEED_DIR_REG3_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datareg3;

class seedDirReg3 {
   public:
      seedDirReg3(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~seedDirReg3() {}

      void compSeeds(void);

   private:

      void dirSweep(Datareg3 &reg);

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
