
// $Id: seedAll.h,v 1.1 2003/09/02 17:27:18 scorzell Exp $

#ifndef SEED_ALL_H
#define SEED_ALL_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class seedAll {
   public:
      seedAll(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~seedAll() {}

      void compSeeds(void);

   private:

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
