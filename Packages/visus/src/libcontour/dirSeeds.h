
// $Id: dirSeeds.h,v 1.1 2003/09/02 17:27:16 scorzell Exp $

#ifndef DIR_SEEDS_H
#define DIR_SEEDS_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datavol;
class Dataslc;

class dirSeeds {
   public:
      dirSeeds(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~dirSeeds() {}

      void compSeeds(void);

   private:

      void dirSweep(Datavol &vol);
      void dirSweep(Dataslc &slc);

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
