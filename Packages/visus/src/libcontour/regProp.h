
// $Id: regProp.h,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#ifndef REG_PROP_H
#define REG_PROP_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datavol;
class Dataslc;

class regProp {
   public:
      regProp(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~regProp() {}

      void compSeeds(void);

   private:

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
