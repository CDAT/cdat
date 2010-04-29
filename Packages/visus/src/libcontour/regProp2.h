
// $Id: regProp2.h,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#ifndef REG_PROP2_H
#define REG_PROP2_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datavol;
class Dataslc;

class regProp2 {
   public:
      regProp2(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~regProp2() {}

      void compSeeds(void);

   private:

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
