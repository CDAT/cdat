
// $Id: respProp2.h,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#ifndef RESP_PROP2_H
#define RESP_PROP2_H

#include "range.h"
#include "seedCells.h"
#include "conplot.h"
#include "data.h"

class Datavol;
class Dataslc;

class respProp2 {
   public:
      respProp2(Data &d, SeedCells &s, Conplot &p) : data(d), seeds(s), plot(p) {}
      ~respProp2() {}

      void compSeeds(void);

   private:

      Data &data;
      SeedCells &seeds;
      Conplot   &plot;
};

#endif
