//----------------------------------------------------------------
//
// seedCells.h - maintain a list of seed cells
//
// Copyright (c) 1997 Dan Schikore
//----------------------------------------------------------------

// $Id: seedCells.h,v 1.1 2003/09/02 17:27:18 scorzell Exp $

#ifndef SEED_CELLS_H
#define SEED_CELLS_H

#include <sys/types.h>

#ifndef u_int
#define u_int unsigned int
#endif

typedef struct SeedCell {
   float min, max;
   u_int cell_id;
} *SeedCellP;

class SeedCells {
   public:
     SeedCells();
     ~SeedCells();

     int    getNCells(void)    { return(ncells); }
     u_int  getCellID(int i)   { return(cells[i].cell_id); }
     float  getMin(int i)      { return(cells[i].min); }
     float  getMax(int i)      { return(cells[i].max); }
     void   Clear(void)        { ncells = 0; }
     SeedCell *getCellPointer(){ return(cells); }

     int AddSeed(u_int, float, float);
     void AddToRange(u_int i, float mn, float mx)
          {
             if (mn < cells[i].min)
                cells[i].min = mn;
             if (mx > cells[i].max)
                cells[i].max = mx;
          }

   private:
     int ncells;
     int cell_size;
     SeedCellP cells;
};

#endif
