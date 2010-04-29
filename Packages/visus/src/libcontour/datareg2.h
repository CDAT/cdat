//--------------------------------------------------------------------
//
// Datareg2d - class for a 2d regular grid of scalar data
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999
//
//--------------------------------------------------------------------

// $Id: datareg2.h,v 1.2.2.1 2008/07/23 21:44:18 cottom1 Exp $

#ifndef DATAREG2D_H
#define DATAREG2D_H

#include "data.h"

// added by Emilio: these are forward declarations necessary to avoid
//		    complaints by the new SGI C++ compiler

class dirSeedsReg2;
class regProp2;
class respProp2;
class seedChkr2;
class Conplotreg2;

//--------------------------------------------------------------------
//
// Datareg2d - a volume of scalar data.
//
//--------------------------------------------------------------------
class Datareg2 : public Data {
   public:
      // constructors and destructors

      inline Datareg2(Data::DataType, int ndata, char *rawfile);
      Datareg2(Data::DataType, int ndata, int *dim, u_char *data);
      inline ~Datareg2();


      int maxCellIndex(void) { return(index2cell(dim[0]-2, dim[1]-2)); }

      // get data or gradient approximations (by differencing)
      void getCellValues(int c, float *val)
           { int i,j;
             cell2index(c,i,j);
             getCellValues(i,j,val);
           }
      void getCellValues(int i, int j, float *val)
           {
             val[0] = getValue(index2vert(i,j));
             val[1] = getValue(index2vert(i+1,j));
             val[2] = getValue(index2vert(i+1,j+1));
             val[3] = getValue(index2vert(i,j+1));
           }

      u_int   getCellVert(int c, int v)
             { int i, j;
               cell2index(c,i,j);
               switch (v) {
                  case 0:
                     return(index2vert(i,j));
                  case 1:
                     return(index2vert(i+1,j));
                  case 2:
                     return(index2vert(i+1,j+1));
                  case 3:
                     return(index2vert(i,j+1));
               }
               return(-1);
             }

      int getNCellVerts(void) { return(4); }
      int getNCellFaces(void) { return(4); }
      int getCellAdj(int c, int f)
              { int i, j;
                cell2index(c,i,j);
                switch (f) {
                  case 0:
                     return(j==0? -1 : index2cell(i,j-1));
                  case 1:
                     return(i==dim[0]-2? -1 : index2cell(i+1,j));
                  case 2:
                     return(j==dim[1]-2? -1 : index2cell(i,j+1));
                  case 3:
                     return(i==0? -1 : index2cell(i-1,j));
                }
                return(-1);
              }

      float xCoord(int i) { return(orig[0] + i*span[0]); }
      float yCoord(int j) { return(orig[1] + j*span[1]); }

      void getCellRange(int c, float &min, float &max)
           {
              float t;
              int i;
              max = min = getValue(getCellVert(c,0));
              for (i=1; i<getNCellVerts(); i++)
                 if ((t=getValue(getCellVert(c,i))) < min)
                    min = t;
                 else if (t > max)
                    max = t;
           }
      void getFaceRange(int c, int f, float &min, float &max)
           {
              float t;
              min = max = getValue(getCellVert(c,f));
              if ((t=getValue(getCellVert(c,f<3?f+1:0))) < min)
                 min = t;
              else if (t > max)
                 max = t;
           }

      int getNFunctions(void)          { return(4); }
      float *compFunction(int, int &, float **);
      float *compFunction(int, int &, float ***, 
                          float ***, float ***){return(NULL);} // add by fan
      const char *fName(int);

   protected:
      // called by the constructor functions
      friend class Conplotreg2;

      void cell2index(int c, int &i, int &j)
          { int _left;
            i = c&xmask;
            _left = c>>xbits;
            j = _left&ymask;
          }

      int index2cell(int i, int j)
          { return((j << yshift) | i); }
            
      int index2vert(int i, int j)
          { return(i*dim[1] + j); }

      void commonConstructor(Data::DataType, int, char *);

      float *compLength(int &, float **);
      float *compArea(int &, float **);
      float *compMaxArea(int &, float **);
      float *compGradient(int &, float **);

   private:
      friend class dirSeedsReg2;
      friend class regProp2;
      friend class respProp2;
      friend class seedChkr2;

      u_int dim[2];
      float orig[2];
      float span[2];
      int xbits, ybits;
      int xmask, ymask;
      int yshift;
};

//------------------------------------------------------------------------
//
// Datareg2() - create a volume with the specified type and dimensions
//
//------------------------------------------------------------------------
inline Datareg2::Datareg2(DataType t, int ndata, char *fn)
        :Data(t, ndata, fn)
{
   commonConstructor(t, ndata, fn);
}

//------------------------------------------------------------------------
//
// ~Datareg2() - destroy a volume
//
//------------------------------------------------------------------------
inline Datareg2::~Datareg2()
{
}

#endif
