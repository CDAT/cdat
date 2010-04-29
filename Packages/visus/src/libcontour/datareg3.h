//--------------------------------------------------------------------
//
// Datareg3d - class for a 3d regular grid of scalar data
//
// Copyright (c) 1997 Dan Schikore
//--------------------------------------------------------------------

// $Id: datareg3.h,v 1.3.2.1 2008/07/23 21:44:18 cottom1 Exp $

#ifndef DATAREG3D_H
#define DATAREG3D_H


#ifdef WIN32
#pragma warning (disable:4244)
#pragma warning (disable:4018)
#endif

#include "data.h"

// added by Emilio: these are forward declarations necessary to avoid
//                  complaints by the new SGI C++ compiler

class seedChkr3;
class regProp; 
class seedDirReg3;
class Conplotreg3;

//--------------------------------------------------------------------
//
// Datareg3d - a volume of scalar data.
//
//--------------------------------------------------------------------
class Datareg3 : public Data {

   private:

      friend class regProp;
      friend class seedDirReg3;

      u_int	dim[3];				// nr of elements in x,y,z
      float	orig[3];			// position of first cell
      float	span[3];			// size in world coords

      int	xbits, ybits, zbits;
      int	xmask, ymask, zmask;
      int	yshift, zshift;

   public:				// constructors and destructors

      Datareg3(Data::DataType, int ndata=1, char *rawfile=NULL);
      Datareg3(Data::DataType, int ndata, int *dim, u_char *data);
      ~Datareg3() {}

      int maxCellIndex(void) { return(index2cell(dim[0]-2, dim[1]-2, dim[2]-2)); }

		    // get data or gradient approximations (by differencing)

	   // modify fan : f==0 is not set
      void getCellValues(int c, float *val, int f)
           { int i,j,k;
             cell2index(c,i,j,k);
             getCellValues(i,j,k,val,f);
           }
      void getCellValues(int i, int j, int k, float *val, int f)
           { val[0] = getValue(i,     j,  k  , f);
             val[1] = getValue(i+1,   j,  k  , f);
             val[2] = getValue(i+1,   j,  k+1, f);
             val[3] = getValue(i,     j,  k+1, f);
             val[4] = getValue(i,   j+1,  k  , f);
             val[5] = getValue(i+1, j+1,  k  , f);
             val[6] = getValue(i+1, j+1,  k+1, f);
             val[7] = getValue(i,   j+1,  k+1, f);
           }   
      float getValue(int i, int j, int k, int f)
	{ return(Data::getValue(index2vert(i,j,k), f)); }

      // add by fan
      void getCellValues(int c, float *val)
           { int i,j,k;
             cell2index(c,i,j,k);
             getCellValues(i,j,k,val,funcontour);
           }

      void getCellValues(int i, int j, int k, float *val)
           { val[0] = getValue(i,     j,  k  , funcontour);
             val[1] = getValue(i+1,   j,  k  , funcontour);
             val[2] = getValue(i+1,   j,  k+1, funcontour);
             val[3] = getValue(i,     j,  k+1, funcontour);
             val[4] = getValue(i,   j+1,  k  , funcontour);
             val[5] = getValue(i+1, j+1,  k  , funcontour);
             val[6] = getValue(i+1, j+1,  k+1, funcontour);
             val[7] = getValue(i,   j+1,  k+1, funcontour);
           }

      float getValue(int i, int j, int k)
            { return(Data::getValue(index2vert(i,j,k), funcontour)); }
      // end fan


      void getCellGrad(int c, float val[3][8])
           { int i,j,k;
             cell2index(c,i,j,k);
             getVertGrad(i,     j,  k  , val[0][0], val[1][0], val[2][0]);
             getVertGrad(i+1,   j,  k  , val[0][1], val[1][1], val[2][1]);
             getVertGrad(i+1,   j,  k+1, val[0][2], val[1][2], val[2][2]);
             getVertGrad(i,     j,  k+1, val[0][3], val[1][3], val[2][3]);
             getVertGrad(i,   j+1,  k  , val[0][4], val[1][4], val[2][4]);
             getVertGrad(i+1, j+1,  k  , val[0][5], val[1][5], val[2][5]);
             getVertGrad(i+1, j+1,  k+1, val[0][6], val[1][6], val[2][6]);
             getVertGrad(i,   j+1,  k+1, val[0][7], val[1][7], val[2][7]);
           }
#if 0
           { int i,j,k;
             cell2index(c,i,j,k);
             if (i == 0) {
                val[0][0] = getValue(index2vert(1, j, k)) - getValue(index2vert(0, j,  k  ));
                val[0][1] = 0.5*(getValue(index2vert(2, j, k)) - getValue(index2vert(0, j,  k  )));
                val[0][2] = 0.5*(getValue(index2vert(2,j,k+1))-getValue(index2vert(0,   j,  k+1)));
                val[0][3] = getValue(index2vert(1,j,k+1)) - getValue(index2vert(0,     j,  k+1));
                val[0][4] = getValue(index2vert(1,j+1,k)) - getValue(index2vert(0,   j+1,  k  ));
                val[0][5] = 0.5*(getValue(index2vert(2,j+1,k)) - getValue(index2vert(0, j+1,  k)));
                val[0][6] = 0.5*(getValue(index2vert(2,j+1,k+1))-getValue(index2vert(2, j+1, k+1)));
                val[0][7] = getValue(index2vert(1,j+1,k+1))-getValue(index2vert(0,   j+1,  k+1));
             }
             else if (i == dim[0]-2) {
                int n = dim[0]-1;
                int m = dim[0]-2;
                int l = dim[0]-3;
                val[0][0] = 0.5*(getValue(index2vert(n, j, k)) - getValue(index2vert(l, j,  k)));
                val[0][1] = getValue(index2vert(2, j, k)) - getValue(index2vert(0, j,  k  )));
                val[0][2] = 0.5*(getValue(index2vert(2,j,k+1))-getValue(index2vert(0,   j,  k+1)));
                val[0][3] = getValue(index2vert(1,j,k+1)) - getValue(index2vert(0,     j,  k+1));
                val[0][4] = getValue(index2vert(1,j+1,k)) - getValue(index2vert(0,   j+1,  k  ));
                val[0][5] = 0.5*(getValue(index2vert(2,j+1,k)) - getValue(index2vert(0, j+1,  k)));
                val[0][6] = 0.5*(getValue(index2vert(2,j+1,k+1))-getValue(index2vert(2, j+1, k+1)));
                val[0][7] = getValue(index2vert(1,j+1,k+1))-getValue(index2vert(0,   j+1,  k+1));
             }
             else {
             }
           }
#endif

      u_int   getCellVert(int c, int v)
             { int i, j, k;
               cell2index(c,i,j,k);
               switch (v) {
                  case 0:
                     return(index2vert(i,j,k));
                  case 1:
                     return(index2vert(i+1,j,k));
                  case 2:
                     return(index2vert(i+1,j,k+1));
                  case 3:
                     return(index2vert(i,j,k+1));
                  case 4:
                     return(index2vert(i,j+1,k));
                  case 5:
                     return(index2vert(i+1,j+1,k));
                  case 6:
                     return(index2vert(i+1,j+1,k+1));
                  case 7:
                     return(index2vert(i,j+1,k+1));
               }
               return(-1);
             }

      int getNCellVerts(void) { return(8); }
      int getNCellFaces(void) { return(6); }
      int getCellAdj(int c, int f)
              { int i, j, k;
                cell2index(c,i,j,k);
                switch (f) {
                  case 0:
                     return(j==0? -1 : index2cell(i,j-1,k));
                  case 1:
                     return(i==0? -1 : index2cell(i-1,j,k));
                  case 2:
                     return(j==dim[1]-2? -1 : index2cell(i,j+1,k));
                  case 3:
                     return(i==dim[0]-2? -1 : index2cell(i+1,j,k));
                  case 4:
                     return(k==dim[2]-2? -1 : index2cell(i,j,k+1));
                  case 5:
                     return(k==0? -1 : index2cell(i,j,k-1));
                }
                return(-1);
              }

      float xCoord(int i) { return(orig[0] + i*span[0]); }
      float yCoord(int j) { return(orig[1] + j*span[1]); }
      float zCoord(int k) { return(orig[2] + k*span[2]); }

      void getCellRange(int c, float &min, float &max)
           {
//            float t;
              float v[8];
              int i;

              getCellValues(c, v);
              max = min = v[0];
              for (i=1; i<getNCellVerts(); i++)
                 if (v[i] < min)
                    min = v[i];
                 else if (v[i] > max)
                    max = v[i];
//              max = min = getValue(getCellVert(c,0));
//              for (i=1; i<getNCellVerts(); i++)
//                 if ((t=getValue(getCellVert(c,i))) < min)
//                    min = t;
//                 else if (t > max)
//                    max = t;
           }
      void getFaceRange(int c, int f, float &min, float &max)
           {
              float t;
              int i;
              min = max = Data::getValue(getCellVert(c,cellFaceVert[f][0]));
              for (i=1; i<4; i++)
                 if ((t=Data::getValue(getCellVert(c,cellFaceVert[f][i]))) < min)
                    min = t;
                 else if (t > max)
                    max = t;
           }

	// signature functions, currently 4  functions: fan

      int	getNFunctions(void)  { return(4); } 
      float	*compFunction(int, int &, float **);
      float	*compFunction(int, int &, float ***, float ***, float ***); // add by fan
      const char *fName(int);   // signature function name

      void	getDim(u_int *v)  { memcpy(v, dim, 3 * sizeof(u_int)); }
      void	getOrig(float *v) { memcpy(v, orig, 3 * sizeof(float)); }
      void	getSpan(float *v) { memcpy(v, span, 3 * sizeof(float)); }

      int	getSlice(int variable, char axis, int index, datatypes *buffer);

   protected:				// called by the constructor functions

      friend class Conplotreg3;
      friend class seedChkr3;

      void getVertGrad(int i, int j, int k, float &gx, float &gy, float &gz) {
         if (i==0) {
            /* use right difference */
            gx = getValue(i+1, j, k) - getValue(i, j, k);
         }
         else if (i==dim[0]-1) {
            /* use left difference */
            gx = getValue(i, j, k) - getValue(i-1, j, k);
         }
         else {
            /* use central difference */
            gx = (getValue(i+1, j, k) - getValue(i-1, j, k)) * 0.5;
         }

         if (j==0) {
            gy = getValue(i, j+1, k) - getValue(i, j, k);
         }
         else if (j==dim[1]-1) {
            gy = getValue(i, j, k) - getValue(i, j-1, k);
         }
         else {
            gy = (getValue(i, j+1, k) - getValue(i, j-1, k)) * 0.5;
         }

         if (k==0) {
            gz = getValue(i, j, k+1) - getValue(i, j, k);
         }
         else if (k==dim[2]-1) {
            gz = getValue(i, j, k) - getValue(i, j, k-1);
         }
         else {
            gz = (getValue(i, j, k+1) - getValue(i, j, k-1)) * 0.5;
         }

	 gx = -gx;
	 gy = -gy;
	 gz = -gz;
      }

      static int cellFaceVert[6][4];

						// signature functions
      float *compLength(int &, float **);
      float *compArea(int &, float **);
      float *compMaxArea(int &, float **);
      float *compGradient(int &, float **);
      
					 	// add by fan: compute topology

      float *compVolRelation(int &, float ***, float ***, float ***);

      void cell2index(int c, int &i, int &j, int &k)
          { int _left;
            i = c&xmask;
            _left = c>>xbits;
            j = _left&ymask;
            _left = _left>>ybits;
            k = _left&zmask;
          }

      int index2cell(int i, int j, int k)
          { return((k << zshift) | (j << yshift) | i); }
            
      void _cell2index(int c, int &i, int &j, int &k)
          { int _left;
            i = c%(dim[0]-1);
            _left = c/(dim[0]-1);
            j = _left%(dim[1]-1);
            _left = _left/(dim[1]-1);
            k = _left;
          }

      int _index2cell(int i, int j, int k)
          { return(k*(dim[1]-1)*(dim[0]-1) + j*(dim[0]-1) + i); }
            
      int index2vert(int i, int j, int k)
          { return(k*dim[1]*dim[0] + j*dim[0] + i); }

};

#endif
