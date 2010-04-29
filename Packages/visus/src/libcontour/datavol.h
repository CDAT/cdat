//--------------------------------------------------------------------
//
// Datavol - class for a tetrahedral volume of scalar data
//
// Copyright (c) 1997 Dan Schikore
//--------------------------------------------------------------------

// $Id: datavol.h,v 1.1.2.1 2008/07/23 21:44:13 cottom1 Exp $

#ifndef DATAVOL_H
#define DATAVOL_H

#include <math.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include "data.h"

//--------------------------------------------------------------------
//
// Datavol - a volume of scalar data.
//
//--------------------------------------------------------------------
class Datavol : public Data {
   public:
      // constructors and destructors

      inline Datavol(Data::DataType, int nd=1, char *rawfile=NULL);
      Datavol(Data::DataType t, int ndata, int nverts, int ncells,
	      double *verts, u_int *cells, int *celladj, u_char *data); 
      inline ~Datavol();

      // get data or gradient approximations (by differencing)

      u_int *getCell(int c) const
           { return(cells[c]); }
      int *getCellAdjs(int c) const
           { return(celladj[c]); }
      float *getVert(int v) const
           { return(verts[v]); }
      float *getGrad(int v) const
           { return(grad[v]); }

      void getCellValues(int c, float *val)
           { val[0] = getValue(cells[c][0]);
             val[1] = getValue(cells[c][1]);
             val[2] = getValue(cells[c][2]);
             val[3] = getValue(cells[c][3]);
           }
      u_int *getCellVerts(int c) { return(cells[c]); }
      u_int   getCellVert(int c, int v) {return(cells[c][v]); }

      void getCellGrad4(int c, float grad[4]) {
         float u[4], v[4], w[4];
         u[0] = verts[cells[c][1]][0] - verts[cells[c][0]][0];
         u[1] = verts[cells[c][1]][1] - verts[cells[c][0]][1];
         u[2] = verts[cells[c][1]][2] - verts[cells[c][0]][2];
         u[3] = getValue(cells[c][1]) - getValue(cells[c][0]);
         v[0] = verts[cells[c][2]][0] - verts[cells[c][0]][0];
         v[1] = verts[cells[c][2]][1] - verts[cells[c][0]][1];
         v[2] = verts[cells[c][2]][2] - verts[cells[c][0]][2];
         v[3] = getValue(cells[c][2]) - getValue(cells[c][0]);
         w[0] = verts[cells[c][3]][0] - verts[cells[c][0]][0];
         w[1] = verts[cells[c][3]][1] - verts[cells[c][0]][1];
         w[2] = verts[cells[c][3]][2] - verts[cells[c][0]][2];
         w[3] = getValue(cells[c][3]) - getValue(cells[c][0]);
         grad[0] = u[1]*(v[2]*w[3]-v[3]*w[2]) +
                   u[2]*(v[3]*w[1]-v[1]*w[3]) +
                   u[3]*(v[1]*w[2]-v[2]*w[1]);
         grad[1] = u[0]*(v[2]*w[3]-v[3]*w[2]) +
                   u[2]*(v[3]*w[0]-v[0]*w[3]) +
                   u[3]*(v[0]*w[2]-v[2]*w[0]);
         grad[2] = u[0]*(v[1]*w[3]-v[3]*w[1]) +
                   u[1]*(v[3]*w[0]-v[0]*w[3]) +
                   u[3]*(v[0]*w[1]-v[1]*w[0]);
         grad[3] = u[0]*(v[1]*w[2]-v[2]*w[1]) +
                   u[1]*(v[2]*w[0]-v[0]*w[2]) +
                   u[2]*(v[0]*w[1]-v[1]*w[0]);
if (grad[3] == 0.0) {
   printf("hmm\n");
   printf("u = %f %f %f %f\n", u[0], u[1], u[2], u[3]);
   printf("v = %f %f %f %f\n", v[0], v[1], v[2], v[3]);
   printf("w = %f %f %f %f\n", w[0], w[1], w[2], w[3]);
   printf("v0 = %f %f %f %f\n", verts[cells[c][0]][0], verts[cells[c][0]][1], verts[cells[c][0]][2], getValue(cells[c][0]));
   printf("v1 = %f %f %f %f\n", verts[cells[c][1]][0], verts[cells[c][1]][1], verts[cells[c][1]][2], getValue(cells[c][1]));
   printf("v2 = %f %f %f %f\n", verts[cells[c][2]][0], verts[cells[c][2]][1], verts[cells[c][2]][2], getValue(cells[c][2]));
   printf("v3 = %f %f %f %f\n", verts[cells[c][3]][0], verts[cells[c][3]][1], verts[cells[c][3]][2], getValue(cells[c][3]));
   printf("tet=%d %d %d %d\n", cells[c][0], cells[c][1], cells[c][2], cells[c][3]);
//   sleep(4);
}
      }


      virtual int   getNCellVerts(void) { return(4); }
      virtual int   getNCellFaces(void) { return(4); }
      virtual int   getCellAdj(int c, int f) { return(celladj[c][f]); }

      virtual void  getCellRange(int c, float &min, float &max)
                    {
                       float t;
                       max = min = getValue(cells[c][0]);
                       if ((t=getValue(cells[c][1])) < min)
                          min = t;
                       if (t > max)
                          max = t;
                       if ((t=getValue(cells[c][2])) < min)
                          min = t;
                       if (t > max)
                          max = t;
                       if ((t=getValue(cells[c][3])) < min)
                          min = t;
                       if (t > max)
                          max = t;
                    }
      virtual void  getFaceRange(int c, int f, float &min, float &max)
           {
              float t;
              int i;
              min = 1e10;
              max = -1e10;
              for (i=0; i<getNCellVerts(); i++)
                 if (i != f) {
                    if ((t=getValue(cells[c][i])) < min)
                       min = t;
                    if (t > max)
                       max = t;
                 }
           }

      int getNFunctions(void) { return(4); }
      float *compFunction(int, int &, float **);
      float *compFunction(int, int &, float ***,
                          float ***, float ***){return(NULL);} // add by fan
 
      const char *fName(int);

   protected:
      // called by the constructor functions
      inline void commonConstructor(Data::DataType, int, char *);

      float *compLength(int&, float**);		// 
      float *compArea(int&, float**);
      float *compMaxArea(int&, float**);
      float *compGradient(int&, float**);

      void compGrad(void);			// to compute gradients (?)

   private:
      float (*grad)[3];    		// gradients for each vertex
      float (*verts)[3];		// array of mesh vertices
      u_int (*cells)[4];		// cells (tetrahedra) in the mesh
      int   (*celladj)[4];		// indices to adjacent mesh cells
};

//------------------------------------------------------------------------
//
// Datavol() - create a volume with the specified type and dimensions
//            with data taken from the given file
//
//------------------------------------------------------------------------
inline Datavol::Datavol(Data::DataType t, int nd, char *fn)
        :Data(t, nd, fn)
{
   commonConstructor(t, nd, fn);
}

#define normalize(v) { \
                       float len; \
                       len = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]); \
                       if (len != 0.0) { \
                          v[0] /= len; \
                          v[1] /= len; \
                          v[2] /= len; \
                       } \
                     }

inline void
Datavol::compGrad(void)
{
   int i;
   float u[4], v[4], w[4], x[4], y[4], z[4], g[4];
   int v0, v1, v2, v3;
   float len;
   float weight;

   memset(grad, 0, sizeof(float[3])*getNVerts());

   for (i=0; i<getNCells(); i++) {
//printf("grad for cell %d\n", i);
      v0 = cells[i][0];
      v1 = cells[i][1];
      v2 = cells[i][2];
      v3 = cells[i][3];
      u[0] = verts[v1][0] - verts[v0][0];
      u[1] = verts[v1][1] - verts[v0][1];
      u[2] = verts[v1][2] - verts[v0][2];
      u[3] = getValue(v1) - getValue(v0);
      v[0] = verts[v2][0] - verts[v0][0];
      v[1] = verts[v2][1] - verts[v0][1];
      v[2] = verts[v2][2] - verts[v0][2];
      v[3] = getValue(v2) - getValue(v0);
      w[0] = verts[v3][0] - verts[v0][0];
      w[1] = verts[v3][1] - verts[v0][1];
      w[2] = verts[v3][2] - verts[v0][2];
      w[3] = getValue(v3) - getValue(v0);

      x[0] = verts[v3][0] - verts[v1][0];
      x[1] = verts[v3][1] - verts[v1][1];
      x[2] = verts[v3][2] - verts[v1][2];
      y[0] = verts[v3][0] - verts[v2][0];
      y[1] = verts[v3][1] - verts[v2][1];
      y[2] = verts[v3][2] - verts[v2][2];
      z[0] = verts[v2][0] - verts[v1][0];
      z[1] = verts[v2][1] - verts[v1][1];
      z[2] = verts[v2][2] - verts[v1][2];

      g[0] =   u[1] * (v[2]*w[3] - v[3]*w[2])
             + u[2] * (v[3]*w[1] - v[1]*w[3])
             + u[3] * (v[1]*w[2] - v[2]*w[1]);
      g[1] =   u[0] * (v[2]*w[3] - v[3]*w[2])
             + u[2] * (v[3]*w[0] - v[0]*w[3])
             + u[3] * (v[0]*w[2] - v[2]*w[0]);
      g[2] =   u[0] * (v[1]*w[3] - v[3]*w[1])
             + u[1] * (v[3]*w[0] - v[0]*w[3])
             + u[3] * (v[0]*w[1] - v[1]*w[0]);
//printf(" grad %f %f %f\n", g[0], g[1], g[2]);

#if 0
{
   len = sqrt(g[0]*g[0] + g[1]*g[1] + g[2]*g[2]);
   if (len != 0.0) {
      g[0]/=len;
      g[1]/=len;
      g[2]/=len;
   }
}
   normalize(u);
   normalize(v);
   normalize(w);
   normalize(x);
   normalize(y);
   normalize(z);
#endif

if (v0 == 101 || v1 == 101 || v2 == 101 || v3 == 101)
   printf("v100: %f %f %f\n", g[0], g[1], g[2]);

      weight =   u[0]*(v[1]*w[2] - v[2]*w[1])
               - u[1]*(v[2]*w[0] - v[0]*w[2])
               + u[2]*(v[0]*w[1] - v[1]*w[0]);
weight=1;
      grad[v0][0] += g[0] * weight;
      grad[v0][1] += g[1] * weight;
      grad[v0][2] += g[2] * weight;

      weight =   (-u[0])*(x[1]*z[2] - x[2]*z[1])
               - (-u[1])*(x[2]*z[0] - x[0]*z[2])
               + (-u[2])*(x[0]*z[1] - x[1]*z[0]);
weight=1;
      grad[v1][0] += g[0] * weight;
      grad[v1][1] += g[1] * weight;
      grad[v1][2] += g[2] * weight;

      weight =   (-v[0])*((-z[1])*y[2] - (-z[2])*y[1])
               - (-v[1])*((-z[2])*y[0] - (-z[0])*y[2])
               + (-v[2])*((-z[0])*y[1] - (-z[1])*y[0]);
weight=1;
      grad[v2][0] += g[0] * weight;
      grad[v2][1] += g[1] * weight;
      grad[v2][2] += g[2] * weight;

      weight =   (-w[0])*((-y[1])*(-x[2]) - (-y[2])*(-x[1]))
               - (-w[1])*((-y[2])*(-x[0]) - (-y[0])*(-x[2]))
               + (-w[2])*((-y[0])*(-x[1]) - (-y[1])*(-x[0]));
weight=1;
      grad[v3][0] += g[0] * weight;
      grad[v3][1] += g[1] * weight;
      grad[v3][2] += g[2] * weight;
   }

   for (i=0; i<getNVerts(); i++) {
//printf("scaling vgrad %d\n", i);
      len = sqrt(grad[i][0]*grad[i][0] + grad[i][1]*grad[i][1] +
                 grad[i][2]*grad[i][2]);
      if (len != 0.0) {
         grad[i][0] /= len;
         grad[i][1] /= len;
         grad[i][2] /= len;
      }
   }
   printf("grad101 = %f %f %f\n", grad[101][0], grad[101][1], grad[101][2]);
}

//------------------------------------------------------------------------
//
// commonConstructor() - called by the constructors to initialize a
//                       volume
//
//------------------------------------------------------------------------
inline void
Datavol::commonConstructor(Data::DataType t, int nd, char *fn)
{
   int i;

   verts = (float (*)[3])malloc(sizeof(float[3])*getNVerts());
   cells = (u_int (*)[4])malloc(sizeof(u_int[4])*getNCells());
   celladj = (int (*)[4])malloc(sizeof(int[4])*getNCells());
   grad = (float (*)[3])malloc(sizeof(float[3])*getNVerts());

   fread(verts, sizeof(float[3]), getNVerts(), fp);
   for (i=0; i<getNCells(); i++) {
      fread(cells[i], sizeof(u_int[4]), 1, fp);
      fread(celladj[i], sizeof(int[4]), 1, fp);
if (cells[i][0] == 100 ||
    cells[i][1] == 100 ||
    cells[i][2] == 100 ||
    cells[i][3] == 100) {
   printf("%d %d %d %d\n", cells[i][0], cells[i][1], cells[i][2], cells[i][3]);
}
if (cells[i][0] == 101 ||
    cells[i][1] == 101 ||
    cells[i][2] == 101 ||
    cells[i][3] == 101) {
   printf("%d %d %d %d\n", cells[i][0], cells[i][1], cells[i][2], cells[i][3]);
}

#if 0
printf("cell %d: %d %d %d %d (%d %d %d %d)\n", i,
 cells[i][0],
 cells[i][1],
 cells[i][2],
 cells[i][3],
 celladj[i][0],
 celladj[i][1],
 celladj[i][2],
 celladj[i][3]);
#endif
   }

   for (i=0; i<getNCells(); i++) {
      for (int j=0; j<getNCellFaces(); j++) {
         int adj = celladj[i][j];
         int same = 0;
         if (adj != -1) {
         for (int k=0; k<4; k++)
            for (int l=0; l<4; l++)
               if (cells[i][k] == cells[adj][l])
                  same++;
         if (same != 3)
            printf("cell %d (%d %d %d %d) not adj to %d (%d %d %d %d)\n",
                   i, cells[i][0], cells[i][1], cells[i][2], cells[i][3],
                 adj, cells[adj][0], cells[adj][1], cells[adj][2], cells[adj][3]);
        }
      }
   }

   readData();

   compGrad();
}

//------------------------------------------------------------------------
//
// ~Datavol() - destroy a volume
//
//------------------------------------------------------------------------

inline Datavol::~Datavol()
{
    if (filename)
	{
	free(verts);
	free(cells);
	free(celladj);
	}
}

#endif
